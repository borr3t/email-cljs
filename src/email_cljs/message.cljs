(ns email-cljs.message
  "Messages as data: parsing an RFC 822 message (string / Bytes / a fetched
   IMAP message map with :source) into ONE kebab map through mailparser,
   building the RFC 822 bytes of a kebab message map through nodemailer's
   MailComposer without a transport (drafts, Sent copies for imap/append!,
   tests), and the pure helpers that work on the parsed shapes.

   Interop-free: mailparser is called by email-cljs.parse-binding
   (simple-parse, which returns raw CLJS data with opaque Buffers), the
   composer by email-cljs.smtp-binding (compose); this namespace only
   normalises data. The parsed shape:

     {:subject \"..\" :text \"..\" :html \"..\" :text-as-html \"..\"
      :from {:name .. :address ..} :from-all [..] :sender {..}|nil
      :reply-to [..] :to [..] :cc [..] :bcc [..]            ; [{:name :address}] (:group on members)
      :date \"1997-11-21T15:55:06.000Z\"|nil :date-raw \"Fri, 21 Nov ..\"|nil
      :message-id \"<..>\"|nil :in-reply-to \"<..>\"|nil :references [\"<..>\" ..]
      :priority :high|:normal|:low
      :headers {\"Content-Type\" {:value .. :params {..}} \"X-Custom-Header\" \"v1\" ..}
      :header-keys [\"X-Custom-Header\" ..] :header-lines [{:key :line} ..]
      :attachments [{:filename :content-type :content-disposition :attachment|:inline
                     :cid :content-id :related? :inline? :part-id :size :checksum
                     :checksum-algo :content <Bytes> :headers {..}} ..]}

   Header names are canonical-cased (\"Message-ID\", \"X-Custom-Header\";
   `header` looks them up case-insensitively). Address header values are
   vectors of {:name :address}; groups are flattened into their members
   with :group.
   Attachment content is a Bytes wrapper (email-cljs.attachment) — base64
   on demand via attachment->base64, never eagerly.

   Streaming parse (mailparser's MailParser stream class) is NOT exposed:
   it needs a Node Readable as input, which no interop-free caller can
   produce, and imap/download! already drains parts to Bytes with a
   :max-bytes cap. parse-message! takes a string or Bytes."
  (:require [clojure.string :as str]
            [email-cljs.attachment :as att]
            [email-cljs.parse-binding :as pb]
            [email-cljs.smtp :as smtp]
            [email-cljs.smtp-binding :as sb]
            [email-cljs.util :as util]
            [promesa.core :as p]))

;; ---------------------------------------------------------------------------
;; Header names
;; ---------------------------------------------------------------------------

(def header-acronyms
  "Header names whose canonical spelling is not Initial-Caps per word."
  {"message-id" "Message-ID" "content-id" "Content-ID" "mime-version" "MIME-Version"
   "dkim-signature" "DKIM-Signature" "arc-seal" "ARC-Seal"
   "arc-message-signature" "ARC-Message-Signature"
   "arc-authentication-results" "ARC-Authentication-Results"
   "x-ms-exchange-organization-authas" "X-MS-Exchange-Organization-AuthAs"
   "list-id" "List-ID" "x-mailer" "X-Mailer" "x-msmail-priority" "X-MSMail-Priority"
   "x-original-to" "X-Original-To" "x-spam-status" "X-Spam-Status"})

(defn canonical-header-name
  "Pure: a header name in any case -> its canonical wire spelling
   (\"x-custom-header\" -> \"X-Custom-Header\", \"message-id\" ->
   \"Message-ID\")."
  [k]
  (let [lower (str/lower-case (name k))]
    (or (get header-acronyms lower) (util/normalize-header-name lower))))

(defn header
  "Pure: the value of header `name` (any case, keyword or string) in a
   parsed message (or any map with :headers), nil when absent."
  [parsed name]
  (get (:headers parsed) (canonical-header-name name)))

;; ---------------------------------------------------------------------------
;; Normalisation of the raw simple-parse data (pure)
;; ---------------------------------------------------------------------------

(defn- blank->nil
  [s]
  (when-not (str/blank? (str s)) s))

(defn- flatten-addresses
  "mailparser address entries ({:address :name} or {:name :group [..]}) ->
   [{:name :address (:group)}]."
  [entries]
  (into []
        (mapcat (fn [{:keys [address name group]}]
                  (if (sequential? group)
                    (map (fn [m] (assoc m :group (blank->nil name))) (flatten-addresses group))
                    [{:name (blank->nil name) :address address}])))
        entries))

(defn- address-header?
  [lower]
  (contains? #{"from" "to" "cc" "bcc" "sender" "reply-to" "delivered-to" "return-path" "disposition-notification-to"} lower))

(defn- address-field
  "A mailparser address header value ({:value [..] :text :html}) -> vector
   of address maps; nil / missing -> []."
  [v]
  (cond
    (nil? v) []
    (and (map? v) (sequential? (:value v))) (flatten-addresses (:value v))
    (sequential? v) (flatten-addresses v)
    :else []))

(defn- header-value
  "Normalises one header value by its (lower-case) name: addresses to
   vectors of address maps, structured values keep {:value :params},
   everything else as mailparser left it (string, vector, ISO date)."
  [lower v]
  (cond
    (address-header? lower) (address-field v)
    (and (map? v) (contains? v :value) (contains? v :params)) v
    :else v))

(defn- normalize-headers
  [raw]
  (into {}
        (map (fn [[k v]] [(canonical-header-name k) (header-value (str/lower-case (str k)) v)]))
        raw))

(defn- header-lines
  [raw]
  (mapv (fn [{:keys [key line]}] {:key (canonical-header-name (str key)) :line line}) (or raw [])))

(defn- raw-header-value
  "The text after `Name:` in the first raw header line for `lower` (folded
   lines unfolded), or nil."
  [lines lower]
  (some (fn [{:keys [key line]}]
          (when (= lower (str/lower-case (str key)))
            (-> (str line) (str/replace #"^[^:]*:\s*" "") (str/replace #"\r?\n\s*" " ") str/trim)))
        lines))

(defn- priority-keyword
  [v]
  (case (some-> v str str/lower-case)
    "high" :high
    "low" :low
    :normal))

(defn- attachment-map
  [a]
  (let [disposition (some-> (:content-disposition a) str/lower-case keyword)
        content (att/bytes (:content a))]
    {:filename (:filename a)
     :content-type (:content-type a)
     :content-disposition (or disposition :attachment)
     :cid (:cid a)
     :content-id (:content-id a)
     :related? (boolean (:related a))
     :inline? (= :inline disposition)
     :part-id (:part-id a)
     :size (or (:size a) (when content (att/size content)))
     :checksum (:checksum a)
     :checksum-algo (:checksum-algo a)
     :content content
     :headers (normalize-headers (:headers a))}))

(defn normalize-parsed
  "Pure: the raw simple-parse data (mailparser's shapes with kebab keys)
   -> the library's parsed-message map (see the namespace docstring).
   `checksum-algo` names the digest mailparser used. A Date: header that
   did not parse is reported as :date nil + :date-raw (mailparser itself
   substitutes 'now')."
  [raw checksum-algo]
  (let [lines (header-lines (:header-lines raw))
        headers (normalize-headers (:headers raw))
        raw-date (raw-header-value lines "date")
        parsed-date (when raw-date (pb/parse-date-iso raw-date))
        from (address-field (:from raw))
        references (:references raw)
        ;; mailparser folds X-Priority / X-MSMail-Priority / Importance / Priority
        ;; into ONE headers entry named "priority" and never
        ;; copies it to the top level (6.1 was wrong about that)
        priority (or (:priority raw) (get (:headers raw) "priority"))]
    {:subject (:subject raw)
     :text (:text raw)
     :html (:html raw)
     :text-as-html (:text-as-html raw)
     :from (first from)
     :from-all from
     :sender (first (address-field (:sender raw)))
     :reply-to (address-field (:reply-to raw))
     :to (address-field (:to raw))
     :cc (address-field (:cc raw))
     :bcc (address-field (:bcc raw))
     :date parsed-date
     :date-raw raw-date
     :message-id (:message-id raw)
     :in-reply-to (:in-reply-to raw)
     :references (cond (nil? references) [] (string? references) [references] :else (vec references))
     :priority (priority-keyword priority)
     :headers headers
     :header-keys (into [] (distinct) (map :key lines))
     :header-lines lines
     :attachments (mapv #(attachment-map (assoc % :checksum-algo checksum-algo)) (:attachments raw))}))

;; ---------------------------------------------------------------------------
;; parse-message!
;; ---------------------------------------------------------------------------

(def parse-option-keys
  "Options parse-message! accepts."
  #{:keep-cid-links? :skip-html-to-text? :skip-text-to-html? :skip-image-links?
    :skip-text-links? :max-html-length :checksum-algo :keep-delivery-status?})

(defn parse-options->wire
  "Pure: kebab parse options -> the string-keyed camelCase mailparser
   option map. Defaults: keepCidLinks true (html keeps its cid: links and
   the attachment carries :cid — the caller decides whether to inline),
   checksumAlgo md5. Throws :argument-error for unknown keys."
  [opts]
  (let [opts (or opts {})
        unknown (remove parse-option-keys (keys opts))]
    (when (seq unknown)
      (throw (util/argument-error (str "unknown parse option(s) " (pr-str (vec unknown)))
                                  {:reason :unknown-option :keys (vec unknown)})))
    (->> {"keepCidLinks" (if (contains? opts :keep-cid-links?) (boolean (:keep-cid-links? opts)) true)
          "skipHtmlToText" (:skip-html-to-text? opts)
          "skipTextToHtml" (:skip-text-to-html? opts)
          "skipImageLinks" (:skip-image-links? opts)
          "skipTextLinks" (:skip-text-links? opts)
          "maxHtmlLengthToParse" (:max-html-length opts)
          "checksumAlgo" (name (or (:checksum-algo opts) :md5))
          "keepDeliveryStatus" (:keep-delivery-status? opts)}
         (remove (comp nil? val))
         (into {}))))

(defn- source-of
  "string | Bytes | {:source Bytes|string} -> the value handed to the
   binding (string or opaque Buffer). Throws :argument-error otherwise."
  [source]
  (cond
    (string? source) source
    (att/bytes? source) (att/unwrap source)
    (and (map? source) (some? (:source source))) (source-of (:source source))
    :else (throw (util/argument-error "parse-message! needs a string, Bytes or a fetched message map with :source"
                                      {:reason :bad-source :value-type (pr-str (type source))}))))

(defn parse-message!
  "Parses an RFC 822 message — a string, Bytes (imap/source!, a
   :source from fetch-one! with {:source? true}, build's :source) or a
   fetched message map carrying :source — with mailparser. Promise of the
   parsed map described in the namespace docstring. Options (kebab, see
   parse-options->wire): :keep-cid-links? (default true) :skip-html-to-text?
   :skip-text-to-html? :skip-image-links? :skip-text-links?
   :max-html-length :checksum-algo (:md5 default, :sha1, :sha256)
   :keep-delivery-status?. The whole message and every attachment are held
   in memory. Rejects with :email-cljs/parse-error
   {:reason :mailparser :message ..} or :argument-error."
  ([source] (parse-message! source nil))
  ([source opts]
   (-> (p/then (p/resolved nil)
               (fn [_]
                 (let [wire (parse-options->wire opts)
                       algo (keyword (or (get wire "checksumAlgo") "md5"))]
                   (p/then (pb/simple-parse (source-of source) wire)
                           (fn [raw] (normalize-parsed raw algo))))))
       (p/catch (fn [e]
                  (if (util/error? e)
                    (p/rejected e)
                    (p/rejected (util/parse-error (or (ex-message e) (str e))
                                                  {:reason :mailparser :message (or (ex-message e) (str e))}))))))))

;; ---------------------------------------------------------------------------
;; Helpers on the parsed shapes (pure)
;; ---------------------------------------------------------------------------

(defn format-parsed-address
  "Pure: a parsed address map {:name :address} -> \"Name <a@b.c>\" (name
   quoted when it contains specials) or \"a@b.c\"; nil -> nil."
  [addr]
  (when (some? addr) (util/format-address addr)))

(defn format-parsed-addresses
  "Pure: a vector of parsed address maps -> \"a@b.c, Name <d@e.f>\"."
  [addrs]
  (str/join ", " (keep format-parsed-address addrs)))

(defn attachment->base64
  "Pure: the base64 string of a parsed attachment's :content (computed on
   demand)."
  [a]
  (some-> (:content a) att/->base64))

(defn- node-encoding
  "The Node encoding name for a MIME charset when Node decodes it natively
   (utf-8, us-ascii, iso-8859-1 / latin1, utf-16le); nil otherwise."
  [charset]
  (case (some-> charset str str/lower-case (str/replace #"[_\s]" "-"))
    ("utf-8" "utf8") "utf8"
    ("us-ascii" "ascii") "ascii"
    ("iso-8859-1" "latin1" "latin-1" "windows-1252" "cp1252") "latin1"
    ("utf-16le" "utf16le" "ucs-2" "ucs2") "utf16le"
    nil))

(defn attachment-charset
  "Pure: the charset parameter of a parsed attachment's Content-Type
   (\"utf-8\" ..), nil when absent."
  [a]
  (or (get-in (header a "content-type") [:params "charset"])
      (get-in (header a "content-type") [:params :charset])))

(defn attachment->string
  "Pure: the text of a parsed attachment's :content. The charset comes
   from the part's Content-Type (or `encoding`, a Node encoding name) when
   Node decodes it natively; anything else falls back to UTF-8 (mailparser
   converts TEXT BODIES to UTF-8 but leaves attachment bytes as sent)."
  ([a] (attachment->string a nil))
  ([a encoding]
   (some-> (:content a) (att/->string (or encoding (node-encoding (attachment-charset a)) "utf8")))))

(defn attachment-of
  "Pure: the first attachment whose :cid, :content-id or :filename equals
   `id` (a bare cid, a bracketed Content-ID or a filename), nil when none."
  [parsed id]
  (let [id (str id)
        bare (str/replace id #"^<|>$" "")]
    (some (fn [a] (when (or (= bare (:cid a)) (= id (:content-id a)) (= id (:filename a))) a))
          (:attachments parsed))))

(defn inline-attachments
  "Pure: the attachments referenced from the HTML body (cid + inline
   disposition or a multipart/related ancestor)."
  [parsed]
  (filterv (fn [a] (and (:cid a) (or (:inline? a) (:related? a)))) (:attachments parsed)))

(defn file-attachments
  "Pure: the attachments that are NOT inline images (the complement of
   inline-attachments)."
  [parsed]
  (let [inline (set (map :part-id (inline-attachments parsed)))]
    (filterv (fn [a] (not (contains? inline (:part-id a)))) (:attachments parsed))))

(defn text-or-html
  "Pure: the plain-text body, else the HTML body, else nil."
  [parsed]
  (or (blank->nil (:text parsed)) (blank->nil (:html parsed))))

;; ---------------------------------------------------------------------------
;; build — RFC 822 bytes without a transport
;; ---------------------------------------------------------------------------

(def ^:private priority-headers
  {"high" {"X-Priority" "1 (Highest)" "X-MSMail-Priority" "High" "Importance" "High"}
   "low" {"X-Priority" "5 (Lowest)" "X-MSMail-Priority" "Low" "Importance" "Low"}
   "normal" {"X-Priority" "3 (Normal)" "X-MSMail-Priority" "Normal" "Importance" "Normal"}})

(defn build-wire
  "Pure: a kebab message map -> the string-keyed camelCase MailComposer
   map: smtp/message->wire plus what nodemailer's mailer (not the
   composer) would add — the X-Priority / X-MSMail-Priority / Importance
   headers for :priority, disableFileAccess / disableUrlAccess from the
   policy, `newline` (:unix / :windows) and `keepBcc` (:keep-bcc? — the
   composer DROPS the Bcc: header by default, as the SMTP transport does).
   Throws :argument-error like message->wire. A message with only a :from
   and no recipients is allowed when {:allow-no-recipients? true}
   (drafts)."
  ([message] (build-wire message nil))
  ([message {:keys [policy newline allow-no-recipients? keep-bcc?]}]
   (let [wire (smtp/message->wire message (cond-> (or policy {})
                                             allow-no-recipients? (assoc :allow-no-recipients? true)))
         prio (get wire "priority")]
     (cond-> (assoc wire
                    "disableFileAccess" (not (:allow-file-access? policy))
                    "disableUrlAccess" (not (:allow-url-access? policy)))
       prio (update "headers" #(merge (get priority-headers prio) %))
       keep-bcc? (assoc "keepBcc" true)
       newline (assoc "newline" (name newline))))))

(defn build
  "Builds the RFC 822 bytes of a kebab message map (the shape send! takes)
   with nodemailer's MailComposer and NO transport: promise of {:source
   <Bytes> :message-id \"<..>\" :envelope {:from :to [..]} :size n}. For
   imap/append! (drafts, Sent copies — SMTP never stores one), tests, and
   parse-message! round trips. Options: :policy
   {:allow-file-access? :allow-url-access?} (default: both refused, as for
   a client without the flags), :newline :unix|:windows (CRLF default),
   :allow-no-recipients? true for drafts, :keep-bcc? true to WRITE the
   Bcc: header into the bytes (MailComposer strips it by default, like the
   SMTP transport; the Bcc recipients are always in :envelope :to).
   Rejects with :argument-error / :parse-error."
  ([message] (build message nil))
  ([message opts]
   (-> (p/then (p/resolved nil) (fn [_] (sb/compose (build-wire message opts))))
       (p/catch (fn [e]
                  (if (util/error? e)
                    (p/rejected e)
                    (p/rejected (util/parse-error (str "cannot build message: " (or (ex-message e) (str e)))
                                                  {:reason :compose :message (or (ex-message e) (str e))}))))))))

(defn build-string
  "build, resolving to the RFC 822 text (UTF-8) instead of the map; LF
   line endings by default (:newline :unix) for readable fixtures."
  ([message] (build-string message {:newline :unix}))
  ([message opts]
   (p/then (build message (merge {:newline :unix} opts)) (fn [r] (att/->string (:source r))))))
