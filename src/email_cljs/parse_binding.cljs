(ns email-cljs.parse-binding
  "Binding namespace (JavaScript interop lives here): the Node Buffer, fs,
   os and process.env touches the rest of the library needs — the carrier
   behind the opaque Bytes wrapper (email-cljs.attachment), the secrets-file
   read and the environment map behind util/from-env, and (Phase 4) the
   mailparser call behind email-cljs.message/parse-message! (inventory
   14.4 keeps the binding namespaces at three: smtp-binding, imap-binding,
   parse-binding).

   Everything exported takes and returns CLJS data or an opaque Buffer /
   stream handle that callers never touch; no other namespace outside the
   bindings mentions Buffer. Buffers inside the parsed-mail data returned
   by simple-parse are left as opaque Buffers for email-cljs.message to
   wrap into Bytes (email-cljs.attachment depends on this namespace, so the
   wrapping cannot happen here)."
  (:require ["fs" :as fs]
            ["mailparser" :refer (simpleParser)]
            ["os" :as os]
            ["path" :as path]
            [clojure.string :as str]
            [promesa.core :as p]))

;; ---------------------------------------------------------------------------
;; Buffer
;; ---------------------------------------------------------------------------

(defn buffer?
  "True when `x` is a Node Buffer."
  [x]
  (and (some? x) (js/Buffer.isBuffer x)))

(defn uint8-array?
  "True when `x` is a Uint8Array (or ArrayBuffer view) that is not a Buffer."
  [x]
  (and (some? x) (instance? js/Uint8Array x) (not (buffer? x))))

(defn buffer-from-base64
  "Decodes a base64 string (standard or URL-safe alphabet) into a Buffer."
  [s]
  (js/Buffer.from (str s) "base64"))

(defn buffer-from-string
  "Encodes a string into a Buffer; `encoding` is a Node encoding name
   (\"utf8\" default, \"latin1\", \"ascii\", \"utf16le\", \"hex\", \"base64\")."
  [s encoding]
  (js/Buffer.from (str s) (or encoding "utf8")))

(defn buffer-from-ints
  "A Buffer from a sequential collection of byte values (0-255)."
  [ints]
  (js/Buffer.from (into-array ints)))

(defn buffer-from-uint8
  "A Buffer sharing the memory of a Uint8Array."
  [u]
  (js/Buffer.from u))

(defn buffer->base64
  "Standard base64 string of the Buffer."
  [b]
  (js-invoke b "toString" "base64"))

(defn buffer->string
  "Decodes the Buffer as text; `encoding` as for buffer-from-string
   (iconv charsets are Phase 4)."
  [b encoding]
  (js-invoke b "toString" (or encoding "utf8")))

(defn buffer->ints
  "Vector of byte values."
  [b]
  (vec (js/Array.from b)))

(defn buffer-length
  "Byte length."
  [b]
  (unchecked-get b "length"))

(defn buffer-equals?
  "Byte-wise equality of two Buffers."
  [a b]
  (js-invoke a "equals" b))

(defn buffer-concat
  "One Buffer from a sequence of Buffers."
  [bufs]
  (js/Buffer.concat (into-array bufs)))

(defn buffer-slice
  "A copy of bytes [start, end) (end defaults to the length)."
  [b start end]
  (js/Buffer.from (js-invoke b "subarray" start (or end (buffer-length b)))))

(defn stream->buffer
  "Drains a Node Readable into one Buffer. Promise; rejects with the stream's
   error. `max-bytes` (optional) rejects with an :email-cljs/argument-error-free
   plain js/Error tagged code \"DownloadOverflow\" once exceeded so the
   caller's error mapping treats it like imapflow's own overflow."
  ([readable] (stream->buffer readable nil))
  ([readable max-bytes]
   (p/create
    (fn [resolve reject]
      (let [chunks (array)
            total (atom 0)]
        (js-invoke readable "on" "data"
                   (fn [chunk]
                     (let [chunk (if (buffer? chunk) chunk (js/Buffer.from chunk))]
                       (swap! total + (buffer-length chunk))
                       (if (and max-bytes (> @total max-bytes))
                         (let [err (js/Error. (str "download exceeded " max-bytes " bytes"))]
                           (unchecked-set err "code" "DownloadOverflow")
                           (when (fn? (unchecked-get readable "destroy")) (js-invoke readable "destroy"))
                           (reject err))
                         (.push chunks chunk)))))
        (js-invoke readable "on" "error" (fn [err] (reject err)))
        (js-invoke readable "on" "end" (fn [] (resolve (js/Buffer.concat chunks)))))))))

;; ---------------------------------------------------------------------------
;; Environment and files (util/from-env, secrets file)
;; ---------------------------------------------------------------------------

(defn env-map
  "process.env as a {\"NAME\" \"value\"} map (walked key by key: process.env
   is an exotic object js->clj leaves untouched)."
  []
  (let [env (unchecked-get js/process "env")]
    (into {} (map (fn [k] [k (unchecked-get env k)])) (js/Object.keys env))))

(defn expand-home
  "Replaces a leading `~` with the home directory."
  [p]
  (let [p (str p)]
    (if (or (= p "~") (.startsWith p "~/"))
      (path/join (os/homedir) (subs p 1))
      p)))

(defn read-file-text
  "The UTF-8 text of a file (`~` expanded), or nil when it does not exist or
   cannot be read. Synchronous: only used for configuration at start-up."
  [p]
  (when (and (some? p) (not= "" (str p)))
    (try
      (fs/readFileSync (expand-home p) "utf8")
      (catch :default _ nil))))

(defn file-exists?
  "True when the path (`~` expanded) exists."
  [p]
  (try (fs/existsSync (expand-home p)) (catch :default _ false)))

(defn read-file-buffer
  "Promise of the Buffer with the whole content of a file (`~` expanded);
   rejects with the Node error (ENOENT, EACCES ...)."
  [p]
  (p/create (fn [resolve reject]
              (fs/readFile (expand-home p) (fn [err buf] (if err (reject err) (resolve buf)))))))

(defn write-file-buffer
  "Promise writing a Buffer to a file (`~` expanded, parent directory must
   exist); resolves to the path written."
  [p buf]
  (let [full (expand-home p)]
    (p/create (fn [resolve reject]
                (fs/writeFile full buf (fn [err] (if err (reject err) (resolve full))))))))

;; ---------------------------------------------------------------------------
;; Dates
;; ---------------------------------------------------------------------------

(defn parse-date-iso
  "Parses a date string the way JavaScript does (RFC 5322 date-times
   included) and returns its ISO-8601 UTC form, or nil when it does not
   parse. Used to tell a real Date: header from mailparser's 'now'
   substitute (inventory 12.5)."
  [s]
  (when (string? s)
    (let [t (js/Date.parse s)]
      (when-not (js/isNaN t) (js-invoke (js/Date. t) "toISOString")))))

(defn now-iso
  "The current time as an ISO-8601 UTC string."
  []
  (js-invoke (js/Date.) "toISOString"))

;; ---------------------------------------------------------------------------
;; mailparser (email-cljs.message/parse-message!)
;; ---------------------------------------------------------------------------

(defn- js-date?
  [x]
  (instance? js/Date x))

(defn- plain-object?
  [x]
  (and (some? x)
       (identical? "object" (js* "typeof ~{}" x))
       (let [proto (js/Object.getPrototypeOf x)]
         (or (nil? proto) (identical? proto js/Object.prototype)))))

(defn- camel->kebab-key
  [k]
  (-> (str k)
      (str/replace #"([a-z0-9])([A-Z])" "$1-$2")
      str/lower-case
      keyword))

(def ^:private verbatim-object-keys
  "Plain-object keys whose nested keys are wire names (MIME parameter names
   such as charset / boundary / filename), kept as strings."
  #{:params})

(defn mail->data
  "Deep conversion of a mailparser result (or any of its parts) into CLJS
   data: Map -> map with its string keys, Set -> set, array -> vector,
   Date -> ISO-8601 UTC string, BigInt -> decimal string, Buffer -> the
   Buffer itself (opaque; wrapped into Bytes by email-cljs.message), plain
   object -> map with kebab keyword keys (the keys under `params` stay
   strings), functions / streams -> nil, primitives as they are."
  ([x] (mail->data x false))
  ([x verbatim?]
   (cond
     (or (nil? x) (undefined? x)) nil
     (string? x) x
     (number? x) x
     (boolean? x) x
     (identical? "bigint" (js* "typeof ~{}" x)) (js-invoke x "toString")
     (fn? x) nil
     (js-date? x) (let [t (js-invoke x "getTime")] (when-not (js/isNaN t) (js-invoke x "toISOString")))
     (buffer? x) x
     (instance? js/Map x) (into {} (map (fn [[k v]] [(mail->data k true) (mail->data v verbatim?)])) (es6-iterator-seq (js-invoke x "entries")))
     (instance? js/Set x) (into #{} (map #(mail->data % verbatim?)) (es6-iterator-seq (js-invoke x "values")))
     (array? x) (into [] (map #(mail->data % verbatim?)) x)
     (plain-object? x) (into {}
                             (keep (fn [k]
                                     (let [v (unchecked-get x k)
                                           kk (if verbatim? k (camel->kebab-key k))]
                                       (when-not (fn? v)
                                         [kk (mail->data v (or verbatim? (contains? verbatim-object-keys kk)))]))))
                             (js/Object.keys x))
     :else nil)))

(defn simple-parse
  "mailparser simpleParser over a string or Buffer `source` with a
   string-keyed camelCase option map (keepCidLinks, skipHtmlToText,
   skipTextToHtml, skipImageLinks, skipTextLinks, maxHtmlLengthToParse,
   checksumAlgo, keepDeliveryStatus). Promise of the mail object converted
   by mail->data: {:headers {\"lower-name\" value} :header-lines [{:key
   :line}] :subject :text :html :text-as-html :date :message-id
   :in-reply-to :references :priority :from {:value [..] :text :html}
   :to .. :cc .. :bcc .. :reply-to .. :sender .. :attachments [{:content
   <Buffer> :content-type :part-id :content-disposition :filename
   :content-id :cid :related :checksum :size :headers {..}}]}. Rejects with
   mailparser's own error (a js/Error) — classified by the caller."
  [source options]
  (p/then (js/Promise.resolve (simpleParser source (clj->js (or options {}))))
          mail->data))

;; ---------------------------------------------------------------------------
;; Timers (the IDLE watcher's restart timer; unref'd so a stopped watcher
;; never keeps the process alive)
;; ---------------------------------------------------------------------------

(defn set-timer
  "Schedules `f` after `ms` milliseconds; returns an opaque timer handle that
   does not keep the Node process alive."
  [ms f]
  (let [t (js/setTimeout f ms)]
    (when (fn? (unchecked-get t "unref")) (js-invoke t "unref"))
    t))

(defn clear-timer
  "Cancels a timer handle from set-timer (nil-safe)."
  [t]
  (when (some? t) (js/clearTimeout t)))

(defn now-ms
  "Milliseconds since the epoch."
  []
  (.now js/Date))
