(ns email-cljs.imap
  "IMAP access over imapflow — the public, interop-free surface. UID-first:
   every function takes and returns UIDs (sequence numbers are exposed
   read-only as :seq); kebab maps in and out; promesa promises for every
   operation; a core.async channel for the IDLE watcher (watch!).

   All imapflow interop is in email-cljs.imap-binding; this namespace
   translates data (compile-search, fetch-query, flag->wire, range->wire,
   error-map->ex are pure and unit-tested offline) and sequences calls.

   Connection model (inventory 10): every connection the library opens has
   disableAutoIdle true. The main connection (imap-client + connect!) runs
   commands; watch! opens a SECOND connection from the same config, selects
   the mailbox read-only and drives an explicit idle() loop, so command
   traffic and push notifications never share a socket. Errors are
   :email-cljs/imap-error (NO/BAD with the server text), :auth-error,
   :network-error (timeouts, TLS, closed) or :argument-error (inventory 8.2)."
  (:require [clojure.core.async :as async]
            [clojure.string :as str]
            [email-cljs.attachment :as att]
            [email-cljs.imap-binding :as ib]
            [email-cljs.parse-binding :as pb]
            [email-cljs.util :as util]
            [promesa.core :as p]))

;; ---------------------------------------------------------------------------
;; Client configuration
;; ---------------------------------------------------------------------------

(def default-port
  "993, implicit TLS (inventory 3.3)."
  993)

(def default-max-literal-size
  "256 MB cap on a single IMAP literal (imapflow's own default is 1 GB;
   inventory 13.14)."
  268435456)

(def default-max-idle-ms
  "The watcher re-issues IDLE every 29 minutes (RFC 2177 asks for at most
   30; inventory 13.10 — NATed clients may want 5 minutes)."
  (* 29 60 1000))

(def client-option-keys
  "Every key imap-client accepts."
  #{:host :port :secure :auth :tls :servername :logger? :client-info
    :disable-auto-idle :max-literal-size :max-line-length :max-response-size
    :qresync? :max-idle-ms :missing-idle-command
    :connection-timeout-ms :greeting-timeout-ms :socket-timeout-ms
    :disable-compression? :disable-binary? :disable-imap4rev2?
    :verify-only? :make-conn :secrets-file})

(defn- tls->wire
  [tls]
  (when (map? tls)
    (into {} (map (fn [[k v]] [(util/kebab->camel k) v])) tls)))

(defn- resolve-port+secure
  [{:keys [port secure]}]
  (let [port (cond (nil? port) (cond (true? secure) 993 (false? secure) 143 :else default-port)
                   :else (or (util/parse-port port)
                             (throw (util/config-error (str "invalid IMAP port " (pr-str port))
                                                       {:reason :bad-port :port port}))))
        secure (cond (boolean? secure) secure
                     (some? (util/parse-bool secure)) (util/parse-bool secure)
                     :else (= 993 port))]
    {:port port :secure secure}))

(defn- auth->wire
  [auth]
  (when-not (map? auth)
    (throw (util/config-error "IMAP :auth {:user :pass} is required" {:reason :missing-credentials})))
  (when (or (nil? (:user auth)) (nil? (:pass auth)))
    (throw (util/config-error "IMAP :auth needs both :user and :pass" {:reason :missing-credentials})))
  (cond-> {"user" (util/secret (:user auth))
           "pass" (util/secret (:pass auth))}
    (:login-method auth) (assoc "loginMethod" (:login-method auth))))

(defn client-options->wire
  "Pure: kebab client options -> the string-keyed camelCase ImapFlowOptions
   map (auth Secrets still wrapped). Defaults: port 993 / secure true,
   disableAutoIdle true (always — the watcher owns IDLE), logger false,
   qresync true, maxLiteralSize 256 MB, clientInfo {name \"email-cljs\"}.
   Throws :email-cljs/config-error for a missing host / bad port / missing
   credentials."
  [opts]
  (let [unknown (remove client-option-keys (keys opts))]
    (when (seq unknown)
      (throw (util/config-error (str "unknown imap-client option(s) " (pr-str (vec unknown)))
                                {:reason :unknown-option :keys (vec unknown)}))))
  (when (str/blank? (str (:host opts)))
    (throw (util/config-error "IMAP :host is required" {:reason :bad-host})))
  (let [{:keys [port secure]} (resolve-port+secure opts)]
    (->> {"host" (str/trim (:host opts))
          "port" port
          "secure" secure
          "servername" (:servername opts)
          "auth" (auth->wire (:auth opts))
          "tls" (tls->wire (:tls opts))
          "logger" (boolean (:logger? opts))
          "clientInfo" (or (:client-info opts) {"name" "email-cljs"})
          "disableAutoIdle" true
          "qresync" (if (contains? opts :qresync?) (boolean (:qresync? opts)) true)
          "maxIdleTime" false
          "missingIdleCommand" (some-> (:missing-idle-command opts) name str/upper-case)
          "maxLiteralSize" (or (:max-literal-size opts) default-max-literal-size)
          "maxLineLength" (:max-line-length opts)
          "maxResponseSize" (:max-response-size opts)
          "connectionTimeout" (:connection-timeout-ms opts)
          "greetingTimeout" (:greeting-timeout-ms opts)
          "socketTimeout" (:socket-timeout-ms opts)
          "disableCompression" (:disable-compression? opts)
          "disableBinary" (:disable-binary? opts)
          "disableIMAP4rev2" (:disable-imap4rev2? opts)
          "verifyOnly" (:verify-only? opts)}
         (remove (comp nil? val))
         (into {}))))

(defn- new-state
  []
  (atom {:connected? false :lock nil :mailbox nil :capabilities nil :last-error nil}))

(defn imap-client
  "Builds an IMAP client map {:conn <opaque> :config <kebab, secrets
   redacted> :options <wire options minus auth> :state <atom>}. Options:
   :host (required) :port (993) :secure (true for 993) :auth {:user :pass
   (Secrets or strings) :login-method} :tls {:ca <Secret|PEM|Bytes>
   :reject-unauthorized ..} :servername :logger? false :client-info
   :qresync? true :max-literal-size 268435456 :max-line-length
   :max-response-size :max-idle-ms 1740000 (watch! restart period)
   :missing-idle-command (:noop :select :status) :connection-timeout-ms
   :greeting-timeout-ms :socket-timeout-ms :disable-compression?
   :disable-binary? :disable-imap4rev2?
   :make-conn (fn [wire-options on-error] conn) — connection factory,
   default imap-binding/make-client; tests inject fakes here and watch!
   uses it for its own connection.
   disableAutoIdle is always true (inventory 10.1). Does not connect —
   see connect!. Throws :email-cljs/config-error."
  [opts]
  (let [opts (or opts {})
        wire (client-options->wire (dissoc opts :max-idle-ms :make-conn :secrets-file))
        state (new-state)
        make-conn (or (:make-conn opts) ib/make-client)
        conn (make-conn wire (fn [err] (swap! state assoc :last-error err)))]
    {:conn conn
     :config (util/redact-config (dissoc opts :make-conn))
     :options (dissoc wire "auth")
     :wire-options wire
     :make-conn make-conn
     :max-idle-ms (or (:max-idle-ms opts) default-max-idle-ms)
     :state state}))

(defn client?
  "True for a map built by imap-client."
  [x]
  (and (map? x) (some? (:conn x)) (some? (:state x))))

(defn config-from-env
  "Pure: {:username :password :imap :tls} from util/env-config -> imap-client
   options. Throws :email-cljs/config-error when host or credentials are
   missing."
  [{:keys [username password imap tls]}]
  (when (or (nil? username) (nil? password))
    (throw (util/config-error "no IMAP credentials (USERNAME / PASSWORD) in the environment or secrets file"
                              {:reason :missing-credentials})))
  (when (str/blank? (str (:host imap)))
    (throw (util/config-error "no IMAP host (IMAP_HOST) in the environment or secrets file"
                              {:reason :secrets-file-incomplete :key "IMAP_HOST"})))
  (cond-> {:host (:host imap)
           :auth {:user username :pass password}}
    (:port imap) (assoc :port (:port imap))
    (some? (:secure imap)) (assoc :secure (:secure imap))
    (:ca tls) (assoc :tls {:ca (:ca tls)})))

(defn imap-client-from-env
  "imap-client configured from <prefix>USERNAME / PASSWORD / IMAP_HOST /
   IMAP_PORT / IMAP_TLS / TLS_CA via util/from-env (environment over the
   secrets file named by :secrets-file in `opts`, EMAIL_SECRETS_FILE, or
   ~/.secrets/borret.txt). `opts` merge over the environment values."
  ([prefix] (imap-client-from-env prefix nil))
  ([prefix opts]
   (let [env (util/from-env (:secrets-file opts) prefix)]
     (imap-client (util/deep-merge (config-from-env env) (dissoc opts :secrets-file))))))

;; ---------------------------------------------------------------------------
;; Error classification (pure; inventory 8.2)
;; ---------------------------------------------------------------------------

(def ^:private timeout-codes #{"CONNECT_TIMEOUT" "GREETING_TIMEOUT" "ETIMEOUT" "EConnectionClosed"})
(def ^:private tls-codes #{"UPGRADE_TIMEOUT" "STARTTLS_INJECTION"})
(def ^:private argument-codes #{"InvalidSequenceSet" "InvalidStringValue" "InvalidTextValue" "InvalidTokenValue"})
(def ^:private limit-codes #{"LineTooLarge" "LiteralTooLarge" "ResponseTooLarge" "DownloadOverflow" "MAX_IMAP_NESTING_REACHED"})
(def ^:private protocol-codes #{"InvalidResponse" "UnexpectedTag" "ParserErrorExchange" "ResponseProcessingFailed" "COMPRESS_TRAILING_DATA" "PollFailed"})

(defn error-map->ex
  "Pure: an ImapFlowError read into a map by imap-binding/error->map ->
   the house ex-info. authenticationFailed -> :auth-error :imap;
   tlsFailed / UPGRADE_TIMEOUT / STARTTLS_INJECTION -> :network-error
   :tls? true; CONNECT_TIMEOUT GREETING_TIMEOUT ETIMEOUT EConnectionClosed
   or a BYE reason -> :network-error retryable; NoConnection ->
   :network-error :reason :not-connected; ETHROTTLE -> :imap-error
   retryable with :retry-after-ms; LockTimeout -> :imap-error
   :reason :lock-timeout retryable; MissingServerExtension ->
   :argument-error :reason :missing-server-extension; Invalid* ->
   :argument-error; size limits -> :imap-error :reason :limit-exceeded;
   parser codes -> :imap-error :reason :protocol; NotFound -> :imap-error
   :reason :not-found; NO / BAD tagged responses -> :imap-error with
   :response-status, :response-text (the server's words),
   :server-response-code, :command, :path; anything else -> :imap-error.
   The imapflow code is always kept under :code. ex-info passes through."
  [m]
  (if (util/error? m)
    m
    (let [{:keys [code command path response-status response-text server-response-code
                  message authentication-failed? tls-failed? throttle-reset-ms
                  mailbox-missing? reason executed-command]} m
          message (or message "unknown imapflow error")
          base (cond-> {:code code :imapflow-code code :command command :path path
                        :response-status response-status :response-text response-text
                        :server-response-code server-response-code}
                 executed-command (assoc :executed-command executed-command))]
      (cond
        (or authentication-failed? (= "AUTHENTICATIONFAILED" server-response-code))
        (util/auth-error :imap message (assoc base :response response-text))

        (or tls-failed? (contains? tls-codes code))
        (util/network-error :imap message (assoc base :tls? true :retryable? false))

        (contains? timeout-codes code)
        (util/network-error :imap message (assoc base :retryable? true))

        (= "NoConnection" code)
        (util/network-error :imap message (assoc base :reason :not-connected :retryable? false))

        (and (nil? code) (some? reason) (nil? response-status))
        (util/network-error :imap message (assoc base :reason :bye :bye-reason reason :retryable? true))

        (= "ETHROTTLE" code)
        (util/imap-error message (assoc base :retryable? true :retry-after-ms throttle-reset-ms))

        (= "LockTimeout" code)
        (util/imap-error message (assoc base :reason :lock-timeout :retryable? true))

        (= "MissingServerExtension" code)
        (util/argument-error message (assoc base :reason :missing-server-extension))

        (contains? argument-codes code)
        (util/argument-error message (assoc base :reason :invalid-argument))

        (contains? limit-codes code)
        (util/imap-error message (assoc base :reason :limit-exceeded))

        (contains? protocol-codes code)
        (util/imap-error message (assoc base :reason :protocol))

        (= "NotFound" code)
        (util/imap-error message (assoc base :reason :not-found))

        (contains? #{"NO" "BAD"} response-status)
        (util/imap-error (str response-status " " (or response-text message))
                         (assoc base :reason (cond mailbox-missing? :mailbox-missing
                                                   (= "BAD" response-status) :bad-command
                                                   :else :refused)
                                :mailbox-missing? mailbox-missing?))

        :else
        (util/imap-error message base)))))

(defn- ->ex
  [e]
  (if (util/error? e) e (error-map->ex (ib/error->map e))))

(defn- start
  [f]
  (p/then (p/resolved nil) (fn [_] (f))))

(defn- run
  "Runs (f) as a promise and maps every rejection through ->ex."
  [f]
  (p/catch (start f) (fn [e] (p/rejected (->ex e)))))

;; ---------------------------------------------------------------------------
;; Connection lifecycle
;; ---------------------------------------------------------------------------

(defn connected?
  "True when connect! succeeded and the connection is still usable."
  [client]
  (and (:connected? @(:state client)) (ib/usable? (:conn client))))

(defn capabilities
  "Server capabilities after connect!: {:idle true :uidplus true
   :appendlimit 26214400 ..} (nil before)."
  [client]
  (:capabilities @(:state client)))

(defn last-error
  "The last 'error' event the connection emitted (kebab map) or nil."
  [client]
  (:last-error @(:state client)))

(defn connect!
  "Connects, negotiates TLS, ENABLEs extensions and authenticates. Promise
   of the client map (with :capabilities recorded) that resolves only once
   the connection is usable; rejects with :auth-error / :network-error."
  [client]
  (run (fn []
         (p/let [_ (ib/connect (:conn client))]
           (if (ib/usable? (:conn client))
             (do (swap! (:state client) assoc
                        :connected? true
                        :capabilities (ib/capabilities (:conn client))
                        :enabled (ib/enabled (:conn client)))
                 client)
             (throw (util/network-error :imap "connected but the connection is not usable"
                                        {:reason :not-usable :retryable? true})))))))

(defn close-imap!
  "LOGOUT and close (falls back to a plain close when LOGOUT fails or the
   connection is already gone). Promise of true."
  [client]
  (run (fn []
         (let [conn (:conn client)]
           (swap! (:state client) assoc :connected? false :lock nil :mailbox nil)
           (-> (if (ib/usable? conn) (ib/logout conn) (p/resolved nil))
               (p/catch (fn [_] (ib/close conn) nil))
               (p/then (fn [_] true)))))))

;; ---------------------------------------------------------------------------
;; Mailboxes
;; ---------------------------------------------------------------------------

(defn- release-lock!
  [client]
  (when-let [lock (:lock @(:state client))]
    (try ((:release lock)) (catch :default _ nil))
    (swap! (:state client) assoc :lock nil :mailbox nil)))

(defn mailbox-open!
  "Selects `mailbox` (path string, or a vector joined with the server's
   delimiter) under an imapflow mailbox lock (a weak transaction: no other
   lock holder interleaves). {:read-only? true} EXAMINEs instead of
   SELECTs — use it for pure readers, since a later CLOSE would expunge
   \\Deleted messages only in read-write mode. A previously held lock is
   released first. Promise of the mailbox state {:path :delimiter :flags
   #{..} :permanent-flags #{..} :uid-validity \"..\" :uid-next :exists
   :exists-count :unseen (first unseen seq, not a count) :highest-modseq
   :read-only ..}."
  ([client mailbox] (mailbox-open! client mailbox nil))
  ([client mailbox {:keys [read-only?]}]
   (run (fn []
          (release-lock! client)
          (p/let [lock (ib/get-mailbox-lock (:conn client) mailbox {"readOnly" (boolean read-only?)})
                  mb (ib/current-mailbox (:conn client))
                  mb (assoc mb :exists-count (:exists mb))]
            (swap! (:state client) assoc :lock lock :mailbox mb)
            mb)))))

(defn mailbox-close!
  "Releases the mailbox lock taken by mailbox-open!. With {:expunge? true}
   also sends CLOSE, which permanently removes every \\Deleted message of a
   read-write mailbox (RFC 3501 6.4.2; inventory 12.7) — off by default.
   Promise of true."
  ([client] (mailbox-close! client nil))
  ([client {:keys [expunge?]}]
   (run (fn []
          (release-lock! client)
          (if expunge?
            (p/then (ib/mailbox-close (:conn client)) (fn [_] true))
            true)))))

(defn current-mailbox
  "The mailbox state recorded by the last mailbox-open! (nil when none)."
  [client]
  (:mailbox @(:state client)))

(defn mailboxes!
  "LIST (+ LSUB / LIST-EXTENDED) -> promise of [{:path :name :parent-path
   :parent [..] :delimiter :flags #{\"\\\\HasNoChildren\" ..} :special-use
   \"\\\\Sent\" :special-use-source :listed :subscribed :status {..}} ..].
   Options: {:status? true} adds STATUS (messages / unseen / uid-next /
   uid-validity / highest-modseq) per mailbox; {:special-use-hints {:sent
   \"Sent\" ..}} names folders when the server has no SPECIAL-USE."
  ([client] (mailboxes! client nil))
  ([client {:keys [status? special-use-hints list-only?]}]
   (run (fn []
          (ib/list-mailboxes (:conn client)
                             (cond-> {}
                               status? (assoc "statusQuery" {"messages" true "unseen" true "uidNext" true
                                                             "uidValidity" true "highestModseq" true})
                               special-use-hints (assoc "specialUseHints" special-use-hints)
                               list-only? (assoc "listOnly" true)))))))

(def status-items
  "STATUS items requested by default."
  [:messages :recent :uid-next :uid-validity :unseen :highest-modseq])

(defn mailbox-status!
  "STATUS without selecting -> promise of {:path :messages :recent :uid-next
   :uid-validity \"..\" :unseen (a COUNT here) :highest-modseq :size}.
   `items` (optional vector of the kebab item keywords, plus :size /
   :deleted) limits what is asked; items the server cannot report are
   simply absent."
  ([client mailbox] (mailbox-status! client mailbox status-items))
  ([client mailbox items]
   (run (fn []
          (ib/status (:conn client) mailbox (into {} (map (fn [k] [(util/kebab->camel k) true])) items))))))

(defn quota!
  "GETQUOTAROOT -> promise of {:path :storage {:used :limit} (BYTES —
   imapflow converts the KiB wire unit; inventory 13.12) :messages {:used
   :limit}} or nil when the server has no QUOTA / the path is unknown."
  ([client] (quota! client nil))
  ([client mailbox]
   (run (fn [] (ib/get-quota (:conn client) mailbox)))))

;; ---------------------------------------------------------------------------
;; Search DSL (pure; inventory 5.4)
;; ---------------------------------------------------------------------------

(def ^:private flag-search-keys
  {:answered "answered" :deleted "deleted" :draft "draft" :flagged "flagged" :seen "seen"})

(def ^:private negated-flag-search-keys
  {:unanswered "answered" :undeleted "deleted" :undraft "draft" :unflagged "flagged" :unseen "seen"})

(def ^:private true-search-keys #{:all :new :old :recent})

(def ^:private string-search-keys
  {:from "from" :to "to" :cc "cc" :bcc "bcc" :subject "subject" :body "body" :text "text"
   :keyword "keyword" :un-keyword "unKeyword" :unkeyword "unKeyword"
   :email-id "emailId" :thread-id "threadId" :gmraw "gmraw" :gmail-raw "gmailraw"})

(def ^:private number-search-keys {:larger "larger" :smaller "smaller"})

(def ^:private date-search-keys
  {:before "before" :on "on" :since "since"
   :sent-before "sentBefore" :sent-on "sentOn" :sent-since "sentSince"})

(def ^:private month-names ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

(def ^:private month-index
  (into {} (map-indexed (fn [i m] [(str/lower-case m) (inc i)])) month-names))

(defn- pad2 [n] (if (< n 10) (str "0" n) (str n)))

(defn- ymd
  [y m d]
  (when-not (and (integer? y) (integer? m) (integer? d) (<= 1 m 12) (<= 1 d 31))
    (throw (util/argument-error "search date components out of range" {:reason :bad-search-date :value [y m d]})))
  (str y "-" (pad2 m) "-" (pad2 d)))

(defn search-date
  "Pure: a search date -> the \"YYYY-MM-DD\" form handed to imapflow.
   Accepts \"YYYY-MM-DD\", \"DD-Mon-YYYY\" (IMAP wire spelling), an #inst
   (UTC calendar day) or {:year :month :day}. imapflow 2.0.6 re-parses
   every date value with `new Date()` and renders it from UTC ISO
   components (tools.js formatDate), so a bare \"DD-Mon-YYYY\" string
   would be read as LOCAL midnight and come out a day early east of UTC —
   the date-only ISO form is the one spelling `new Date()` reads as UTC
   midnight, which imapflow then renders back unchanged (and, for BEFORE,
   does not shift by a day). Anything else -> :argument-error
   :bad-search-date."
  [k v]
  (cond
    (and (string? v) (re-matches #"^\d{4}-\d{2}-\d{2}$" v)) v
    (string? v) (if-let [[_ d mon y] (re-matches #"^(\d{1,2})-([A-Za-z]{3})-(\d{4})$" (str/trim v))]
                  (let [m (get month-index (str/lower-case mon))]
                    (when-not m
                      (throw (util/argument-error (str "unknown month in search date " v) {:reason :bad-search-date :key k :value v})))
                    (ymd (js/parseInt y 10) m (js/parseInt d 10)))
                  (throw (util/argument-error (str "search " k " must be \"YYYY-MM-DD\" or \"DD-Mon-YYYY\" (got " (pr-str v) ")")
                                              {:reason :bad-search-date :key k :value v})))
    (inst? v) (ib/inst->search-date v)
    (and (map? v) (:year v) (:month v) (:day v)) (ymd (:year v) (:month v) (:day v))
    :else (throw (util/argument-error (str "search " k " must be a \"YYYY-MM-DD\" / \"DD-Mon-YYYY\" string, an #inst or {:year :month :day}")
                                      {:reason :bad-search-date :key k :value v}))))

(defn- sequence-set
  [k v]
  (cond
    (integer? v) (str v)
    (string? v) v
    (and (sequential? v) (seq v) (every? integer? v)) (str/join "," v)
    :else (throw (util/argument-error (str "search " k " must be a UID, a vector of UIDs or a sequence-set string")
                                      {:reason :bad-sequence-set :key k :value v}))))

(defn- compile-search*
  [query]
  (when-not (map? query)
    (throw (util/argument-error "search query must be a map" {:reason :bad-search-query :value query})))
  (reduce-kv
   (fn [acc k v]
     (cond
       (nil? v) acc
       (contains? #{:uid :seq} k) (assoc acc (name k) (sequence-set k v))
       (contains? flag-search-keys k) (assoc acc (get flag-search-keys k) (boolean v))
       (contains? negated-flag-search-keys k) (assoc acc (get negated-flag-search-keys k) (not (boolean v)))
       (contains? true-search-keys k) (if v (assoc acc (name k) true) acc)
       (contains? string-search-keys k) (assoc acc (get string-search-keys k) (str v))
       (contains? number-search-keys k) (if (number? v)
                                          (assoc acc (get number-search-keys k) v)
                                          (throw (util/argument-error (str "search " k " must be a byte count")
                                                                      {:reason :bad-search-value :key k :value v})))
       (contains? date-search-keys k) (assoc acc (get date-search-keys k) (search-date k v))
       (= :header k) (if (and (map? v) (seq v))
                       (assoc acc "header" (into {} (map (fn [[hk hv]] [(if (keyword? hk) (util/normalize-header-name hk) (str hk))
                                                                       (if (true? hv) true (str hv))])) v))
                       (throw (util/argument-error "search :header must be a non-empty {\"Name\" value-or-true} map"
                                                   {:reason :bad-search-value :key k :value v})))
       (= :modseq k) (assoc acc "modseq" (if (string? v) v (str v)))
       (= :labels k) (assoc acc "labels" v)
       (= :not k) (assoc acc "not" (compile-search* v))
       (= :or k) (if (and (sequential? v) (>= (count v) 2))
                   (assoc acc "or" (mapv compile-search* v))
                   (throw (util/argument-error "search :or needs a vector of at least two sub-queries"
                                               {:reason :bad-search-value :key k :value v})))
       :else (throw (util/argument-error (str "unknown search key " k)
                                         {:reason :unknown-search-key :key k}))))
   {}
   query))

(defn compile-search
  "Pure: the kebab search DSL -> imapflow SearchObject (string-keyed camel
   map). Keys: :uid / :seq (number, vector or sequence-set string);
   :seen :answered :flagged :deleted :draft (boolean; false -> UN*) and the
   :unseen :unanswered .. sugar; :all :new :old :recent (true); :from :to
   :cc :bcc :subject :body :text :keyword :un-keyword :email-id :thread-id
   :gmraw (strings); :larger :smaller (bytes); :before :on :since
   :sent-before :sent-on :sent-since (\"YYYY-MM-DD\", \"DD-Mon-YYYY\", an
   #inst taken as its UTC calendar day, or {:year :month :day} — all
   normalised by search-date, see there for why; inventory 12.4 / 13.7);
   :header {\"Message-ID\" \"<..>\" :x-foo true} (keyword names rendered
   as X-Foo; imapflow upper-cases them on the wire);
   :modseq; :not {..}; :or [{..} {..}]. Every key is ANDed. An empty query
   (no keys, or only nil values) is rejected with :email-cljs/argument-error
   :reason :empty-search unless {:all true} is explicit (inventory 13.6)."
  [query]
  (let [compiled (compile-search* query)]
    (when (empty? compiled)
      (throw (util/argument-error "empty search query — pass {:all true} to match every message"
                                  {:reason :empty-search})))
    compiled))

(defn range->wire
  "Pure: a UID (number), a vector of UIDs, a sequence-set string (\"1:*\",
   \"100:*\", \"*\") or a search DSL map -> the imapflow MessageRange. Used
   with {uid true} everywhere."
  [range]
  (cond
    (integer? range) (str range)
    (string? range) (if (str/blank? range)
                      (throw (util/argument-error "empty range" {:reason :bad-range}))
                      range)
    (map? range) (compile-search range)
    (and (sequential? range) (seq range) (every? integer? range)) (str/join "," range)
    :else (throw (util/argument-error "range must be a UID, a vector of UIDs, a sequence-set string or a search map"
                                      {:reason :bad-range :value range}))))

(defn search!
  "UID SEARCH with the compile-search DSL -> promise of a vector of UIDs
   (ascending). Options: {:return [:count :min :max :all]} asks for
   ESEARCH and resolves to {:count :min :max :all \"1,5:10\"} instead
   (imapflow emulates it on servers without ESEARCH). Needs an open
   mailbox (rejects :imap-error :reason :no-mailbox otherwise)."
  ([client query] (search! client query nil))
  ([client query {:keys [return]}]
   (run (fn []
          (let [q (compile-search query)
                opts (cond-> {"uid" true}
                       (seq return) (assoc "returnOptions" (mapv #(str/upper-case (name %)) return)))]
            (p/let [r (ib/search (:conn client) q opts)]
              (cond
                (nil? r) (throw (util/imap-error "search needs an open mailbox" {:reason :no-mailbox :command "SEARCH"}))
                (vector? r) r
                :else r)))))))

;; ---------------------------------------------------------------------------
;; Fetch
;; ---------------------------------------------------------------------------

(def default-fetch-opts
  "What fetch-one! / fetch-all! ask for when no opts are given."
  {:uid true :flags true :envelope true :internal-date true :size true})

(defn fetch-query
  "Pure: kebab fetch opts -> imapflow FetchQueryObject (string-keyed camel
   map). :uid :flags :envelope :body-structure :internal-date :size
   :thread-id :labels :email-id (booleans); :headers? true (raw header
   block) or :headers [\"Subject\" ..] (only those); :source? true (whole
   RFC 822 as Bytes) or :source {:start :max-length}; :body-parts [\"1\"
   \"2.1\"]; :peek is accepted and ignored — imapflow always fetches with
   BODY.PEEK so nothing here sets \\Seen (inventory 13.8); :changed-since
   (modseq) rides in the options, not the query. uid is always requested."
  [opts]
  (let [opts (or opts default-fetch-opts)]
    (->> (cond-> {"uid" true}
           (:flags opts) (assoc "flags" true)
           (:envelope opts) (assoc "envelope" true)
           (:body-structure opts) (assoc "bodyStructure" true)
           (:internal-date opts) (assoc "internalDate" true)
           (:size opts) (assoc "size" true)
           (:thread-id opts) (assoc "threadId" true)
           (:labels opts) (assoc "labels" true)
           (:email-id opts) (assoc "emailId" true)
           (:headers? opts) (assoc "headers" true)
           (sequential? (:headers opts)) (assoc "headers" (vec (:headers opts)))
           (true? (:headers opts)) (assoc "headers" true)
           (:source? opts) (assoc "source" true)
           (true? (:source opts)) (assoc "source" true)
           (map? (:source opts)) (assoc "source" {"start" (get-in opts [:source :start] 0)
                                                  "maxLength" (get-in opts [:source :max-length])})
           (:body-parts opts) (assoc "bodyParts" (vec (:body-parts opts))))
         (remove (comp nil? val))
         (into {}))))

(def ^:private iso-date-re #"^\d{4}-\d{2}-\d{2}T")

(defn- fix-envelope-date
  [env]
  (if (and (map? env) (string? (:date env)) (not (re-find iso-date-re (:date env))))
    (assoc env :date nil :date-raw (:date env))
    env))

(defn- fix-address-list
  [addrs]
  (when (sequential? addrs)
    (mapv (fn [a] {:name (let [n (:name a)] (when-not (str/blank? (str n)) n))
                   :address (:address a)})
          addrs)))

(defn- normalize-message
  "Post-walk touch-ups: envelope addresses to {:name :address} with nil
   names, an unparseable envelope date to :date nil + :date-raw."
  [m]
  (cond-> m
    (map? (:envelope m))
    (update :envelope (fn [env]
                        (-> env
                            fix-envelope-date
                            (as-> e (reduce (fn [e k] (if (contains? e k) (update e k fix-address-list) e))
                                            e [:from :sender :reply-to :to :cc :bcc])))))))

(defn- fetch-options
  [opts]
  (cond-> {"uid" true}
    (:changed-since opts) (assoc "changedSince" (str (:changed-since opts)))
    (:binary? opts) (assoc "binary" true)))

(defn fetch-all!
  "UID FETCH for a range (UID, vector of UIDs, sequence-set string or search
   map — see range->wire) -> promise of a vector of kebab messages
   {:uid :seq :modseq \"..\" :size :internal-date \"ISO\" :flags #{\"\\\\Seen\"}
   :envelope {:date :subject :message-id :in-reply-to :from [{:name :address}]
   :to .. :cc .. :bcc ..} :body-structure {:type :parameters {..} :part
   :child-nodes [..] ..} :source Bytes :headers Bytes}. Drained with
   fetchAll (no generator deadlocks; inventory 12.16): keep ranges bounded."
  ([client range] (fetch-all! client range nil))
  ([client range opts]
   (run (fn []
          (p/let [r (ib/fetch-all (:conn client) (range->wire range) (fetch-query opts) (fetch-options opts))]
            (mapv normalize-message r))))))

(defn fetch-one!
  "One message by UID (or a search map resolved to its first hit) with the
   fetch-query opts -> promise of the message map or nil when it does not
   exist."
  ([client uid] (fetch-one! client uid nil))
  ([client uid opts]
   (run (fn []
          (if (map? uid)
            (p/let [r (ib/fetch-all (:conn client) (range->wire uid) (fetch-query opts) (fetch-options opts))]
              (some-> (first r) normalize-message))
            (p/let [r (ib/fetch-one (:conn client) (range->wire uid) (fetch-query opts) (fetch-options opts))]
              (some-> r normalize-message)))))))

(defn download!
  "Downloads one body part (IMAP part number from :body-structure, e.g.
   \"1.2\" or \"2\"; mailparser's part-id agrees — inventory 5.6) of the
   message with `uid`, decoded (transfer encoding removed, text converted
   to UTF-8). `part-id` nil downloads the whole RFC 822 source. Options:
   {:max-bytes n}. Promise of {:meta {:content-type :charset :disposition
   :filename :encoding :expected-size :flowed :del-sp} :content Bytes} or
   nil when the part does not exist."
  ([client uid part-id] (download! client uid part-id nil))
  ([client uid part-id {:keys [max-bytes]}]
   (run (fn []
          (ib/download (:conn client) (str uid) part-id
                       (cond-> {"uid" true} max-bytes (assoc "maxBytes" max-bytes :max-bytes max-bytes)))))))

(defn body-part!
  "The decoded bytes of one body part (see download!) -> promise of Bytes,
   or nil when the part does not exist."
  ([client uid part-id] (body-part! client uid part-id nil))
  ([client uid part-id opts]
   (p/then (download! client uid part-id opts) :content)))

(defn source!
  "The whole RFC 822 message -> promise of Bytes (download! without a part)."
  [client uid]
  (body-part! client uid nil))

;; ---------------------------------------------------------------------------
;; Flags
;; ---------------------------------------------------------------------------

(def system-flags
  "Keyword -> wire form of the RFC 3501 system flags."
  {:seen "\\Seen" :answered "\\Answered" :flagged "\\Flagged" :deleted "\\Deleted" :draft "\\Draft"})

(defn flag->wire
  "Pure: :seen -> \"\\\\Seen\" (system flags), any other keyword -> its name
   as a keyword flag (:$forwarded -> \"$forwarded\"), strings verbatim.
   :recent is read-only and rejected (:argument-error)."
  [flag]
  (cond
    (= :recent flag) (throw (util/argument-error "\\Recent is read-only" {:reason :flag-not-permitted :flag flag}))
    (contains? system-flags flag) (get system-flags flag)
    (keyword? flag) (name flag)
    (and (string? flag) (not (str/blank? flag))) flag
    :else (throw (util/argument-error "flags must be keywords or strings" {:reason :bad-flag :flag flag}))))

(defn flags->wire
  "Pure: a flag or a collection of flags -> vector of wire strings."
  [flags]
  (mapv flag->wire (if (or (sequential? flags) (set? flags)) flags [flags])))

(defn- store-options
  [opts]
  (cond-> {"uid" true}
    (:unchanged-since opts) (assoc "unchangedSince" (str (:unchanged-since opts)))
    (:silent? opts) (assoc "silent" true)
    (:use-labels? opts) (assoc "useLabels" true)))

(defn flags-set!
  "STORE FLAGS: replaces the flags of the messages in `range` (UID, vector,
   sequence set or search map). Flags: :seen :answered :flagged :deleted
   :draft or custom keyword strings. Options {:unchanged-since modseq
   :silent? true}. Promise of boolean (false when the server refused or
   dropped everything — imapflow silently drops flags the mailbox's
   permanent-flags do not allow, inventory 12.17)."
  ([client range flags] (flags-set! client range flags nil))
  ([client range flags opts]
   (run (fn [] (ib/message-flags-set (:conn client) (range->wire range) (flags->wire flags) (store-options opts))))))

(defn flags-add!
  "STORE +FLAGS (see flags-set!). Promise of boolean."
  ([client range flags] (flags-add! client range flags nil))
  ([client range flags opts]
   (run (fn [] (ib/message-flags-add (:conn client) (range->wire range) (flags->wire flags) (store-options opts))))))

(defn flags-del!
  "STORE -FLAGS (see flags-set!). Promise of boolean."
  ([client range flags] (flags-del! client range flags nil))
  ([client range flags opts]
   (run (fn [] (ib/message-flags-remove (:conn client) (range->wire range) (flags->wire flags) (store-options opts))))))

(defn mark-seen!
  "flags-add! \\Seen."
  [client range] (flags-add! client range :seen))

(defn mark-unseen!
  "flags-del! \\Seen."
  [client range] (flags-del! client range :seen))

;; ---------------------------------------------------------------------------
;; Move / copy / delete / expunge / append
;; ---------------------------------------------------------------------------

(defn- copy-like
  [f client range mailbox verb]
  (run (fn []
         (p/let [r (f (:conn client) (range->wire range) mailbox {"uid" true})]
           (or r (throw (util/imap-error (str verb " failed (server refused or nothing matched)")
                                         {:reason :command-failed :command (str/upper-case verb) :path mailbox})))))))

(defn move!
  "UID MOVE (or COPY + \\Deleted + expunge when the server lacks MOVE) to
   `mailbox`. Promise of {:path :destination :uid-validity \"..\" :uid-map
   {src-uid dst-uid} (needs UIDPLUS / COPYUID)}; rejects :imap-error
   :reason :command-failed."
  [client range mailbox]
  (copy-like ib/message-move client range mailbox "move"))

(defn copy!
  "UID COPY to `mailbox`. Same result shape as move!."
  [client range mailbox]
  (copy-like ib/message-copy client range mailbox "copy"))

(defn delete!
  "Marks `range` \\Deleted and expunges it: UID EXPUNGE <range> when the
   server has UIDPLUS (only those messages go); otherwise imapflow falls
   back to a plain EXPUNGE, which ALSO removes every other message any
   client marked \\Deleted in this mailbox (inventory 5.7 / 13.11).
   {:strict? true} rejects with :argument-error :missing-server-extension
   instead of falling back. Promise of true; rejects :imap-error
   :reason :command-failed when the server refused."
  ([client range] (delete! client range nil))
  ([client range {:keys [strict?]}]
   (run (fn []
          (when (and strict? (not (ib/has-capability? (:conn client) "UIDPLUS")))
            (throw (util/argument-error "server has no UIDPLUS: delete! would fall back to EXPUNGE"
                                        {:reason :missing-server-extension :extension "UIDPLUS"})))
          (p/let [ok (ib/message-delete (:conn client) (range->wire range) {"uid" true})]
            (or ok (throw (util/imap-error "delete failed (server refused or nothing matched)"
                                           {:reason :command-failed :command "EXPUNGE"}))))))))

(defn expunge!
  "Plain EXPUNGE of the currently open mailbox: permanently removes EVERY
   message flagged \\Deleted (by any client). `mailbox` must match the
   open mailbox's path (rejects :argument-error otherwise — a guard
   against expunging the wrong folder); nil skips the check. Promise of
   true."
  ([client] (expunge! client nil))
  ([client mailbox]
   (run (fn []
          (let [open (:path (ib/current-mailbox (:conn client)))]
            (when (nil? open)
              (throw (util/imap-error "expunge needs an open mailbox" {:reason :no-mailbox :command "EXPUNGE"})))
            (when (and mailbox (not= mailbox open))
              (throw (util/argument-error (str "expunge! asked for " mailbox " but " open " is open")
                                          {:reason :mailbox-mismatch :open open :requested mailbox}))))
          (p/then (ib/exec (:conn client) "EXPUNGE" nil) (fn [_] true))))))

(defn uid-expunge!
  "UID EXPUNGE of the given UIDs (RFC 4315 UIDPLUS): only those messages,
   and only if they are flagged \\Deleted. Without UIDPLUS the documented
   fallback is a plain EXPUNGE of the whole mailbox (every \\Deleted
   message goes) — {:strict? true} rejects with :argument-error
   :missing-server-extension instead. Promise of true."
  ([client uids] (uid-expunge! client uids nil))
  ([client uids {:keys [strict?]}]
   (run (fn []
          (let [conn (:conn client)
                set (range->wire uids)]
            (when (map? uids)
              (throw (util/argument-error "uid-expunge! takes UIDs, not a search map" {:reason :bad-range})))
            (cond
              (ib/has-capability? conn "UIDPLUS")
              (p/then (ib/exec conn "UID EXPUNGE" [{:type "SEQUENCE" :value set}]) (fn [_] true))

              strict?
              (throw (util/argument-error "server has no UIDPLUS: UID EXPUNGE unavailable"
                                          {:reason :missing-server-extension :extension "UIDPLUS"}))

              :else
              (p/then (ib/exec conn "EXPUNGE" nil) (fn [_] true))))))))

(defn append!
  "APPEND a message (RFC 822 string or Bytes) to `mailbox` with optional
   flags and internal date (ISO string). Promise of {:destination :uid
   (nil without UIDPLUS / APPENDUID) :uid-validity :seq}; rejects
   :imap-error when the server refused."
  ([client mailbox content] (append! client mailbox content nil))
  ([client mailbox content {:keys [flags date]}]
   (run (fn []
          (p/let [r (ib/append (:conn client) mailbox content (flags->wire (or flags [])) date)]
            (or r (throw (util/imap-error "append failed" {:reason :command-failed :command "APPEND" :path mailbox}))))))))

;; ---------------------------------------------------------------------------
;; watch! — the IDLE watcher (inventory 10.2)
;; ---------------------------------------------------------------------------

(defonce ^:private watchers
  ;; channel -> control atom, so stop-watch! can find the watcher by its channel
  (atom {}))

(defn- watcher-emit!
  "Puts an event; when the consumer closed the channel, stops the watcher."
  [ctrl event]
  (let [{:keys [ch stop!]} @ctrl]
    (when-not (:finished? @ctrl)
      (when-not (async/put! ch event)
        (stop! :consumer-closed)))))

(defn- watcher-finish!
  "Idempotent teardown: marks finished, cancels the restart timer, emits
   :closed, closes the channel, deregisters, and logs the connection out
   (close on failure). Promise."
  [ctrl reason]
  (let [{:keys [ch conn timer path finished?]} @ctrl]
    (if finished?
      (p/resolved false)
      (do
        (swap! ctrl assoc :finished? true :stopped? true)
        (pb/clear-timer timer)
        (async/put! ch {:event :closed :path path :reason reason})
        (async/close! ch)
        (swap! watchers dissoc ch)
        (-> (if (ib/usable? conn) (ib/logout conn) (p/resolved nil))
            (p/catch (fn [_] (try (ib/close conn) (catch :default _ nil)) nil))
            (p/then (fn [_] true)))))))

(defn- watcher-idle-loop!
  [ctrl]
  (let [{:keys [conn stopped? max-idle-ms path]} @ctrl]
    (if (or stopped? (not (ib/usable? conn)))
      (watcher-finish! ctrl (if stopped? :stopped :connection-lost))
      (let [cycle (swap! ctrl update :cycle inc)
            cycle-n (:cycle cycle)
            timer (pb/set-timer max-idle-ms
                                (fn []
                                  (when (and (= cycle-n (:cycle @ctrl)) (not (:stopped? @ctrl)))
                                    (swap! ctrl assoc :restart-requested? true)
                                    ;; a NOOP is a command: imapflow sends DONE, idle() resolves, the loop re-enters IDLE
                                    (p/catch (ib/noop conn) (fn [_] nil)))))]
        (swap! ctrl assoc :timer timer)
        (-> (ib/idle conn)
            (p/then (fn [_]
                      (pb/clear-timer timer)
                      (let [{:keys [stopped? restart-requested?]} @ctrl]
                        (cond
                          stopped? (watcher-finish! ctrl :stopped)
                          (not (ib/usable? conn)) (watcher-finish! ctrl :connection-lost)
                          :else (do (when restart-requested?
                                      (swap! ctrl assoc :restart-requested? false)
                                      (watcher-emit! ctrl {:event :idle-restart :path path}))
                                    (watcher-idle-loop! ctrl))))))
            (p/catch (fn [e]
                       (pb/clear-timer timer)
                       (watcher-emit! ctrl {:event :error :error (->ex e)})
                       (watcher-finish! ctrl :error))))))))

(defn- watcher-on-exists!
  [ctrl {:keys [path count prev-count] :as ev}]
  (let [{:keys [conn fetch? query]} @ctrl
        base {:event :new-mail :path path :count count :prev-count prev-count}]
    (if (and fetch? (number? count) (number? prev-count) (> count prev-count))
      ;; a FETCH on the watcher's own connection breaks IDLE; the loop re-enters it
      (-> (ib/fetch-all conn (str (inc prev-count) ":*") (fetch-query query) {"uid" false})
          (p/then (fn [msgs]
                    (let [msgs (mapv normalize-message msgs)]
                      (watcher-emit! ctrl (assoc base :messages msgs :uids (mapv :uid msgs))))))
          (p/catch (fn [e] (watcher-emit! ctrl (assoc base :fetch-error (->ex e))))))
      (watcher-emit! ctrl (cond-> base (nil? count) (merge (dissoc ev :event)))))))

(defn watch!
  "Watches `mailbox` for changes on a DEDICATED connection (opened from the
   client's config via its :make-conn, disableAutoIdle, selected read-only)
   and returns a core.async channel of kebab events:
     {:event :open :path .. :mailbox {..}}                 once, after SELECT
     {:event :new-mail :path .. :count n :prev-count m}    every EXISTS whose
        count changed; with {:fetch? true} also :uids [..] :messages [..]
        (fetched with :query, default default-fetch-opts) — only new mail
        (count > prev-count) is fetched
     {:event :expunge :path .. :uid n :seq nil :vanished true} (:uid only
        with QRESYNC; otherwise :seq — inventory 10.3)
     {:event :flags :path .. :uid n :seq n :flags #{..} :modseq \"..\"}
     {:event :idle-restart :path ..}   every :max-idle-ms (29 min default)
        the loop breaks IDLE with a NOOP and re-enters it (RFC 2177)
     {:event :error :error <ex-info>}
     {:event :closed :path .. :reason :stopped|:consumer-closed|
        :connection-lost|:error|\"BYE ..\"}  last event; the channel is closed
        right after. The watcher never reconnects (inventory 13.16).
   Options: :buffer n (sliding buffer, default 64; the newest events win
   under back-pressure) or :buffer :block (unbuffered — the loop's put!
   waits on the consumer); :fetch? false; :query fetch opts; :max-idle-ms;
   :read-only? true. Stop with stop-watch! (or close the channel: the
   watcher notices on its next event and logs out)."
  ([client mailbox] (watch! client mailbox nil))
  ([client mailbox {:keys [buffer fetch? query max-idle-ms read-only?] :or {buffer 64}}]
   (let [ch (if (= :block buffer) (async/chan) (async/chan (async/sliding-buffer buffer)))
         ctrl (atom {:ch ch :path mailbox :cycle 0 :stopped? false :finished? false
                     :fetch? (boolean fetch?) :query query
                     :max-idle-ms (or max-idle-ms (:max-idle-ms client) default-max-idle-ms)})
         conn ((:make-conn client) (:wire-options client)
               (fn [err] (watcher-emit! ctrl {:event :error :error (error-map->ex err)})))]
     (swap! ctrl assoc :conn conn
            :stop! (fn [reason] (swap! ctrl assoc :stopped? true) (watcher-finish! ctrl reason)))
     (swap! watchers assoc ch ctrl)
     (ib/on conn "exists" (fn [ev] (watcher-on-exists! ctrl ev)))
     (ib/on conn "expunge" (fn [ev] (watcher-emit! ctrl (merge {:event :expunge :uid nil :seq nil} ev))))
     (ib/on conn "flags" (fn [ev] (watcher-emit! ctrl (merge {:event :flags} ev))))
     (ib/on conn "close" (fn [_] (watcher-finish! ctrl :connection-lost)))
     (-> (p/let [_ (ib/connect conn)
                 mb (ib/mailbox-open conn mailbox {"readOnly" (if (nil? read-only?) true (boolean read-only?))})]
           (watcher-emit! ctrl {:event :open :path (or (:path mb) mailbox) :mailbox mb})
           (watcher-idle-loop! ctrl))
         (p/catch (fn [e]
                    (watcher-emit! ctrl {:event :error :error (->ex e)})
                    (watcher-finish! ctrl :error))))
     ch)))

(defn stop-watch!
  "Stops the watcher behind `ch` (a channel from watch!): logs its
   connection out, emits {:event :closed :reason :stopped} and closes the
   channel. Promise of true (false when `ch` is not a live watcher)."
  [ch]
  (if-let [ctrl (get @watchers ch)]
    ((:stop! @ctrl) :stopped)
    (p/resolved false)))

(defn watching?
  "True while the watcher behind `ch` is alive."
  [ch]
  (contains? @watchers ch))
