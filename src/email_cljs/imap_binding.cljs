(ns email-cljs.imap-binding
  "Binding namespace (JavaScript interop lives here): every imapflow call the
   library makes, wrapped 1:1 with the JS-collection walking the inventory
   prescribes (14.2): Set -> set of strings, Map -> map, BigInt -> decimal
   string, Buffer -> Bytes, Date -> ISO-8601 UTC string, plain objects ->
   kebab-keyword maps (parameter maps and header maps keep their wire keys),
   arrays -> vectors. Inputs go the other way through ->js (Secrets
   revealed, Bytes unwrapped, keywords named) at the last moment.

   Connections are opaque: email-cljs.imap keeps one under :conn and hands
   it back to these functions. Every promise-returning function resolves to
   CLJS data and rejects with the raw ImapFlowError (email-cljs.imap
   classifies it via error->map + error-map->ex)."
  (:require ["imapflow" :refer (ImapFlow)]
            ["imapflow/lib/search-compiler" :refer (searchCompiler)]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [email-cljs.attachment :as att]
            [email-cljs.parse-binding :as pb]
            [email-cljs.util :as util]
            [promesa.core :as p]))

;; ---------------------------------------------------------------------------
;; JS -> CLJS
;; ---------------------------------------------------------------------------

(def passthrough-keys
  "Object keys whose nested keys are wire names, not camelCase option names:
   BODYSTRUCTURE parameter maps, HEADER search maps, the QUOTA resource map
   entries are single words anyway."
  #{:parameters :disposition-parameters :header})

(defn bigint?
  "True for a JS BigInt."
  [x]
  (identical? "bigint" (js* "typeof ~{}" x)))

(defn js-date?
  "True for a JS Date."
  [x]
  (instance? js/Date x))

(defn date->iso
  "JS Date -> ISO-8601 UTC string; an invalid Date -> nil."
  [d]
  (let [t (js-invoke d "getTime")]
    (when-not (js/isNaN t) (js-invoke d "toISOString"))))

(defn inst->search-date
  "JS Date / #inst -> its UTC calendar day as \"YYYY-MM-DD\" (the form
   imapflow's `new Date()` reads back as UTC midnight; see
   email-cljs.imap/search-date). Invalid dates -> nil."
  [d]
  (some-> (date->iso d) (subs 0 10)))

(defn- plain-object?
  [x]
  (and (some? x)
       (identical? "object" (js* "typeof ~{}" x))
       (let [proto (js/Object.getPrototypeOf x)]
         (or (nil? proto) (identical? proto js/Object.prototype)))))

(defn js->data
  "Deep JS -> CLJS conversion for imapflow results. `key-fn` (default
   util/camel->kebab) renames plain-object keys; subtrees under
   passthrough-keys keep string keys. Map keys are kept as they are
   (strings / numbers); Set members are converted values (strings mostly)."
  ([x] (js->data x util/camel->kebab false))
  ([x key-fn] (js->data x key-fn false))
  ([x key-fn verbatim?]
   (cond
     (or (nil? x) (undefined? x)) nil
     (string? x) x
     (number? x) x
     (boolean? x) x
     (bigint? x) (js-invoke x "toString")
     (js-date? x) (date->iso x)
     (pb/buffer? x) (att/bytes x)
     (instance? js/Map x) (into {} (map (fn [[k v]] [(js->data k key-fn true) (js->data v key-fn verbatim?)])) (es6-iterator-seq (js-invoke x "entries")))
     (instance? js/Set x) (into #{} (map #(js->data % key-fn verbatim?)) (es6-iterator-seq (js-invoke x "values")))
     (array? x) (into [] (map #(js->data % key-fn verbatim?)) x)
     (plain-object? x) (into {}
                             (map (fn [k]
                                    (let [v (unchecked-get x k)
                                          kk (if verbatim? k (key-fn k))]
                                      [kk (js->data v key-fn (or verbatim? (contains? passthrough-keys kk)))])))
                             (js/Object.keys x))
     :else x)))

(defn ->js
  "CLJS -> JS for imapflow inputs: Secrets revealed, Bytes unwrapped to
   Buffers, keywords named, nested maps/vectors converted. Strings and JS
   values pass through."
  [x]
  (clj->js (walk/postwalk (fn [v]
                            (cond
                              (util/secret? v) (util/reveal v)
                              (att/bytes? v) (att/unwrap v)
                              (keyword? v) (name v)
                              :else v))
                          x)))

(defn- as-promise
  [v]
  (js/Promise.resolve v))

;; ---------------------------------------------------------------------------
;; Errors
;; ---------------------------------------------------------------------------

(defn- field
  [e k]
  (let [v (unchecked-get e k)]
    (when-not (undefined? v) v)))

(defn error->map
  "Reads ImapFlowError / AuthenticationFailure fields (inventory 4.6) into a
   kebab map: {:message :name :code :command :path :response-status
   :response-text :server-response-code :authentication-failed?
   :tls-failed? :throttle-reset-ms :mailbox-missing? :reason
   :executed-command (dropped for AUTH/LOGIN commands, 12.11)}. ex-info
   passes through; other values -> {:message (str v)}."
  [e]
  (cond
    (util/error? e) e
    (nil? e) {:message "unknown error"}
    (instance? js/Error e)
    (let [cmd (field e "command")
          executed (field e "executedCommand")
          auth-cmd? (boolean (re-find #"(?i)^(AUTHENTICATE|LOGIN)" (str (or executed cmd ""))))
          response (field e "response")]
      (cond-> {:message (unchecked-get e "message")
               :name (unchecked-get e "name")
               :code (field e "code")
               :command cmd
               :path (field e "path")
               :response-status (field e "responseStatus")
               :response-text (or (field e "responseText") (when (string? response) response))
               :server-response-code (field e "serverResponseCode")
               :authentication-failed? (boolean (field e "authenticationFailed"))
               :tls-failed? (boolean (field e "tlsFailed"))
               :throttle-reset-ms (field e "throttleReset")
               :mailbox-missing? (boolean (field e "mailboxMissing"))
               :reason (field e "reason")}
        (and executed (not auth-cmd?)) (assoc :executed-command executed)))
    :else {:message (str e)}))

;; ---------------------------------------------------------------------------
;; Client construction and lifecycle
;; ---------------------------------------------------------------------------

(defn- capture-logger
  "A logger object for imapflow's `logger` option that drops trace / debug
   / info entries and hands the `err` of warn / error / fatal entries
   (read through error->map — response status and text, never the raw
   entry, which could carry command lines) to `f` as a map tagged
   {:level :warn|:error}. imapflow logs a NO / BAD it swallows (SEARCH,
   inventory 18.2) at warn level, so this is how email-cljs.imap learns
   the server's words."
  [f]
  (let [noop (fn [& _] nil)
        capture (fn [level]
                  (fn [entry & _]
                    (let [err (when (and (some? entry) (identical? "object" (js* "typeof ~{}" entry)))
                                (unchecked-get entry "err"))
                          msg (when (and (some? entry) (identical? "object" (js* "typeof ~{}" entry)))
                                (unchecked-get entry "msg"))]
                      (f (assoc (cond
                                  (instance? js/Error err) (error->map err)
                                  (string? msg) {:message msg}
                                  (string? entry) {:message entry}
                                  :else {:message "imapflow log entry"})
                                :level level)))))]
    #js {:trace noop :debug noop :info noop
         :warn (capture :warn) :error (capture :error) :fatal (capture :error)}))

(defn make-client
  "new ImapFlow(options): `options` is the string-keyed camelCase map built
   by email-cljs.imap (auth Secrets still wrapped). An 'error' listener is
   attached immediately (an unhandled 'error' would crash Node, inventory
   12.9); it calls `on-error` (fn [error-map]) when given. Unless the
   options ask for imapflow's own console logger (\"logger\" true), a
   capturing logger is installed that forwards warn / error entries to the
   same `on-error` tagged with :level (see capture-logger)."
  [options on-error]
  (let [options (if (or (true? (get options "logger")) (nil? on-error))
                  options
                  (assoc options "logger" (capture-logger on-error)))
        client (ImapFlow. (->js options))]
    (js-invoke client "on" "error" (fn [e] (when on-error (on-error (error->map e)))))
    client))

(defn connect
  "client.connect(): promise resolving after greeting, TLS, ENABLE and AUTH."
  [conn]
  (p/then (as-promise (js-invoke conn "connect")) (fn [_] nil)))

(defn logout
  "client.logout(): LOGOUT then close; promise."
  [conn]
  (p/then (as-promise (js-invoke conn "logout")) (fn [_] nil)))

(defn close
  "client.close(): closes the socket without LOGOUT; synchronous."
  [conn]
  (js-invoke conn "close")
  nil)

(defn noop
  "client.noop(); promise. Also the documented way to break a manual IDLE."
  [conn]
  (p/then (as-promise (js-invoke conn "noop")) (fn [_] nil)))

(defn usable?
  "client.usable — the connection can take commands."
  [conn]
  (boolean (unchecked-get conn "usable")))

(defn closed?
  "client.isClosed."
  [conn]
  (boolean (unchecked-get conn "isClosed")))

(defn idling?
  "client.idling."
  [conn]
  (boolean (unchecked-get conn "idling")))

(defn capabilities
  "client.capabilities Map -> {:uidplus true :appendlimit 26214400 ..}
   (keys lower-cased keywords; \"UTF8=ACCEPT\" -> :utf8=accept)."
  [conn]
  (let [m (unchecked-get conn "capabilities")]
    (if (instance? js/Map m)
      (into {} (map (fn [[k v]] [(keyword (str/lower-case (str k))) v])) (es6-iterator-seq (js-invoke m "entries")))
      {})))

(defn enabled
  "client.enabled Set -> set of strings (\"CONDSTORE\" ...)."
  [conn]
  (let [s (unchecked-get conn "enabled")]
    (if (instance? js/Set s) (into #{} (es6-iterator-seq (js-invoke s "values"))) #{})))

(defn has-capability?
  "True when the server advertised `cap` (e.g. \"UIDPLUS\")."
  [conn cap]
  (let [m (unchecked-get conn "capabilities")]
    (boolean (and (instance? js/Map m) (js-invoke m "has" cap)))))

(defn current-mailbox
  "client.mailbox -> kebab map (see mailbox-open) or nil when none is selected."
  [conn]
  (let [mb (unchecked-get conn "mailbox")]
    (when (and mb (not (false? mb))) (js->data mb))))

(defn server-info
  "client.serverInfo (ID response) -> map or nil."
  [conn]
  (js->data (unchecked-get conn "serverInfo")))

(defn tls-info
  "client.tls -> {:authorized .. :cipher ..} or nil (cleartext)."
  [conn]
  (let [t (unchecked-get conn "tls")]
    (when (and t (not (false? t))) (js->data t))))

;; ---------------------------------------------------------------------------
;; Mailboxes
;; ---------------------------------------------------------------------------

(defn list-mailboxes
  "client.list(options) -> vector of ListResponse maps (flags as sets,
   status with uid-validity as string)."
  [conn options]
  (p/then (as-promise (js-invoke conn "list" (->js options))) js->data))

(defn status
  "client.status(path, query) -> StatusObject map."
  [conn path query]
  (p/then (as-promise (js-invoke conn "status" path (->js query))) js->data))

(defn get-quota
  "client.getQuota(path) -> quota map (storage already in bytes, inventory
   13.12) or nil when unsupported."
  [conn path]
  (p/then (as-promise (if path (js-invoke conn "getQuota" path) (js-invoke conn "getQuota")))
          (fn [r] (when (and r (not (false? r))) (js->data r)))))

(defn mailbox-open
  "client.mailboxOpen(path, {readOnly}) -> MailboxObject map."
  [conn path options]
  (p/then (as-promise (js-invoke conn "mailboxOpen" path (->js options))) js->data))

(defn mailbox-close
  "client.mailboxClose() — sends CLOSE (expunges \\Deleted in a read-write
   mailbox, inventory 12.7). Promise of boolean."
  [conn]
  (p/then (as-promise (js-invoke conn "mailboxClose")) boolean))

(defn get-mailbox-lock
  "client.getMailboxLock(path, {readOnly}) -> {:path .. :release (fn)}."
  [conn path options]
  (p/then (as-promise (js-invoke conn "getMailboxLock" path (->js options)))
          (fn [lock] {:path (unchecked-get lock "path")
                      :release (fn [] (js-invoke lock "release") nil)})))

(defn mailbox-create
  "client.mailboxCreate(path) -> {:path :mailbox-id :created}."
  [conn path]
  (p/then (as-promise (js-invoke conn "mailboxCreate" path)) js->data))

(defn mailbox-delete
  "client.mailboxDelete(path) -> {:path}."
  [conn path]
  (p/then (as-promise (js-invoke conn "mailboxDelete" path)) js->data))

(defn mailbox-rename
  "client.mailboxRename(path, new-path) -> {:path :new-path}."
  [conn path new-path]
  (p/then (as-promise (js-invoke conn "mailboxRename" path new-path)) js->data))

;; ---------------------------------------------------------------------------
;; Messages
;; ---------------------------------------------------------------------------

(defn search
  "client.search(query, options) -> vector of numbers, an ESEARCH map, or
   nil (no mailbox selected / false)."
  [conn query options]
  (p/then (as-promise (js-invoke conn "search" (->js query) (->js options)))
          (fn [r] (cond (or (nil? r) (false? r) (undefined? r)) nil
                        :else (js->data r)))))

(defn fetch-all
  "client.fetchAll(range, query, options) -> vector of message maps."
  [conn range query options]
  (p/then (as-promise (js-invoke conn "fetchAll" (->js range) (->js query) (->js options))) js->data))

(defn fetch-one
  "client.fetchOne(seq, query, options) -> message map or nil."
  [conn seq query options]
  (p/then (as-promise (js-invoke conn "fetchOne" (->js seq) (->js query) (->js options)))
          (fn [r] (when (and r (not (false? r))) (js->data r)))))

(defn download
  "client.download(range, part, options): drains the content stream.
   Promise of {:meta {..} :content Bytes} or nil when nothing was found."
  [conn range part options]
  (p/let [r (as-promise (if part
                          (js-invoke conn "download" (->js range) part (->js options))
                          (js-invoke conn "download" (->js range) js/undefined (->js options))))
          content (unchecked-get r "content")]
    (when (and content (not (false? content)))
      (p/let [buf (pb/stream->buffer content (:max-bytes options))]
        {:meta (js->data (unchecked-get r "meta"))
         :content (att/bytes buf)}))))

(defn- store
  [method conn range flags options]
  (p/then (as-promise (js-invoke conn method (->js range) (->js (vec flags)) (->js options))) boolean))

(defn message-flags-set
  "client.messageFlagsSet -> boolean."
  [conn range flags options]
  (store "messageFlagsSet" conn range flags options))

(defn message-flags-add
  "client.messageFlagsAdd -> boolean."
  [conn range flags options]
  (store "messageFlagsAdd" conn range flags options))

(defn message-flags-remove
  "client.messageFlagsRemove -> boolean."
  [conn range flags options]
  (store "messageFlagsRemove" conn range flags options))

(defn message-delete
  "client.messageDelete(range, options): \\Deleted + UID EXPUNGE (UIDPLUS) or
   EXPUNGE. Promise of boolean."
  [conn range options]
  (p/then (as-promise (js-invoke conn "messageDelete" (->js range) (->js options))) boolean))

(defn- copy-result
  [r]
  (when (and r (not (false? r)))
    (let [m (js->data r)]
      (cond-> m (:uid-map m) (update :uid-map #(into {} (map (fn [[k v]] [(js/Number k) v])) %))))))

(defn message-move
  "client.messageMove -> {:path :destination :uid-validity :uid-map {src dst}} or nil."
  [conn range destination options]
  (p/then (as-promise (js-invoke conn "messageMove" (->js range) destination (->js options))) copy-result))

(defn message-copy
  "client.messageCopy -> same shape as message-move."
  [conn range destination options]
  (p/then (as-promise (js-invoke conn "messageCopy" (->js range) destination (->js options))) copy-result))

(defn append
  "client.append(path, content, flags, idate) -> {:destination :uid
   :uid-validity :seq} or nil."
  [conn path content flags idate]
  (p/then (as-promise (js-invoke conn "append" path (->js content) (->js (vec flags)) (if idate (js/Date. idate) js/undefined)))
          (fn [r] (when (and r (not (false? r))) (js->data r)))))

(defn exec
  "Runs a raw IMAP command through imapflow's compiler (used for the bare
   EXPUNGE and UID EXPUNGE that imapflow does not expose as methods).
   `attributes` is a vector of {:type \"SEQUENCE\"|\"ATOM\" :value ..} maps
   or nil. Promise of the parsed tagged response (kebab)."
  [conn command attributes]
  (p/let [r (as-promise (js-invoke conn "exec" command (if attributes (->js attributes) false)))]
    (when (and r (fn? (unchecked-get r "next"))) (js-invoke r "next"))
    (js->data (unchecked-get r "response"))))

(defn idle
  "client.idle(): resolves when the IDLE session ends (broken by another
   command, by the server, or by the connection closing); undefined when no
   mailbox is selected; false when the session failed."
  [conn]
  (as-promise (js-invoke conn "idle")))

;; ---------------------------------------------------------------------------
;; Events
;; ---------------------------------------------------------------------------

(defn on
  "client.on(event, f): `f` receives the converted payload (kebab map with
   flags as sets, modseq as string; nil for 'close'). Returns the listener
   handle for off."
  [conn event f]
  (let [listener (fn [payload] (f (js->data payload)))]
    (js-invoke conn "on" event listener)
    listener))

(defn off
  "Removes a listener handle returned by on."
  [conn event listener]
  (js-invoke conn "off" event listener)
  nil)

;; ---------------------------------------------------------------------------
;; Offline search compilation (tests / diagnostics)
;; ---------------------------------------------------------------------------

(defn compile-search-attributes
  "Runs imapflow's searchCompiler against `conn` (only its mailbox,
   capabilities and enabled fields are read — a fake works) with a
   string-keyed camelCase query map; returns the wire attribute vector
   ([{:type \"ATOM\" :value \"UNSEEN\"} ...]) without contacting a server.
   Throws imapflow's own error for invalid queries."
  [conn query]
  (js->data (searchCompiler conn (->js query))))
