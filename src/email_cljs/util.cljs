(ns email-cljs.util
  "Small shared helpers: keyword case translation between kebab-case
   ClojureScript keywords and the camelCase option / result names of
   nodemailer, imapflow and mailparser; the opaque Secret that carries the
   account password; secrets-file parsing; SMTP reply-code classification;
   ex-info error construction and detection.

   Pure: no I/O and no JavaScript interop. Converting CLJS maps to JS option
   objects, reading `code`/`response`/`responseCode` off a thrown JS error and
   touching Buffers/streams is interop and therefore lives in the binding
   namespaces (email-cljs.smtp-binding, email-cljs.imap-binding,
   email-cljs.parse-binding — Phase 2+); this namespace only deals in
   strings, keywords and maps (docs/email-api-inventory.org section 9)."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; kebab-case <-> camelCase (inventory 9.2: nodemailer / imapflow / mailparser
;; are one camelCase dialect: contentType, bodyStructure, internalDate ...)
;; ---------------------------------------------------------------------------

(defn kebab->camel
  "\"content-type\" -> \"contentType\". Keywords render as their name (the
   namespace is dropped); strings pass through untouched (they are assumed
   to be exact upstream names already). Single-word names are unchanged
   (:uid -> \"uid\")."
  [k]
  (cond
    (keyword? k) (let [[head & tail] (str/split (name k) #"-")]
                   (apply str head (map str/capitalize tail)))
    (string? k) k
    :else (str k)))

(defn camel->kebab
  "\"contentType\" -> :content-type, \"bodyStructure\" -> :body-structure.
   Works on strings and keywords, returns a keyword. Namespaced keywords are
   returned unchanged. Names that already contain hyphens or underscores are
   only lower-cased (mailparser header names such as \"content-type\")."
  [k]
  (if (and (keyword? k) (namespace k))
    k
    (-> (name k)
        (str/replace #"([a-z0-9])([A-Z])" "$1-$2")
        str/lower-case
        keyword)))

(defn transform-keys
  "Recursively applies `f` to every map key in `x` (maps inside vectors/lists
   included). Keys listed in `passthrough` (a set) keep their subtree
   untouched — user-owned values (custom `headers`, attachment `content`,
   a raw message, imapflow `search` header maps) must never be re-cased."
  ([f x] (transform-keys f #{} x))
  ([f passthrough x]
   (cond
     (map? x) (into (empty x)
                    (map (fn [[k v]]
                           (if (contains? passthrough k)
                             [(f k) v]
                             [(f k) (transform-keys f passthrough v)])))
                    x)
     (sequential? x) (into (empty x) (map #(transform-keys f passthrough %)) x)
     :else x)))

(def option-passthrough-keys
  "Kebab keys whose subtree is handed to the wrapped package verbatim: user
   data and opaque upstream values (inventory 9.2)."
  #{:headers :content :raw :header :tls :auth :dkim :envelope :secret :params})

(defn ->camel-keys
  "Kebab keyword option map -> string-keyed camelCase map, nil values
   dropped, nested option maps translated, subtrees under
   option-passthrough-keys untouched. nil in -> nil out. Pure: the result is
   still a CLJS map; a binding namespace turns it into a JS object."
  [opts]
  (when (some? opts)
    (transform-keys kebab->camel option-passthrough-keys
                    (into {} (remove (comp nil? val)) opts))))

(defn ->kebab-keys
  "Deeply translates camelCase string/keyword keys (as js->clj yields them)
   to kebab keywords; values are never touched. Subtrees under `passthrough`
   keep their keys (e.g. a parsed message's :headers map)."
  ([x] (transform-keys camel->kebab x))
  ([passthrough x] (transform-keys camel->kebab passthrough x)))

;; ---------------------------------------------------------------------------
;; Opaque secret (the account password; inventory 1.4)
;; ---------------------------------------------------------------------------

(defprotocol ISecret
  (reveal [this] "The secret string. Used only by the binding namespaces to build the auth object."))

(deftype Secret [v]
  ISecret
  (reveal [_] v)
  IEquiv
  (-equiv [_ other] (and (satisfies? ISecret other) (= v (reveal other))))
  IHash
  (-hash [_] (hash v))
  IPrintWithWriter
  (-pr-writer [_ writer _] (-write writer "#email-cljs/secret <redacted>"))
  Object
  (toString [_] "<redacted>"))

(defn secret
  "Wraps a string into an opaque Secret (idempotent). nil -> nil."
  [s]
  (cond (nil? s) nil
        (satisfies? ISecret s) s
        :else (Secret. (str s))))

(defn secret?
  "True when `x` is an opaque Secret."
  [x] (satisfies? ISecret x))

(defn clean-secret
  "Cleans a secret copied from a UI or a secrets file: trims whitespace and
   strips ONE layer of surrounding double/single/back quotes. Unlike an API
   key, a mail password may legitimately start or end with punctuation, so
   NO further decoration is stripped. nil for blank input."
  [v]
  (let [v (when (string? v) (str/trim v))]
    (when-not (str/blank? v)
      (let [n (count v)
            v (if (and (> n 2)
                       (or (and (str/starts-with? v "\"") (str/ends-with? v "\""))
                           (and (str/starts-with? v "'") (str/ends-with? v "'"))
                           (and (str/starts-with? v "`") (str/ends-with? v "`"))))
                (str/trim (subs v 1 (dec n)))
                v)]
        (when-not (str/blank? v) v)))))

(def secret-keys
  "Config keys whose values are wrapped by redact-config."
  #{:password :pass :access-token :refresh-token :client-secret})

(defn redact-config
  "Wraps every secret-keys value of a (possibly nested :auth) config map in
   an opaque Secret so the map can be printed or logged safely. Idempotent."
  [cfg]
  (when (some? cfg)
    (into (empty cfg)
          (map (fn [[k v]]
                 [k (cond
                      (contains? secret-keys k) (secret v)
                      (map? v) (redact-config v)
                      :else v)]))
          cfg)))

(defn parse-secrets-file
  "Parses secrets-file text into {\"NAME\" value ...}: `NAME=value` lines
   (optional `export`, surrounding whitespace, one layer of quotes via
   clean-secret), `#` comments and blank lines ignored. Values keep their
   punctuation. Pure; the caller reads the file (binding/core namespace)."
  [text]
  (reduce (fn [m line]
            (let [line (str/trim (str line))]
              (if (or (str/blank? line) (str/starts-with? line "#"))
                m
                (if-let [[_ k v] (re-matches #"^(?:export\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.*)$" line)]
                  (assoc m k (clean-secret v))
                  m))))
          {}
          (str/split-lines (str text))))

(defn parse-bool
  "\"true\"/\"1\"/\"yes\"/\"on\" -> true, \"false\"/\"0\"/\"no\"/\"off\" -> false
   (case-insensitive, trimmed); booleans pass through; anything else nil."
  [v]
  (cond
    (boolean? v) v
    (string? v) (case (str/lower-case (str/trim v))
                  ("true" "1" "yes" "on") true
                  ("false" "0" "no" "off") false
                  nil)
    :else nil))

;; ---------------------------------------------------------------------------
;; SMTP reply codes (inventory 3.2)
;; ---------------------------------------------------------------------------

(defn smtp-code-class
  "Classifies an SMTP reply code (integer or numeric string):
   :preliminary (1xx) :success (2xx) :intermediate (3xx)
   :transient (4xx, retry later) :permanent (5xx); nil when malformed."
  [code]
  (let [n (cond (number? code) code
                (string? code) (when (re-matches #"^\d{3}$" (str/trim code))
                                 (js/parseInt code 10))
                :else nil)]
    (when (and n (<= 100 n 599))
      (case (quot n 100)
        1 :preliminary 2 :success 3 :intermediate 4 :transient 5 :permanent))))

(defn smtp-retryable?
  "True for 4xx (transient) reply codes."
  [code]
  (= :transient (smtp-code-class code)))

;; ---------------------------------------------------------------------------
;; Errors (inventory section 6)
;; ---------------------------------------------------------------------------

(def error-types
  "Every :type an email-cljs ex-info can carry."
  #{:email-cljs/config-error :email-cljs/argument-error
    :email-cljs/auth-error :email-cljs/network-error
    :email-cljs/smtp-error :email-cljs/imap-error :email-cljs/parse-error})

(defn- make-error
  [type prefix message data]
  (ex-info (str prefix message)
           (merge {:type type} data)))

(defn config-error
  "ex-info for a client-configuration problem (missing/unreadable/corrupt
   credentials, bad host/port). Data: {:type :email-cljs/config-error
   :reason .. ..}. Never embed secret material in `data`."
  [message & [data]]
  (make-error :email-cljs/config-error "email-cljs: " message data))

(defn argument-error
  "ex-info for an invalid argument detected before any connection is used."
  [message & [data]]
  (make-error :email-cljs/argument-error "email-cljs: " message data))

(defn auth-error
  "ex-info for a rejected login (nodemailer EAUTH / imapflow
   authenticationFailed). Data: {:type :email-cljs/auth-error :protocol
   :smtp|:imap :code .. :response .. :response-code ..}."
  [protocol message & [data]]
  (make-error :email-cljs/auth-error (str "email-cljs " (name protocol) " authentication failed: ") message
              (assoc data :protocol protocol)))

(defn network-error
  "ex-info wrapping a connection-level failure (ECONNECTION, ETIMEDOUT, EDNS,
   ESOCKET, TLS handshake). Data: {:type :email-cljs/network-error :protocol
   .. :code .. :retryable? .. :cause ..}."
  [protocol message & [data]]
  (make-error :email-cljs/network-error (str "email-cljs " (name protocol) " network error: ") message
              (assoc data :protocol protocol)))

(defn smtp-error
  "ex-info for an SMTP-level failure reported by the server after connecting
   (EENVELOPE, EMESSAGE, per-recipient rejection). Data: {:type
   :email-cljs/smtp-error :code .. :command .. :response .. :response-code ..
   :rejected [..] :accepted [..] :retryable? ..}."
  [message & [data]]
  (make-error :email-cljs/smtp-error "email-cljs SMTP error: " message
              (cond-> data
                (and (nil? (:retryable? data)) (:response-code data))
                (assoc :retryable? (smtp-retryable? (:response-code data))))))

(defn imap-error
  "ex-info for an IMAP command failure (NO/BAD tagged response, connection
   not open, mailbox not selected). Data: {:type :email-cljs/imap-error
   :command .. :response-status \"NO\"|\"BAD\" :response-text .. :server-response-code
   .. :code ..}."
  [message & [data]]
  (make-error :email-cljs/imap-error "email-cljs IMAP error: " message data))

(defn parse-error
  "ex-info for a message that could not be parsed or built."
  [message & [data]]
  (make-error :email-cljs/parse-error "email-cljs parse error: " message data))

(defn error-data
  "Returns the structured data of an email-cljs error, or nil when `e` is
   not one."
  [e]
  (when-let [d (ex-data e)]
    (when (contains? error-types (:type d))
      d)))

(defn error-type
  "One of the email-cljs error :type keywords (see error-types) or nil."
  [e]
  (:type (error-data e)))

(defn error?
  "True when `e` is an email-cljs ex-info."
  [e] (some? (error-data e)))

(defn retryable?
  "True when the error carries :retryable? true (transient SMTP code,
   timeout, connection reset)."
  [e] (boolean (:retryable? (error-data e))))
