(ns email-cljs.smtp
  "SMTP sending over nodemailer — the public, interop-free surface.

   (smtp-client opts) builds a client map; (send! client message) sends a
   kebab-case message map and resolves to the kebab send result;
   (verify-smtp! client) checks DNS + TCP + TLS + AUTH; (close-smtp! client)
   closes a pooled transport. Every async operation returns a promesa
   promise; every failure is an ex-info from email-cljs.util (:auth-error,
   :network-error, :smtp-error, :argument-error, :config-error — inventory
   8.1) that never carries the password.

   All nodemailer interop is in email-cljs.smtp-binding; this namespace
   only translates data: kebab message -> wire camelCase map
   (message->wire), kebab client options -> transport options
   (client-options->wire), NodemailerError fields -> ex-info
   (error-map->ex). Those three are pure and unit-tested without a
   transport.

   House positions (inventory 13.9, 13.20, 12.15, 11.4): single-connection
   transport by default, :pool? opt-in; send! resolves even when SOME
   recipients were rejected (the accepted ones were delivered and cannot be
   recalled) and reports them under :rejected / :all-accepted? — :strict?
   true turns that into an :email-cljs/smtp-error rejection; send! never
   waits for IMAP visibility; Bcc: is stripped from the transmitted message
   by nodemailer's SMTP transport (docs claim; the Phase 5 smoke asserts
   it); file (:path) and URL (:href) content sources are refused unless the
   client was created with :allow-file-access? / :allow-url-access? true."
  (:require [clojure.string :as str]
            [email-cljs.attachment :as att]
            [email-cljs.smtp-binding :as sb]
            [email-cljs.util :as util]
            [promesa.core :as p]))

;; ---------------------------------------------------------------------------
;; Client configuration
;; ---------------------------------------------------------------------------

(def default-port
  "Port used when neither :port nor :secure is given: 465, implicit TLS
   (RFC 8314; the house account — inventory 3.3). 587 + STARTTLS needs an
   explicit {:port 587 :secure false}."
  465)

(def client-option-keys
  "Every key smtp-client accepts."
  #{:host :port :secure :auth :auth-method :name :tls :logger? :pool?
    :max-connections :max-messages :rate-limit :rate-delta
    :connection-timeout-ms :greeting-timeout-ms :socket-timeout-ms :dns-timeout-ms
    :require-tls? :ignore-tls? :max-recipients
    :allow-file-access? :allow-url-access? :defaults :transport :secrets-file})

(defn- tls->wire
  "Kebab :tls blob -> camelCase Node tls.connect options. :ca may be a Secret,
   a PEM string, Bytes or a vector of those (revealed/unwrapped in the
   binding). rejectUnauthorized is never defaulted to false here: it is
   whatever the caller wrote, and the documented way to trust a private
   chain is :ca (inventory 13.5)."
  [tls]
  (when (map? tls)
    (into {} (map (fn [[k v]] [(util/kebab->camel k) v])) tls)))

(defn- resolve-port+secure
  [{:keys [port secure]}]
  (let [port (cond (nil? port) (cond (true? secure) 465 (false? secure) 587 :else default-port)
                   :else (or (util/parse-port port)
                             (throw (util/config-error (str "invalid SMTP port " (pr-str port))
                                                       {:reason :bad-port :port port}))))
        secure (cond (boolean? secure) secure
                     (some? (util/parse-bool secure)) (util/parse-bool secure)
                     :else (= 465 port))]
    {:port port :secure secure}))

(defn- auth->wire
  [auth]
  (when (some? auth)
    (when-not (map? auth)
      (throw (util/config-error ":auth must be a map {:user :pass}" {:reason :missing-credentials})))
    (let [user (:user auth) pass (:pass auth)]
      (when (or (nil? user) (nil? pass))
        (throw (util/config-error "SMTP :auth needs both :user and :pass"
                                  {:reason :missing-credentials})))
      (cond-> {"type" "login"
               "user" (util/secret user)
               "pass" (util/secret pass)}
        (:method auth) (assoc "method" (str/upper-case (name (:method auth))))))))

(defn client-options->wire
  "Pure: kebab client options -> the string-keyed camelCase nodemailer
   transport option map (Secrets still wrapped; revealed by the binding).
   Defaults: port 465 / secure true (or secure true whenever the port is
   465), logger off, single connection, disableFileAccess and
   disableUrlAccess on. Throws :email-cljs/config-error for a missing host
   or bad port / credentials."
  [opts]
  (let [unknown (remove client-option-keys (keys opts))]
    (when (seq unknown)
      (throw (util/config-error (str "unknown smtp-client option(s) " (pr-str (vec unknown)))
                                {:reason :unknown-option :keys (vec unknown)}))))
  (when (str/blank? (str (:host opts)))
    (throw (util/config-error "SMTP :host is required" {:reason :bad-host})))
  (let [{:keys [port secure]} (resolve-port+secure opts)
        {:keys [pool? auth-method]} opts]
    (->> (cond-> {"host" (str/trim (:host opts))
                  "port" port
                  "secure" secure
                  "auth" (auth->wire (:auth opts))
                  "authMethod" (when auth-method (str/upper-case (name auth-method)))
                  "name" (:name opts)
                  "tls" (tls->wire (:tls opts))
                  "logger" (boolean (:logger? opts))
                  "connectionTimeout" (:connection-timeout-ms opts)
                  "greetingTimeout" (:greeting-timeout-ms opts)
                  "socketTimeout" (:socket-timeout-ms opts)
                  "dnsTimeout" (:dns-timeout-ms opts)
                  "requireTLS" (:require-tls? opts)
                  "ignoreTLS" (:ignore-tls? opts)
                  "maxRecipients" (:max-recipients opts)
                  "disableFileAccess" (not (:allow-file-access? opts))
                  "disableUrlAccess" (not (:allow-url-access? opts))}
           pool? (assoc "pool" true
                        "maxConnections" (or (:max-connections opts) 5)
                        "maxMessages" (or (:max-messages opts) 100)
                        "rateLimit" (:rate-limit opts)
                        "rateDelta" (:rate-delta opts)))
         (remove (comp nil? val))
         (into {}))))

(defn- policy
  [opts]
  {:allow-file-access? (boolean (:allow-file-access? opts))
   :allow-url-access? (boolean (:allow-url-access? opts))})

(defn- message-defaults
  [opts]
  (cond-> {"disableFileAccess" (not (:allow-file-access? opts))
           "disableUrlAccess" (not (:allow-url-access? opts))}
    (map? (:defaults opts)) (merge (util/->camel-keys (:defaults opts)))))

(defn smtp-client
  "Builds an SMTP client map {:transport <opaque> :config <kebab, secrets
   redacted> :policy {..} :pool? bool}. Options:
   :host (required) :port (465 default) :secure (implicit TLS; default true
   for 465) :auth {:user <Secret|string> :pass <Secret|string> :method}
   :auth-method (:plain :login :cram-md5) :name (EHLO name)
   :tls {:ca <Secret|PEM|Bytes> :reject-unauthorized :min-version :servername}
   :pool? false :max-connections 5 :max-messages 100 :rate-limit :rate-delta
   :connection-timeout-ms :greeting-timeout-ms :socket-timeout-ms :dns-timeout-ms
   :require-tls? :ignore-tls? :max-recipients :logger? false
   :allow-file-access? false :allow-url-access? false
   :defaults {message defaults merged into every send}
   :transport <opaque plugin> (tests / jsonTransport: replaces host+port).
   Never sets tls.rejectUnauthorized to false itself; a private chain is
   trusted through :ca (inventory 13.5). Throws :email-cljs/config-error."
  [opts]
  (let [opts (or opts {})
        plugin (:transport opts)
        wire (when (nil? plugin) (client-options->wire opts))
        transport (sb/create-transport {:options wire :plugin plugin :defaults (message-defaults opts)})]
    {:transport transport
     :config (util/redact-config (dissoc opts :transport))
     :options (some-> wire (dissoc "auth"))
     :policy (policy opts)
     :pool? (boolean (:pool? opts))}))

(defn client?
  "True for a map built by smtp-client."
  [x]
  (and (map? x) (some? (:transport x)) (contains? x :policy)))

(defn config-from-env
  "Pure: {:username :password :smtp :tls} from util/env-config -> smtp-client
   options ({:host :port :secure :auth {:user :pass} :tls {:ca}}). Throws
   :email-cljs/config-error when the host or credentials are missing."
  [{:keys [username password smtp tls]}]
  (when (or (nil? username) (nil? password))
    (throw (util/config-error "no SMTP credentials (USERNAME / PASSWORD) in the environment or secrets file"
                              {:reason :missing-credentials})))
  (when (str/blank? (str (:host smtp)))
    (throw (util/config-error "no SMTP host (SMTP_HOST) in the environment or secrets file"
                              {:reason :secrets-file-incomplete :key "SMTP_HOST"})))
  (cond-> {:host (:host smtp)
           :auth {:user username :pass password}}
    (:port smtp) (assoc :port (:port smtp))
    (some? (:secure smtp)) (assoc :secure (:secure smtp))
    (:ca tls) (assoc :tls {:ca (:ca tls)})))

(defn smtp-client-from-env
  "smtp-client configured from <prefix>USERNAME / PASSWORD / SMTP_HOST /
   SMTP_PORT / SMTP_TLS / TLS_CA read by util/from-env (environment over the
   secrets file — :secrets-file in `opts` names it, else EMAIL_SECRETS_FILE,
   else ~/.secrets/borret.txt). `prefix` e.g. \"BORRET_EMAIL_\"; `opts` are
   merged over the environment values (deep). Throws
   :email-cljs/config-error when credentials or host are missing."
  ([prefix] (smtp-client-from-env prefix nil))
  ([prefix opts]
   (let [env (util/from-env (:secrets-file opts) prefix)]
     (smtp-client (util/deep-merge (config-from-env env) (dissoc opts :secrets-file))))))

;; ---------------------------------------------------------------------------
;; Message translation (pure)
;; ---------------------------------------------------------------------------

(def message-keys
  "Every key a message map may carry (inventory 2.4 / 11.1)."
  #{:from :sender :reply-to :to :cc :bcc
    :subject :text :html :watch-html :amp :ical-event
    :attachments :alternatives :headers :list :envelope
    :in-reply-to :references :message-id :date :priority
    :encoding :text-encoding :x-mailer :raw :dsn :newline})

(def content-source-keys
  "Exactly one of these must be present on an attachment / alternative."
  #{:content :bytes :path :href :raw})

(defn- address-field
  [x]
  (mapv (fn [a] (if (:raw a) (:raw a) a)) (util/addresses x)))

(defn- text-content
  [v k]
  (cond
    (nil? v) nil
    (string? v) v
    (att/bytes? v) v
    (map? v) v
    :else (throw (util/argument-error (str k " must be a string, Bytes or a content map")
                                      {:reason :bad-content :key k}))))

(defn- content-source->wire
  "Attachment / alternative map -> wire map (kebab keys kept; camelised by
   the caller). Validates the single content source and the file/URL
   policy (inventory 9.2, 11.4)."
  [policy what idx a]
  (when-not (map? a)
    (throw (util/argument-error (str what " " idx " must be a map") {:reason :bad-attachment :index idx})))
  (let [sources (filter #(some? (get a %)) content-source-keys)]
    (when (not= 1 (count sources))
      (throw (util/argument-error (str what " " idx " needs exactly one of :content :bytes :path :href :raw")
                                  {:reason :attachment-source :index idx :sources (vec sources)})))
    (when (and (:path a) (not (:allow-file-access? policy)))
      (throw (util/argument-error (str what " " idx " uses :path but file access is disabled (smtp-client :allow-file-access? true)")
                                  {:reason :file-access-disabled :index idx})))
    (when (and (:href a) (not (:allow-url-access? policy)))
      (throw (util/argument-error (str what " " idx " uses :href but URL access is disabled (smtp-client :allow-url-access? true)")
                                  {:reason :url-access-disabled :index idx})))
    (let [bytes (:bytes a)
          bytes (when (some? bytes) (att/bytes bytes))]
      (cond-> (dissoc a :bytes)
        (some? bytes) (assoc :content bytes)
        (keyword? (:encoding a)) (update :encoding name)
        (keyword? (:content-disposition a)) (update :content-disposition name)
        (keyword? (:content-transfer-encoding a)) (update :content-transfer-encoding name)))))

(defn- validate-message-keys
  [message]
  (when-not (map? message)
    (throw (util/argument-error "message must be a map" {:reason :bad-message})))
  (let [unknown (remove message-keys (keys message))]
    (when (seq unknown)
      (throw (util/argument-error (str "unknown message key(s) " (pr-str (vec unknown)))
                                  {:reason :unknown-message-key :keys (vec unknown)})))))

(defn message->wire
  "Pure: kebab message map -> string-keyed camelCase nodemailer
   SendMailOptions map (Bytes stay wrapped; Secrets never appear).
   Addresses (:from :sender :reply-to :to :cc :bcc) go through
   util/addresses ({:name :address} maps; strings that are not one mailbox
   pass verbatim for nodemailer's parser). Keyword enums render as strings
   (:priority :high -> \"high\"). :headers / :envelope / :list are
   user-owned and pass verbatim. Attachments and alternatives: exactly one
   of :content (string or Bytes) / :bytes / :path / :href / :raw; :path and
   :href need the client policy flags. `policy` {:allow-file-access?
   :allow-url-access? :allow-no-recipients? (message/build drafts)}.
   Throws :email-cljs/argument-error:
   :unknown-message-key, :no-recipients, :raw-needs-envelope,
   :bad-priority, :attachment-source, :file-access-disabled,
   :url-access-disabled, :bad-address."
  ([message] (message->wire message {}))
  ([message policy]
   (validate-message-keys message)
   (let [to (address-field (:to message))
         cc (address-field (:cc message))
         bcc (address-field (:bcc message))
         from (address-field (:from message))
         priority (:priority message)]
     (when (and (empty? to) (empty? cc) (empty? bcc) (empty? (get-in message [:envelope :to]))
                (not (:allow-no-recipients? policy)))
       (throw (util/argument-error "message has no recipients (:to / :cc / :bcc / :envelope :to)"
                                   {:reason :no-recipients})))
     (when (and (some? (:raw message)) (nil? (:envelope message)))
       (throw (util/argument-error ":raw messages need an explicit :envelope {:from :to}"
                                   {:reason :raw-needs-envelope})))
     (when (and (some? priority) (not (contains? #{:high :normal :low "high" "normal" "low"} priority)))
       (throw (util/argument-error "priority must be :high, :normal or :low" {:reason :bad-priority :value priority})))
     (let [m (cond-> (dissoc message :from :sender :reply-to :to :cc :bcc :attachments :alternatives)
               (seq from) (assoc :from (first from))
               (:sender message) (assoc :sender (first (address-field (:sender message))))
               (:reply-to message) (assoc :reply-to (address-field (:reply-to message)))
               (seq to) (assoc :to to)
               (seq cc) (assoc :cc cc)
               (seq bcc) (assoc :bcc bcc)
               (contains? message :text) (update :text text-content :text)
               (contains? message :html) (update :html text-content :html)
               (:attachments message) (assoc :attachments (vec (map-indexed #(content-source->wire policy "attachment" %1 %2) (:attachments message))))
               (:alternatives message) (assoc :alternatives (vec (map-indexed #(content-source->wire policy "alternative" %1 %2) (:alternatives message))))
               (keyword? priority) (assoc :priority (name priority))
               (keyword? (:encoding message)) (update :encoding name)
               (keyword? (:text-encoding message)) (update :text-encoding name)
               (:message-id message) (update :message-id util/normalize-message-id)
               (:in-reply-to message) (update :in-reply-to util/normalize-message-id)
               (:references message) (update :references #(if (sequential? %) (mapv util/normalize-message-id %) (util/normalize-message-id %))))]
       (util/->camel-keys m)))))

;; ---------------------------------------------------------------------------
;; Error classification (pure; inventory 8.1)
;; ---------------------------------------------------------------------------

(def ^:private network-codes #{"ECONNECTION" "ETIMEDOUT" "ESOCKET" "EDNS" "ETLS" "EPROTOCOL"})
(def ^:private smtp-codes #{"EENVELOPE" "EMESSAGE" "EREQUIRETLS"})
(def ^:private argument-codes #{"EMAXRECIPIENTS" "ESTREAM" "EFILEACCESS" "EURLACCESS" "EFETCH"})
(def ^:private config-codes #{"ECONFIG" "EPROXY" "ESES" "ESENDMAIL"})

(defn error-map->ex
  "Pure: a NodemailerError read into a map by smtp-binding/error->map
   ({:code :command :response :response-code :message :rejected :accepted
   :rejected-errors :recipient}) -> the house ex-info:
   EAUTH / EOAUTH2 -> :auth-error (454 retryable); ENOAUTH -> :config-error
   :missing-credentials; ECONNECTION ETIMEDOUT ESOCKET -> :network-error
   retryable; EDNS -> :network-error; ETLS -> :network-error :tls? true;
   EPROTOCOL -> :network-error :protocol-violation? true; EENVELOPE EMESSAGE
   EREQUIRETLS -> :smtp-error (with :rejected / :accepted / :rejected-errors,
   :retryable? from the reply code); EMAXRECIPIENTS ESTREAM EFILEACCESS
   EURLACCESS EFETCH -> :argument-error; ECONFIG EPROXY ESES ESENDMAIL ->
   :config-error; a code-less pool shutdown / requeue exhaustion ->
   :network-error retryable; anything else -> :smtp-error. The nodemailer
   code is always recorded under :code (:nodemailer-code too). An ex-info
   passes through unchanged."
  [m]
  (if (util/error? m)
    m
    (let [{:keys [code command response response-code message rejected accepted rejected-errors recipient]} m
          message (or message "unknown nodemailer error")
          base (cond-> {:code code :nodemailer-code code :command command
                        :response response :response-code response-code}
                 recipient (assoc :recipient recipient))]
      (cond
        (contains? #{"EAUTH" "EOAUTH2"} code)
        (util/auth-error :smtp message (assoc base :retryable? (= 454 response-code)))

        (= "ENOAUTH" code)
        (util/config-error message (assoc base :reason :missing-credentials))

        (contains? network-codes code)
        (util/network-error :smtp message
                            (assoc base
                                   :retryable? (contains? #{"ECONNECTION" "ETIMEDOUT" "ESOCKET"} code)
                                   :tls? (= "ETLS" code)
                                   :protocol-violation? (= "EPROTOCOL" code)))

        (contains? smtp-codes code)
        (util/smtp-error message (assoc base
                                        :rejected (vec (or rejected []))
                                        :accepted (vec (or accepted []))
                                        :rejected-errors (vec (or rejected-errors []))))

        (contains? argument-codes code)
        (util/argument-error message (assoc base :reason (if (= "EMAXRECIPIENTS" code) :too-many-recipients :content-source)))

        (contains? config-codes code)
        (util/config-error message (assoc base :reason :transport))

        (and (nil? code) (re-find #"(?i)pool was closed|maximum number of retries" (str message)))
        (util/network-error :smtp message (assoc base :retryable? true :reason :pool-closed))

        :else
        (util/smtp-error message base)))))

(defn- ->ex
  [e]
  (if (util/error? e) e (error-map->ex (sb/error->map e))))

(defn- start
  "Runs (f) inside a promise so a synchronous throw becomes a rejection."
  [f]
  (p/then (p/resolved nil) (fn [_] (f))))

;; ---------------------------------------------------------------------------
;; Operations
;; ---------------------------------------------------------------------------

(defn verify-smtp!
  "nodemailer verify(): resolves DNS, connects, negotiates TLS and
   authenticates, then QUITs. Promise of true; rejects with :auth-error /
   :network-error / :config-error. Does NOT check that a sender address is
   accepted (inventory 2.1)."
  [client]
  (-> (start #(sb/verify (:transport client)))
      (p/catch (fn [e] (p/rejected (->ex e))))))

(defn close-smtp!
  "Closes the transport (pooled: idle connections now, busy ones after
   their message; single-connection: no-op). Promise of true."
  [client]
  (-> (start #(do (sb/close (:transport client)) true))
      (p/catch (fn [e] (p/rejected (->ex e))))))

(defn send!
  "Sends a kebab message map (see message->wire for the shape). Promise of
   {:message-id :envelope {:from :to} :accepted [..] :rejected [..]
   :rejected-errors [..] :all-accepted? :response :response-code :ehlo
   :envelope-time-ms :message-time-ms :message-size}.

   Partial rejection: when the server refused some recipients at RCPT TO
   but accepted others, the message WAS delivered to the accepted ones and
   the promise resolves with them under :rejected (:all-accepted? false)
   — inventory 12.15. With {:strict? true} that case rejects with
   :email-cljs/smtp-error {:reason :partial-rejection :rejected :accepted}
   instead (after delivery; nothing can be recalled). All recipients
   rejected always rejects (:smtp-error, code EENVELOPE). Never waits for
   IMAP visibility."
  ([client message] (send! client message nil))
  ([client message {:keys [strict?]}]
   (-> (start (fn [] (message->wire message (:policy client))))
       (p/then (fn [wire] (sb/send-mail (:transport client) wire)))
       (p/then (fn [info]
                 (let [rejected (vec (:rejected info))
                       info (assoc info
                                   :all-accepted? (empty? rejected)
                                   :response-code (some-> (:response info)
                                                          (->> (re-find #"^\d{3}"))
                                                          (js/parseInt 10)))]
                   (if (and strict? (seq rejected))
                     (throw (util/smtp-error (str (count rejected) " recipient(s) rejected: " (str/join ", " rejected))
                                             {:reason :partial-rejection
                                              :rejected rejected
                                              :accepted (vec (:accepted info))
                                              :rejected-errors (vec (:rejected-errors info))
                                              :message-id (:message-id info)
                                              :response-code (:response-code (first (:rejected-errors info)))
                                              :retryable? false}))
                     info))))
       (p/catch (fn [e] (p/rejected (->ex e)))))))
