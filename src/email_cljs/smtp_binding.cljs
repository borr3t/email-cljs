(ns email-cljs.smtp-binding
  "Binding namespace (JavaScript interop lives here): every nodemailer call
   the library makes. Takes CLJS data (string-keyed camelCase option / message
   maps produced by email-cljs.smtp, with Secret and Bytes values still
   wrapped), turns it into JS objects at the last moment, and returns
   promesa promises of kebab-case CLJS maps. Secrets are revealed only
   inside ->js, immediately before nodemailer sees them; they never travel
   in results or errors (inventory 8.1, 12.11).

   Transport objects are opaque to callers: they live under :transport in
   the client map email-cljs.smtp/smtp-client builds and are only ever
   handed back to the functions of this namespace."
  (:require ["nodemailer" :as nodemailer]
            ["nodemailer/lib/mail-composer" :as MailComposer]
            [clojure.walk :as walk]
            [email-cljs.attachment :as att]
            [email-cljs.util :as util]
            [promesa.core :as p]))

;; ---------------------------------------------------------------------------
;; CLJS -> JS
;; ---------------------------------------------------------------------------

(defn ->js
  "CLJS option / message map -> JS object: Secrets revealed, Bytes unwrapped
   to Buffers, keywords rendered by name (keys and enum values), js/Date and
   other opaque JS values passed through. Used for everything nodemailer
   receives, so no other namespace needs clj->js."
  [x]
  (clj->js (walk/postwalk (fn [v]
                            (cond
                              (util/secret? v) (util/reveal v)
                              (att/bytes? v) (att/unwrap v)
                              (keyword? v) (name v)
                              :else v))
                          x)))

;; ---------------------------------------------------------------------------
;; JS error -> map (pure classification happens in email-cljs.smtp)
;; ---------------------------------------------------------------------------

(defn- js-error-field
  [e k]
  (let [v (unchecked-get e k)]
    (when-not (undefined? v) v)))

(defn error->map
  "Reads the NodemailerError fields (inventory 8.1) off a thrown value into
   a kebab map: {:message :name :code :command :response :response-code
   :recipient :rejected :accepted :rejected-errors [..maps..] :cause-message}.
   An ex-info passes through unchanged (already classified). Non-Error
   values become {:message (str v)}."
  [e]
  (cond
    (util/error? e) e
    (nil? e) {:message "unknown error"}
    (instance? js/Error e)
    (let [arr (fn [v] (when (array? v) (vec v)))]
      (cond-> {:message (unchecked-get e "message")
               :name (unchecked-get e "name")
               :code (js-error-field e "code")
               :command (js-error-field e "command")
               :response (js-error-field e "response")
               :response-code (js-error-field e "responseCode")
               :recipient (js-error-field e "recipient")
               :rejected (arr (js-error-field e "rejected"))
               :accepted (arr (js-error-field e "accepted"))}
        (array? (js-error-field e "rejectedErrors"))
        (assoc :rejected-errors (mapv error->map (vec (js-error-field e "rejectedErrors"))))))
    :else {:message (str e)}))

;; ---------------------------------------------------------------------------
;; Transport lifecycle
;; ---------------------------------------------------------------------------

(defn create-transport
  "nodemailer createTransport. `options` is the string-keyed camelCase SMTP
   option map (Secrets inside :auth / tls.ca still wrapped) — or, when
   `plugin` is given, an opaque transport plugin object (tests inject a
   fake transport this way; nodemailer's jsonTransport / streamTransport
   objects also qualify) — and `defaults` the message defaults map
   (disableFileAccess / disableUrlAccess ride here so a plugin transport is
   sandboxed too). Returns the opaque transporter (a nodemailer Mail)."
  [{:keys [options plugin defaults]}]
  (if (some? plugin)
    (.createTransport nodemailer plugin (->js (or defaults {})))
    (.createTransport nodemailer (->js options) (->js (or defaults {})))))

(defn- info->map
  [info]
  (let [f (fn [k] (let [v (unchecked-get info k)] (when-not (undefined? v) v)))
        arr (fn [v] (if (array? v) (vec v) []))
        env (f "envelope")]
    (cond-> {:message-id (f "messageId")
             :envelope (when env
                         {:from (let [v (unchecked-get env "from")] (if (string? v) v nil))
                          :to (arr (unchecked-get env "to"))})
             :accepted (arr (f "accepted"))
             :rejected (arr (f "rejected"))
             :rejected-errors (mapv error->map (arr (f "rejectedErrors")))
             :pending (arr (f "pending"))
             :response (f "response")
             :ehlo (arr (f "ehlo"))
             :envelope-time-ms (f "envelopeTime")
             :message-time-ms (f "messageTime")
             :message-size (f "messageSize")}
      ;; stream / json transports: the built message (Buffer, string or object)
      (some? (f "message"))
      (assoc :message (let [m (f "message")]
                        (cond
                          (string? m) m
                          (js/Buffer.isBuffer m) (att/bytes m)
                          :else (js->clj m :keywordize-keys true)))))))

(defn send-mail
  "transporter.sendMail with a string-keyed camelCase message map (Bytes /
   Secrets still wrapped). Promise of the kebab send-result map (inventory
   2.5 / 14.3): {:message-id :envelope {:from :to} :accepted :rejected
   :rejected-errors :pending :response :ehlo :envelope-time-ms
   :message-time-ms :message-size (:message)}. Rejects with the raw
   NodemailerError (classified by the caller through error->map)."
  [transport message]
  (p/then (js/Promise.resolve (js-invoke transport "sendMail" (->js message))) info->map))

(defn verify
  "transporter.verify (SMTP: DNS + TCP + TLS + AUTH, then QUIT). Promise of
   true; rejects with the raw connection error."
  [transport]
  (p/then (js/Promise.resolve (js-invoke transport "verify")) (fn [_] true)))

(defn close
  "transporter.close (pool: closes idle connections; single: no-op).
   Synchronous; returns nil."
  [transport]
  (js-invoke transport "close")
  nil)

(defn idle?
  "transporter.isIdle — true when the transport can take a message now."
  [transport]
  (boolean (js-invoke transport "isIdle")))

(defn transport-name
  "The name nodemailer reports for the underlying transport (\"SMTP\",
   \"SMTP (pool)\", a plugin's own name)."
  [transport]
  (let [t (unchecked-get transport "transporter")]
    (when t (unchecked-get t "name"))))

;; ---------------------------------------------------------------------------
;; MailComposer (email-cljs.message/build — no transport)
;; ---------------------------------------------------------------------------

(defn compose
  "Builds the RFC 822 bytes of a string-keyed camelCase message map (the
   output of email-cljs.smtp/message->wire; Bytes still wrapped; a
   \"keepBcc\" true entry keeps the Bcc: header in the bytes) with
   nodemailer's MailComposer, without any transport: promise of
   {:source <Bytes> :message-id \"<..>\" :envelope {:from :to [..]} :size n}.
   Rejects with the raw nodemailer error (EFILEACCESS / EURLACCESS when a
   path or href slipped past the policy, stream errors)."
  [message]
  (let [keep-bcc? (boolean (get message "keepBcc"))
        composer (MailComposer. (->js (dissoc message "keepBcc")))
        node (js-invoke composer "compile")
        ;; keepBcc is a property of the compiled MimeNode (the stream transport
        ;; sets it the same way), not a composer option
        _ (when keep-bcc? (unchecked-set node "keepBcc" true))
        env (js-invoke node "getEnvelope")]
    (p/let [buf (js/Promise.resolve (js-invoke node "build"))]
      {:source (att/bytes buf)
       :message-id (js-invoke node "messageId")
       :envelope {:from (let [v (unchecked-get env "from")] (if (string? v) v nil))
                  :to (let [v (unchecked-get env "to")] (if (array? v) (vec v) []))}
       :size (unchecked-get buf "length")})))
