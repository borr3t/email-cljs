(ns email-cljs.test-util
  "Shared fixtures for the email-cljs tests: a throwaway password (NOT a
   real credential), promise helpers, and the offline test doubles —
   a nodemailer transport PLUGIN that runs the real Mail/MailComposer
   normalisation and captures the message, and a fake ImapFlow connection
   (an EventEmitter with scripted methods) plus JS value constructors
   (Map / Set / BigInt / Buffer / Date / Readable) for result fixtures.

   This is the ONE test namespace that uses JavaScript interop: the
   doubles have to be JS objects because the binding namespaces call
   methods on them. Test namespaces themselves stay interop-free and only
   use the constructors below. Not a test namespace (the name does not end
   in -test)."
  (:require ["events" :refer (EventEmitter)]
            ["stream" :refer (Readable)]
            [cljs.test :refer [is]]
            [clojure.core.async :as async]
            [promesa.core :as p]))

(def fake-password "hunter2-not-a-real-password!")

(def fake-secrets-text
  "A secrets file in the ~/.secrets/borret.txt layout with invented values."
  (str "# fake\n"
       "BORRET_EMAIL_USERNAME=someone@example.test\n"
       "BORRET_EMAIL_PASSWORD=\"" fake-password "\"\n"
       "BORRET_EMAIL_SMTP_HOST=mail.example.test\n"
       "BORRET_EMAIL_SMTP_PORT=465\n"
       "BORRET_EMAIL_SMTP_TLS=true\n"
       "BORRET_EMAIL_IMAP_HOST=mail.example.test\n"
       "BORRET_EMAIL_IMAP_PORT=993\n"
       "BORRET_EMAIL_IMAP_TLS=true\n"))

;; ---------------------------------------------------------------------------
;; Promise helpers
;; ---------------------------------------------------------------------------

(defn rejection
  "Promise of the rejection reason of `p` (fails the assertion if it resolves)."
  ;; NB: promesa treats a js/Error RETURNED from a catch handler as a
  ;; rejection (IPromiseFactory on js/Error), so the error must be wrapped in
  ;; an already-resolved promise to travel as a value.
  [promise]
  (-> promise
      (p/then (fn [v] (throw (ex-info "expected rejection" {:resolved v}))))
      (p/catch (fn [e] (p/resolved e)))))

(defn thrown
  "The value thrown by (f), nil when it returns normally."
  [f]
  (try (f) nil (catch :default e e)))

(defn finish
  "For cljs.test async blocks: reports an unexpected rejection of `promise`
   as a failed assertion, then calls `done`."
  [done promise]
  (-> promise
      (p/catch (fn [e] (is (nil? e) (str "unexpected rejection: " (pr-str e) " " (ex-message e)))))
      (p/finally (fn [_ _] (done)))))

(defn wait
  "Promise resolving after `ms`."
  [ms]
  (p/delay ms nil))

;; ---------------------------------------------------------------------------
;; JS value constructors for imapflow-shaped fixtures
;; ---------------------------------------------------------------------------

(defn js-set
  "A JS Set of the given values."
  [xs]
  (js/Set. (into-array xs)))

(defn js-map
  "A JS Map from a CLJS map (keys and values as given; CLJS collections
   converted with clj->js)."
  [m]
  (let [jm (js/Map.)]
    (doseq [[k v] m] (.set jm (clj->js k) (if (coll? v) (clj->js v) v)))
    jm))

(defn js-array
  "A JS array of the given CLJS values (clj->js)."
  [xs]
  (clj->js xs))

(defn bigint
  "A JS BigInt from a decimal string or integer."
  [x]
  (js/BigInt (str x)))

(defn js-date
  "A JS Date from an ISO string."
  [iso]
  (js/Date. iso))

(defn buffer-of
  "A Node Buffer of a UTF-8 string."
  [s]
  (js/Buffer.from s "utf8"))

(defn readable-of
  "A Node Readable stream yielding the given UTF-8 string in two chunks."
  [s]
  (let [n (quot (count s) 2)]
    (Readable.from (into-array [(js/Buffer.from (subs s 0 n) "utf8") (js/Buffer.from (subs s n) "utf8")]))))

(defn js-error
  "A JS Error with `:message` and extra own properties (`:code`,
   `:responseStatus` ... given as keywords or strings, set as camelCase
   property names verbatim)."
  [{:keys [message] :as props}]
  (let [e (js/Error. (or message "fake error"))]
    (doseq [[k v] (dissoc props :message)]
      (unchecked-set e (name k) (if (coll? v) (clj->js v) v)))
    e))

(defn ->clj
  "js->clj with keyword keys (for inspecting captured JS arguments)."
  [x]
  (js->clj x :keywordize-keys true))

;; ---------------------------------------------------------------------------
;; nodemailer: a transport plugin that captures the normalised message
;; ---------------------------------------------------------------------------

(defn fake-transport
  "A nodemailer transport plugin (inventory 2.1: an object with name /
   version / send). `send` runs mail.normalize() — the real MailComposer
   path: addresses parsed, Buffers base64'd, envelope and Message-ID
   computed — records the normalised data under :messages (kebab-free
   keywordised camelCase, as nodemailer produced it) and answers with a
   canned SMTP-shaped info: accepted = envelope.to minus `:rejected`,
   `:rejected-errors` (maps -> Errors), `:response` (\"250 OK\" default),
   `:ehlo`. `:error` (a map for js-error) makes send fail; `:verify-error`
   makes verify fail. Returns {:plugin <opaque> :messages atom :calls atom}."
  ([] (fake-transport nil))
  ([{:keys [rejected rejected-errors response error verify-error ehlo]}]
   (let [messages (atom [])
         calls (atom [])
         plugin #js {:name "fake-smtp"
                     :version "0.0.0"
                     :send (fn [mail cb]
                             (swap! calls conj :send)
                             (.normalize mail
                                         (fn [err data]
                                           (if err
                                             (cb err)
                                             (let [d (->clj data)]
                                               (swap! messages conj d)
                                               (if error
                                                 (cb (js-error error))
                                                 (let [to (vec (get-in d [:envelope :to]))
                                                       rejected (vec (or rejected []))
                                                       accepted (vec (remove (set rejected) to))]
                                                   (cb nil (clj->js {:envelope (:envelope d)
                                                                     :messageId (:messageId d)
                                                                     :accepted accepted
                                                                     :rejected rejected
                                                                     :rejectedErrors (mapv js-error (or rejected-errors []))
                                                                     :response (or response "250 2.0.0 Ok: queued as FAKE1")
                                                                     :ehlo (or ehlo ["PIPELINING" "SIZE 52428800" "AUTH PLAIN LOGIN"])
                                                                     :envelopeTime 1 :messageTime 2 :messageSize (count (str (:text d)))})))))))))
                     :verify (fn []
                               (swap! calls conj :verify)
                               (if verify-error
                                 (js/Promise.reject (js-error verify-error))
                                 (js/Promise.resolve true)))
                     :close (fn [] (swap! calls conj :close) nil)}]
     {:plugin plugin :messages messages :calls calls})))

;; ---------------------------------------------------------------------------
;; imapflow: a fake connection
;; ---------------------------------------------------------------------------

(defn- ->jsv
  "Scripted return values: CLJS collections via clj->js (JS leaves such as
   Maps / Sets / BigInts / Buffers inside are kept), everything else as is."
  [v]
  (if (coll? v) (clj->js v) v))

(defn fake-imap
  "A fake ImapFlow connection: an EventEmitter carrying `usable`,
   `mailbox`, `capabilities` (Map) and `enabled` (Set) plus methods. Every
   method call is recorded in :calls as [name [args...]] (args keywordised
   via js->clj) and answered by the scripted fn under :methods {\"search\"
   (fn [query opts] ..)} — fns receive keywordised CLJS args and return a
   CLJS value (converted with clj->js; JS leaves kept) or throw (-> rejected
   promise). Defaults: connect (usable true), logout / close (usable false,
   emits 'close'), noop, mailboxOpen / getMailboxLock (sets `mailbox` from
   the :mailbox fixture), idle (pending until noop / logout / close), exec,
   fetchAll [] , search [], messageFlags* true, messageDelete true.
   `:capabilities` is a vector of strings (default IMAP4rev1 IDLE UIDPLUS
   MOVE CONDSTORE). Returns {:conn <opaque> :calls atom :state atom
   :emit! (fn [event payload-map])}."
  ([] (fake-imap nil))
  ([{:keys [mailbox capabilities enabled methods usable?]
     :or {capabilities ["IMAP4rev1" "IDLE" "UIDPLUS" "MOVE" "CONDSTORE"]
          enabled ["CONDSTORE"]
          usable? false}}]
   (let [conn (EventEmitter.)
         calls (atom [])
         state (atom {:idle nil :released 0})
         mailbox-js (fn [] (when mailbox (clj->js mailbox)))
         resolve-idle! (fn []
                         (when-let [d (:idle @state)]
                           (swap! state assoc :idle nil)
                           (p/resolve d true)))
         close! (fn []
                  (unchecked-set conn "usable" false)
                  (unchecked-set conn "isClosed" true)
                  (resolve-idle!)
                  (.emit conn "close"))
         defaults {"connect" (fn [] (unchecked-set conn "usable" true) nil)
                   "logout" (fn [] (close!) nil)
                   "close" (fn [] (close!) nil)
                   "noop" (fn [] (resolve-idle!) nil)
                   "mailboxOpen" (fn [_path _opts] (unchecked-set conn "mailbox" (mailbox-js)) (unchecked-get conn "mailbox"))
                   "mailboxClose" (fn [] (unchecked-set conn "mailbox" false) true)
                   "getMailboxLock" (fn [path _opts]
                                      (unchecked-set conn "mailbox" (mailbox-js))
                                      #js {:path path :release (fn [] (swap! state update :released inc) nil)})
                   "idle" (fn []
                            (let [d (p/deferred)]
                              (swap! state assoc :idle d)
                              (swap! state update :idle-count (fnil inc 0))
                              d))
                   "exec" (fn [_cmd _attrs] #js {:response #js {:tag "A1" :command "OK"} :next (fn [] nil)})
                   "fetchAll" (fn [& _] [])
                   "fetchOne" (fn [& _] false)
                   "search" (fn [& _] [])
                   "list" (fn [& _] [])
                   "status" (fn [path & _] {:path path})
                   "getQuota" (fn [& _] false)
                   "messageFlagsSet" (fn [& _] true)
                   "messageFlagsAdd" (fn [& _] true)
                   "messageFlagsRemove" (fn [& _] true)
                   "messageDelete" (fn [& _] true)
                   "messageMove" (fn [& _] false)
                   "messageCopy" (fn [& _] false)
                   "append" (fn [& _] false)
                   "download" (fn [& _] #js {})}
         install! (fn [nm f]
                    (unchecked-set conn nm
                                   (fn [& js-args]
                                     (swap! calls conj [nm (mapv ->clj js-args)])
                                     (try
                                       (let [r (apply f (map ->clj js-args))]
                                         (if (instance? js/Promise r) r (js/Promise.resolve (->jsv r))))
                                       (catch :default e (js/Promise.reject e))))))]
     (unchecked-set conn "usable" (boolean usable?))
     (unchecked-set conn "isClosed" false)
     (unchecked-set conn "mailbox" false)
     (unchecked-set conn "capabilities" (js-map (into {} (map (fn [c] [c true])) capabilities)))
     (unchecked-set conn "enabled" (js-set enabled))
     (doseq [[nm f] (merge defaults methods)] (install! nm f))
     ;; synchronous accessors the binding reads directly
     {:conn conn
      :calls calls
      :state state
      :emit! (fn [event payload] (.emit conn event (->jsv payload)) nil)})))

(defn calls-named
  "The argument vectors of every recorded call to `name`."
  [fake name]
  (into [] (comp (filter #(= name (first %))) (map second)) @(:calls fake)))

(defn take-events!
  "Collects up to `n` events from a core.async channel into a promise of a
   vector, waiting at most `timeout-ms` (resolves with what it has by then,
   or earlier when the channel closes). take! callbacks, no go blocks."
  [ch n timeout-ms]
  (p/create
   (fn [resolve _]
     (let [acc (atom [])
           done? (atom false)
           finish (fn [] (when-not @done? (reset! done? true) (resolve @acc)))]
       (js/setTimeout finish timeout-ms)
       (letfn [(step []
                 (async/take! ch (fn [v]
                                (if (nil? v)
                                  (finish)
                                  (do (swap! acc conj v)
                                      (if (>= (count @acc) n) (finish) (step)))))))]
         (step))))))
