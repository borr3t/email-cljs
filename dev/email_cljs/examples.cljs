(ns email-cljs.examples
  "The examples of the README, runnable OFFLINE:

     npx shadow-cljs compile examples && node target/examples.js

   Messages are built and parsed in the process. Where an example sends or
   reads mail, the library does all it would do for a server, and what it
   talks to is a fake: a nodemailer transport that delivers nowhere, an
   IMAP connection over a vector of messages. Nothing leaves the machine
   and no account is needed.

   What stands between a README:begin and a README:end mark stands in
   README.md as written (test/email_cljs/examples_test.cljs holds the
   README to it and runs every function of this file)."
  (:require ["events" :refer [EventEmitter]]
            [cljs.core.async :as async :refer [<! go-loop]]
            [cljs.pprint :as pprint]
            [clojure.string :as str]
            [email-cljs.imap :as imap]
            [email-cljs.message :as msg]
            [email-cljs.smtp :as smtp]
            [promesa.core :as p]))

;; ---------------------------------------------------------------------------
;; Quick start: a message built and parsed
;; ---------------------------------------------------------------------------

;; README:begin quick-start
(def message
  {:from        {:name "Ada Lovelace" :address "ada@example.test"}
   :to          ["grace@example.test"]
   :subject     "Notes on the engine"
   :date        "Fri, 21 Nov 1997 09:55:06 -0600"
   :message-id  "<notes-1@example.test>"
   :text        "The notes are attached."
   :html        "<p>The notes are <b>attached</b>.</p>"
   :attachments [{:filename "notes.txt" :content "Note G" :content-type "text/plain"}]})

(defn quick-start []
  (p/let [built  (msg/build message)
          parsed (msg/parse-message! (:source built))]
    (-> (select-keys parsed [:subject :from :to :date :message-id :text :html])
        (assoc :attachments
               (mapv (fn [attachment]
                       (-> (select-keys attachment [:filename :content-type :size])
                           (assoc :text (msg/attachment->string attachment))))
                     (:attachments parsed))))))
;; README:end quick-start

;; ---------------------------------------------------------------------------
;; The fakes
;; ---------------------------------------------------------------------------

;; README:begin fake-smtp
(defn fake-smtp
  "A nodemailer transport that delivers nowhere. nodemailer builds the
   message as it would for a server; the transport keeps what it would
   have transmitted in the atom `outbox` and answers as a server does
   that knows no mailbox of `unknown`, or that fails with `failure` (the
   fields of a nodemailer error)."
  [outbox {:keys [unknown failure]}]
  #js {:name    "fake-smtp"
       :version "1.0.0"
       :send    (fn [mail callback]
                  (let [built    (unchecked-get mail "message")
                        envelope (js->clj (js-invoke built "getEnvelope") :keywordize-keys true)
                        refused  (filterv (set unknown) (:to envelope))]
                    (js-invoke built "build"
                               (fn [err raw]
                                 (cond
                                   err     (callback err)
                                   failure (callback (js/Object.assign (js/Error. (:message failure))
                                                                       (clj->js failure)))
                                   :else   (do (swap! outbox conj {:envelope envelope
                                                                   :raw      (js-invoke raw "toString" "utf8")})
                                               (callback nil (clj->js
                                                              {:envelope  envelope
                                                               :messageId (js-invoke built "messageId")
                                                               :accepted  (remove (set refused) (:to envelope))
                                                               :rejected  refused
                                                               :response  "250 2.0.0 Ok: queued"}))))))))})
;; README:end fake-smtp

(defn- in-range?
  "Is the number `n` in the IMAP range `range`: \"7\", \"3,7\" or \"3:*\"?"
  [range n]
  (boolean
   (some (fn [part]
           (let [[from to] (str/split part #":")]
             (cond
               (nil? to)  (= n (js/parseInt from 10))
               (= "*" to) (<= (js/parseInt from 10) n)
               :else      (<= (js/parseInt from 10) n (js/parseInt to 10)))))
         (str/split (str range) #","))))

(defn fake-server
  "An IMAP server that is not there: `mail`, a vector of RFC 822 texts,
   is its inbox, none of it read."
  [mail]
  {:inbox (atom (vec (map-indexed (fn [i raw] {:uid (+ 101 i) :seen? false :raw raw}) mail)))
   :conns (atom #{})})

(defn fake-imap
  "The :make-conn of a client of `server`: connections in the shape of
   imapflow's, over the inbox of the server."
  [{:keys [inbox conns]}]
  (fn [_options _on-error]
    (let [conn    (EventEmitter.)
          idle    (atom nil)
          wake!   (fn [] (when-let [resolve @idle] (reset! idle nil) (resolve true)))
          select! (fn [path]
                    (unchecked-set conn "mailbox" #js {:path path :exists (count @inbox)})
                    (unchecked-get conn "mailbox"))
          leave!  (fn []
                    (unchecked-set conn "usable" false)
                    (swap! conns disj conn)
                    (wake!)
                    (js-invoke conn "emit" "close"))
          fetched (fn [by range]
                    (keep-indexed (fn [i {:keys [uid seen? raw]}]
                                    (when (in-range? range (if (= :uid by) uid (inc i)))
                                      #js {:seq    (inc i)
                                           :uid    uid
                                           :flags  (js/Set. (if seen? #js ["\\Seen"] #js []))
                                           :source (js/Buffer.from raw "utf8")}))
                                  @inbox))
          by      (fn [options] (if (unchecked-get options "uid") :uid :seq))
          methods {:connect         (fn [] (unchecked-set conn "usable" true) (swap! conns conj conn) nil)
                   :logout          (fn [] (leave!) nil)
                   :close           (fn [] (leave!) nil)
                   :noop            (fn [] (wake!) nil)
                   :getMailboxLock  (fn [path _] (select! path) #js {:path path :release (fn [] nil)})
                   :mailboxOpen     (fn [path _] (select! path))
                   :idle            (fn [] (js/Promise. (fn [resolve _] (reset! idle resolve))))
                   :search          (fn [query _]
                                      (let [seen (unchecked-get query "seen")
                                            from (unchecked-get query "from")]
                                        (clj->js (for [{:keys [uid seen? raw]} @inbox
                                                       :when (and (or (undefined? seen) (= seen? seen))
                                                                  (or (undefined? from) (str/includes? raw from)))]
                                                   uid))))
                   :fetchOne        (fn [range _ options] (or (first (fetched (by options) range)) false))
                   :fetchAll        (fn [range _ options] (clj->js (fetched (by options) range)))
                   :messageFlagsAdd (fn [range flags _]
                                      (when (some #{"\\Seen"} flags)
                                        (swap! inbox (fn [mail]
                                                       (mapv #(cond-> %
                                                                (in-range? range (:uid %)) (assoc :seen? true))
                                                             mail))))
                                      true)}]
      (unchecked-set conn "usable" false)
      (unchecked-set conn "mailbox" false)
      (unchecked-set conn "capabilities" (js/Map. #js [#js ["IMAP4rev1" true] #js ["IDLE" true]]))
      (unchecked-set conn "enabled" (js/Set.))
      (doseq [[method f] methods]
        (unchecked-set conn (name method)
                       (fn [& args] (js/Promise.resolve (apply f args)))))
      conn)))

(defn deliver!
  "Mail arrives at `server`: `raw` joins the inbox, and every connection
   with the inbox open hears of it."
  [{:keys [inbox conns]} raw]
  (let [before (count @inbox)]
    (swap! inbox conj {:uid (+ 101 before) :seen? false :raw raw})
    (doseq [conn @conns
            :let  [mailbox (unchecked-get conn "mailbox")]
            :when mailbox]
      (js-invoke conn "emit" "exists" #js {:path      (unchecked-get mailbox "path")
                                           :count     (inc before)
                                           :prevCount before}))))

;; ---------------------------------------------------------------------------
;; Scenario 1: send a message
;; ---------------------------------------------------------------------------

;; README:begin send
(defn send-notes
  "Sends `message` with a copy and a blind copy. Resolves with what the
   server said of each recipient."
  [client]
  (p/let [sent (smtp/send! client (assoc message
                                         :cc  ["nobody@example.test"]
                                         :bcc ["charles@example.test"]))]
    (select-keys sent [:message-id :accepted :rejected :all-accepted? :response-code])))
;; README:end send

(defn send-offline
  "`send-notes` through a fake transport that knows no nobody@example.test,
   with what the transport would have transmitted."
  []
  (p/let [outbox      (atom [])
          client      (smtp/smtp-client
                       {:transport (fake-smtp outbox {:unknown ["nobody@example.test"]})})
          sent        (send-notes client)
          transmitted (msg/parse-message! (:raw (first @outbox)))]
    {:sent        sent
     :transmitted {:envelope (:envelope (first @outbox))
                   :cc       (mapv :address (:cc transmitted))
                   :bcc      (mapv :address (:bcc transmitted))}}))

;; ---------------------------------------------------------------------------
;; Scenario 2: read the unread mail of a mailbox
;; ---------------------------------------------------------------------------

;; README:begin read
(defn read-unread
  "The unread mail of the inbox, oldest first, each message marked as
   read once all of them are parsed."
  [client]
  (p/let [_    (imap/connect! client)
          _    (imap/mailbox-open! client "INBOX")
          uids (imap/search! client {:unseen true})
          mail (p/all (for [uid uids]
                        (p/let [fetched (imap/fetch-one! client uid {:flags true :source? true})
                                parsed  (msg/parse-message! fetched)]
                          {:uid         uid
                           :from        (:address (:from parsed))
                           :subject     (:subject parsed)
                           :attachments (mapv :filename (:attachments parsed))})))
          _    (when (seq uids) (imap/mark-seen! client uids))
          left (imap/search! client {:unseen true})
          _    (imap/close-imap! client)]
    {:mail mail :left-unread (count left)}))
;; README:end read

(def reply
  "What grace answers to `message`."
  {:from        "grace@example.test"
   :to          ["ada@example.test"]
   :subject     "Re: Notes on the engine"
   :date        "Sat, 22 Nov 1997 10:00:00 -0600"
   :message-id  "<notes-2@example.test>"
   :in-reply-to "<notes-1@example.test>"
   :text        "Read, with thanks."})

(defn account
  "The options of an IMAP client of `server`. The host and the login are
   of no server: the fake connection never looks at them."
  [server]
  {:host      "imap.example.test"
   :auth      {:user "ada@example.test" :pass "not-a-password"}
   :make-conn (fake-imap server)})

(defn read-offline
  "`read-unread` over a fake server with two messages in its inbox."
  []
  (p/let [first-mail  (msg/build-string message)
          second-mail (msg/build-string reply)]
    (read-unread (imap/imap-client (account (fake-server [first-mail second-mail]))))))

;; ---------------------------------------------------------------------------
;; Scenario 3: watch a mailbox
;; ---------------------------------------------------------------------------

;; README:begin watch
(defn arrivals
  "Watches `mailbox` on a connection of its own. Resolves, once the
   mailbox is open, with {:mail .. :stop! ..}: `mail` is a channel of
   {:uid :subject}, one per message that arrives, closed when the watcher
   ends; `(stop!)` ends it."
  [client mailbox]
  (let [events (imap/watch! client mailbox {:fetch? true :query {:source? true}})
        mail   (async/chan 16)
        open   (p/deferred)]
    (go-loop []
      (if-let [{:keys [event messages error]} (<! events)]
        (do (case event
              :open     (p/resolve open {:mail mail :stop! #(imap/stop-watch! events)})
              :error    (p/reject open error)
              :new-mail (doseq [fetched messages]
                          (p/let [parsed (msg/parse-message! fetched)]
                            (async/put! mail {:uid (:uid fetched) :subject (:subject parsed)})))
              nil)
            (recur))
        (async/close! mail)))
    open))
;; README:end watch

(defn watch-offline
  "`arrivals` over a fake server at which one message arrives."
  []
  (p/let [first-mail  (msg/build-string message)
          second-mail (msg/build-string reply)
          server      (fake-server [first-mail])
          {:keys [mail stop!]} (arrivals (imap/imap-client (account server)) "INBOX")
          _           (deliver! server second-mail)
          arrived     (p/create (fn [resolve _] (async/take! mail resolve)))
          stopped     (stop!)]
    {:arrived arrived :stopped stopped}))

;; ---------------------------------------------------------------------------
;; Error handling
;; ---------------------------------------------------------------------------

;; README:begin errors
(defn delivery
  "The message sent, or the failure as data."
  [client message opts]
  (-> (smtp/send! client message opts)
      (p/then (fn [sent] {:ok? true :message-id (:message-id sent)}))
      (p/catch (fn [e]
                 (let [{:keys [type reason rejected response-code retryable?]} (ex-data e)]
                   (case type
                     :email-cljs/smtp-error     {:ok? false :why :refused :reason reason :rejected rejected}
                     :email-cljs/auth-error     {:ok? false :why :login :response-code response-code}
                     :email-cljs/network-error  {:ok? false :why :unreachable :retry? retryable?}
                     :email-cljs/argument-error {:ok? false :why :message :reason reason}
                     :email-cljs/config-error   {:ok? false :why :configuration :reason reason}
                     (throw e)))))))
;; README:end errors

(defn errors-offline
  "`delivery` through fake transports: one that accepts, one that knows no
   grace@example.test, one that refuses the login, one that is not there."
  []
  (let [client (fn [answers] (smtp/smtp-client {:transport (fake-smtp (atom []) answers)}))
        copied (assoc message :cc ["charles@example.test"])]
    (p/let [sent        (delivery (client {}) message nil)
            refused     (delivery (client {:unknown ["grace@example.test"]}) copied {:strict? true})
            login       (delivery (client {:failure {:code         "EAUTH"
                                                     :responseCode 535
                                                     :message      "Invalid login"}})
                                  message nil)
            unreachable (delivery (client {:failure {:code    "ECONNECTION"
                                                     :message "Connection refused"}})
                                  message nil)
            from-a-file (delivery (client {})
                                  (assoc message :attachments [{:filename "notes.txt" :path "notes.txt"}])
                                  nil)]
      {:sent        sent
       :refused     refused
       :login       login
       :unreachable unreachable
       :from-a-file from-a-file})))

;; ---------------------------------------------------------------------------
;; Runner
;; ---------------------------------------------------------------------------

(defn- show [title value]
  (println (str ";; " title))
  (pprint/pprint value))

(defn main [& _]
  (-> (p/let [parsed  (quick-start)
              sent    (send-offline)
              unread  (read-offline)
              watched (watch-offline)
              failed  (errors-offline)]
        (show "quick start: a message built and parsed" parsed)
        (show "send a message" sent)
        (show "read the unread mail of a mailbox" unread)
        (show "watch a mailbox" watched)
        (show "failures are data" failed))
      (p/catch (fn [e]
                 (println "example failed:" (ex-message e) (pr-str (ex-data e)))
                 (set! (.-exitCode js/process) 1)))))
