(ns email-cljs.imap-test
  (:require [cljs.test :refer [deftest is testing async]]
            [clojure.core.async :as async]
            [clojure.string :as str]
            [email-cljs.attachment :as att]
            [email-cljs.imap :as imap]
            [email-cljs.imap-binding :as ib]
            [email-cljs.test-util :as tu]
            [email-cljs.util :as util]
            [promesa.core :as p]))

(def auth {:user "someone@example.test" :pass (util/secret tu/fake-password)})

(def inbox-fixture
  "A MailboxObject as imapflow hands it over (JS leaves built with test-util)."
  {:path "INBOX" :delimiter "." :flags (tu/js-set ["\\Seen" "\\Answered" "\\Flagged" "\\Deleted" "\\Draft"])
   :permanentFlags (tu/js-set ["\\Seen" "\\*"]) :uidValidity (tu/bigint "1758758400") :uidNext 1002
   :exists 42 :unseen 40 :highestModseq (tu/bigint "77") :readOnly false})

(defn- client-with
  "An imap-client whose connection factory hands out the given fakes in
   order (the first to the client, the next to watch!)."
  [& fakes]
  (let [queue (atom (vec fakes))
        made (atom [])]
    [(imap/imap-client {:host "mail.example.test" :auth auth
                        :make-conn (fn [wire on-error]
                                     (swap! made conj {:wire wire :on-error on-error})
                                     (let [f (first @queue)]
                                       (swap! queue subvec 1)
                                       (:conn f)))})
     made]))

;; ---------------------------------------------------------------------------
;; client options (pure)
;; ---------------------------------------------------------------------------

(deftest client-options-defaults
  (let [w (imap/client-options->wire {:host "mail.example.test" :auth auth})]
    (is (= 993 (get w "port")))
    (is (true? (get w "secure")))
    (is (true? (get w "disableAutoIdle")) "the watcher owns IDLE — always")
    (is (true? (get w "qresync")))
    (is (false? (get w "logger")) "imapflow would log to stdout otherwise")
    (is (false? (get w "maxIdleTime")) "restarts are driven by the watcher loop")
    (is (= 268435456 (get w "maxLiteralSize")))
    (is (= {"name" "email-cljs"} (get w "clientInfo")))
    (is (util/secret? (get-in w ["auth" "pass"])))
    (is (not (str/includes? (pr-str w) tu/fake-password)))))

(deftest client-options-rules-and-errors
  (is (false? (get (imap/client-options->wire {:host "h" :port 143 :auth auth}) "secure")))
  (is (= 143 (get (imap/client-options->wire {:host "h" :secure false :auth auth}) "port")))
  (is (= "TLSv1.2" (get-in (imap/client-options->wire {:host "h" :auth auth :tls {:min-version "TLSv1.2"}}) ["tls" "minVersion"])))
  (is (= "STATUS" (get (imap/client-options->wire {:host "h" :auth auth :missing-idle-command :status}) "missingIdleCommand")))
  (is (= :bad-host (:reason (ex-data (tu/thrown #(imap/client-options->wire {:auth auth}))))))
  (is (= :missing-credentials (:reason (ex-data (tu/thrown #(imap/client-options->wire {:host "h"}))))))
  (is (= :bad-port (:reason (ex-data (tu/thrown #(imap/client-options->wire {:host "h" :auth auth :port 0}))))))
  (is (= :unknown-option (:reason (ex-data (tu/thrown #(imap/client-options->wire {:host "h" :auth auth :hots 1}))))))
  (let [cfg (util/env-config (util/parse-secrets-file tu/fake-secrets-text) "BORRET_EMAIL_")
        o (imap/config-from-env cfg)]
    (is (= {:host "mail.example.test" :port 993 :secure true} (dissoc o :auth)))
    (is (= :missing-credentials (:reason (ex-data (tu/thrown #(imap/config-from-env {}))))))))

(deftest imap-client-map-is-safe-to-print
  (let [[c made] (client-with (tu/fake-imap))]
    (is (imap/client? c))
    (is (= 1 (count @made)))
    (is (not (str/includes? (pr-str (:config c)) tu/fake-password)))
    (is (nil? (get (:options c) "auth")))
    (is (util/secret? (get-in (first @made) [:wire "auth" "pass"])) "the factory receives wrapped secrets")
    (is (true? (get-in (first @made) [:wire "disableAutoIdle"])))
    (is (false? (imap/connected? c)))
    (is (= (* 29 60 1000) (:max-idle-ms c)))))

;; ---------------------------------------------------------------------------
;; search DSL (pure) + imapflow's own compiler offline
;; ---------------------------------------------------------------------------

(deftest compile-search-dsl
  (is (= {"seen" false "flagged" true "from" "alice" "larger" 1024}
         (imap/compile-search {:seen false :flagged true :from "alice" :larger 1024})))
  (is (= {"seen" false "answered" false} (imap/compile-search {:unseen true :unanswered true})) "UN* sugar")
  (is (= {"seen" true} (imap/compile-search {:unseen false})))
  (is (= {"all" true} (imap/compile-search {:all true})))
  (is (= {"uid" "5"} (imap/compile-search {:uid 5})))
  (is (= {"uid" "1,2,3"} (imap/compile-search {:uid [1 2 3]})))
  (is (= {"uid" "100:*"} (imap/compile-search {:uid "100:*"})))
  (is (= {"since" "2026-09-25"} (imap/compile-search {:since "2026-09-25"})) "ISO date strings pass as they are")
  (is (= {"before" "2026-09-01"} (imap/compile-search {:before "1-Sep-2026"})) "IMAP wire spelling normalised")
  (is (= {"since" "2026-09-24"} (imap/compile-search {:since #inst "2026-09-24T23:30:00.000Z"})) "#inst -> UTC calendar day")
  (is (= {"sentSince" "2026-03-05"} (imap/compile-search {:sent-since {:year 2026 :month 3 :day 5}})))
  (is (= {"header" {"Message-ID" "<abc@x>" "X-Flag" true}} (imap/compile-search {:header {"Message-ID" "<abc@x>" :x-flag true}})))
  (is (= {"unKeyword" "$Junk" "keyword" "$Forwarded"} (imap/compile-search {:un-keyword "$Junk" :keyword "$Forwarded"})))
  (is (= {"modseq" "77"} (imap/compile-search {:modseq 77})))
  (is (= {"not" {"from" "spam"}} (imap/compile-search {:not {:from "spam"}})))
  (is (= {"or" [{"from" "a"} {"to" "b"}] "seen" true} (imap/compile-search {:or [{:from "a"} {:to "b"}] :seen true})))
  (is (= {"subject" "x"} (imap/compile-search {:subject "x" :from nil})) "nil values dropped"))

(deftest adjust-search-for-within-guards-future-days
  (let [today "2026-09-25"]
    (is (= {:empty? true} (imap/adjust-search-for-within {"since" "2026-09-26" "seen" false} today)) "since tomorrow: nothing")
    (is (= {:query {"seen" false}} (imap/adjust-search-for-within {"before" "2026-09-26" "seen" false} today)) "before tomorrow: always true, dropped")
    (is (= {:query {"all" true}} (imap/adjust-search-for-within {"before" "2026-10-01"} today)) "nothing left -> ALL")
    (is (= {:query {"since" "2026-09-25" "before" "2026-09-25"}} (imap/adjust-search-for-within {"since" "2026-09-25" "before" "2026-09-25"} today)) "today untouched")
    (is (= {:query {"since" "2026-09-01"}} (imap/adjust-search-for-within {"since" "2026-09-01"} today)))
    (is (= {:query {"or" [{"since" "2999-01-01"}]}} (imap/adjust-search-for-within {"or" [{"since" "2999-01-01"}]} today)) "nested left alone")))

(deftest compile-search-rejects-bad-queries
  (let [reason (fn [q] (:reason (ex-data (tu/thrown #(imap/compile-search q)))))]
    (is (= :empty-search (reason {})))
    (is (= :empty-search (reason {:from nil})))
    (is (= :email-cljs/argument-error (util/error-type (tu/thrown #(imap/compile-search {})))))
    (is (= :unknown-search-key (reason {:subjetc "x"})))
    (is (= :bad-search-value (reason {:or [{:from "a"}]})))
    (is (= :bad-search-date (reason {:since 12345})))
    (is (= :bad-search-date (reason {:since "yesterday"})))
    (is (= :bad-search-date (reason {:since "24-Sept-2026"})))
    (is (= :bad-search-date (reason {:since {:year 2026 :month 13 :day 1}})))
    (is (= :bad-search-value (reason {:header "Subject"})))
    (is (= :bad-search-value (reason {:larger "big"})))
    (is (= :bad-sequence-set (reason {:uid [1 "x"]})))
    (is (= :bad-search-query (reason "x")))))

(deftest imapflow-search-compiler-agrees-offline
  (let [fake (tu/fake-imap {:mailbox inbox-fixture})
        attrs (fn [q] (mapv :value (ib/compile-search-attributes (:conn fake) (imap/compile-search q))))]
    (is (= ["UNSEEN" "FROM" "alice"] (attrs {:unseen true :from "alice"})))
    (is (= ["UID" "1,2,3"] (attrs {:uid [1 2 3]})))
    (is (= ["SINCE" "24-Sep-2026"] (attrs {:since #inst "2026-09-24T23:30:00.000Z"})) "#inst: UTC day survives imapflow's re-parse")
    (is (= ["SINCE" "24-Sep-2026"] (attrs {:since "24-Sep-2026"})) "wire-spelled string: no day shift on a UTC+3 machine")
    (is (= ["SINCE" "24-Sep-2026"] (attrs {:since "2026-09-24"})))
    (is (= ["SINCE" "24-Sep-2026"] (attrs {:since {:year 2026 :month 9 :day 24}})))
    (is (= ["BEFORE" "24-Sep-2026"] (attrs {:before "2026-09-24"})) "BEFORE not shifted by imapflow's +24h rule (midnight UTC)")
    (is (= ["HEADER" "MESSAGE-ID" "<abc@x>"] (attrs {:header {"Message-ID" "<abc@x>"}})) "imapflow upper-cases header names on the wire")
    (is (= ["NOT" "FROM" "spam"] (attrs {:not {:from "spam"}})))
    (is (= ["OR" "FROM" "a" "TO" "b"] (attrs {:or [{:from "a"} {:to "b"}]})))
    (is (= ["ALL"] (attrs {:all true})))
    (is (= ["CHARSET" "UTF-8" "SUBJECT" "ünï"] (attrs {:subject "ünï"})) "non-ASCII adds CHARSET when UTF8=ACCEPT is not enabled")))

(deftest range-fetch-query-and-flags-are-pure
  (is (= "5" (imap/range->wire 5)))
  (is (= "1,2" (imap/range->wire [1 2])))
  (is (= "1:*" (imap/range->wire "1:*")))
  (is (= {"seen" false} (imap/range->wire {:unseen true})))
  (is (= :bad-range (:reason (ex-data (tu/thrown #(imap/range->wire ""))))))
  (is (= :bad-range (:reason (ex-data (tu/thrown #(imap/range->wire [1 :x]))))))
  (is (= {"uid" true "flags" true "envelope" true "internalDate" true "size" true} (imap/fetch-query nil)) "default query")
  (is (= {"uid" true "bodyStructure" true "headers" true "source" true}
         (imap/fetch-query {:body-structure true :headers? true :source? true :peek true})) ":peek accepted and ignored (imapflow always PEEKs)")
  (is (= {"uid" true "headers" ["Subject" "Date"] "source" {"start" 0 "maxLength" 1024} "bodyParts" ["1" "2.1"]}
         (imap/fetch-query {:headers ["Subject" "Date"] :source {:max-length 1024} :body-parts ["1" "2.1"]})))
  (is (= "\\Seen" (imap/flag->wire :seen)))
  (is (= ["\\Seen" "\\Flagged" "$Forwarded" "\\Draft"] (imap/flags->wire [:seen :flagged "$Forwarded" :draft])))
  (is (= ["$label1"] (imap/flags->wire :$label1)) "custom keyword flags by name")
  (is (= :flag-not-permitted (:reason (ex-data (tu/thrown #(imap/flag->wire :recent))))))
  (is (= :bad-flag (:reason (ex-data (tu/thrown #(imap/flag->wire 42)))))))

;; ---------------------------------------------------------------------------
;; JS collection walking
;; ---------------------------------------------------------------------------

(deftest js->data-walks-maps-sets-bigints-buffers-dates
  (let [fake (tu/fake-imap {:mailbox inbox-fixture})
        mb (do ((:emit! fake) "nothing" {}) nil)
        walked (ib/js->data (tu/js-map {"bodyParts" (tu/js-map {"1" (tu/buffer-of "hello")})
                                        "labels" (tu/js-set ["\\Inbox" "work"])
                                        "modseq" (tu/bigint "18446744073709551615")
                                        "internalDate" (tu/js-date "2026-09-25T10:00:01.000Z")
                                        "bodyStructure" {:type "text/plain" :parameters {:charset "utf-8" :fileName "a.txt"} :childNodes []}
                                        "uidMap" (tu/js-map {1 1001})}))]
    (is (nil? mb))
    (is (= "hello" (att/->string (get-in walked ["bodyParts" "1"]))) "Buffer -> Bytes; Map keys kept as strings")
    (is (= #{"\\Inbox" "work"} (get walked "labels")) "Set -> set of strings")
    (is (= "18446744073709551615" (get walked "modseq")) "BigInt -> decimal string, no precision loss")
    (is (= "2026-09-25T10:00:01.000Z" (get walked "internalDate")))
    (is (= {:type "text/plain" :parameters {"charset" "utf-8" "fileName" "a.txt"} :child-nodes []} (get walked "bodyStructure"))
        "plain objects get kebab keys except parameter maps")
    (is (= {1 1001} (get walked "uidMap")) "numeric Map keys stay numbers")
    (is (nil? (ib/js->data nil)))
    (is (= [1 "a" true] (ib/js->data (tu/js-array [1 "a" true]))))))

;; ---------------------------------------------------------------------------
;; error taxonomy (pure)
;; ---------------------------------------------------------------------------

(deftest error-map->ex-classifies-imapflow-errors
  (let [t (fn [m] (util/error-type (imap/error-map->ex m)))
        d (fn [m] (util/error-data (imap/error-map->ex m)))]
    (is (= :email-cljs/auth-error (t {:authentication-failed? true :response-text "[AUTHENTICATIONFAILED] Authentication failed." :server-response-code "AUTHENTICATIONFAILED" :message "Authentication failed"})))
    (is (= :imap (:protocol (d {:authentication-failed? true}))))
    (is (= "AUTHENTICATIONFAILED" (:server-response-code (d {:authentication-failed? true :server-response-code "AUTHENTICATIONFAILED"}))))
    (is (= :email-cljs/network-error (t {:tls-failed? true :message "self signed"})))
    (is (true? (:tls? (d {:code "UPGRADE_TIMEOUT"}))))
    (is (true? (:retryable? (d {:code "CONNECT_TIMEOUT"}))))
    (is (true? (:retryable? (d {:code "ETIMEOUT"}))))
    (is (= :not-connected (:reason (d {:code "NoConnection"}))))
    (is (= :bye (:reason (d {:reason "BYE Autologout" :message "BYE"}))))
    (is (= 5000 (:retry-after-ms (d {:code "ETHROTTLE" :throttle-reset-ms 5000}))))
    (is (= :lock-timeout (:reason (d {:code "LockTimeout"}))))
    (is (= :email-cljs/argument-error (t {:code "MissingServerExtension"})))
    (is (= :email-cljs/argument-error (t {:code "InvalidSequenceSet"})))
    (is (= :limit-exceeded (:reason (d {:code "LiteralTooLarge"}))))
    (is (= :protocol (:reason (d {:code "UnexpectedTag"}))))
    (is (= :not-found (:reason (d {:code "NotFound"}))))
    (let [no (imap/error-map->ex {:response-status "NO" :response-text "[TRYCREATE] Mailbox doesn't exist: Nope" :server-response-code "TRYCREATE" :command "SELECT" :path "Nope" :mailbox-missing? true :message "Command failed"})]
      (is (= :email-cljs/imap-error (util/error-type no)))
      (is (= "NO" (:response-status (ex-data no))))
      (is (= "[TRYCREATE] Mailbox doesn't exist: Nope" (:response-text (ex-data no))) "server text kept")
      (is (= :mailbox-missing (:reason (ex-data no))))
      (is (str/includes? (ex-message no) "Mailbox doesn't exist")))
    (is (= :bad-command (:reason (d {:response-status "BAD" :response-text "parse error"}))))
    (is (= :email-cljs/imap-error (t {:message "?"})))
    (is (= "ETHROTTLE" (:imapflow-code (d {:code "ETHROTTLE"}))) "imapflow code recorded")))

;; ---------------------------------------------------------------------------
;; connection + mailboxes through the fake
;; ---------------------------------------------------------------------------

(deftest search!-explains-a-refused-search-and-skips-future-within-days
  (async done
    (let [made (atom nil)
          fake (tu/fake-imap {:mailbox inbox-fixture
                              :capabilities ["IMAP4rev1" "IDLE" "UIDPLUS" "WITHIN"]
                              :methods {"search" (fn [q _opts]
                                                   (if (:before q)
                                                     (do ((:on-error (first @made))
                                                          {:level :warn :message "Command failed" :response-status "BAD"
                                                           :response-text "Error in IMAP command UID SEARCH: Invalid search interval parameter (0.001 + 0.000 secs)."
                                                           :command "SEARCH"})
                                                         false)
                                                     [5]))}})
          [c made*] (client-with fake)
          _ (reset! made @made*)
          unselected (tu/fake-imap {:methods {"search" (fn [& _] false)}})
          [cu _] (client-with unselected)]
      (tu/finish done
        (p/let [_ (imap/connect! c)
                _ (imap/connect! cu)
                no-mailbox (tu/rejection (imap/search! cu {:unseen true}))
                _ (imap/mailbox-open! c "INBOX")
                future-since (imap/search! c {:since "2999-01-01" :unseen true})
                future-since-es (imap/search! c {:since "2999-01-01"} {:return [:count]})
                future-before (imap/search! c {:before "2999-01-01" :unseen true})
                only-before (imap/search! c {:before {:year 2999 :month 1 :day 1}})
                refused (tu/rejection (imap/search! c {:before "2026-01-01"}))
                searches (tu/calls-named fake "search")]
          (is (= :no-mailbox (:reason (ex-data no-mailbox))))
          (is (= [] future-since) "since a future UTC day: no round trip, no hits")
          (is (= {:count 0} future-since-es))
          (is (= [5] future-before))
          (is (= [5] only-before))
          (is (= [{:seen false} {:all true} {:before "2026-01-01"}] (mapv first searches))
              "future :before dropped, lone future :before -> ALL, past :before sent (and refused by the fake)")
          (is (= :email-cljs/imap-error (util/error-type refused)))
          (is (= :search-failed (:reason (ex-data refused))))
          (is (= "BAD" (:response-status (ex-data refused))))
          (is (str/includes? (:response-text (ex-data refused)) "Invalid search interval"))
          (is (= {"before" "2026-01-01"} (:query (ex-data refused))))
          (is (= "INBOX" (:path (ex-data refused))))
          (is (= :warn (:level (imap/last-warning c))))
          (is (nil? (imap/last-error c)) "log entries never masquerade as 'error' events"))))))

(deftest real-imapflow-client-captures-log-warnings-without-connecting
  (let [c (imap/imap-client {:host "mail.example.test" :auth auth})]
    (is (nil? (imap/last-warning c)))
    (tu/log-warn! (:conn c) {:message "Command failed" :responseStatus "NO" :responseText "[CANNOT] nope" :command "SEARCH"})
    (is (= {:level :warn :response-status "NO" :response-text "[CANNOT] nope" :command "SEARCH"}
           (select-keys (imap/last-warning c) [:level :response-status :response-text :command])))
    (is (nil? (imap/last-error c)))
    (is (not (str/includes? (pr-str (imap/last-warning c)) tu/fake-password)))
    (is (= "mail.example.test" (get (:options c) "host")))
    (is (false? (get (:options c) "logger")) "the capturing logger is installed by the binding, not part of the printable options")))

(deftest connect!-records-capabilities-and-maps-auth-failure
  (async done
    (let [fake (tu/fake-imap {:mailbox inbox-fixture})
          [c _] (client-with fake)
          bad (tu/fake-imap {:methods {"connect" (fn [] (throw (tu/js-error {:message "Authentication failed" :authenticationFailed true :serverResponseCode "AUTHENTICATIONFAILED" :response "[AUTHENTICATIONFAILED] Authentication failed."})))}})
          [cbad _] (client-with bad)
          unusable (tu/fake-imap {:methods {"connect" (fn [] nil)}})
          [cun _] (client-with unusable)]
      (tu/finish done
        (p/let [r (imap/connect! c)
                connected-before (imap/connected? c)
                e (tu/rejection (imap/connect! cbad))
                e2 (tu/rejection (imap/connect! cun))
                closed (imap/close-imap! c)]
          (is (identical? c r))
          (is (true? connected-before) "before close")
          (is (= {:imap4rev1 true :idle true :uidplus true :move true :condstore true} (imap/capabilities c)))
          (is (= :email-cljs/auth-error (util/error-type e)))
          (is (= "[AUTHENTICATIONFAILED] Authentication failed." (:response (ex-data e))))
          (is (= :not-usable (:reason (ex-data e2))))
          (is (true? closed))
          (is (= [["connect" []] ["logout" []]] @(:calls fake)))
          (is (false? (imap/connected? c))))))))

(deftest mailbox-open-close-list-status-quota
  (async done
    (let [fake (tu/fake-imap {:mailbox inbox-fixture
                              :methods {"list" (fn [_] [{:path "INBOX.Sent" :name "Sent" :parentPath "INBOX" :parent ["INBOX"] :delimiter "."
                                                          :flags (tu/js-set ["\\HasNoChildren" "\\Sent"]) :specialUse "\\Sent" :specialUseSource "extension"
                                                          :listed true :subscribed true
                                                          :status {:path "INBOX.Sent" :messages 12 :unseen 0 :uidNext 13 :uidValidity (tu/bigint "17")}}])
                                        "status" (fn [path _] {:path path :messages 43 :unseen 1 :uidNext 1002 :uidValidity (tu/bigint "1758758400") :highestModseq (tu/bigint "77")})
                                        "getQuota" (fn [& _] {:path "INBOX" :storage {:used 10485760 :limit 1073741824} :messages {:used 43 :limit 100000}})}})
          [c _] (client-with fake)]
      (tu/finish done
        (p/let [_ (imap/connect! c)
                mb (imap/mailbox-open! c "INBOX")
                current (imap/current-mailbox c)
                boxes (imap/mailboxes! c {:status? true})
                st (imap/mailbox-status! c "INBOX")
                q (imap/quota! c)
                _ (imap/mailbox-close! c)
                _ (imap/mailbox-open! c "INBOX" {:read-only? true})
                _ (imap/mailbox-close! c {:expunge? true})]
          (testing "open"
            (is (= "INBOX" (:path mb)))
            (is (= 42 (:exists mb)))
            (is (= 42 (:exists-count mb)))
            (is (= "1758758400" (:uid-validity mb)) "BigInt -> string")
            (is (= "77" (:highest-modseq mb)))
            (is (= #{"\\Seen" "\\*"} (:permanent-flags mb)))
            (is (= 1002 (:uid-next mb)))
            (is (= mb current))
            (is (= [["INBOX" {:readOnly false}] ["INBOX" {:readOnly true}]] (tu/calls-named fake "getMailboxLock")) "lock with readOnly flag"))
          (testing "list"
            (is (= 1 (count boxes)))
            (let [b (first boxes)]
              (is (= "INBOX.Sent" (:path b)))
              (is (= #{"\\HasNoChildren" "\\Sent"} (:flags b)))
              (is (= "\\Sent" (:special-use b)))
              (is (= "extension" (:special-use-source b)))
              (is (= "17" (get-in b [:status :uid-validity])))
              (is (= 13 (get-in b [:status :uid-next]))))
            (is (= [[{:statusQuery {:messages true :unseen true :uidNext true :uidValidity true :highestModseq true}}]]
                   (tu/calls-named fake "list"))))
          (testing "status + quota"
            (is (= {:path "INBOX" :messages 43 :unseen 1 :uid-next 1002 :uid-validity "1758758400" :highest-modseq "77"} st))
            (is (= [["INBOX" {:messages true :recent true :uidNext true :uidValidity true :unseen true :highestModseq true}]] (tu/calls-named fake "status")))
            (is (= 10485760 (get-in q [:storage :used])) "bytes as imapflow converts them"))
          (testing "close"
            (is (= 2 (:released @(:state fake))) "both locks released")
            (is (= [[]] (tu/calls-named fake "mailboxClose")) "CLOSE only with :expunge? true")
            (is (nil? (imap/current-mailbox c)))))))))

;; ---------------------------------------------------------------------------
;; search / fetch / download
;; ---------------------------------------------------------------------------

(def message-fixture
  {:seq 43 :uid 1001 :modseq (tu/bigint "77") :size 1649
   :internalDate (tu/js-date "2026-09-25T10:00:01.000Z")
   :flags (tu/js-set ["\\Seen"]) :flagColor nil
   :envelope {:date (tu/js-date "2026-09-24T23:08:56.000Z") :subject "Ünïcode ✓" :messageId "<b73@example.test>" :inReplyTo nil
              :from [{:name "Doe, John" :address "john@example.test"}] :sender [{:name "Doe, John" :address "john@example.test"}]
              :replyTo [] :to [{:name "" :address "a@example.test"} {:name "Bé" :address "b@example.test"}] :cc [] :bcc []}
   :bodyStructure {:type "multipart/mixed" :parameters {:boundary "--b1"}
                   :childNodes [{:part "1" :type "text/plain" :parameters {:charset "utf-8"} :encoding "quoted-printable" :size 12 :lineCount 1}
                                {:part "2" :type "image/png" :parameters {:name "logo.png"} :id "<logo@x>" :encoding "base64" :size 70
                                 :disposition "inline" :dispositionParameters {:filename "logo.png"}}]}
   :source (tu/buffer-of "From: john@example.test\r\n\r\nbody")
   :headers (tu/buffer-of "From: john@example.test\r\n")})

(deftest search-fetch-and-download
  (async done
    (let [fake (tu/fake-imap {:mailbox inbox-fixture
                              :methods {"search" (fn [q opts] (if (:returnOptions opts) {:count 2 :max 1001} [1000 1001]))
                                        "fetchAll" (fn [_range _q _o] [message-fixture])
                                        "fetchOne" (fn [range _q _o] (if (= "1001" range)
                                                                       (assoc-in message-fixture [:envelope :date] "not a date")
                                                                       false))
                                        "download" (fn [_range part _o] (if (= part "2")
                                                                          {:meta {:contentType "image/png" :charset nil :disposition "inline" :filename "logo.png" :encoding "base64" :expectedSize 8}
                                                                           :content (tu/readable-of "PNGBYTES")}
                                                                          {}))}})
          [c _] (client-with fake)]
      (tu/finish done
        (p/let [_ (imap/connect! c)
                _ (imap/mailbox-open! c "INBOX")
                uids (imap/search! c {:unseen true :since "2026-09-01"})
                es (imap/search! c {:all true} {:return [:count :max]})
                empty-err (tu/rejection (imap/search! c {}))
                msgs (imap/fetch-all! c [1000 1001] {:envelope true :flags true :body-structure true :size true :internal-date true :source? true :headers? true})
                one (imap/fetch-one! c 1001)
                none (imap/fetch-one! c 9999)
                by-query (imap/fetch-one! c {:header {"Message-ID" "<b73@example.test>"}})
                dl (imap/download! c 1001 "2")
                part (imap/body-part! c 1001 "2")
                missing (imap/body-part! c 1001 "9")]
          (testing "search"
            (is (= [1000 1001] uids))
            (is (= [[{:seen false :since "2026-09-01"} {:uid true}]
                    [{:all true} {:uid true :returnOptions ["COUNT" "MAX"]}]]
                   (tu/calls-named fake "search")) "compiled query + uid true reach imapflow")
            (is (= {:count 2 :max 1001} es))
            (is (= :empty-search (:reason (ex-data empty-err))))
            (is (= 2 (count (tu/calls-named fake "search"))) "the empty query never reached the connection"))
          (testing "fetch"
            (let [m (first msgs)]
              (is (= ["1000,1001" {:uid true :flags true :envelope true :bodyStructure true :internalDate true :size true :source true :headers true} {:uid true}]
                     (first (tu/calls-named fake "fetchAll"))))
              (is (= 1001 (:uid m)))
              (is (= "77" (:modseq m)))
              (is (= #{"\\Seen"} (:flags m)))
              (is (= "2026-09-25T10:00:01.000Z" (:internal-date m)))
              (is (= "2026-09-24T23:08:56.000Z" (get-in m [:envelope :date])))
              (is (= "Ünïcode ✓" (get-in m [:envelope :subject])))
              (is (= "<b73@example.test>" (get-in m [:envelope :message-id])))
              (is (= [{:name "Doe, John" :address "john@example.test"}] (get-in m [:envelope :from])))
              (is (= [{:name nil :address "a@example.test"} {:name "Bé" :address "b@example.test"}] (get-in m [:envelope :to])) "empty name -> nil")
              (is (= [] (get-in m [:envelope :cc])))
              (is (= "multipart/mixed" (get-in m [:body-structure :type])))
              (is (= {"boundary" "--b1"} (get-in m [:body-structure :parameters])) "wire parameter names kept")
              (is (= "2" (get-in m [:body-structure :child-nodes 1 :part])))
              (is (= {"filename" "logo.png"} (get-in m [:body-structure :child-nodes 1 :disposition-parameters])))
              (is (= 1 (get-in m [:body-structure :child-nodes 0 :line-count])) "other keys kebab")
              (is (att/bytes? (:source m)))
              (is (str/starts-with? (att/->string (:source m)) "From: "))
              (is (att/bytes? (:headers m))))
            (is (nil? (get-in one [:envelope :date])))
            (is (= "not a date" (get-in one [:envelope :date-raw])) "unparseable Date: header kept raw")
            (is (nil? none))
            (is (= 1001 (:uid by-query)) "search-map ranges resolve through fetchAll")
            (is (= [{:header {:Message-ID "<b73@example.test>"}} {:uid true :flags true :envelope true :internalDate true :size true} {:uid true}]
                   (second (tu/calls-named fake "fetchAll"))))
            (is (= [["1001" {:uid true :flags true :envelope true :internalDate true :size true} {:uid true}]
                    ["9999" {:uid true :flags true :envelope true :internalDate true :size true} {:uid true}]]
                   (tu/calls-named fake "fetchOne"))))
          (testing "download"
            (is (= {:content-type "image/png" :charset nil :disposition "inline" :filename "logo.png" :encoding "base64" :expected-size 8} (:meta dl)))
            (is (= "PNGBYTES" (att/->string (:content dl))))
            (is (= (att/bytes "PNGBYTES") part))
            (is (= "UE5HQllURVM=" (att/->base64 part)) "lazy base64 out")
            (is (nil? missing))
            (is (= [["1001" "2" {:uid true}] ["1001" "2" {:uid true}] ["1001" "9" {:uid true}]] (tu/calls-named fake "download")))))))))

;; ---------------------------------------------------------------------------
;; flags / move / copy / delete / expunge / append
;; ---------------------------------------------------------------------------

(deftest flags-move-copy-delete-expunge-append
  (async done
    (let [fake (tu/fake-imap {:mailbox inbox-fixture
                              :methods {"messageMove" (fn [_ dest _] {:path "INBOX" :destination dest :uidValidity (tu/bigint "99") :uidMap (tu/js-map {1001 5})})
                                        "messageCopy" (fn [& _] false)
                                        "append" (fn [path _ _ _] {:destination path :uid 77 :uidValidity (tu/bigint "99") :seq 43})}})
          [c _] (client-with fake)]
      (tu/finish done
        (p/let [_ (imap/connect! c)
                _ (imap/mailbox-open! c "INBOX")
                added (imap/flags-add! c 1001 [:seen :flagged])
                set (imap/flags-set! c [1000 1001] "$Forwarded" {:unchanged-since "77" :silent? true})
                removed (imap/mark-unseen! c 1001)
                moved (imap/move! c 1001 "INBOX.Archive")
                copy-err (tu/rejection (imap/copy! c 1001 "INBOX.Archive"))
                deleted (imap/delete! c [1000 1001])
                uidx (imap/uid-expunge! c [1000 1001])
                exp (imap/expunge! c "INBOX")
                exp-err (tu/rejection (imap/expunge! c "INBOX.Other"))
                appended (imap/append! c "INBOX.Sent" (att/bytes "From: a@x\r\n\r\nhi") {:flags [:seen] :date "2026-09-25T10:00:00.000Z"})]
          (is (true? added))
          (is (true? set))
          (is (true? removed))
          (is (= [["1001" ["\\Seen" "\\Flagged"] {:uid true}]] (tu/calls-named fake "messageFlagsAdd")))
          (is (= [["1000,1001" ["$Forwarded"] {:uid true :unchangedSince "77" :silent true}]] (tu/calls-named fake "messageFlagsSet")))
          (is (= [["1001" ["\\Seen"] {:uid true}]] (tu/calls-named fake "messageFlagsRemove")))
          (is (= {:path "INBOX" :destination "INBOX.Archive" :uid-validity "99" :uid-map {1001 5}} moved) "uid-map with integer keys")
          (is (= [["1001" "INBOX.Archive" {:uid true}]] (tu/calls-named fake "messageMove")))
          (is (= :command-failed (:reason (ex-data copy-err))))
          (is (= :email-cljs/imap-error (util/error-type copy-err)))
          (is (true? deleted))
          (is (= [["1000,1001" {:uid true}]] (tu/calls-named fake "messageDelete")))
          (is (true? uidx))
          (is (true? exp))
          (is (= [["UID EXPUNGE" [{:type "SEQUENCE" :value "1000,1001"}]] ["EXPUNGE" false]] (tu/calls-named fake "exec")))
          (is (= :mailbox-mismatch (:reason (ex-data exp-err))))
          (is (= {:destination "INBOX.Sent" :uid 77 :uid-validity "99" :seq 43} appended))
          (let [[path content flags] (first (tu/calls-named fake "append"))]
            (is (= "INBOX.Sent" path))
            (is (= ["\\Seen"] flags))
            (is (some? content))))))))

(deftest uid-expunge-fallback-without-uidplus
  (async done
    (let [fake (tu/fake-imap {:mailbox inbox-fixture :capabilities ["IMAP4rev1" "IDLE"]})
          [c _] (client-with fake)]
      (tu/finish done
        (p/let [_ (imap/connect! c)
                _ (imap/mailbox-open! c "INBOX")
                ok (imap/uid-expunge! c [5])
                strict (tu/rejection (imap/uid-expunge! c [5] {:strict? true}))
                del-strict (tu/rejection (imap/delete! c 5 {:strict? true}))]
          (is (true? ok))
          (is (= [["EXPUNGE" false]] (tu/calls-named fake "exec")) "documented fallback: plain EXPUNGE")
          (is (= :missing-server-extension (:reason (ex-data strict))))
          (is (= "UIDPLUS" (:extension (ex-data strict))))
          (is (= :missing-server-extension (:reason (ex-data del-strict))))
          (is (= [] (tu/calls-named fake "messageDelete")) "strict delete never reached the server"))))))

;; ---------------------------------------------------------------------------
;; watch! — the IDLE watcher on its own connection
;; ---------------------------------------------------------------------------

(deftest watch!-lifecycle-events-and-stop
  (async done
    (let [main (tu/fake-imap {:mailbox inbox-fixture})
          watcher (tu/fake-imap {:mailbox inbox-fixture})
          [c made] (client-with main watcher)
          ch (imap/watch! c "INBOX" {:buffer 16})]
      (tu/finish done
        (p/let [open-ev (tu/take-events! ch 1 2000)
                _ (do ((:emit! watcher) "exists" {:path "INBOX" :count 43 :prevCount 42})
                      ((:emit! watcher) "flags" {:path "INBOX" :seq 42 :uid 1000 :modseq (tu/bigint "78") :flags (tu/js-set ["\\Seen" "\\Flagged"])})
                      ((:emit! watcher) "expunge" {:path "INBOX" :uid 998 :vanished true}))
                evs (tu/take-events! ch 3 2000)
                stopped (imap/stop-watch! ch)
                closed-ev (tu/take-events! ch 1 500)
                after (tu/take-events! ch 1 200)
                stopped-again (imap/stop-watch! ch)]
          (is (= 2 (count @made)) "watch! opened its own connection from the same config")
          (is (true? (get-in @made [1 :wire "disableAutoIdle"])))
          (is (= :open (:event (first open-ev))))
          (is (= "INBOX" (:path (first open-ev))))
          (is (= 42 (get-in (first open-ev) [:mailbox :exists])))
          (is (= [{:event :new-mail :path "INBOX" :count 43 :prev-count 42}
                  {:event :flags :path "INBOX" :seq 42 :uid 1000 :modseq "78" :flags #{"\\Seen" "\\Flagged"}}
                  {:event :expunge :path "INBOX" :uid 998 :seq nil :vanished true}]
                 evs))
          (is (true? stopped))
          (is (= [{:event :closed :path "INBOX" :reason :stopped}] closed-ev))
          (is (= [] after) "channel closed after :closed")
          (is (false? (imap/watching? ch)))
          (is (= ["connect" "mailboxOpen" "idle" "logout"] (mapv first @(:calls watcher))))
          (is (= [["INBOX" {:readOnly true}]] (tu/calls-named watcher "mailboxOpen")) "watcher opens read-only")
          (is (= [] @(:calls main)) "the main connection was never touched")
          (is (false? stopped-again) "stopping twice is a no-op"))))))

(deftest watch!-restarts-idle-with-a-noop-every-max-idle-ms
  (async done
    (let [main (tu/fake-imap {:mailbox inbox-fixture})
          watcher (tu/fake-imap {:mailbox inbox-fixture})
          [c _] (client-with main watcher)
          ch (imap/watch! c "INBOX" {:max-idle-ms 30})]
      (tu/finish done
        (p/let [evs (tu/take-events! ch 3 1500)
                _ (imap/stop-watch! ch)]
          (is (= [:open :idle-restart :idle-restart] (mapv :event evs)))
          (is (<= 2 (count (tu/calls-named watcher "noop"))) "IDLE broken by NOOP")
          (is (<= 3 (:idle-count @(:state watcher))) "IDLE re-entered after each restart"))))))

(deftest watch!-ends-when-the-connection-closes
  (async done
    (let [main (tu/fake-imap {:mailbox inbox-fixture})
          watcher (tu/fake-imap {:mailbox inbox-fixture})
          [c _] (client-with main watcher)
          ch (imap/watch! c "INBOX")]
      (tu/finish done
        (p/let [_ (tu/take-events! ch 1 1000)
                _ (do ((:emit! watcher) "close" nil) nil)
                evs (tu/take-events! ch 2 1000)]
          (is (= [{:event :closed :path "INBOX" :reason :connection-lost}] evs))
          (is (false? (imap/watching? ch))))))))

(deftest watch!-fetches-new-mail-when-asked
  (async done
    (let [main (tu/fake-imap {:mailbox inbox-fixture})
          watcher (tu/fake-imap {:mailbox inbox-fixture
                                 :methods {"fetchAll" (fn [range _ _] (if (= "43:*" range) [message-fixture] []))}})
          [c _] (client-with main watcher)
          ch (imap/watch! c "INBOX" {:fetch? true :query {:envelope true :flags true}})]
      (tu/finish done
        (p/let [_ (tu/take-events! ch 1 1000)
                _ (do ((:emit! watcher) "exists" {:path "INBOX" :count 43 :prevCount 42}) nil)
                evs (tu/take-events! ch 1 1000)
                _ (imap/stop-watch! ch)]
          (let [ev (first evs)]
            (is (= :new-mail (:event ev)))
            (is (= [1001] (:uids ev)))
            (is (= "Ünïcode ✓" (get-in ev [:messages 0 :envelope :subject])))
            (is (= [["43:*" {:uid true :flags true :envelope true} {:uid false}]] (tu/calls-named watcher "fetchAll"))
                "seq range after prev-count, on the watcher's own connection")))))))

(deftest watch!-reports-connect-failure-and-closes
  (async done
    (let [main (tu/fake-imap {:mailbox inbox-fixture})
          watcher (tu/fake-imap {:methods {"connect" (fn [] (throw (tu/js-error {:message "connect timeout" :code "CONNECT_TIMEOUT"})))}})
          [c _] (client-with main watcher)
          ch (imap/watch! c "INBOX")]
      (tu/finish done
        (p/let [evs (tu/take-events! ch 2 1000)]
          (is (= [:error :closed] (mapv :event evs)))
          (is (= :email-cljs/network-error (util/error-type (:error (first evs)))))
          (is (= :error (:reason (second evs)))))))))

(deftest watch!-consumer-closing-the-channel-stops-the-watcher
  (async done
    (let [main (tu/fake-imap {:mailbox inbox-fixture})
          watcher (tu/fake-imap {:mailbox inbox-fixture})
          [c _] (client-with main watcher)
          ch (imap/watch! c "INBOX")]
      (tu/finish done
        (p/let [_ (tu/take-events! ch 1 1000)
                _ (do (async/close! ch) ((:emit! watcher) "exists" {:path "INBOX" :count 43 :prevCount 42}) nil)
                _ (tu/wait 50)]
          (is (false? (imap/watching? ch)))
          (is (= ["connect" "mailboxOpen" "idle" "logout"] (mapv first @(:calls watcher)))))))))
