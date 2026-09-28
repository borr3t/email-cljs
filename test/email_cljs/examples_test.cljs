(ns email-cljs.examples-test
  "The examples of the README, run. Network-free: messages are built and
   parsed in the process, mail is sent through the fake transport and read
   over the fake connection of dev/email_cljs/examples.cljs.

   The README is held to the code:
   - what stands between a README:begin and a README:end mark of
     dev/email_cljs/examples.cljs stands in README.md as written;
   - every one-liner of `shown` stands in README.md with the value it
     evaluates to;
   - every result README.md prints is the result the example returns.
   Spacing and commas are not compared."
  (:require ["fs" :as fs]
            [cljs.test :refer [deftest is testing async]]
            [clojure.core.async :as async]
            [clojure.string :as str]
            [email-cljs.attachment :as att]
            [email-cljs.examples :as ex]
            [email-cljs.imap :as imap]
            [email-cljs.message :as msg]
            [email-cljs.smtp :as smtp]
            [email-cljs.util :as util]
            [promesa.core :as p])
  (:require-macros [email-cljs.doctest :refer [claims]]))

;; ---------------------------------------------------------------------------
;; The one-liners
;; ---------------------------------------------------------------------------

(def shown
  "The one-liners of the README, each with its value."
  (claims
   (util/parse-address "Ada Lovelace <ada@example.test>")
   (util/format-address {:name "Lovelace, Ada" :address "ada@example.test"})
   (msg/canonical-header-name "message-id")
   (util/secret "not-a-password")
   (util/env-config {"WORK_EMAIL_USERNAME" "ada@example.test" "WORK_EMAIL_PASSWORD" "not-a-password" "WORK_EMAIL_SMTP_HOST" "smtp.example.test" "WORK_EMAIL_SMTP_PORT" "465"} "WORK_EMAIL_")
   (select-keys (smtp/client-options->wire {:host "smtp.example.test" :port 587}) ["port" "secure"])
   (util/smtp-code-class 451)
   (imap/compile-search {:unseen true :from "ada@example.test" :since "21-Nov-1997"})
   (imap/range->wire [101 102])
   (imap/flags->wire [:seen :flagged])
   (att/->base64 (att/from-string "Note G"))
   (att/->string (att/from-base64 "Tm90ZSBH"))))

;; ---------------------------------------------------------------------------
;; Reading the README
;; ---------------------------------------------------------------------------

(defn- slurp [path]
  (fs/readFileSync path "utf8"))

(defn- plain
  "`text` without what is not compared: commas, and any run of whitespace
   as one space."
  [text]
  (-> text
      (str/replace "," "")
      (str/replace #"\s+" " ")
      str/trim))

(defn- region
  "The text between the marks README:begin `label` and README:end `label`
   of the file at `path`, nil when either mark is missing."
  [path label]
  (let [[_ found] (re-find (re-pattern (str "(?s);; README:begin " label "\\n(.*?)\\n\\s*;; README:end " label))
                           (slurp path))]
    found))

(defn- shows?
  "Does README.md show `text`?"
  [text]
  (str/includes? (plain (slurp "README.md")) (plain text)))

(defn- shown-in-readme
  "Asserts README.md prints `value`."
  [value]
  (is (shows? (pr-str value)) (str "README.md does not show:\n" (pr-str value))))

(defn- run
  "Runs the promise of `(f)` as an async test body."
  [done f]
  (-> (f)
      (p/catch (fn [e] (is false (str "unexpected rejection: " (ex-message e) " " (pr-str (ex-data e))))))
      (p/finally (fn [_ _] (done)))))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(deftest the-quick-start-builds-and-parses-a-message
  (async done
    (run done
      (fn []
        (p/let [r (ex/quick-start)]
          (testing "what was written is what is read"
            (is (= "Notes on the engine" (:subject r)))
            (is (= {:name "Ada Lovelace" :address "ada@example.test"} (:from r)))
            (is (= [{:name nil :address "grace@example.test"}] (:to r)))
            (is (= "<notes-1@example.test>" (:message-id r)))
            (is (= "The notes are attached." (:text r)))
            (is (= "<p>The notes are <b>attached</b>.</p>" (:html r))))
          (testing "the date arrives as an instant, in UTC"
            (is (= "1997-11-21T15:55:06.000Z" (:date r))))
          (testing "the attachment arrives with its bytes"
            (is (= [{:filename "notes.txt" :content-type "text/plain" :size 6 :text "Note G"}]
                   (:attachments r))))
          (shown-in-readme r))))))

(deftest a-message-is-sent
  (async done
    (run done
      (fn []
        (p/let [r (ex/send-offline)]
          (testing "the server is heard on each recipient"
            (is (= {:message-id "<notes-1@example.test>"
                    :accepted ["grace@example.test" "charles@example.test"]
                    :rejected ["nobody@example.test"]
                    :all-accepted? false
                    :response-code 250}
                   (:sent r))))
          (testing "the blind copy is in the envelope and in no header"
            (is (= ["grace@example.test" "nobody@example.test" "charles@example.test"]
                   (get-in r [:transmitted :envelope :to])))
            (is (= ["nobody@example.test"] (get-in r [:transmitted :cc])))
            (is (= [] (get-in r [:transmitted :bcc]))))
          (shown-in-readme r))))))

(deftest unread-mail-is-read
  (async done
    (run done
      (fn []
        (p/let [first-mail (msg/build-string ex/message)
                second-mail (msg/build-string ex/reply)
                server (ex/fake-server [first-mail second-mail])
                client (imap/imap-client (ex/account server))
                r (ex/read-unread client)
                offline (ex/read-offline)]
          (is (= {:mail [{:uid 101 :from "ada@example.test" :subject "Notes on the engine"
                          :attachments ["notes.txt"]}
                         {:uid 102 :from "grace@example.test" :subject "Re: Notes on the engine"
                          :attachments []}]
                  :left-unread 0}
                 r))
          (is (= r offline))
          (testing "what was read is marked as read, on the server"
            (is (every? :seen? @(:inbox server))))
          (testing "the connection is closed"
            (is (false? (imap/connected? client)))
            (is (empty? @(:conns server))))
          (shown-in-readme r))))))

(deftest an-empty-inbox-is-read
  (async done
    (run done
      (fn []
        (p/let [r (ex/read-unread (imap/imap-client (ex/account (ex/fake-server []))))]
          (is (= {:mail [] :left-unread 0} r)))))))

(deftest a-mailbox-is-watched
  (async done
    (run done
      (fn []
        (p/let [r (ex/watch-offline)]
          (is (= {:arrived {:uid 102 :subject "Re: Notes on the engine"} :stopped true} r))
          (shown-in-readme r))))))

(deftest a-stopped-watcher-closes-its-channel
  (async done
    (run done
      (fn []
        (p/let [first-mail (msg/build-string ex/message)
                server (ex/fake-server [first-mail])
                {:keys [mail stop!]} (ex/arrivals (imap/imap-client (ex/account server)) "INBOX")
                _ (stop!)
                end (p/create (fn [resolve _] (async/take! mail (fn [v] (resolve (if (nil? v) :closed v))))))]
          (is (= :closed end))
          (is (empty? @(:conns server)) "the watcher logged out"))))))

(deftest failures-are-data
  (async done
    (run done
      (fn []
        (p/let [r (ex/errors-offline)]
          (is (= {:sent {:ok? true :message-id "<notes-1@example.test>"}
                  :refused {:ok? false :why :refused :reason :partial-rejection
                            :rejected ["grace@example.test"]}
                  :login {:ok? false :why :login :response-code 535}
                  :unreachable {:ok? false :why :unreachable :retry? true}
                  :from-a-file {:ok? false :why :message :reason :file-access-disabled}}
                 r))
          (shown-in-readme r))))))

(deftest the-one-liners-hold
  (let [value (into {} shown)]
    (is (= (count shown) (count value)) "no one-liner is shown twice")
    (is (= {:name "Ada Lovelace" :address "ada@example.test"}
           (value "(util/parse-address \"Ada Lovelace <ada@example.test>\")")))
    (is (= "Message-ID" (value "(msg/canonical-header-name \"message-id\")")))
    (is (= :transient (value "(util/smtp-code-class 451)")))
    (is (= "101,102" (value "(imap/range->wire [101 102])")))
    (is (= ["\\Seen" "\\Flagged"] (value "(imap/flags->wire [:seen :flagged])")))
    (is (= "Tm90ZSBH" (value "(att/->base64 (att/from-string \"Note G\"))")))
    (is (= "Note G" (value "(att/->string (att/from-base64 \"Tm90ZSBH\"))")))
    (testing "a secret never prints"
      (is (not-any? #(str/includes? (pr-str (second %)) "not-a-password") shown)))))

(deftest the-readme-shows-what-runs
  (testing "the marked code of the examples"
    (doseq [label ["quick-start" "fake-smtp" "send" "read" "watch" "errors"]
            :let [code (region "dev/email_cljs/examples.cljs" label)]]
      (is (some? code) (str "no region " label))
      (is (shows? code) (str "README.md does not show " label ":\n" code))))
  (testing "every one-liner, with its value"
    (doseq [[source value] shown
            :let [line (str source "\n;; => " (pr-str value))]]
      (is (shows? line) (str "README.md does not show:\n" line))))
  (testing "how the examples are run, and where the manual is"
    (is (shows? "npx shadow-cljs compile examples && node target/examples.js"))
    (is (shows? "docs/email-cljs.org"))
    (is (shows? "docs/email-api-inventory.org"))))
