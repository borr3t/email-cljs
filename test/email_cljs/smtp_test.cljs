(ns email-cljs.smtp-test
  (:require [cljs.test :refer [deftest is testing async]]
            [clojure.string :as str]
            [email-cljs.attachment :as att]
            [email-cljs.smtp :as smtp]
            [email-cljs.test-util :as tu]
            [email-cljs.util :as util]
            [promesa.core :as p]))

(def auth {:user "someone@example.test" :pass (util/secret tu/fake-password)})

;; ---------------------------------------------------------------------------
;; client options (pure)
;; ---------------------------------------------------------------------------

(deftest client-options-defaults-to-465-implicit-tls
  (let [w (smtp/client-options->wire {:host "mail.example.test" :auth auth})]
    (is (= "mail.example.test" (get w "host")))
    (is (= 465 (get w "port")))
    (is (true? (get w "secure")))
    (is (false? (get w "logger")))
    (is (true? (get w "disableFileAccess")))
    (is (true? (get w "disableUrlAccess")))
    (is (nil? (get w "pool")))
    (is (= "login" (get-in w ["auth" "type"])))
    (is (util/secret? (get-in w ["auth" "pass"])) "password stays wrapped until the binding reveals it")
    (is (util/secret? (get-in w ["auth" "user"])))
    (is (not (str/includes? (pr-str w) tu/fake-password)))))

(deftest client-options-port-and-secure-rules
  (is (= {:p 587 :s false} (let [w (smtp/client-options->wire {:host "h" :port 587})] {:p (get w "port") :s (get w "secure")})) "587 -> STARTTLS")
  (is (true? (get (smtp/client-options->wire {:host "h" :port 465}) "secure")) "465 implies secure")
  (is (= 587 (get (smtp/client-options->wire {:host "h" :secure false}) "port")))
  (is (true? (get (smtp/client-options->wire {:host "h" :port "465" :secure "yes"}) "secure")) "strings via parse-bool / parse-port")
  (is (false? (get (smtp/client-options->wire {:host "h" :port 465 :secure false}) "secure")) "explicit wins")
  (is (= :bad-port (:reason (ex-data (tu/thrown #(smtp/client-options->wire {:host "h" :port 70000}))))))
  (is (= :bad-host (:reason (ex-data (tu/thrown #(smtp/client-options->wire {:port 465}))))))
  (is (= :missing-credentials (:reason (ex-data (tu/thrown #(smtp/client-options->wire {:host "h" :auth {:user "u"}}))))))
  (is (= :unknown-option (:reason (ex-data (tu/thrown #(smtp/client-options->wire {:host "h" :hots "typo"}))))))
  (is (= :email-cljs/config-error (util/error-type (tu/thrown #(smtp/client-options->wire {:port 465}))))))

(deftest client-options-pool-tls-and-timeouts
  (let [ca (util/secret "-----BEGIN CERTIFICATE-----\nfake\n-----END CERTIFICATE-----")
        w (smtp/client-options->wire {:host "h" :pool? true :max-connections 2 :rate-limit 5
                                      :tls {:ca ca :reject-unauthorized true :min-version "TLSv1.2"}
                                      :connection-timeout-ms 5000 :auth-method :login :name "ehlo.example.test"
                                      :require-tls? true})]
    (is (true? (get w "pool")))
    (is (= 2 (get w "maxConnections")))
    (is (= 100 (get w "maxMessages")) "pool default")
    (is (= 5 (get w "rateLimit")))
    (is (= {"ca" ca "rejectUnauthorized" true "minVersion" "TLSv1.2"} (get w "tls")) "tls keys camelised, CA stays a Secret")
    (is (= 5000 (get w "connectionTimeout")))
    (is (= "LOGIN" (get w "authMethod")))
    (is (= "ehlo.example.test" (get w "name")))
    (is (true? (get w "requireTLS")))))

(deftest config-from-env-uses-the-house-keys
  (let [cfg (util/env-config (util/parse-secrets-file tu/fake-secrets-text) "BORRET_EMAIL")
        opts (smtp/config-from-env cfg)]
    (is (= "mail.example.test" (:host opts)))
    (is (= 465 (:port opts)))
    (is (true? (:secure opts)))
    (is (util/secret? (get-in opts [:auth :user])))
    (is (= tu/fake-password (util/reveal (get-in opts [:auth :pass]))))
    (is (= :missing-credentials (:reason (ex-data (tu/thrown #(smtp/config-from-env {}))))))
    (is (= :secrets-file-incomplete (:reason (ex-data (tu/thrown #(smtp/config-from-env {:username (util/secret "u") :password (util/secret "p")}))))))))

(deftest smtp-client-from-env-without-credentials-is-a-config-error
  (let [e (tu/thrown #(smtp/smtp-client-from-env "EMAIL_CLJS_TEST_NOBODY_SETS_THIS_"
                                                  {:secrets-file "/nonexistent/email-cljs-none.txt"}))]
    (is (= {} (util/from-env "/nonexistent/email-cljs-none.txt" "EMAIL_CLJS_TEST_NOBODY_SETS_THIS_")))
    (is (= :email-cljs/config-error (util/error-type e)) (pr-str e))
    (is (= :missing-credentials (:reason (ex-data e))))))

;; ---------------------------------------------------------------------------
;; message translation (pure)
;; ---------------------------------------------------------------------------

(deftest message->wire-translates-the-common-fields
  (let [w (smtp/message->wire {:from {:name "Doe, John" :address "john@example.test"}
                               :to ["a@example.test" {:name "Bé" :address "b@example.test"}]
                               :cc "c@example.test"
                               :bcc [{:address "hidden@example.test"}]
                               :reply-to "r@example.test"
                               :subject "Ünïcode ✓" :text "plain" :html "<p>hi</p>"
                               :priority :high
                               :in-reply-to "id@x" :references ["<a@x>" "b@x"]
                               :message-id "mine@x"
                               :headers {"X-Custom-Header" "v1" :x-lower ["a" "b"]}
                               :envelope {:from "bounce@example.test" :to ["a@example.test"]}
                               :encoding :quoted-printable :text-encoding :base64 :x-mailer false})]
    (is (= {"name" "Doe, John" "address" "john@example.test"} (get w "from")))
    (is (= [{"name" nil "address" "a@example.test"} {"name" "Bé" "address" "b@example.test"}] (get w "to")))
    (is (= [{"name" nil "address" "c@example.test"}] (get w "cc")))
    (is (= [{"name" nil "address" "hidden@example.test"}] (get w "bcc")))
    (is (= [{"name" nil "address" "r@example.test"}] (get w "replyTo")))
    (is (= "high" (get w "priority")) "enum keyword rendered")
    (is (= "<id@x>" (get w "inReplyTo")) "Message-ID brackets normalised")
    (is (= ["<a@x>" "<b@x>"] (get w "references")))
    (is (= "<mine@x>" (get w "messageId")))
    (is (= {"X-Custom-Header" "v1" :x-lower ["a" "b"]} (get w "headers")) "headers verbatim")
    (is (= {:from "bounce@example.test" :to ["a@example.test"]} (get w "envelope")) "envelope verbatim")
    (is (= "quoted-printable" (get w "encoding")))
    (is (= "base64" (get w "textEncoding")))
    (is (false? (get w "xMailer")))
    (is (every? string? (keys w)) "wire keys are strings")))

(deftest message->wire-address-string-forms
  (let [w (smtp/message->wire {:from "\"Doe, John\" <j@x.test>" :to "Plain <p@x.test>" :cc "bare@x.test"})]
    (is (= {"name" "Doe, John" "address" "j@x.test"} (get w "from")) "quoted display name unquoted")
    (is (= [{"name" "Plain" "address" "p@x.test"}] (get w "to")))
    (is (= [{"name" nil "address" "bare@x.test"}] (get w "cc"))))
  (is (= ["a@x.test, b@x.test"] (get (smtp/message->wire {:to "a@x.test, b@x.test"}) "to"))
      "a list string passes verbatim for nodemailer's parser")
  (is (= :bad-address (:reason (ex-data (tu/thrown #(smtp/message->wire {:to [{:name "no address"}]}))))))
  (is (= :bad-address (:reason (ex-data (tu/thrown #(smtp/message->wire {:to 42})))))))

(deftest message->wire-attachments
  (let [png (att/from-base64 "iVBORw0KGgo=")
        w (smtp/message->wire {:to "a@x.test"
                               :attachments [{:filename "a.txt" :content "hello" :content-type "text/plain"}
                                             {:filename "logo.png" :content "iVBORw0KGgo=" :encoding :base64 :cid "logo@x" :content-disposition :inline}
                                             {:filename "big.bin" :bytes png}
                                             {:filename "raw.bin" :content png :content-transfer-encoding :base64 :headers {"X-Part" "1"}}]
                               :alternatives [{:content-type "text/x-custom" :content "alt"}]})
        [a b c d] (get w "attachments")]
    (is (= {"filename" "a.txt" "content" "hello" "contentType" "text/plain"} a))
    (is (= {"filename" "logo.png" "content" "iVBORw0KGgo=" "encoding" "base64" "cid" "logo@x" "contentDisposition" "inline"} b))
    (is (= png (get c "content")) ":bytes becomes content Bytes")
    (is (nil? (get c "bytes")))
    (is (= png (get d "content")) "Bytes content stays wrapped")
    (is (= "base64" (get d "contentTransferEncoding")))
    (is (= {"X-Part" "1"} (get d "headers")))
    (is (= [{"contentType" "text/x-custom" "content" "alt"}] (get w "alternatives")))))

(deftest message->wire-content-source-policy
  (is (= :file-access-disabled (:reason (ex-data (tu/thrown #(smtp/message->wire {:to "a@x.test" :attachments [{:path "/etc/hostname"}]}))))))
  (is (= :url-access-disabled (:reason (ex-data (tu/thrown #(smtp/message->wire {:to "a@x.test" :attachments [{:href "https://x.test/f"}]}))))))
  (is (= [{"path" "/tmp/f" "filename" "f"}]
         (get (smtp/message->wire {:to "a@x.test" :attachments [{:path "/tmp/f" :filename "f"}]} {:allow-file-access? true}) "attachments"))
      "allowed by the client policy")
  (is (= :attachment-source (:reason (ex-data (tu/thrown #(smtp/message->wire {:to "a@x.test" :attachments [{:content "a" :path "/x"}]}))))) "two sources")
  (is (= :attachment-source (:reason (ex-data (tu/thrown #(smtp/message->wire {:to "a@x.test" :attachments [{:filename "none"}]}))))) "no source")
  (is (= :bad-attachment (:reason (ex-data (tu/thrown #(smtp/message->wire {:to "a@x.test" :attachments ["not-a-map"]})))))))

(deftest message->wire-validation
  (is (= :no-recipients (:reason (ex-data (tu/thrown #(smtp/message->wire {:from "a@x.test" :subject "s"}))))))
  (is (some? (smtp/message->wire {:envelope {:to ["a@x.test"]} :raw "From: a@x.test\r\n\r\nhi"})) "envelope recipients suffice")
  (is (= :raw-needs-envelope (:reason (ex-data (tu/thrown #(smtp/message->wire {:to "a@x.test" :raw "raw"}))))))
  (is (= :unknown-message-key (:reason (ex-data (tu/thrown #(smtp/message->wire {:to "a@x.test" :subjetc "typo"}))))))
  (is (= :bad-priority (:reason (ex-data (tu/thrown #(smtp/message->wire {:to "a@x.test" :priority :urgent}))))))
  (is (= :bad-message (:reason (ex-data (tu/thrown #(smtp/message->wire "nope"))))))
  (is (= :email-cljs/argument-error (util/error-type (tu/thrown #(smtp/message->wire {}))))))

;; ---------------------------------------------------------------------------
;; error taxonomy (pure)
;; ---------------------------------------------------------------------------

(deftest error-map->ex-classifies-nodemailer-codes
  (let [ex (fn [m] (smtp/error-map->ex m))
        t (fn [m] (util/error-type (ex m)))
        d (fn [m] (util/error-data (ex m)))]
    (is (= :email-cljs/auth-error (t {:code "EAUTH" :response-code 535 :command "AUTH PLAIN" :message "Invalid login"})))
    (is (= :smtp (:protocol (d {:code "EAUTH" :response-code 535}))))
    (is (= "EAUTH" (:nodemailer-code (d {:code "EAUTH"}))) "nodemailer kind recorded")
    (is (true? (:retryable? (d {:code "EAUTH" :response-code 454}))) "454 temporary auth failure")
    (is (= :email-cljs/config-error (t {:code "ENOAUTH"})))
    (is (= :missing-credentials (:reason (d {:code "ENOAUTH"}))))
    (is (= :email-cljs/network-error (t {:code "ECONNECTION"})))
    (is (true? (:retryable? (d {:code "ETIMEDOUT"}))))
    (is (true? (:retryable? (d {:code "ESOCKET"}))))
    (is (false? (:retryable? (d {:code "EDNS"}))) "DNS: fix the config")
    (is (true? (:tls? (d {:code "ETLS"}))))
    (is (true? (:protocol-violation? (d {:code "EPROTOCOL"}))))
    (is (= :email-cljs/smtp-error (t {:code "EENVELOPE" :response-code 550 :rejected ["x@y"]})))
    (is (false? (:retryable? (d {:code "EENVELOPE" :response-code 550}))))
    (is (true? (:retryable? (d {:code "EENVELOPE" :response-code 451}))) "greylisting is transient")
    (is (= ["x@y"] (:rejected (d {:code "EENVELOPE" :rejected ["x@y"]}))))
    (is (= :email-cljs/smtp-error (t {:code "EMESSAGE" :response-code 552})))
    (is (= :email-cljs/argument-error (t {:code "EMAXRECIPIENTS"})))
    (is (= :content-source (:reason (d {:code "EFILEACCESS"}))))
    (is (= :email-cljs/config-error (t {:code "ECONFIG"})))
    (is (= :email-cljs/network-error (t {:message "Connection pool was closed"})))
    (is (= :email-cljs/smtp-error (t {:code "EWHATEVER" :message "?"})) "unknown codes stay SMTP errors")
    (is (= "EWHATEVER" (:code (d {:code "EWHATEVER"}))))
    (let [already (util/parse-error "x")]
      (is (identical? already (ex already)) "ex-info passes through"))))

;; ---------------------------------------------------------------------------
;; through the fake transport (real nodemailer normalisation, no network)
;; ---------------------------------------------------------------------------

(defn- client-with
  ([] (client-with nil))
  ([fake-opts]
   (let [fake (tu/fake-transport fake-opts)]
     [(smtp/smtp-client {:transport (:plugin fake) :auth auth}) fake])))

(deftest smtp-client-map-is-safe-to-print
  (let [[c _] (client-with)]
    (is (smtp/client? c))
    (is (some? (:transport c)))
    (is (not (str/includes? (pr-str c) tu/fake-password)) "client map prints redacted")
    (is (util/secret? (get-in c [:config :auth :pass])))
    (is (false? (:pool? c)))
    (is (= {:allow-file-access? false :allow-url-access? false} (:policy c)))))

(deftest send!-translates-and-resolves
  (async done
    (let [[c fake] (client-with)
          png (att/from-base64 "iVBORw0KGgo=")]
      (tu/finish done
        (p/let [r (smtp/send! c {:from {:name "Doe, John" :address "j@x.test"}
                                 :to ["a@x.test" {:name "Bé" :address "b@x.test"}]
                                 :bcc "hidden@x.test"
                                 :subject "s" :text "body text" :html "<p>hi</p>"
                                 :priority :high
                                 :headers {"X-Custom" "v"}
                                 :attachments [{:filename "a.txt" :content "hello" :content-type "text/plain"}
                                               {:filename "logo.png" :bytes png :cid "logo@x"}
                                               {:filename "b.txt" :content "aGk=" :encoding :base64}]})
                m (first @(:messages fake))]
          (testing "result"
            (is (= ["a@x.test" "b@x.test" "hidden@x.test"] (:accepted r)))
            (is (= [] (:rejected r)))
            (is (true? (:all-accepted? r)))
            (is (str/starts-with? (:message-id r) "<"))
            (is (str/ends-with? (:message-id r) "@x.test>") "Message-ID generated from the From domain")
            (is (= "j@x.test" (get-in r [:envelope :from])))
            (is (= ["a@x.test" "b@x.test" "hidden@x.test"] (get-in r [:envelope :to])) "bcc rides only in the envelope")
            (is (= 250 (:response-code r)))
            (is (= "250 2.0.0 Ok: queued as FAKE1" (:response r)))
            (is (vector? (:ehlo r))))
          (testing "what nodemailer normalised"
            (is (= {:name "Doe, John" :address "j@x.test"} (:from m)))
            (is (= [{:address "a@x.test" :name ""} {:name "Bé" :address "b@x.test"}] (:to m)))
            (is (= [{:address "hidden@x.test" :name ""}] (:bcc m)))
            (is (= "high" (:priority m)))
            (is (= {:X-Custom "v"} (:headers m)))
            (is (= "body text" (:text m)))
            (is (true? (:disableFileAccess m)) "sandbox reaches a plugin transport through defaults")
            (let [[a b c] (:attachments m)]
              (is (= "text/plain" (:contentType a)))
              (is (nil? (:encoding a)) "string content stays a plain string")
              (is (= "hello" (:content a)))
              (is (= {:filename "logo.png" :cid "logo@x" :content "iVBORw0KGgo=" :encoding "base64" :contentType "image/png"} b)
                  "Bytes travel as a Buffer and come back base64; type from the filename")
              (is (= "aGk=" (:content c))))))))))

(deftest send!-partial-rejection-resolves-unless-strict
  (async done
    (let [[c _] (client-with {:rejected ["bad@x.test"]
                              :rejected-errors [{:message "550 5.1.1 no such user" :code "EENVELOPE" :recipient "bad@x.test" :response "550 5.1.1 no such user" :responseCode 550 :command "RCPT TO"}]})
          msg {:from "j@x.test" :to ["good@x.test" "bad@x.test"] :text "t"}]
      (tu/finish done
        (p/let [r (smtp/send! c msg)
                e (tu/rejection (smtp/send! c msg {:strict? true}))]
          (is (= ["good@x.test"] (:accepted r)))
          (is (= ["bad@x.test"] (:rejected r)))
          (is (false? (:all-accepted? r)))
          (is (= 550 (:response-code (first (:rejected-errors r)))))
          (is (= "bad@x.test" (:recipient (first (:rejected-errors r)))))
          (is (= :email-cljs/smtp-error (util/error-type e)))
          (is (= :partial-rejection (:reason (ex-data e))))
          (is (= ["bad@x.test"] (:rejected (ex-data e))))
          (is (= ["good@x.test"] (:accepted (ex-data e))))
          (is (false? (util/retryable? e))))))))

(deftest send!-maps-transport-errors
  (async done
    (let [send-with (fn [err] (let [[c _] (client-with {:error err})]
                                (tu/rejection (smtp/send! c {:from "j@x.test" :to "a@x.test" :text "t"}))))]
      (tu/finish done
        (p/let [auth (send-with {:message "Invalid login: 535 5.7.8 Error: authentication failed" :code "EAUTH" :response "535 5.7.8 Error: authentication failed" :responseCode 535 :command "AUTH PLAIN"})
                conn (send-with {:message "connect ECONNREFUSED" :code "ECONNECTION" :command "CONN"})
                tmo (send-with {:message "Greeting never received" :code "ETIMEDOUT" :command "CONN"})
                dns (send-with {:message "getaddrinfo ENOTFOUND" :code "EDNS" :command "CONN"})
                tls (send-with {:message "self signed certificate" :code "ETLS" :command "CONN"})
                env (send-with {:message "Can't send mail - all recipients were rejected: 550 5.1.1" :code "EENVELOPE" :response "550 5.1.1 no such user" :responseCode 550 :command "RCPT TO" :rejected ["a@x.test"]})]
          (is (= :email-cljs/auth-error (util/error-type auth)))
          (is (= 535 (:response-code (ex-data auth))))
          (is (= "AUTH PLAIN" (:command (ex-data auth))))
          (is (= "535 5.7.8 Error: authentication failed" (:response (ex-data auth))))
          (is (not (str/includes? (pr-str (ex-data auth)) tu/fake-password)))
          (is (= :email-cljs/network-error (util/error-type conn)))
          (is (util/retryable? conn))
          (is (= "ECONNECTION" (:code (ex-data conn))))
          (is (util/retryable? tmo))
          (is (= :email-cljs/network-error (util/error-type dns)))
          (is (not (util/retryable? dns)))
          (is (true? (:tls? (ex-data tls))))
          (is (= :email-cljs/smtp-error (util/error-type env)))
          (is (= ["a@x.test"] (:rejected (ex-data env))))
          (is (= 550 (:response-code (ex-data env))))
          (is (not (util/retryable? env))))))))

(deftest send!-rejects-bad-messages-before-touching-the-transport
  (async done
    (let [[c fake] (client-with)]
      (tu/finish done
        (p/let [e (tu/rejection (smtp/send! c {:subject "no recipients"}))
                e2 (tu/rejection (smtp/send! c {:to "a@x.test" :attachments [{:path "/etc/passwd"}]}))]
          (is (= :email-cljs/argument-error (util/error-type e)))
          (is (= :no-recipients (:reason (ex-data e))))
          (is (= :file-access-disabled (:reason (ex-data e2))))
          (is (= [] @(:calls fake)) "transport never called"))))))

(deftest verify-and-close-through-the-transport
  (async done
    (let [[c fake] (client-with)
          [bad _] (client-with {:verify-error {:message "Invalid login" :code "EAUTH" :responseCode 535 :response "535 5.7.8 nope" :command "AUTH PLAIN"}})]
      (tu/finish done
        (p/let [ok (smtp/verify-smtp! c)
                closed (smtp/close-smtp! c)
                e (tu/rejection (smtp/verify-smtp! bad))]
          (is (true? ok))
          (is (true? closed))
          (is (= [:verify :close] @(:calls fake)))
          (is (= :email-cljs/auth-error (util/error-type e)))
          (is (= "535 5.7.8 nope" (:response (ex-data e)))))))))
