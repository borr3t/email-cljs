(ns email-cljs.util-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [email-cljs.test-util :as tu]
            [email-cljs.util :as util]))

(deftest kebab-camel-roundtrip
  (is (= "contentType" (util/kebab->camel :content-type)))
  (is (= "bodyStructure" (util/kebab->camel :body-structure)))
  (is (= "uid" (util/kebab->camel :uid)) "single word unchanged")
  (is (= "alreadyCamel" (util/kebab->camel "alreadyCamel")) "strings pass through")
  (is (= :content-type (util/camel->kebab "contentType")))
  (is (= :internal-date (util/camel->kebab :internalDate)))
  (is (= :high-water-mark (util/camel->kebab "highWaterMark")))
  (is (= :content-type (util/camel->kebab "content-type")) "mailparser header names only lower-cased")
  (is (= :email-cljs/x (util/camel->kebab :email-cljs/x)) "namespaced keywords untouched")
  (doseq [k [:content-disposition :envelope-from :reply-to :in-reply-to :message-id :x-mailer]]
    (is (= k (util/camel->kebab (util/kebab->camel k))) (str "roundtrip " k))))

(deftest transform-keys-honours-passthrough
  (is (= {:a-b {:c-d 1} :headers {:xMailer 2}}
         (util/transform-keys util/camel->kebab #{:headers} {:aB {:cD 1} :headers {:xMailer 2}})))
  (is (= [{:a-b 1}] (util/transform-keys util/camel->kebab [{:aB 1}]))))

(deftest ->camel-keys-drops-nil-and-keeps-user-blobs
  (let [m (util/->camel-keys {:content-type "text/plain"
                              :nil-key nil
                              :headers {"X-Custom-Header" "v" :another-one "w"}
                              :attachments [{:file-name "a.txt" :content "raw"}]
                              :tls {:reject-unauthorized false}})]
    (is (= #{"contentType" "headers" "attachments" "tls"} (set (keys m))) "nil dropped, keys camelised")
    (is (= {"X-Custom-Header" "v" :another-one "w"} (get m "headers")) "headers verbatim")
    (is (= {:reject-unauthorized false} (get m "tls")) "tls verbatim (nodemailer/imapflow own that shape)")
    (is (= [{"fileName" "a.txt" "content" "raw"}] (get m "attachments")))
    (is (nil? (util/->camel-keys nil)))))

(deftest ->kebab-keys-translates-results
  (is (= {:content-type "text/html" :attachments [{:file-name "a" :content-id "c"}]}
         (util/->kebab-keys {:contentType "text/html" :attachments [{:fileName "a" :contentId "c"}]})))
  (is (= {:headers {:xMailer 1}} (util/->kebab-keys #{:headers} {:headers {:xMailer 1}}))))

(deftest secret-is-opaque
  (let [s (util/secret tu/fake-password)]
    (is (util/secret? s))
    (is (= tu/fake-password (util/reveal s)))
    (is (= "#email-cljs/secret <redacted>" (pr-str s)))
    (is (= "<redacted>" (str s)))
    (is (not (str/includes? (pr-str {:auth {:pass s}}) tu/fake-password)) "nested printing never leaks")
    (is (identical? s (util/secret s)) "idempotent")
    (is (= s (util/secret tu/fake-password)) "value equality")
    (is (nil? (util/secret nil)))
    (is (not (util/secret? tu/fake-password)))))

(deftest clean-secret-keeps-punctuation
  (is (= "p@ss!" (util/clean-secret "  p@ss!  ")))
  (is (= "p@ss!" (util/clean-secret "\"p@ss!\"")))
  (is (= "p@ss!" (util/clean-secret "'p@ss!'")))
  (is (= "--lead-and-trail--" (util/clean-secret "--lead-and-trail--")) "no decoration stripping for passwords")
  (is (= "\"" (util/clean-secret "\"")) "a lone quote is not a quote pair")
  (is (nil? (util/clean-secret "   ")))
  (is (nil? (util/clean-secret nil)))
  (is (nil? (util/clean-secret 42))))

(deftest redact-config-wraps-secret-keys
  (let [c (util/redact-config {:host "h" :auth {:user "u" :pass "pw"} :password "pw2"})]
    (is (util/secret? (get-in c [:auth :pass])))
    (is (util/secret? (:password c)))
    (is (= "u" (get-in c [:auth :user])))
    (is (not (str/includes? (pr-str c) "pw")))
    (is (= c (util/redact-config c)) "idempotent")
    (is (nil? (util/redact-config nil)))))

(deftest parse-secrets-file-reads-the-house-layout
  (let [m (util/parse-secrets-file tu/fake-secrets-text)]
    (is (= "someone@example.test" (get m "BORRET_EMAIL_USERNAME")))
    (is (= tu/fake-password (get m "BORRET_EMAIL_PASSWORD")) "quotes stripped")
    (is (= "465" (get m "BORRET_EMAIL_SMTP_PORT")))
    (is (= "true" (get m "BORRET_EMAIL_IMAP_TLS")))
    (is (= 8 (count m)) "comment ignored")
    (is (= {"A" "1"} (util/parse-secrets-file "export A = 1\nnot a pair\n\n# c")))
    (is (= {} (util/parse-secrets-file "")))))

(deftest parse-bool-accepts-common-spellings
  (is (true? (util/parse-bool "true")))
  (is (true? (util/parse-bool " YES ")))
  (is (true? (util/parse-bool "1")))
  (is (false? (util/parse-bool "false")))
  (is (false? (util/parse-bool "0")))
  (is (true? (util/parse-bool true)))
  (is (nil? (util/parse-bool "maybe")))
  (is (nil? (util/parse-bool nil))))

(deftest smtp-code-classification
  (is (= :success (util/smtp-code-class 250)))
  (is (= :success (util/smtp-code-class 235)) "auth ok")
  (is (= :intermediate (util/smtp-code-class 354)))
  (is (= :transient (util/smtp-code-class 421)))
  (is (= :transient (util/smtp-code-class "450")))
  (is (= :permanent (util/smtp-code-class 550)))
  (is (= :permanent (util/smtp-code-class 535)) "auth failed")
  (is (nil? (util/smtp-code-class 42)))
  (is (nil? (util/smtp-code-class "abc")))
  (is (nil? (util/smtp-code-class nil)))
  (is (util/smtp-retryable? 451))
  (is (not (util/smtp-retryable? 550))))

(deftest error-constructors-and-detection
  (let [cfg (util/config-error "no password" {:reason :missing-credentials})
        arg (util/argument-error "bad" {})
        auth (util/auth-error :smtp "535 5.7.8 rejected" {:code "EAUTH" :response-code 535})
        net (util/network-error :imap "ETIMEDOUT" {:code "ETIMEDOUT" :retryable? true})
        smtp (util/smtp-error "recipient rejected" {:code "EENVELOPE" :response-code 550
                                                     :rejected ["x@y.z"] :accepted []})
        smtp4 (util/smtp-error "greylisted" {:response-code 451})
        imap (util/imap-error "NO [TRYCREATE]" {:command "SELECT" :response-status "NO"})
        prs (util/parse-error "truncated" {})]
    (is (= :email-cljs/config-error (util/error-type cfg)))
    (is (= :missing-credentials (:reason (ex-data cfg))))
    (is (str/starts-with? (ex-message cfg) "email-cljs: "))
    (is (= :email-cljs/argument-error (util/error-type arg)))
    (is (= :email-cljs/auth-error (util/error-type auth)))
    (is (= :smtp (:protocol (util/error-data auth))))
    (is (= "email-cljs smtp authentication failed: 535 5.7.8 rejected" (ex-message auth)))
    (is (= :email-cljs/network-error (util/error-type net)))
    (is (util/retryable? net))
    (is (= :email-cljs/smtp-error (util/error-type smtp)))
    (is (= ["x@y.z"] (:rejected (util/error-data smtp))))
    (is (false? (:retryable? (util/error-data smtp))) "5xx derived not retryable")
    (is (true? (:retryable? (util/error-data smtp4))) "4xx derived retryable")
    (is (util/retryable? smtp4))
    (is (= :email-cljs/imap-error (util/error-type imap)))
    (is (= "NO" (:response-status (util/error-data imap))))
    (is (= :email-cljs/parse-error (util/error-type prs)))
    (is (util/error? prs))
    (is (not (util/retryable? prs)))
    (is (nil? (util/error-data (ex-info "other" {:type :something/else}))))
    (is (nil? (util/error-data (js/Error. "plain"))))
    (is (not (util/error? (js/Error. "plain"))))
    (is (nil? (tu/thrown #(util/secret "x"))))
    (is (some? (tu/thrown #(throw (util/parse-error "boom")))))))
