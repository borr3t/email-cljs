(ns email-cljs.message-test
  "message/build (MailComposer, no transport) -> message/parse-message!
   (mailparser) round trips, network-free, plus the pure helpers."
  (:require [cljs.test :refer [deftest is testing async]]
            [clojure.string :as str]
            [email-cljs.attachment :as att]
            [email-cljs.message :as msg]
            [email-cljs.test-util :as tu]
            [email-cljs.util :as util]
            [promesa.core :as p]))

(def png-base64
  "A valid 1x1 PNG (70 bytes)."
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==")

(def full-message
  {:from {:name "Doe, John" :address "john@example.test"}
   :to ["a@example.test" {:name "Bé" :address "b@example.test"}]
   :cc "c@example.test"
   :bcc "hidden@example.test"
   :reply-to "Reply <r@example.test>"
   :subject "Ünïcode ✓ subject"
   :text "plain ünïcode body\nline two"
   :html "<p>hi <b>there</b> <img src=\"cid:logo@x\"></p>"
   :date #inst "2026-09-24T23:08:56.000Z"
   :message-id "mine@example.test"
   :in-reply-to "<parent@example.test>"
   :references ["<a@example.test>" "b@example.test"]
   :priority :high
   :headers {"X-Custom-Header" "v1" "X-Lower" ["a" "b"]}
   :attachments [{:filename "a.txt" :content "hello" :content-type "text/plain"}
                 {:filename "logo.png" :bytes (att/from-base64 png-base64) :cid "logo@x"}
                 {:filename "big.bin" :bytes (att/bytes [0 1 2 255])}]})

;; ---------------------------------------------------------------------------
;; pure
;; ---------------------------------------------------------------------------

(deftest canonical-header-names-and-lookup
  (is (= "X-Custom-Header" (msg/canonical-header-name "x-custom-header")))
  (is (= "Message-ID" (msg/canonical-header-name :message-id)))
  (is (= "Content-Type" (msg/canonical-header-name "CONTENT-TYPE")))
  (is (= "MIME-Version" (msg/canonical-header-name "mime-version")))
  (let [parsed {:headers {"Subject" "s" "X-Custom-Header" "v"}}]
    (is (= "s" (msg/header parsed "subject")))
    (is (= "v" (msg/header parsed :X-CUSTOM-HEADER)))
    (is (nil? (msg/header parsed "nope")))))

(deftest parse-options-translate-and-default
  (is (= {"keepCidLinks" true "checksumAlgo" "md5"} (msg/parse-options->wire nil))
      "cid links kept by default")
  (is (= {"keepCidLinks" false "skipHtmlToText" true "maxHtmlLengthToParse" 10 "checksumAlgo" "sha256"}
         (msg/parse-options->wire {:keep-cid-links? false :skip-html-to-text? true :max-html-length 10 :checksum-algo :sha256})))
  (is (= :unknown-option (:reason (ex-data (tu/thrown #(msg/parse-options->wire {:keep-cid? true})))))))

(deftest normalize-parsed-shapes
  (let [raw {:headers {"from" {:value [{:address "j@x.test" :name "Doe, John"}] :text "" :html ""}
                       "to" {:value [{:name "Team" :group [{:address "a@x.test" :name ""} {:address "q@x.test" :name "Q"}]}]}
                       "subject" "s"
                       "date" "2026-09-24T23:08:56.000Z"
                       "content-type" {:value "text/plain" :params {"charset" "utf-8"}}
                       "x-lower" ["a" "b"]
                       "references" "<one@x>"}
             :header-lines [{:key "from" :line "From: \"Doe, John\" <j@x.test>"}
                            {:key "date" :line "Date: Thu, 24 Sep 2026 23:08:56 +0000"}
                            {:key "x-lower" :line "X-Lower: a"} {:key "x-lower" :line "X-Lower: b"}]
             :from {:value [{:address "j@x.test" :name "Doe, John"}]}
             :to {:value [{:name "Team" :group [{:address "a@x.test" :name ""} {:address "q@x.test" :name "Q"}]}]}
             :subject "s" :text "t" :references "<one@x>" :priority "high"
             :attachments [{:content (tu/buffer-of "hi") :content-type "text/plain" :part-id "2"
                            :content-disposition "attachment" :filename "a.txt" :checksum "abc" :size 2
                            :headers {"content-type" {:value "text/plain" :params {"charset" "latin1"}}}}]}
        m (msg/normalize-parsed raw :md5)]
    (is (= {:name "Doe, John" :address "j@x.test"} (:from m)))
    (is (= [{:name nil :address "a@x.test" :group "Team"} {:name "Q" :address "q@x.test" :group "Team"}] (:to m)) "groups flattened")
    (is (= [] (:cc m)))
    (is (nil? (:sender m)))
    (is (= "2026-09-24T23:08:56.000Z" (:date m)))
    (is (= "Thu, 24 Sep 2026 23:08:56 +0000" (:date-raw m)))
    (is (= ["<one@x>"] (:references m)) "single reference string becomes a vector")
    (is (= :high (:priority m)))
    (is (= ["From" "Date" "X-Lower"] (:header-keys m)) "wire order, canonical names, distinct")
    (is (= {:value "text/plain" :params {"charset" "utf-8"}} (msg/header m "content-type")) "params keep wire keys")
    (is (= ["a" "b"] (msg/header m "x-lower")))
    (is (= [{:name "Doe, John" :address "j@x.test"}] (get-in m [:headers "From"])) "address headers normalised inside :headers too")
    (let [[a] (:attachments m)]
      (is (= "a.txt" (:filename a)))
      (is (= :attachment (:content-disposition a)))
      (is (false? (:inline? a)))
      (is (false? (:related? a)))
      (is (att/bytes? (:content a)))
      (is (= :md5 (:checksum-algo a)))
      (is (= "latin1" (msg/attachment-charset a)))
      (is (= "hi" (msg/attachment->string a)))
      (is (= "aGk=" (msg/attachment->base64 a))))))

(deftest normalize-parsed-invalid-date-and-missing-fields
  (let [m (msg/normalize-parsed {:headers {"date" "2026-09-25T01:00:00.000Z"}
                                 :header-lines [{:key "date" :line "Date: garbage"}]
                                 :attachments []} :md5)]
    (is (nil? (:date m)) "mailparser's 'now' substitute is not reported as the message date")
    (is (= "garbage" (:date-raw m)))
    (is (= :normal (:priority m)))
    (is (= [] (:references m)))
    (is (= [] (:attachments m)))
    (is (nil? (:from m)))
    (is (nil? (msg/text-or-html m))))
  (let [m (msg/normalize-parsed {:headers {} :header-lines [] :attachments []} :md5)]
    (is (nil? (:date m)))
    (is (nil? (:date-raw m)))))

(deftest parsed-address-and-attachment-helpers
  (is (= "\"Doe, John\" <j@x.test>" (msg/format-parsed-address {:name "Doe, John" :address "j@x.test"})))
  (is (= "j@x.test" (msg/format-parsed-address {:name nil :address "j@x.test"})))
  (is (nil? (msg/format-parsed-address nil)))
  (is (= "a@x.test, B <b@x.test>" (msg/format-parsed-addresses [{:name nil :address "a@x.test"} {:name "B" :address "b@x.test"}])))
  (let [logo {:filename "logo.png" :cid "logo@x" :content-id "<logo@x>" :related? true :inline? true :part-id "1.2.2" :content (att/bytes "png")}
        file {:filename "a.txt" :cid nil :related? false :inline? false :part-id "2" :content (att/bytes "hello")}
        parsed {:text "  " :html "<p>x</p>" :attachments [logo file]}]
    (is (= logo (msg/attachment-of parsed "logo@x")))
    (is (= logo (msg/attachment-of parsed "<logo@x>")))
    (is (= file (msg/attachment-of parsed "a.txt")))
    (is (nil? (msg/attachment-of parsed "nope")))
    (is (= [logo] (msg/inline-attachments parsed)))
    (is (= [file] (msg/file-attachments parsed)))
    (is (= "<p>x</p>" (msg/text-or-html parsed)) "blank text falls through to html")))

(deftest build-wire-adds-priority-headers-and-policy
  (let [w (msg/build-wire {:to "a@x.test" :text "t" :priority :high :headers {"X-Custom" "v"}} {:newline :unix})]
    (is (= "unix" (get w "newline")))
    (is (true? (get w "disableFileAccess")))
    (is (true? (get w "disableUrlAccess")))
    (is (= {"X-Priority" "1 (Highest)" "X-MSMail-Priority" "High" "Importance" "High" "X-Custom" "v"} (get w "headers"))
        "mailer-level priority headers added; user headers kept"))
  (is (nil? (get (msg/build-wire {:to "a@x.test" :text "t"}) "headers")))
  (is (= :no-recipients (:reason (ex-data (tu/thrown #(msg/build-wire {:from "a@x.test" :subject "draft"}))))))
  (is (some? (msg/build-wire {:from "a@x.test" :subject "draft"} {:allow-no-recipients? true})) "drafts")
  (is (= :file-access-disabled (:reason (ex-data (tu/thrown #(msg/build-wire {:to "a@x.test" :attachments [{:path "/etc/hostname"}]}))))))
  (is (false? (get (msg/build-wire {:to "a@x.test" :attachments [{:path "/tmp/x"}]} {:policy {:allow-file-access? true}}) "disableFileAccess"))))

;; ---------------------------------------------------------------------------
;; build -> parse round trips (real MailComposer + mailparser, in process)
;; ---------------------------------------------------------------------------

(deftest build-then-parse-full-message
  (async done
    (tu/finish done
      (p/let [built (msg/build full-message)
              text (msg/build-string full-message)
              m (msg/parse-message! (:source built))]
        (testing "build result"
          (is (att/bytes? (:source built)))
          (is (= "<mine@example.test>" (:message-id built)))
          (is (= {:from "john@example.test" :to ["a@example.test" "b@example.test" "c@example.test" "hidden@example.test"]} (:envelope built))
              "bcc is in the envelope")
          (is (= (att/size (:source built)) (:size built)))
          (is (not (str/includes? text "Bcc:")) "the composer strips Bcc: by default (like the SMTP transport)")
          (is (str/includes? text "X-Priority: 1 (Highest)"))
          (is (not (str/includes? text "\r\n")) "build-string uses LF"))
        (testing "envelope-ish fields"
          (is (= "Ünïcode ✓ subject" (:subject m)) "RFC 2047 decoded")
          (is (= {:name "Doe, John" :address "john@example.test"} (:from m)) "quoted display name round-trips")
          (is (= [{:name "Doe, John" :address "john@example.test"}] (:from-all m)))
          (is (= [{:name nil :address "a@example.test"} {:name "Bé" :address "b@example.test"}] (:to m)))
          (is (= [{:name nil :address "c@example.test"}] (:cc m)))
          (is (= [] (:bcc m)) "no Bcc: in the built bytes")
          (is (= [{:name "Reply" :address "r@example.test"}] (:reply-to m)))
          (is (= "2026-09-24T23:08:56.000Z" (:date m)))
          (is (= "Thu, 24 Sep 2026 23:08:56 +0000" (:date-raw m)))
          (is (= "<mine@example.test>" (:message-id m)))
          (is (= "<parent@example.test>" (:in-reply-to m)))
          (is (= ["<a@example.test>" "<b@example.test>"] (:references m)) "brackets normalised on both sides")
          (is (= :high (:priority m))))
        (testing "bodies"
          (is (= "plain ünïcode body\nline two" (:text m)))
          (is (= "<p>hi <b>there</b> <img src=\"cid:logo@x\"></p>" (:html m)) "cid: link kept by default")
          (is (str/starts-with? (:text-as-html m) "<p>"))
          (is (= (:text m) (msg/text-or-html m))))
        (testing "headers"
          (is (= "v1" (msg/header m "x-custom-header")))
          (is (= ["a" "b"] (msg/header m "X-Lower")) "repeated header -> vector")
          (is (= "multipart/mixed" (:value (msg/header m :content-type))))
          (is (string? (get-in (msg/header m :content-type) [:params "boundary"])))
          (is (= "1.0" (msg/header m "mime-version")))
          (is (= ["X-Priority" "X-MSMail-Priority" "Importance" "X-Custom-Header" "X-Lower" "From" "To" "Cc" "Reply-To"
                  "In-Reply-To" "References" "Subject" "Message-ID" "Date" "MIME-Version" "Content-Type"]
                 (:header-keys m)))
          (is (= "high" (msg/header m "priority")) "mailparser folds the priority family into one 'priority' header")
          (is (nil? (msg/header m "x-priority")))
          (is (every? (fn [{:keys [key line]}] (and (string? key) (string? line))) (:header-lines m))))
        (testing "attachments"
          (is (= 3 (count (:attachments m))))
          (let [logo (msg/attachment-of m "logo@x")
                txt (msg/attachment-of m "a.txt")
                bin (msg/attachment-of m "big.bin")]
            (is (= "image/png" (:content-type logo)) "type from the filename")
            (is (= :inline (:content-disposition logo)) "image with a cid goes inline")
            (is (true? (:inline? logo)))
            (is (true? (:related? logo)) "inside multipart/related next to the html")
            (is (= "<logo@x>" (:content-id logo)))
            (is (= "1.2.2" (:part-id logo)) "mixed[ alternative[ text, related[ html, png ] ], ..]")
            (is (= 70 (:size logo)))
            (is (= png-base64 (msg/attachment->base64 logo)) "base64 in -> Bytes -> base64 out, byte-identical")
            (is (= (att/from-base64 png-base64) (:content logo)))
            (is (= :md5 (:checksum-algo logo)))
            (is (re-matches #"[0-9a-f]{32}" (:checksum logo)))
            (is (= "hello" (msg/attachment->string txt)))
            (is (= "text/plain" (:content-type txt)))
            (is (= :attachment (:content-disposition txt)))
            (is (= "2" (:part-id txt)))
            (is (= [0 1 2 255] (att/->ints (:content bin))))
            (is (= "application/octet-stream" (:content-type bin)))
            (is (= [logo] (msg/inline-attachments m)))
            (is (= [txt bin] (msg/file-attachments m)))
            (is (= "image/png" (:value (msg/header logo "content-type"))) "the part's own headers")))))))

(deftest parse-alternative-only-and-html-only
  (async done
    (tu/finish done
      (p/let [alt (msg/build-string {:from "a@x.test" :to "b@x.test" :subject "alt" :text "plain" :html "<p>rich</p>"})
              m (msg/parse-message! alt)
              h (msg/parse-message! "Content-Type: text/html\r\nSubject: only html\r\n\r\n<p>hi <b>there</b></p>")
              hs (msg/parse-message! "Content-Type: text/html\r\n\r\n<p>hi</p>" {:skip-html-to-text? true})]
        (is (= "multipart/alternative" (:value (msg/header m "content-type"))))
        (is (= "plain" (:text m)))
        (is (= "<p>rich</p>" (:html m)))
        (is (= [] (:attachments m)))
        (is (= "only html" (:subject h)))
        (is (= "hi there" (:text h)) "html-to-text fallback")
        (is (= "<p>hi <b>there</b></p>" (:html h)))
        (is (str/blank? (:text hs)) "fallback disabled")
        (is (nil? (:date h)))
        (is (nil? (:message-id h)))
        (is (= :normal (:priority h)))))))

(deftest parse-accepts-bytes-string-and-fetched-maps
  (async done
    (tu/finish done
      (p/let [built (msg/build {:from "a@x.test" :to "b@x.test" :subject "src" :text "t"})
              from-bytes (msg/parse-message! (:source built))
              from-string (msg/parse-message! (att/->string (:source built)))
              from-map (msg/parse-message! {:uid 7 :source (:source built)})
              bad (tu/rejection (msg/parse-message! 42))
              bad-opt (tu/rejection (msg/parse-message! "Subject: x\r\n\r\n" {:nope true}))]
        (is (= "src" (:subject from-bytes) (:subject from-string) (:subject from-map)))
        (is (= :bad-source (:reason (ex-data bad))))
        (is (= :email-cljs/argument-error (util/error-type bad)))
        (is (= :unknown-option (:reason (ex-data bad-opt))))))))

(deftest parse-drops-cid-links-when-asked-and-handles-bad-dates
  (async done
    (tu/finish done
      (p/let [text (msg/build-string {:from "a@x.test" :to "b@x.test" :html "<p><img src=\"cid:logo@x\"></p>"
                                      :attachments [{:filename "logo.png" :bytes (att/from-base64 png-base64) :cid "logo@x"}]})
              inlined (msg/parse-message! text {:keep-cid-links? false})
              kept (msg/parse-message! text)
              bad-date (msg/parse-message! "Date: not a date\r\nFrom: Team: a@x.test, \"Q, R\" <q@x.test>;\r\nSubject: =?UTF-8?Q?J=C3=BCrgen?=\r\n\r\nbody")]
        (is (str/includes? (:html inlined) "data:image/png;base64,") "cid rewritten to a data URI on request")
        (is (str/includes? (:html kept) "cid:logo@x"))
        (is (nil? (:date bad-date)))
        (is (= "not a date" (:date-raw bad-date)))
        (is (= "Jürgen" (:subject bad-date)))
        (is (= [{:name nil :address "a@x.test" :group "Team"} {:name "Q, R" :address "q@x.test" :group "Team"}] (:from-all bad-date))
            "group flattened, members tagged")
        (is (= {:name nil :address "a@x.test" :group "Team"} (:from bad-date)))))))

(deftest build-drafts-and-errors
  (async done
    (tu/finish done
      (p/let [draft (msg/build {:from "a@x.test" :subject "draft" :text "wip"} {:allow-no-recipients? true})
              e (tu/rejection (msg/build {:subject "nobody"}))
              parsed (msg/parse-message! (:source draft))
              with-bcc (msg/build-string {:from "a@x.test" :to "b@x.test" :bcc "h@x.test" :text "t"} {:keep-bcc? true})
              parsed-bcc (msg/parse-message! with-bcc)]
        (is (= {:from "a@x.test" :to []} (:envelope draft)))
        (is (= "draft" (:subject parsed)))
        (is (= [] (:to parsed)))
        (is (= :no-recipients (:reason (ex-data e))))
        (is (str/includes? with-bcc "Bcc: h@x.test") ":keep-bcc? writes the header")
        (is (= [{:name nil :address "h@x.test"}] (:bcc parsed-bcc)))))))

(deftest attachment-file-round-trip
  (async done
    (let [path "/tmp/email-cljs-test-attachment.bin"
          b (att/from-base64 png-base64)]
      (tu/finish done
        (p/let [written (att/write-file! path b)
                back (att/read-file! path)
                missing (tu/rejection (att/read-file! "/nonexistent/email-cljs/none.bin"))]
          (is (= path written))
          (is (= b back))
          (is (= png-base64 (att/->base64 back)))
          (is (= :file-unreadable (:reason (ex-data missing)))))))))
