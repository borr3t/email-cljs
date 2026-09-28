(ns email-cljs.smoke
  "Phase 5 LIVE smoke: real SMTP sends to the catch-all account and IMAP
   fetch-back over the library (never the packages directly). Not part of
   run-tests.sh.

     npx shadow-cljs compile smoke && node target/smoke.js

   Credentials: BORRET_EMAIL_USERNAME / _PASSWORD / _SMTP_HOST / _SMTP_PORT /
   _SMTP_TLS / _IMAP_HOST / _IMAP_PORT / _IMAP_TLS from the environment over
   ~/.secrets/borret.txt (util/from-env). The username and password are
   NEVER printed: every output line goes through `scrub`, which replaces
   both with <redacted> should they ever appear (the From/Bcc address of
   the smoke messages IS the account address, so scrubbing matters).

   Structure (house style of openrouter's smoke): numbered sections that
   never abort early — each prints `PASS <name> — <evidence>` or `FAIL
   <name>: <reason>` — then cleanup, the tally, the exit code, and the
   process-liveness assert (an UNREF'd 5 s timer after every close: when
   the library leaks a socket or timer the process stays alive, the timer
   fires and the run FAILs; when nothing leaks the process exits on its
   own before the timer and the last line is the tally).

   Sends exactly THREE messages per run (plain text with a Bcc, rich
   text+html+attachment, and the watch! trigger), all to
   smoke-test-<timestamp>@borret.ro (the account is a catch-all), and
   deletes them at the end (UID EXPUNGE when the server has UIDPLUS). No
   wrong-password probe (fail2ban on the box; inventory 13.3).

   Live behaviours that differ from the inventory are recorded in the
   final 'INVENTORY NOTES' block and in docs/email-api-inventory.org
   section 18."
  (:require [clojure.core.async :as async]
            [clojure.string :as str]
            [email-cljs.attachment :as att]
            [email-cljs.imap :as imap]
            [email-cljs.message :as msg]
            [email-cljs.parse-binding :as pb]
            [email-cljs.smtp :as smtp]
            [email-cljs.util :as util]
            [promesa.core :as p]))

;; ---------------------------------------------------------------------------
;; Bookkeeping
;; ---------------------------------------------------------------------------

(def env-prefix "BORRET_EMAIL_")
(def secrets-file
  "The library reads no secrets file unless one is named: the smoke names
   its own (EMAIL_SECRETS_FILE wins when set)."
  (or (get (pb/env-map) "EMAIL_SECRETS_FILE") "~/.secrets/borret.txt"))
(def domain "borret.ro")
(def smoke-mailbox "email-cljs-smoke")

(def results (atom []))
(def shared (atom {}))
(def notes (atom []))
(def scrubber (atom identity))

(defn scrub [s] (@scrubber (str s)))

(defn say [& xs]
  (println (scrub (str/join " " (map str xs)))))

(defn note!
  "Records a live observation for the INVENTORY NOTES block."
  [s]
  (swap! notes conj (scrub s))
  nil)

(defn error-brief
  [e]
  (let [d (or (util/error-data e) (ex-data e))]
    (if d
      (str (name (or (:type d) :error))
           (when (:reason d) (str " reason=" (pr-str (:reason d))))
           (when (:code d) (str " code=" (:code d)))
           (when (:response-code d) (str " response-code=" (:response-code d)))
           (when (:response-status d) (str " status=" (:response-status d)))
           (when (:response-text d) (str " text=" (pr-str (:response-text d))))
           (when (:command d) (str " command=" (:command d)))
           " — " (or (ex-message e) (:message d)))
      (or (ex-message e) (str e)))))

(defn section!
  [name f]
  (-> (p/do (f))
      (p/then (fn [detail]
                (say (str "PASS " name (when detail (str " — " detail))))
                (swap! results conj {:name name :ok? true})))
      (p/catch (fn [e]
                 (let [reason (error-brief e)]
                   (say (str "FAIL " name ": " reason))
                   (swap! results conj {:name name :ok? false :reason reason}))))))

(defn fail! [reason & [data]]
  (throw (ex-info reason (merge {:type :smoke/fail} data))))

(defn wait [ms] (p/delay ms nil))

(defn take-event!
  "Promise of the next event on `ch` whose :event is in `wanted` (a set;
   nil = any), or nil after `timeout-ms`; other events are recorded under
   :skipped in `shared`."
  [ch wanted timeout-ms]
  (p/create
   (fn [resolve _]
     (let [done? (atom false)
           finish (fn [v] (when-not @done? (reset! done? true) (resolve v)))
           timer (js/setTimeout #(finish nil) timeout-ms)]
       (letfn [(step []
                 (async/take! ch (fn [v]
                                (cond
                                  (nil? v) (do (js/clearTimeout timer) (finish nil))
                                  (or (nil? wanted) (contains? wanted (:event v))) (do (js/clearTimeout timer) (finish v))
                                  :else (do (swap! shared update :skipped (fnil conj []) v) (step))))))]
         (step))))))

(defn now-iso [] (.toISOString (js/Date.)))
(defn now-inst [] (js/Date.))

(defn utc-day
  "\"YYYY-MM-DD\" of `date` shifted by `days`."
  [date days]
  (subs (.toISOString (js/Date. (+ (.getTime date) (* days 86400000)))) 0 10))

(def png-base64
  "A valid 1x1 PNG (70 bytes)."
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==")

(defn message-id
  "A unique bracketed Message-ID for this run."
  [tag]
  (str "<smoke-" tag "-" (:stamp @shared) "@" domain ">"))

;; ---------------------------------------------------------------------------
;; IMAP helpers
;; ---------------------------------------------------------------------------

(defn poll-uid!
  "Polls INBOX (open) for the Message-ID every 2 s, up to ~30 s. Promise of
   {:uid n :waited-ms n :tries n} or nil."
  [c mid]
  (let [started (.now js/Date)]
    (p/loop [try 1]
      (p/let [uids (imap/search! c {:header {"Message-ID" mid}})]
        (cond
          (seq uids) {:uid (last uids) :uids uids :waited-ms (- (.now js/Date) started) :tries try}
          (>= try 15) nil
          :else (p/do (wait 2000) (p/recur (inc try))))))))

(defn require-uid!
  [c mid what]
  (p/let [r (poll-uid! c mid)]
    (when-not r (fail! (str what " did not reach INBOX within 30 s (Message-ID " mid ")")))
    (swap! shared update :uids (fnil conj []) (:uid r))
    r))

(defn find-part
  "Depth-first search of a body-structure tree for the node satisfying pred."
  [node pred]
  (if (pred node)
    node
    (some #(find-part % pred) (:child-nodes node))))

;; ---------------------------------------------------------------------------
;; Sections
;; ---------------------------------------------------------------------------

(defn s0-connect! []
  (let [{:keys [smtp imap]} @shared]
    (p/let [ok (smtp/verify-smtp! smtp)
            _ (imap/connect! imap)]
      (let [caps (imap/capabilities imap)
            tls (imap/tls-info imap)
            info (imap/server-info imap)]
        (swap! shared assoc :caps caps)
        (note! (str "13.2 capabilities: " (pr-str (sort (keys caps)))))
        (note! (str "13.5 IMAP tls-info: " (pr-str tls)))
        (note! (str "18.1 WITHIN advertised: " (pr-str (boolean (:within caps)))
                    " (imapflow sends :since/:before as YOUNGER/OLDER seconds on such servers)"))
        (note! (str "13.23 server ID: " (pr-str info)))
        (when-not (true? ok) (fail! "verify-smtp! did not resolve true"))
        (when-not (imap/connected? imap) (fail! "connect! resolved but connected? is false"))
        (when-not (:idle caps) (fail! (str "server lacks IDLE: " (pr-str (keys caps)))))
        (str "smtp verify=true | imap connected, capabilities=" (count caps) " "
             (pr-str (select-keys caps [:idle :uidplus :move :condstore :qresync :quota :special-use :appendlimit]))
             " tls-authorized=" (pr-str (:authorized tls))
             " server=" (pr-str (select-keys (or info {}) [:name :version :vendor])))))))

(defn s1-send-plain! []
  (let [{:keys [smtp to from]} @shared
        mid (message-id "plain")]
    (swap! shared assoc :mid-plain mid)
    (p/let [r (smtp/send! smtp {:from from :to to :bcc from
                                :subject (str "email-cljs smoke plain " (:stamp @shared))
                                :text (str "plain body " (:stamp @shared) "\nline two")
                                :message-id mid}
                          {:strict? true})]
      (note! (str "13.24 ehlo: " (pr-str (:ehlo r))))
      (when-not (some #{to} (:accepted r)) (fail! (str "recipient not in :accepted " (pr-str (:accepted r)))))
      (when-not (= mid (:message-id r)) (fail! (str "Message-ID not honoured: " (:message-id r))))
      (when-not (:all-accepted? r) (fail! (str "rejected " (pr-str (:rejected r)))))
      (str "accepted=" (count (:accepted r)) " (to + bcc) response=" (pr-str (:response r))
           " code=" (:response-code r) " size=" (:message-size r)
           " envelope-ms=" (:envelope-time-ms r) " message-ms=" (:message-time-ms r)
           " ehlo=" (count (:ehlo r))))))

(defn s2-round-trip! []
  (let [{:keys [imap mid-plain]} @shared]
    (p/let [mb (imap/mailbox-open! imap "INBOX")
            found (require-uid! imap mid-plain "plain message")
            uid (:uid found)
            m (imap/fetch-one! imap uid {:envelope true :flags true :internal-date true :size true :source? true :peek true})
            parsed (msg/parse-message! m)
            flags-before (:flags m)
            _ (imap/mark-seen! imap uid)
            after (imap/fetch-one! imap uid {:flags true})]
      (note! (str "13.1 send -> INBOX latency: " (:waited-ms found) " ms (" (:tries found) " poll(s) at 2 s)"))
      (note! (str "13.8 flags right after a :source fetch: " (pr-str flags-before) " (PEEK holds: no \\Seen)"))
      (note! (str "13.4 catch-all routing: message landed in INBOX (uid " uid ", exists " (:exists mb) ")"))
      (when (contains? flags-before "\\Seen") (fail! (str "\\Seen set by a PEEK fetch: " (pr-str flags-before))))
      (when-not (= (str "email-cljs smoke plain " (:stamp @shared)) (:subject parsed)) (fail! (str "subject mismatch: " (pr-str (:subject parsed)))))
      (when-not (= (str "email-cljs smoke plain " (:stamp @shared)) (get-in m [:envelope :subject])) (fail! "envelope subject mismatch"))
      (when-not (str/starts-with? (str (:text parsed)) (str "plain body " (:stamp @shared))) (fail! (str "text mismatch: " (pr-str (:text parsed)))))
      (when (seq (:bcc parsed)) (fail! (str "Bcc: header present in the delivered message: " (pr-str (:bcc parsed)))))
      (when (msg/header parsed "bcc") (fail! "Bcc header key present"))
      (when-not (contains? (:flags after) "\\Seen") (fail! (str "mark-seen! did not set \\Seen: " (pr-str (:flags after)))))
      (note! "13.20 Bcc: stripped by the SMTP transport — verified live (no Bcc header in the delivered message)")
      (str "uid=" uid " latency=" (:waited-ms found) "ms size=" (:size m) " internal-date=" (:internal-date m)
           " flags-after-peek=" (pr-str flags-before) " -> after mark-seen!=" (pr-str (:flags after))
           " bcc-header=absent text-ok=true date=" (:date parsed)))))

(defn s3-send-rich-and-attachment! []
  (let [{:keys [smtp imap to from]} @shared
        mid (message-id "rich")
        subject (str "email-cljs smoke rich ü✓ " (:stamp @shared))]
    (swap! shared assoc :mid-rich mid :subject-rich subject)
    (p/let [r (smtp/send! smtp {:from from :to to :subject subject
                                :text (str "rich text " (:stamp @shared))
                                :html (str "<p>rich <b>html</b> " (:stamp @shared) "</p>")
                                :message-id mid
                                :attachments [{:filename "pixel.png" :bytes (att/from-base64 png-base64) :content-type "image/png"}]}
                          {:strict? true})
            found (require-uid! imap mid "rich message")
            uid (:uid found)
            m (imap/fetch-one! imap uid {:body-structure true :flags true})
            part (find-part (:body-structure m) #(= "pixel.png" (get-in % [:disposition-parameters "filename"])))
            _ (when-not part (fail! (str "no attachment part in body-structure: " (pr-str (:body-structure m)))))
            bytes (imap/body-part! imap uid (:part part))
            dl (imap/download! imap uid (:part part))]
      (when-not (:all-accepted? r) (fail! "rich send rejected"))
      (when-not (= png-base64 (att/->base64 bytes)) (fail! (str "attachment bytes differ: " (att/size bytes) " bytes")))
      (when-not (= "image/png" (get-in dl [:meta :content-type])) (fail! (str "download meta: " (pr-str (:meta dl)))))
      (str "uid=" uid " latency=" (:waited-ms found) "ms part=" (:part part) " type=" (:type part) " encoding=" (:encoding part)
           " decoded=" (att/size bytes) " bytes base64-equal=true meta=" (pr-str (select-keys (:meta dl) [:content-type :filename :encoding :expected-size]))
           " root=" (get-in m [:body-structure :type])))))

(defn s4-parse-fetched-html! []
  (let [{:keys [imap mid-rich subject-rich]} @shared]
    (p/let [uids (imap/search! imap {:header {"Message-ID" mid-rich}})
            uid (last uids)
            _ (when-not uid (fail! "rich message not found"))
            src (imap/source! imap uid)
            parsed (msg/parse-message! src)
            m (imap/fetch-one! imap uid {:headers ["Subject" "Content-Type"] :envelope true})]
      (when-not (and (string? (:text parsed)) (str/includes? (:text parsed) "rich text")) (fail! (str ":text missing: " (pr-str (:text parsed)))))
      (when-not (and (string? (:html parsed)) (str/includes? (:html parsed) "<b>html</b>")) (fail! (str ":html missing: " (pr-str (:html parsed)))))
      (when-not (= subject-rich (:subject parsed)) (fail! (str "subject: " (pr-str (:subject parsed)))))
      (when-not (= subject-rich (get-in m [:envelope :subject])) (fail! (str "ENVELOPE subject not decoded: " (pr-str (get-in m [:envelope :subject])))))
      (when-not (= 1 (count (:attachments parsed))) (fail! (str "attachments: " (count (:attachments parsed)))))
      (when-not (= png-base64 (msg/attachment->base64 (first (:attachments parsed)))) (fail! "parsed attachment differs"))
      (str "uid=" uid " source=" (att/size src) " bytes content-type=" (:value (msg/header parsed :content-type))
           " text=" (pr-str (:text parsed)) " html=" (pr-str (:html parsed))
           " attachments=" (mapv (juxt :filename :content-type :size :part-id) (:attachments parsed))
           " headers-fetch=" (att/bytes? (:headers m))
           " received-count=" (count (let [v (msg/header parsed "received")] (if (string? v) [v] v)))))))

(defn s5-search-dsl! []
  (let [{:keys [imap mid-rich mid-plain subject-rich]} @shared
        now (now-inst)
        today (utc-day now 0) yesterday (utc-day now -1) tomorrow (utc-day now 1)
        with-mid (fn [q] (assoc q :header {"Message-ID" mid-rich}))]
    (p/let [since-today (imap/search! imap (with-mid {:since now}))
            since-yesterday (imap/search! imap (with-mid {:since yesterday}))
            since-tomorrow (imap/search! imap (with-mid {:since tomorrow}))
            on-today (imap/search! imap (with-mid {:on today}))
            before-tomorrow (imap/search! imap (with-mid {:before tomorrow}))
            unseen (imap/search! imap (with-mid {:unseen true}))
            seen-plain (imap/search! imap {:seen true :header {"Message-ID" mid-plain}})
            by-subject (imap/search! imap {:subject (str "smoke rich ü✓ " (:stamp @shared))})
            by-subject-ascii (imap/search! imap {:subject (str "smoke rich") :since yesterday})
            by-text (imap/search! imap {:text (str "rich text " (:stamp @shared))})
            or-q (imap/search! imap {:or [{:header {"Message-ID" mid-plain}} {:header {"Message-ID" mid-rich}}]})
            not-q (imap/search! imap {:not {:seen true} :header {"Message-ID" mid-rich}})
            esearch (imap/search! imap (with-mid {:all true}) {:return [:count :min :max]})
            m (imap/fetch-one! imap (last since-yesterday) {:internal-date true})]
      (note! (str "13.7 date searches (server receipt " (:internal-date m) "; WITHIN server -> YOUNGER/OLDER seconds, the future-day guard handles "
                  tomorrow "): since " today " (today UTC, #inst form) -> " (count since-today) " hit(s); since " yesterday " -> "
                  (count since-yesterday) "; since " tomorrow " -> " (count since-tomorrow) " (client-side, no round trip); on " today
                  " (calendar ON) -> " (count on-today) "; before " tomorrow " -> " (count before-tomorrow) " (criterion dropped client-side)"))
      (note! (str "13.17 non-ASCII SUBJECT search (ü✓): " (count by-subject) " hit(s); OR / NOT / ESEARCH: " (count or-q) " / " (count not-q) " / " (pr-str esearch)))
      (when-not (seq since-today) (fail! (str "SINCE today (" today ") did not match; internal-date " (:internal-date m))))
      (when-not (seq since-yesterday) (fail! "SINCE yesterday did not match"))
      (when (seq since-tomorrow) (fail! (str "SINCE tomorrow (" tomorrow ") matched: " (pr-str since-tomorrow))))
      (when-not (seq on-today) (fail! (str "ON today (" today ") did not match")))
      (when-not (seq unseen) (fail! "UNSEEN did not match the rich message"))
      (when-not (seq seen-plain) (fail! "SEEN did not match the plain message after mark-seen!"))
      (when-not (seq by-subject-ascii) (fail! "SUBJECT substring search did not match"))
      (when-not (= 2 (count or-q)) (fail! (str "OR returned " (pr-str or-q))))
      (when-not (seq not-q) (fail! "NOT SEEN did not match"))
      (when-not (= 1 (:count esearch)) (fail! (str "ESEARCH count: " (pr-str esearch))))
      (str "since-today=" (count since-today) " since-yesterday=" (count since-yesterday) " since-tomorrow=" (count since-tomorrow)
           " on-today=" (count on-today) " before-tomorrow=" (count before-tomorrow)
           " unseen=" (count unseen) " seen(plain)=" (count seen-plain)
           " subject-utf8=" (count by-subject) " subject-ascii=" (count by-subject-ascii) " text=" (count by-text)
           " or=" (count or-q) " not-seen=" (count not-q) " esearch=" (pr-str esearch)
           " | server day matched: today(UTC)=" today))))

(defn s6-flags-copy-move-append! []
  (let [{:keys [imap mid-rich caps]} @shared]
    (p/let [uids (imap/search! imap {:header {"Message-ID" mid-rich}})
            uid (last uids)
            _ (imap/flags-add! imap uid :flagged)
            flagged (imap/search! imap {:flagged true :header {"Message-ID" mid-rich}})
            _ (imap/flags-del! imap uid :flagged)
            unflagged (imap/search! imap {:flagged true :header {"Message-ID" mid-rich}})
            _ (imap/flags-add! imap uid "$EmailCljsSmoke")
            kw (imap/search! imap {:keyword "$EmailCljsSmoke" :header {"Message-ID" mid-rich}})
            mb (imap/current-mailbox imap)
            _ (imap/flags-del! imap uid "$EmailCljsSmoke")
            created (-> (imap/mailbox-create! imap smoke-mailbox)
                        (p/catch (fn [e] {:path smoke-mailbox :created false :error (error-brief e)})))
            _ (swap! shared assoc :smoke-mailbox-created? true)
            copied (imap/copy! imap uid smoke-mailbox)
            built (msg/build {:from (:from @shared) :to (:to @shared) :subject (str "email-cljs smoke appended " (:stamp @shared))
                              :text "appended draft" :message-id (message-id "appended")})
            appended (imap/append! imap smoke-mailbox (:source built) {:flags [:seen]})
            _ (imap/mailbox-open! imap smoke-mailbox)
            in-smoke (imap/search! imap {:all true})
            moved (imap/move! imap (get-in copied [:uid-map uid]) "INBOX")
            left (imap/search! imap {:all true})
            _ (imap/mailbox-open! imap "INBOX")
            back (imap/search! imap {:header {"Message-ID" mid-rich}})]
      (swap! shared update :uids (fnil into []) back)
      (note! (str "13.11 UIDPLUS: capability " (pr-str (boolean (:uidplus caps))) "; COPYUID uid-map " (pr-str (:uid-map copied))
                  "; APPENDUID uid " (pr-str (:uid appended)) "; MOVE uid-map " (pr-str (:uid-map moved))))
      (note! (str "permanent-flags of INBOX: " (pr-str (:permanent-flags mb)) " (custom keyword $EmailCljsSmoke matched " (count kw) ")"))
      (when-not (seq flagged) (fail! "FLAGGED search empty after flags-add!"))
      (when (seq unflagged) (fail! "still \\Flagged after flags-del!"))
      (when-not (seq kw) (fail! "custom keyword did not match (permanent-flags may exclude \\*)"))
      (when-not (get-in copied [:uid-map uid]) (fail! (str "copy! without uid-map: " (pr-str copied))))
      (when-not (:uid appended) (fail! (str "append! without APPENDUID uid: " (pr-str appended))))
      (when-not (= 2 (count in-smoke)) (fail! (str "smoke mailbox should hold copy + appended, has " (pr-str in-smoke))))
      (when-not (= 1 (count left)) (fail! (str "after move! back the smoke mailbox should hold 1, has " (pr-str left))))
      (when-not (= 2 (count back)) (fail! (str "INBOX should now hold the original + the moved copy: " (pr-str back))))
      (str "uid=" uid " flagged-search=" (count flagged) " after-del=" (count unflagged) " keyword=" (count kw)
           " mailbox-create=" (pr-str (select-keys created [:path :created :error]))
           " copy uid-map=" (pr-str (:uid-map copied)) " append uid=" (:uid appended)
           " smoke-mailbox=" (count in-smoke) " move-back uid-map=" (pr-str (:uid-map moved)) " left=" (count left)
           " inbox-hits=" (pr-str back)))))

(defn s7-watch! []
  (let [{:keys [imap smtp to from]} @shared
        mid (message-id "watch")
        ch (imap/watch! imap "INBOX" {:fetch? true :query {:envelope true :flags true}})]
    (swap! shared assoc :mid-watch mid :watch-ch ch)
    (p/let [open (take-event! ch #{:open :error :closed} 30000)
            _ (when-not (= :open (:event open)) (fail! (str "watcher did not open: " (pr-str open))))
            t0 (.now js/Date)
            r (smtp/send! smtp {:from from :to to :subject (str "email-cljs smoke watch " (:stamp @shared))
                                :text "watch trigger" :message-id mid}
                          {:strict? true})
            ev (take-event! ch #{:new-mail :error :closed} 60000)
            latency (- (.now js/Date) t0)
            found (require-uid! imap mid "watch message")
            _ (imap/mark-seen! imap (:uid found))
            flags-ev (take-event! ch #{:flags :error :closed} 15000)
            stopped (imap/stop-watch! ch)
            closed (take-event! ch nil 5000)]
      (note! (str "13.1 EXISTS via IDLE after send: " latency " ms; :new-mail payload count=" (:count ev) " prev=" (:prev-count ev)
                  " uids=" (pr-str (:uids ev)) "; :flags event from the main connection's STORE: " (pr-str (some-> flags-ev (dissoc :event)))))
      (when-not (:all-accepted? r) (fail! "watch trigger send rejected"))
      (when-not (= :new-mail (:event ev)) (fail! (str "no :new-mail within 60 s: " (pr-str ev) " skipped=" (pr-str (:skipped @shared)))))
      (when-not (some #{(:uid found)} (:uids ev)) (fail! (str ":new-mail uids " (pr-str (:uids ev)) " lack the sent message uid " (:uid found))))
      (when-not (true? stopped) (fail! "stop-watch! returned false"))
      (when-not (and (= :closed (:event closed)) (= :stopped (:reason closed))) (fail! (str "last event: " (pr-str closed))))
      (when (imap/watching? ch) (fail! "watching? still true"))
      (str "open path=" (:path open) " exists=" (get-in open [:mailbox :exists]) " | :new-mail after " latency " ms count=" (:count ev)
           " prev=" (:prev-count ev) " uids=" (pr-str (:uids ev)) " subject=" (pr-str (get-in ev [:messages 0 :envelope :subject]))
           " | :flags event=" (pr-str (some-> flags-ev (select-keys [:uid :flags]))) " | stopped=" stopped " closed=" (pr-str (dissoc closed :path))))))

(defn s8-cleanup-and-extras! []
  (let [{:keys [imap caps mid-plain mid-rich mid-watch]} @shared
        mids [mid-plain mid-rich mid-watch (message-id "appended")]]
    (p/let [quota (imap/quota! imap "INBOX")
            boxes (imap/mailboxes! imap {:status? true})
            sent-path (some #(when (= "\\Sent" (:special-use %)) (:path %)) boxes)
            sent-hits (if sent-path
                        (p/let [_ (imap/mailbox-open! imap sent-path {:read-only? true})
                                hits (imap/search! imap {:or [{:header {"Message-ID" mid-plain}} {:header {"Message-ID" mid-rich}}]})]
                          hits)
                        :no-sent-folder)
            _ (imap/mailbox-open! imap "INBOX")
            uids (imap/search! imap {:or (mapv (fn [m] {:header {"Message-ID" m}}) mids)})
            _ (when (seq uids)
                (if (:uidplus caps)
                  (p/do (imap/flags-add! imap uids :deleted)
                        (imap/uid-expunge! imap uids {:strict? true}))
                  (imap/delete! imap uids)))
            remaining (imap/search! imap {:or (mapv (fn [m] {:header {"Message-ID" m}}) mids)})
            _ (imap/mailbox-close! imap)
            dropped (-> (imap/mailbox-delete! imap smoke-mailbox)
                        (p/catch (fn [e] {:error (error-brief e)})))]
      (note! (str "13.12 quota: " (pr-str quota)))
      (note! (str "13.18 Sent copy after SMTP send: " (pr-str sent-hits) " (Sent folder " (pr-str sent-path) ")"))
      (note! (str "special-use map: " (pr-str (into {} (keep (fn [b] (when (:special-use b) [(:special-use b) (:path b)]))) boxes))))
      (when (seq remaining) (fail! (str "smoke messages still present after expunge: " (pr-str remaining))))
      (when (:error dropped) (fail! (str "could not delete " smoke-mailbox ": " (:error dropped))))
      (str "expunged uids=" (pr-str uids) " via " (if (:uidplus caps) "UID EXPUNGE" "delete!") " remaining=" (count remaining)
           " mailbox " smoke-mailbox " deleted=" (pr-str (:path dropped))
           " | mailboxes=" (count boxes) " quota=" (pr-str (some-> quota (select-keys [:storage :messages])))
           " sent-copy=" (pr-str sent-hits)))))

;; ---------------------------------------------------------------------------
;; main
;; ---------------------------------------------------------------------------

(defn- close-all! []
  (let [{:keys [imap smtp watch-ch]} @shared]
    (-> (p/do (when watch-ch (imap/stop-watch! watch-ch))
              (imap/close-imap! imap)
              (smtp/close-smtp! smtp))
        (p/catch (fn [e] (say "close error:" (error-brief e)))))))

(defn- tally! []
  (let [rs @results
        failed (remove :ok? rs)]
    (say "")
    (say "INVENTORY NOTES (live observations)")
    (doseq [n @notes] (say (str "  " n)))
    (say "")
    (say (str "TALLY: " (count (filter :ok? rs)) "/" (count rs) " sections passed; messages sent: 3; run stamp " (:stamp @shared)))
    (doseq [f failed] (say (str "  FAILED " (:name f) ": " (:reason f))))
    (empty? failed)))

(defn main [& _]
  (let [env (util/from-env secrets-file env-prefix)
        user (some-> (:username env) util/reveal)
        pass (some-> (:password env) util/reveal)]
    (when (or (nil? user) (nil? pass))
      (say "FAIL setup: no BORRET_EMAIL_USERNAME / BORRET_EMAIL_PASSWORD in the environment or ~/.secrets/borret.txt")
      (js/process.exit 2))
    (reset! scrubber (fn [s] (-> s (str/replace user "<redacted>") (str/replace pass "<redacted>")
                                 (str/replace (js/encodeURIComponent user) "<redacted>"))))
    (let [stamp (str (.now js/Date))
          smtp (smtp/smtp-client-from-env env-prefix {:secrets-file secrets-file})
          imap (imap/imap-client-from-env env-prefix {:secrets-file secrets-file})]
      (swap! shared assoc :stamp stamp :smtp smtp :imap imap
             :from user :to (str "smoke-test-" stamp "@" domain))
      (say "email-cljs LIVE smoke —" (now-iso) "— stamp" stamp "— 3 sends to" (:to @shared))
      (say "smtp client:" (pr-str (:config smtp)))
      (say "imap client:" (pr-str (:config imap)))
      (-> (p/do
            (section! "0 verify-smtp! + connect! (capabilities, TLS, server ID)" s0-connect!)
            (section! "1 send! plain text + bcc (unique Message-ID, :strict?)" s1-send-plain!)
            (section! "2 IMAP round-trip: poll by Message-ID, PEEK fetch, parse, mark-seen!, Bcc stripped" s2-round-trip!)
            (section! "3 attachment round-trip: send Bytes -> body-structure -> body-part! base64-equal" s3-send-rich-and-attachment!)
            (section! "4 html + alternative: source! -> message/parse-message! :text and :html" s4-parse-fetched-html!)
            (section! "5 search DSL live: SINCE/ON/BEFORE UTC days, UNSEEN/SEEN, SUBJECT ü✓, OR/NOT, ESEARCH" s5-search-dsl!)
            (section! "6 flags, custom keyword, mailbox-create!, copy!, append! (APPENDUID), move! back" s6-flags-copy-move-append!)
            (section! "7 watch! IDLE: :open, send, :new-mail (+fetch), :flags, stop-watch!, :closed" s7-watch!)
            (section! "8 cleanup: UID EXPUNGE / delete!, mailbox-delete!, quota!, mailboxes! + Sent check" s8-cleanup-and-extras!))
          (p/catch (fn [e] (say "FAIL harness:" (error-brief e))))
          (p/then (fn [_] (close-all!)))
          (p/finally
            (fn [_ _]
              (let [ok? (tally!)]
                (set! (.-exitCode js/process) (if ok? 0 1))
                (say (str "LIVENESS: every connection closed at " (now-iso) "; active resources now: "
                          (pr-str (vec (js/process.getActiveResourcesInfo)))
                          " (PipeWrap = stdout/stderr; a TCPSocketWrap here is a socket still tearing down)"
                          " — if the process exits naturally nothing prints after this line (exit code " (if ok? 0 1) ")."))
                ;; unref'd: it only fires when something ELSE keeps the event loop alive
                (let [t (js/setTimeout (fn []
                                         (say (str "FAIL 9 process liveness: still alive 5 s after cleanup; active resources: "
                                                   (pr-str (vec (js/process.getActiveResourcesInfo)))))
                                         (js/process.exit 1))
                                       5000)]
                  (.unref t)))))))))
