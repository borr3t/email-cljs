(ns email-cljs.test-util
  "Shared fixtures for the email-cljs tests: a throwaway password (NOT a
   real credential) and promise helpers. Not a test namespace itself (the
   name does not end in -test). Phase 2 adds the stubbed transports here
   (nodemailer jsonTransport, fake imapflow objects, canned RFC 5322
   messages) so tests never touch the network or the secrets file."
  (:require [promesa.core :as p]))

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
