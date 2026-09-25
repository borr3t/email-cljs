# email-cljs

ClojureScript (Node.js) email client library: SMTP sending (over
[nodemailer](https://nodemailer.com)), IMAP access with IDLE (over
[imapflow](https://imapflow.com)), message parsing (over
[mailparser](https://nodemailer.com/extras/mailparser/)) and message building,
attachments — the full surface of a mail account, in ClojureScript.

The manual is `docs/email-cljs.org`; the research behind it is
`docs/email-api-inventory.org`. The only runtime npm dependencies are the three
wrapped packages; promesa promises in, kebab-case maps in and out; the IMAP IDLE
watcher returns a core.async channel.

## Status

| Phase | Content | State |
|-------|---------|-------|
| 0 | scaffold: shadow-cljs `:node-test` / `:dev` builds, `run-tests.sh`, `util` (case translation, opaque Secret, error envelope) | done |
| 1 | API inventory (`docs/email-api-inventory.org`): nodemailer 10 / imapflow 2 / mailparser 3.9 census (postal-mime recorded as the parser alternative), SMTP + IMAP + MIME semantics, error taxonomy, IDLE design | done |
| 2 | `smtp` + `smtp-binding` (client config, `smtp-client-from-env`, `send!` with `:strict?`, `verify-smtp!`, `close-smtp!`), the opaque `Bytes` wrapper (`attachment`), Buffer/env/fs touches (`parse-binding`) | done |
| 3 | `imap` + `imap-binding` (connection, mailboxes, search DSL, fetch, download, flags, move/copy/delete/expunge/append, quota, `watch!` IDLE channel on its own connection) | done |
| 4 | `message` (`parse-message!` via mailparser, `build` / `build-string` via MailComposer, header canonicalisation, parsed-shape helpers), `attachment/read-file!` / `write-file!`, mailparser in `parse-binding`, `compose` in `smtp-binding` | done |
| 5 | live smoke (`dev/email_cljs/smoke.cljs`, `:smoke` build): 9 sections green against the house server — send to the catch-all, PEEK fetch-back, attachment bytes, parse, search DSL incl. UTC-day / WITHIN, flags, copy / append / move with UIDPLUS, `watch!` IDLE, `UID EXPUNGE` cleanup, process-liveness assert | done |
| 6 | manual (`docs/email-cljs.org`), README, inventory section 18 (live corrections) | done |

`./run-tests.sh` — no network, no secrets file (66 tests / 700 assertions: SMTP through a
nodemailer transport plugin that runs the real message normalisation, IMAP through a fake
imapflow connection with imapflow's search compiler invoked offline, real MailComposer →
mailparser round trips in process).

`npx shadow-cljs compile smoke && node target/smoke.js` — the LIVE smoke (three real sends,
cleaned up; credentials from `BORRET_EMAIL_*` or `~/.secrets/borret.txt`, never printed).

## Documentation

- **`docs/email-cljs.org` — the manual**: introduction and the binding-namespace
  architecture, quick start, clients and credentials, SMTP (`send!`, partial rejection,
  attachments and the `:path` / `:href` gates), IMAP (mailboxes, the search DSL and its
  UTC-day / WITHIN date semantics, PEEK fetches, body parts, flags, move / copy / delete /
  expunge and the UIDPLUS fallback, `append!`), the message namespace (parsed shape,
  `build`), `Bytes`, `watch!` (event table, 29-minute IDLE restart, liveness contract),
  error handling, design decisions, testing and the live smoke, API reference.
- **`docs/email-api-inventory.org` — the research**: package census, protocol and MIME
  semantics, pitfalls, the 24 uncertainties with house positions, and sections 17–18 with
  everything the implementation and the live server corrected.

## Usage sketch

```clojure
(require '[email-cljs.smtp :as smtp] '[email-cljs.imap :as imap] '[email-cljs.message :as msg]
         '[email-cljs.attachment :as att] '[email-cljs.util :as util]
         '[promesa.core :as p] '[clojure.core.async :as async])

;; SMTP — credentials from BORRET_EMAIL_* (environment over ~/.secrets/borret.txt)
(def mail (smtp/smtp-client-from-env "BORRET_EMAIL_"))          ; 465, implicit TLS
(p/let [r (smtp/send! mail {:from {:name "Doe, John" :address "john@example.test"}
                            :to ["a@example.test"] :bcc "me@example.test"
                            :subject "Ünïcode ✓" :text "plain" :html "<p>hi</p>"
                            :attachments [{:filename "logo.png" :bytes (att/from-base64 "iVBORw0KGgo=") :cid "logo@x"}]}
                      {:strict? true})]                         ; reject on partial rejection
  (:message-id r))                                              ; {:accepted [..] :rejected [] :all-accepted? true ...}

;; IMAP — UID-first, kebab maps, promises, fetches never set \Seen
(def box (imap/imap-client-from-env "BORRET_EMAIL_"))           ; 993, implicit TLS, disableAutoIdle
(p/let [_ (imap/connect! box)
        _ (imap/mailbox-open! box "INBOX" {:read-only? true})
        uids (imap/search! box {:unseen true :since #inst "2026-09-24"})   ; UTC day (YOUNGER n on WITHIN servers)
        m (imap/fetch-one! box (last uids) {:envelope true :flags true :source? true})
        parsed (msg/parse-message! m)]                          ; :subject :from :to :text :html :headers :attachments
  [(:subject parsed) (mapv :filename (:attachments parsed)) (msg/attachment->base64 (first (:attachments parsed)))])

;; Build without a transport (drafts, Sent copies) and append
(p/let [built (msg/build {:from "me@example.test" :to "a@example.test" :subject "draft" :text "wip"})]
  (imap/append! box "Drafts" (:source built) {:flags [:draft]}))

;; IDLE watcher — a second connection, events on a core.async channel
(let [ch (imap/watch! box "INBOX" {:fetch? true})]
  (async/go-loop [] (when-let [ev (async/<! ch)] (println (:event ev) (:uids ev)) (recur)))
  ;; later: (imap/stop-watch! ch) — then (imap/close-imap! box): the process exits on its own
  )
```

## Hard style rules

These are non-negotiable for every phase:

- **ClojureScript only at the surface.** No `^js` type hints anywhere. No
  JavaScript interop at call sites: every `.method`, `.-prop`, `#js`, `clj->js`,
  Buffer or stream touch lives in the binding namespaces
  (`email-cljs.smtp-binding`, `email-cljs.imap-binding`,
  `email-cljs.parse-binding`), written in the `js-invoke` / `unchecked-get`
  style (no inferred-type warnings without hints). `util`, `smtp`, `imap`,
  `attachment`, `message` deal only in ClojureScript data. The one test
  namespace with interop is `email-cljs.test-util` (the fakes must be JS
  objects); test namespaces use its constructors.
- **kebab-case keyword maps in and out.** The wrapped packages speak camelCase
  (`contentType`, `bodyStructure`, `internalDate`); translation happens once,
  at the binding boundary (`util/->camel-keys`, `util/->kebab-keys`), with
  user-owned blobs (`:headers`, `:content`, `:raw`) passed through verbatim.
- **promesa promises for every async operation**; **core.async channels** where
  streaming or subscriptions fit (IMAP IDLE events, streaming parse).
- **Zero runtime npm dependencies beyond the wrapped packages**
  (nodemailer, imapflow, mailparser).
- **Secrets never print.** The account password is an opaque `Secret`
  (`util/secret`) that renders as `#email-cljs/secret <redacted>`; only the
  binding namespaces reveal it to build the auth object. Error data never
  carries secret material.
- **Errors are `ex-info` values** with a `:type` in `util/error-types`
  (`:email-cljs/config-error`, `:argument-error`, `:auth-error`,
  `:network-error`, `:smtp-error`, `:imap-error`, `:parse-error`); see
  `util/error-data`, `util/retryable?`.

## Development

```
./run-tests.sh                                  # exec's node at the end — never chain it mid-command
npx shadow-cljs watch dev && node target/dev.js # REPL runtime
```

Credentials for the live smoke come from `BORRET_EMAIL_*` in the environment or
`~/.secrets/borret.txt` (`BORRET_EMAIL_USERNAME` / `BORRET_EMAIL_PASSWORD` / SMTP and
IMAP host, port, TLS keys); the unit tests never read it.
