# email-cljs

ClojureScript (Node.js) email client library: SMTP sending (over
[nodemailer](https://nodemailer.com)), IMAP access with IDLE (over
[imapflow](https://imapflow.com)), message parsing (over
[mailparser](https://nodemailer.com/extras/mailparser/)) and message building,
attachments — the full surface of a mail account, in ClojureScript.

The authoritative spec is `docs/email-api-inventory.org`. The only runtime npm
dependencies are the three wrapped packages; promesa promises in, kebab-case
maps in and out; the IMAP IDLE watcher returns a core.async channel.

## Status

| Phase | Content | State |
|-------|---------|-------|
| 0 | scaffold: shadow-cljs `:node-test` / `:dev` builds, `run-tests.sh`, `util` (case translation, opaque Secret, error envelope) | done |
| 1 | API inventory (`docs/email-api-inventory.org`): nodemailer 10 / imapflow 2 / mailparser 3.9 census (postal-mime recorded as the parser alternative), SMTP + IMAP + MIME semantics, error taxonomy, IDLE design | done |
| 2 | `smtp` + `smtp-binding` (client config, `smtp-client-from-env`, `send!` with `:strict?`, `verify-smtp!`, `close-smtp!`), the opaque `Bytes` wrapper (`attachment`), Buffer/env/fs touches (`parse-binding`) | done |
| 3 | `imap` + `imap-binding` (connection, mailboxes, search DSL, fetch, download, flags, move/copy/delete/expunge/append, quota, `watch!` IDLE channel on its own connection) | done |
| 4 | `message` (build via MailComposer, parse via mailparser), `attachment` file helpers, streaming parse | planned |
| 5 | live smoke (`dev/email_cljs/smoke.cljs`: send to the catch-all, fetch it back over IMAP, flags, IDLE) | planned |
| 6 | docs polish, README examples verified against the smoke | planned |

`./run-tests.sh` — no network, no secrets file (51 tests / 528 assertions after phase 3:
SMTP through a nodemailer transport plugin that runs the real message normalisation, IMAP
through a fake imapflow connection, imapflow's search compiler invoked offline).

## Usage sketch (phases 2-3)

```clojure
(require '[email-cljs.smtp :as smtp] '[email-cljs.imap :as imap]
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

;; IMAP — UID-first, kebab maps, promises
(def box (imap/imap-client-from-env "BORRET_EMAIL_"))           ; 993, implicit TLS, disableAutoIdle
(p/let [_ (imap/connect! box)
        _ (imap/mailbox-open! box "INBOX" {:read-only? true})
        uids (imap/search! box {:unseen true :since #inst "2026-09-24"})
        msgs (imap/fetch-all! box uids {:envelope true :flags true :body-structure true})
        png (imap/body-part! box (first uids) "2")]             ; Bytes, decoded
  (att/->base64 png))

;; IDLE watcher — a second connection, events on a core.async channel
(let [ch (imap/watch! box "INBOX" {:fetch? true})]
  (async/go-loop [] (when-let [ev (async/<! ch)] (println (:event ev) (:count ev)) (recur)))
  ;; later: (imap/stop-watch! ch)
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

Credentials for the Phase 5 live smoke come from `~/.secrets/borret.txt`
(`BORRET_EMAIL_USERNAME` / `BORRET_EMAIL_PASSWORD` / SMTP and IMAP host, port,
TLS keys); the unit tests never read it.
