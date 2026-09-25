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
| 2 | `core` (client config, secrets file, SMTP transport, `send!`, `verify!`), `message` (build), binding namespaces | planned |
| 3 | `imap` (connection, mailboxes, search, fetch, flags, move/copy/delete, `watch!` IDLE channel) | planned |
| 4 | `message` (parse via mailparser), `attachment` (encode/decode, base64 / byte arrays, streaming) | planned |
| 5 | live smoke (`dev/email_cljs/smoke.cljs`: send to the catch-all, fetch it back over IMAP, flags, IDLE) | planned |
| 6 | docs polish, README examples verified against the smoke | planned |

`./run-tests.sh` — no network, no secrets file.

## Hard style rules

These are non-negotiable for every phase:

- **ClojureScript only at the surface.** No `^js` type hints anywhere. No
  JavaScript interop at call sites: every `.method`, `.-prop`, `#js`, `clj->js`,
  Buffer or stream touch lives in the binding/transport namespaces
  (`email-cljs.smtp-binding`, `email-cljs.imap-binding`,
  `email-cljs.parse-binding`). `util`, `core`, `message`, `imap`, `attachment`
  deal only in ClojureScript data.
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
