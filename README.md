# email-cljs

An idiomatic ClojureScript (Node.js) email client: sending over SMTP (through
[nodemailer](https://nodemailer.com)), reading over IMAP with IDLE (through
[imapflow](https://imapflow.com)), parsing messages (through
[mailparser](https://nodemailer.com/extras/mailparser/)) and building them, with
attachments as bytes. Messages, options and results are maps of kebab-cased keywords,
every operation returns a [promesa](https://github.com/funcool/promesa) promise, the
IMAP watcher is a `core.async` channel, and the password of the account is a value that
never prints. The only runtime npm dependencies are the three wrapped packages.

## Requirements

Node.js 22+, Java 11+ for shadow-cljs. The library is consumed as a source dependency
(git or `:local/root`) by another shadow-cljs Node app, which has `nodemailer`,
`imapflow` and `mailparser` in its own `package.json`; `deps.edn` declares
ClojureScript, core.async and promesa.

## Quick start (offline)

The quick start builds a message — headers, two bodies, an attachment — and parses
what it built. Both happen in the process: no server, no account, nothing leaves the
machine.

```bash
npm install
npx shadow-cljs compile examples && node target/examples.js
```

That runs [`dev/email_cljs/examples.cljs`](dev/email_cljs/examples.cljs), which holds
every example of this page. Its namespace form:

```clojure
(ns email-cljs.examples
  (:require ["events" :refer [EventEmitter]]
            [cljs.core.async :as async :refer [<! go-loop]]
            [clojure.string :as str]
            [email-cljs.imap :as imap]
            [email-cljs.message :as msg]
            [email-cljs.smtp :as smtp]
            [promesa.core :as p]))
```

A message, built and parsed:

```clojure
(def message
  {:from        {:name "Ada Lovelace" :address "ada@example.test"}
   :to          ["grace@example.test"]
   :subject     "Notes on the engine"
   :date        "Fri, 21 Nov 1997 09:55:06 -0600"
   :message-id  "<notes-1@example.test>"
   :text        "The notes are attached."
   :html        "<p>The notes are <b>attached</b>.</p>"
   :attachments [{:filename "notes.txt" :content "Note G" :content-type "text/plain"}]})

(defn quick-start []
  (p/let [built  (msg/build message)
          parsed (msg/parse-message! (:source built))]
    (-> (select-keys parsed [:subject :from :to :date :message-id :text :html])
        (assoc :attachments
               (mapv (fn [attachment]
                       (-> (select-keys attachment [:filename :content-type :size])
                           (assoc :text (msg/attachment->string attachment))))
                     (:attachments parsed))))))
```

`(quick-start)` resolves with:

```clojure
{:subject     "Notes on the engine"
 :from        {:name "Ada Lovelace" :address "ada@example.test"}
 :to          [{:name nil :address "grace@example.test"}]
 :date        "1997-11-21T15:55:06.000Z"
 :message-id  "<notes-1@example.test>"
 :text        "The notes are attached."
 :html        "<p>The notes are <b>attached</b>.</p>"
 :attachments [{:filename "notes.txt" :content-type "text/plain" :size 6 :text "Note G"}]}
```

`msg/build` returns the bytes of the message as they would be transmitted;
`msg/parse-message!` reads any RFC 822 message, a string or bytes, into one map.
The date comes back as an instant in UTC. The content of an attachment is held as
bytes (`#email-cljs/bytes {:size 6}`) and turned into text or base64 on demand. The
parsed map carries more than is selected here: `:cc`, `:bcc`, `:reply-to`,
`:in-reply-to`, `:references`, `:priority`, and every header under `:headers`.

### Going live

Sending and reading need a mail server. **The server, the account and the password
are yours: the library ships none, and the addresses of this page (`example.test`)
reach no one.** A client is configured from the environment of the process, under a
prefix you choose:

```clojure
(def mail (smtp/smtp-client-from-env "WORK_EMAIL_"))   ; port 465, TLS from the first byte
(def box  (imap/imap-client-from-env "WORK_EMAIL_"))   ; port 993, TLS from the first byte

(p/let [_    (smtp/verify-smtp! mail)                  ; connects, negotiates TLS, logs in, quits
        sent (smtp/send! mail {:from    "you@your-domain.example"
                               :to      ["someone@their-domain.example"]
                               :subject "Hello"
                               :text    "Sent with email-cljs."})]
  (println (:accepted sent) (:rejected sent)))
```

| variable            | meaning                                                          |
|---------------------|------------------------------------------------------------------|
| `<PREFIX>USERNAME`  | the login of the account                                         |
| `<PREFIX>PASSWORD`  | its password                                                     |
| `<PREFIX>SMTP_HOST` | the SMTP server                                                  |
| `<PREFIX>SMTP_PORT` | its port; 465 when absent                                        |
| `<PREFIX>SMTP_TLS`  | `true` for TLS from the first byte, `false` for STARTTLS         |
| `<PREFIX>IMAP_HOST` | the IMAP server                                                  |
| `<PREFIX>IMAP_PORT` | its port; 993 when absent                                        |
| `<PREFIX>IMAP_TLS`  | as for SMTP                                                      |
| `<PREFIX>TLS_CA`    | a PEM chain to trust, for a server with a private certificate    |

The same variables can stand in a file of `NAME=value` lines, named by
`{:secrets-file "mail.env"}` in the options of the client or by the
`EMAIL_SECRETS_FILE` variable; the environment wins over the file, and no file is read
unless one is named. A client can also be built from a map — `(smtp/smtp-client {:host
.. :auth {:user .. :pass ..}})`, `(imap/imap-client {..})`. Keep the password out of
the repository, in a file only you can read. The library wraps it on arrival: it
prints as `#email-cljs/secret <redacted>`, and no error carries it.

## Scenarios

Each scenario is a function of `dev/email_cljs/examples.cljs`, shown with the result
it returns against a fake. Given a client of a server of yours, the same function
sends and reads mail.

### Send a message

The fake is a nodemailer transport: nodemailer builds the message exactly as it would
for a server, and the transport answers instead of one.

```clojure
(defn fake-smtp
  "A nodemailer transport that delivers nowhere. nodemailer builds the
   message as it would for a server; the transport keeps what it would
   have transmitted in the atom `outbox` and answers as a server does
   that knows no mailbox of `unknown`, or that fails with `failure` (the
   fields of a nodemailer error)."
  [outbox {:keys [unknown failure]}]
  #js {:name    "fake-smtp"
       :version "1.0.0"
       :send    (fn [mail callback]
                  (let [built    (unchecked-get mail "message")
                        envelope (js->clj (js-invoke built "getEnvelope") :keywordize-keys true)
                        refused  (filterv (set unknown) (:to envelope))]
                    (js-invoke built "build"
                               (fn [err raw]
                                 (cond
                                   err     (callback err)
                                   failure (callback (js/Object.assign (js/Error. (:message failure))
                                                                       (clj->js failure)))
                                   :else   (do (swap! outbox conj {:envelope envelope
                                                                   :raw      (js-invoke raw "toString" "utf8")})
                                               (callback nil (clj->js
                                                              {:envelope  envelope
                                                               :messageId (js-invoke built "messageId")
                                                               :accepted  (remove (set refused) (:to envelope))
                                                               :rejected  refused
                                                               :response  "250 2.0.0 Ok: queued"}))))))))})
```

```clojure
(defn send-notes
  "Sends `message` with a copy and a blind copy. Resolves with what the
   server said of each recipient."
  [client]
  (p/let [sent (smtp/send! client (assoc message
                                         :cc  ["nobody@example.test"]
                                         :bcc ["charles@example.test"]))]
    (select-keys sent [:message-id :accepted :rejected :all-accepted? :response-code])))
```

With `(smtp/smtp-client {:transport (fake-smtp outbox {:unknown
["nobody@example.test"]})})` as the client, `send-notes` resolves with the first map;
the second is what the transport would have transmitted:

```clojure
{:sent        {:message-id    "<notes-1@example.test>"
               :accepted      ["grace@example.test" "charles@example.test"]
               :rejected      ["nobody@example.test"]
               :all-accepted? false
               :response-code 250}
 :transmitted {:envelope {:from "ada@example.test"
                          :to   ["grace@example.test" "nobody@example.test" "charles@example.test"]}
               :cc       ["nobody@example.test"]
               :bcc      []}}
```

A server that refuses some recipients and accepts others has delivered to the ones it
accepted: `send!` resolves, and says who was refused. `{:strict? true}` makes that a
rejection (see [Error handling](#error-handling)); the message is delivered all the
same, since nothing sent can be recalled. The blind copy travels in the envelope and
stands in no header.

### Read the unread mail of a mailbox

```clojure
(defn read-unread
  "The unread mail of the inbox, oldest first, each message marked as
   read once all of them are parsed."
  [client]
  (p/let [_    (imap/connect! client)
          _    (imap/mailbox-open! client "INBOX")
          uids (imap/search! client {:unseen true})
          mail (p/all (for [uid uids]
                        (p/let [fetched (imap/fetch-one! client uid {:flags true :source? true})
                                parsed  (msg/parse-message! fetched)]
                          {:uid         uid
                           :from        (:address (:from parsed))
                           :subject     (:subject parsed)
                           :attachments (mapv :filename (:attachments parsed))})))
          _    (when (seq uids) (imap/mark-seen! client uids))
          left (imap/search! client {:unseen true})
          _    (imap/close-imap! client)]
    {:mail mail :left-unread (count left)}))
```

Over a fake connection to an inbox that holds the message of the quick start and an
answer to it, `(read-unread client)` resolves with:

```clojure
{:mail        [{:uid 101 :from "ada@example.test" :subject "Notes on the engine"
                :attachments ["notes.txt"]}
               {:uid 102 :from "grace@example.test" :subject "Re: Notes on the engine"
                :attachments []}]
 :left-unread 0}
```

The fake connection is `fake-imap` of the examples file, given to the client as its
`:make-conn`. Every function takes and returns UIDs. Fetching never marks a message
as read: `read-unread` does so itself, with `mark-seen!` — on a mailbox of yours that
is a change to the mailbox. `{:read-only? true}` opens a mailbox that cannot be
changed.

### Watch a mailbox

```clojure
(defn arrivals
  "Watches `mailbox` on a connection of its own. Resolves, once the
   mailbox is open, with {:mail .. :stop! ..}: `mail` is a channel of
   {:uid :subject}, one per message that arrives, closed when the watcher
   ends; `(stop!)` ends it."
  [client mailbox]
  (let [events (imap/watch! client mailbox {:fetch? true :query {:source? true}})
        mail   (async/chan 16)
        open   (p/deferred)]
    (go-loop []
      (if-let [{:keys [event messages error]} (<! events)]
        (do (case event
              :open     (p/resolve open {:mail mail :stop! #(imap/stop-watch! events)})
              :error    (p/reject open error)
              :new-mail (doseq [fetched messages]
                          (p/let [parsed (msg/parse-message! fetched)]
                            (async/put! mail {:uid (:uid fetched) :subject (:subject parsed)})))
              nil)
            (recur))
        (async/close! mail)))
    open))
```

Over the fake server, at which the answer arrives while the inbox is watched, the
first value of `mail` and the value of `(stop!)` are:

```clojure
{:arrived {:uid 102 :subject "Re: Notes on the engine"}
 :stopped true}
```

`watch!` opens a second connection for IDLE, so that commands and notifications never
share a socket, and re-enters IDLE every 29 minutes as RFC 2177 asks. The watcher does
not reconnect: its last event is `{:event :closed :reason ..}`, and what follows a
lost connection is the decision of the caller. A stopped watcher leaves nothing
behind that would keep the process alive.

## Pure helpers

No server, no network:

```clojure
(util/parse-address "Ada Lovelace <ada@example.test>")
;; => {:name "Ada Lovelace" :address "ada@example.test"}

(util/format-address {:name "Lovelace, Ada" :address "ada@example.test"})
;; => "\"Lovelace, Ada\" <ada@example.test>"

(msg/canonical-header-name "message-id")
;; => "Message-ID"

(util/secret "not-a-password")
;; => #email-cljs/secret <redacted>

(util/env-config {"WORK_EMAIL_USERNAME" "ada@example.test" "WORK_EMAIL_PASSWORD" "not-a-password" "WORK_EMAIL_SMTP_HOST" "smtp.example.test" "WORK_EMAIL_SMTP_PORT" "465"} "WORK_EMAIL_")
;; => {:username #email-cljs/secret <redacted> :password #email-cljs/secret <redacted> :smtp {:host "smtp.example.test" :port 465}}

(select-keys (smtp/client-options->wire {:host "smtp.example.test" :port 587}) ["port" "secure"])
;; => {"port" 587 "secure" false}

(util/smtp-code-class 451)
;; => :transient

(imap/compile-search {:unseen true :from "ada@example.test" :since "21-Nov-1997"})
;; => {"seen" false "from" "ada@example.test" "since" "1997-11-21"}

(imap/range->wire [101 102])
;; => "101,102"

(imap/flags->wire [:seen :flagged])
;; => ["\\Seen" "\\Flagged"]

(att/->base64 (att/from-string "Note G"))
;; => "Tm90ZSBH"

(att/->string (att/from-base64 "Tm90ZSBH"))
;; => "Note G"
```

`att` is `email-cljs.attachment`, `util` is `email-cljs.util`. The dates of a search
are calendar days in UTC.

## Error handling

Every rejection is an `ex-info` whose data carries a `:type`:

| `:type`                      | when                                                           | data                                         |
|------------------------------|----------------------------------------------------------------|----------------------------------------------|
| `:email-cljs/config-error`   | a client cannot be configured: no host, no credentials         | `:reason`                                    |
| `:email-cljs/argument-error` | a message, a search or an option is invalid: nothing was sent  | `:reason`                                    |
| `:email-cljs/auth-error`     | the server refused the login                                   | `:protocol`, `:response-code`, `:response`   |
| `:email-cljs/network-error`  | no connection, a timeout, a TLS failure                        | `:protocol`, `:code`, `:retryable?`          |
| `:email-cljs/smtp-error`     | the SMTP server refused the envelope or the message            | `:response-code`, `:rejected`, `:accepted`, `:retryable?` |
| `:email-cljs/imap-error`     | the IMAP server answered NO or BAD                             | `:response-status`, `:response-text`, `:reason` |
| `:email-cljs/parse-error`    | a message could not be parsed or built                         | `:reason`, `:message`                        |

```clojure
(defn delivery
  "The message sent, or the failure as data."
  [client message opts]
  (-> (smtp/send! client message opts)
      (p/then (fn [sent] {:ok? true :message-id (:message-id sent)}))
      (p/catch (fn [e]
                 (let [{:keys [type reason rejected response-code retryable?]} (ex-data e)]
                   (case type
                     :email-cljs/smtp-error     {:ok? false :why :refused :reason reason :rejected rejected}
                     :email-cljs/auth-error     {:ok? false :why :login :response-code response-code}
                     :email-cljs/network-error  {:ok? false :why :unreachable :retry? retryable?}
                     :email-cljs/argument-error {:ok? false :why :message :reason reason}
                     :email-cljs/config-error   {:ok? false :why :configuration :reason reason}
                     (throw e)))))))
```

Through fake transports — one that accepts; one that knows no `grace@example.test`,
asked with `{:strict? true}`; one that refuses the login with 535; one that is not
there; and a message whose attachment names a file — `delivery` resolves with:

```clojure
{:sent        {:ok? true :message-id "<notes-1@example.test>"}
 :refused     {:ok? false :why :refused :reason :partial-rejection :rejected ["grace@example.test"]}
 :login       {:ok? false :why :login :response-code 535}
 :unreachable {:ok? false :why :unreachable :retry? true}
 :from-a-file {:ok? false :why :message :reason :file-access-disabled}}
```

`util/retryable?` says whether an error is worth another attempt: a 4xx answer of the
server, a timeout, a connection that was reset. An attachment read from a file
(`:path`) or fetched from a URL (`:href`) is refused unless the client was made with
`:allow-file-access?` or `:allow-url-access?`: a message map that comes from outside
cannot make the process read its own disk.

## Features

* **SMTP** — `send!` with text, HTML, alternatives, attachments, inline images, custom
  headers, priority, DSN; single connection or pool; `verify-smtp!`; partial rejection
  reported, or refused with `:strict?`.
* **IMAP** — mailboxes (list, status, create, rename, delete), a search language of
  maps, fetch by UID, body parts and downloads with a byte cap, flags, move, copy,
  delete, expunge, append, quota.
* **IDLE** — `watch!` gives a channel of `:new-mail`, `:expunge` and `:flags` events
  from a connection of its own, with the restart RFC 2177 asks for.
* **Messages as data** — `parse-message!` reads an RFC 822 message into one map;
  `build` writes one, without a transport, for drafts and copies to append.
* **Attachments as bytes** — one wrapper for binary content, in and out; base64 and
  text on demand; files read and written through it.
* **Readable keys, untouched values** — options and results are kebab-cased keywords;
  header names keep their canonical case, header values and bodies stay as sent.
* **Secrets that never print** — the password and the CA chain are opaque values,
  redacted in every printed form and absent from every error.
* **Reading does not mark as read** — every fetch peeks; flags change only when asked.
* **TLS by default** — 465 and 993, encrypted from the first byte; certificate
  validation is never switched off by the library, and a private chain is trusted
  through `:ca`.
* **Structured errors** — every rejection is an `ex-info` with a `:type`.
* **No interop at call sites** — the public API carries no `^js` hints; JavaScript
  interop lives in three binding namespaces.

## Limits and settings

The limits that matter are those of the server: the size of a message, the number of
recipients, the rate of sending, the quota of the mailbox. The library enforces none
of them on its own account and invents none: it reports what the server refused.

What the caller sets is how the client connects, waits and guards itself. Each has a
default that the caller can replace:

| where              | key                                        | default              | meaning                                           |
|--------------------|--------------------------------------------|----------------------|---------------------------------------------------|
| `smtp/smtp-client` | `:port`, `:secure`                         | 465, `true`          | 587 with `:secure false` is STARTTLS              |
| `smtp/smtp-client` | `:pool?`                                   | `false`              | one connection per message, or a pool             |
| `smtp/smtp-client` | `:max-connections`, `:max-messages`        | 5, 100               | the size of the pool, the messages per connection |
| `smtp/smtp-client` | `:connection-timeout-ms`, `:greeting-timeout-ms`, `:socket-timeout-ms`, `:dns-timeout-ms` | nodemailer's | how long each step may take |
| `smtp/smtp-client` | `:allow-file-access?`, `:allow-url-access?` | `false`             | whether an attachment may name a file or a URL    |
| `smtp/send!`       | `:strict?`                                 | `false`              | a partial rejection rejects                       |
| `imap/imap-client` | `:port`, `:secure`                         | 993, `true`          | where and how the client connects                 |
| `imap/imap-client` | `:max-literal-size`                        | 256 MB               | the largest single literal accepted of the server |
| `imap/imap-client` | `:connection-timeout-ms`, `:greeting-timeout-ms`, `:socket-timeout-ms` | imapflow's | how long each step may take |
| `imap/imap-client` | `:max-idle-ms`                             | 29 minutes           | the period after which the watcher re-enters IDLE |
| `imap/watch!`      | `:buffer`                                  | 64, sliding          | events held; a slow consumer loses the oldest     |
| `imap/watch!`      | `:fetch?`                                  | `false`              | whether new mail is fetched with its event        |
| `imap/download!`   | `:max-bytes`                               | none                 | the largest part downloaded                       |
| both clients       | `:tls {:ca ..}`                            | the trust store of Node | the chain a private server is trusted through  |

## Documentation

* [`docs/email-cljs.org`](docs/email-cljs.org) — the manual: clients and credentials,
  SMTP, IMAP with its search language, the parsed message, bytes, the watcher, error
  handling, and the reference of every public function.
* [`docs/email-api-inventory.org`](docs/email-api-inventory.org) — the surface of
  nodemailer, imapflow and mailparser that the library wraps, and the behaviour of
  SMTP, IMAP and MIME it relies on, with the source each fact was taken from.
* The docstrings of `src/email_cljs/` are the reference of each function.

## Layout

```
src/email_cljs/
  smtp.cljs           sending: client, send!, verify, message and option translation (pure but for the calls)
  imap.cljs           reading: client, mailboxes, search, fetch, flags, move/copy/delete, append, watch!
  message.cljs        messages as data: parse-message!, build, helpers on the parsed map
  attachment.cljs     bytes: the wrapper of binary content, base64 and text views, files
  util.cljs           case translation, secrets, addresses, configuration from the environment, errors
  smtp_binding.cljs   the nodemailer interop
  imap_binding.cljs   the imapflow interop
  parse_binding.cljs  the mailparser, Buffer, file and environment interop
dev/email_cljs/
  examples.cljs       the examples of this page, offline
  smoke.cljs          a live check against a mail account, run by hand
docs/
  email-cljs.org            the manual
  email-api-inventory.org   the API inventory
```

## Development

```bash
npm install
./run-tests.sh     # the publicity gate, then the unit suite; no network
npx shadow-cljs compile examples && node target/examples.js   # the examples of this page; no network
```

The suite covers the translation of messages and options, SMTP through a transport
that runs the message normalisation of nodemailer, IMAP over a fake connection with
the search compiler of imapflow run offline, building and parsing in the process, and
the examples of this page: it fails when this page shows code or a result that the
examples do not produce. `./run-tests.sh` ends by replacing its shell with node, so
nothing can follow it on the same command line.

The live check talks to a mail server and is run by hand, with an account of yours:

```bash
npx shadow-cljs compile smoke && node target/smoke.js
```

It sends three messages, reads them back and deletes them. It is written for the
account of its author: read `dev/email_cljs/smoke.cljs` before running it against
yours.
