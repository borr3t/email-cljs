(ns email-cljs.parse-binding
  "Binding namespace (JavaScript interop lives here): the Node Buffer, fs,
   os and process.env touches the rest of the library needs — the carrier
   behind the opaque Bytes wrapper (email-cljs.attachment), the secrets-file
   read and the environment map behind util/from-env. Phase 4 adds the
   mailparser calls here (inventory 14.4 keeps the binding namespaces at
   three: smtp-binding, imap-binding, parse-binding).

   Everything exported takes and returns CLJS data or an opaque Buffer /
   stream handle that callers never touch; no other namespace outside the
   bindings mentions Buffer."
  (:require ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [promesa.core :as p]))

;; ---------------------------------------------------------------------------
;; Buffer
;; ---------------------------------------------------------------------------

(defn buffer?
  "True when `x` is a Node Buffer."
  [x]
  (and (some? x) (js/Buffer.isBuffer x)))

(defn uint8-array?
  "True when `x` is a Uint8Array (or ArrayBuffer view) that is not a Buffer."
  [x]
  (and (some? x) (instance? js/Uint8Array x) (not (buffer? x))))

(defn buffer-from-base64
  "Decodes a base64 string (standard or URL-safe alphabet) into a Buffer."
  [s]
  (js/Buffer.from (str s) "base64"))

(defn buffer-from-string
  "Encodes a string into a Buffer; `encoding` is a Node encoding name
   (\"utf8\" default, \"latin1\", \"ascii\", \"utf16le\", \"hex\", \"base64\")."
  [s encoding]
  (js/Buffer.from (str s) (or encoding "utf8")))

(defn buffer-from-ints
  "A Buffer from a sequential collection of byte values (0-255)."
  [ints]
  (js/Buffer.from (into-array ints)))

(defn buffer-from-uint8
  "A Buffer sharing the memory of a Uint8Array."
  [u]
  (js/Buffer.from u))

(defn buffer->base64
  "Standard base64 string of the Buffer."
  [b]
  (js-invoke b "toString" "base64"))

(defn buffer->string
  "Decodes the Buffer as text; `encoding` as for buffer-from-string
   (iconv charsets are Phase 4)."
  [b encoding]
  (js-invoke b "toString" (or encoding "utf8")))

(defn buffer->ints
  "Vector of byte values."
  [b]
  (vec (js/Array.from b)))

(defn buffer-length
  "Byte length."
  [b]
  (unchecked-get b "length"))

(defn buffer-equals?
  "Byte-wise equality of two Buffers."
  [a b]
  (js-invoke a "equals" b))

(defn buffer-concat
  "One Buffer from a sequence of Buffers."
  [bufs]
  (js/Buffer.concat (into-array bufs)))

(defn buffer-slice
  "A copy of bytes [start, end) (end defaults to the length)."
  [b start end]
  (js/Buffer.from (js-invoke b "subarray" start (or end (buffer-length b)))))

(defn stream->buffer
  "Drains a Node Readable into one Buffer. Promise; rejects with the stream's
   error. `max-bytes` (optional) rejects with an :email-cljs/argument-error-free
   plain js/Error tagged code \"DownloadOverflow\" once exceeded so the
   caller's error mapping treats it like imapflow's own overflow."
  ([readable] (stream->buffer readable nil))
  ([readable max-bytes]
   (p/create
    (fn [resolve reject]
      (let [chunks (array)
            total (atom 0)]
        (js-invoke readable "on" "data"
                   (fn [chunk]
                     (let [chunk (if (buffer? chunk) chunk (js/Buffer.from chunk))]
                       (swap! total + (buffer-length chunk))
                       (if (and max-bytes (> @total max-bytes))
                         (let [err (js/Error. (str "download exceeded " max-bytes " bytes"))]
                           (unchecked-set err "code" "DownloadOverflow")
                           (when (fn? (unchecked-get readable "destroy")) (js-invoke readable "destroy"))
                           (reject err))
                         (.push chunks chunk)))))
        (js-invoke readable "on" "error" (fn [err] (reject err)))
        (js-invoke readable "on" "end" (fn [] (resolve (js/Buffer.concat chunks)))))))))

;; ---------------------------------------------------------------------------
;; Environment and files (util/from-env, secrets file)
;; ---------------------------------------------------------------------------

(defn env-map
  "process.env as a {\"NAME\" \"value\"} map (walked key by key: process.env
   is an exotic object js->clj leaves untouched)."
  []
  (let [env (unchecked-get js/process "env")]
    (into {} (map (fn [k] [k (unchecked-get env k)])) (js/Object.keys env))))

(defn expand-home
  "Replaces a leading `~` with the home directory."
  [p]
  (let [p (str p)]
    (if (or (= p "~") (.startsWith p "~/"))
      (path/join (os/homedir) (subs p 1))
      p)))

(defn read-file-text
  "The UTF-8 text of a file (`~` expanded), or nil when it does not exist or
   cannot be read. Synchronous: only used for configuration at start-up."
  [p]
  (when (and (some? p) (not= "" (str p)))
    (try
      (fs/readFileSync (expand-home p) "utf8")
      (catch :default _ nil))))

(defn file-exists?
  "True when the path (`~` expanded) exists."
  [p]
  (try (fs/existsSync (expand-home p)) (catch :default _ false)))

;; ---------------------------------------------------------------------------
;; Timers (the IDLE watcher's restart timer; unref'd so a stopped watcher
;; never keeps the process alive)
;; ---------------------------------------------------------------------------

(defn set-timer
  "Schedules `f` after `ms` milliseconds; returns an opaque timer handle that
   does not keep the Node process alive."
  [ms f]
  (let [t (js/setTimeout f ms)]
    (when (fn? (unchecked-get t "unref")) (js-invoke t "unref"))
    t))

(defn clear-timer
  "Cancels a timer handle from set-timer (nil-safe)."
  [t]
  (when (some? t) (js/clearTimeout t)))

(defn now-ms
  "Milliseconds since the epoch."
  []
  (.now js/Date))
