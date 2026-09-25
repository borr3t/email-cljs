(ns email-cljs.attachment
  "The opaque Bytes wrapper: the library's carrier for binary content
   (attachment content going out, message source / downloaded parts coming
   in). A Bytes holds ONE Node Buffer (never copied into a base64 string
   eagerly — inventory 9.3) and offers lazy views: ->base64 (the
   serialisable form), ->string (charset-decoded text), ->ints, size.
   Prints as #email-cljs/bytes {:size N}; equal when the bytes are equal.

   Only email-cljs.parse-binding touches the Buffer; this namespace is
   interop-free. Phase 4 adds the attachment constructor/validation,
   read-file! / write-file! and the mailparser-side helpers here."
  (:refer-clojure :exclude [bytes bytes?])
  (:require [email-cljs.parse-binding :as pb]
            [email-cljs.util :as util]))

(defprotocol IBytes
  (unwrap [this] "The underlying Node Buffer. Binding namespaces only."))

(deftype Bytes [buf]
  IBytes
  (unwrap [_] buf)
  IEquiv
  (-equiv [_ other] (and (satisfies? IBytes other) (pb/buffer-equals? buf (unwrap other))))
  IHash
  (-hash [_] (hash (pb/buffer->base64 buf)))
  ICounted
  (-count [_] (pb/buffer-length buf))
  IPrintWithWriter
  (-pr-writer [_ writer _] (-write writer (str "#email-cljs/bytes {:size " (pb/buffer-length buf) "}")))
  Object
  (toString [_] (str "#email-cljs/bytes {:size " (pb/buffer-length buf) "}")))

(defn bytes?
  "True when `x` is a Bytes wrapper."
  [x]
  (satisfies? IBytes x))

(defn bytes
  "Wraps binary content into Bytes: an existing Bytes (identity), a vector
   of byte values (0-255), an opaque Buffer / Uint8Array handed over by a
   binding namespace, or a string (encoded as UTF-8). nil -> nil. Anything
   else -> :email-cljs/argument-error."
  [x]
  (cond
    (nil? x) nil
    (bytes? x) x
    (pb/buffer? x) (Bytes. x)
    (pb/uint8-array? x) (Bytes. (pb/buffer-from-uint8 x))
    (string? x) (Bytes. (pb/buffer-from-string x "utf8"))
    (and (sequential? x) (every? number? x)) (Bytes. (pb/buffer-from-ints x))
    :else (throw (util/argument-error "cannot make Bytes from this value"
                                      {:reason :not-bytes :value-type (pr-str (type x))}))))

(defn from-base64
  "Bytes from a base64 string (the 'base64 view in')."
  [s]
  (Bytes. (pb/buffer-from-base64 s)))

(defn from-string
  "Bytes from text; `encoding` is a Node encoding name (\"utf8\" default,
   \"latin1\", \"ascii\", \"utf16le\", \"hex\")."
  ([s] (from-string s "utf8"))
  ([s encoding] (Bytes. (pb/buffer-from-string s encoding))))

(defn ->base64
  "The base64 string of the bytes (computed on demand, never cached)."
  [b]
  (pb/buffer->base64 (unwrap b)))

(defn ->string
  "The bytes decoded as text (\"utf8\" default; see from-string for the
   encodings Node decodes natively — iconv charsets arrive with Phase 4)."
  ([b] (->string b "utf8"))
  ([b encoding] (pb/buffer->string (unwrap b) encoding)))

(defn ->ints
  "Vector of byte values."
  [b]
  (pb/buffer->ints (unwrap b)))

(defn size
  "Byte length."
  [b]
  (pb/buffer-length (unwrap b)))

(defn concat-bytes
  "One Bytes from several."
  [& bs]
  (Bytes. (pb/buffer-concat (map unwrap bs))))

(defn slice
  "Bytes [start, end) of `b` (end defaults to the size)."
  ([b start] (slice b start nil))
  ([b start end] (Bytes. (pb/buffer-slice (unwrap b) start end))))
