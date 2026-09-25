(ns email-cljs.attachment-test
  (:require [cljs.test :refer [deftest is testing]]
            [email-cljs.attachment :as att]
            [email-cljs.test-util :as tu]
            [email-cljs.util :as util]))

(deftest bytes-wrapper-views
  (let [b (att/bytes "héllo")]
    (is (att/bytes? b))
    (is (= 6 (att/size b)) "UTF-8 length")
    (is (= 6 (count b)))
    (is (= "héllo" (att/->string b)))
    (is (= "aMOpbGxv" (att/->base64 b)))
    (is (= [104 195 169 108 108 111] (att/->ints b)))
    (is (= "#email-cljs/bytes {:size 6}" (pr-str b)) "prints without leaking content")
    (is (= "#email-cljs/bytes {:size 6}" (str b)))))

(deftest bytes-constructors-and-equality
  (is (= (att/bytes "hi") (att/from-base64 "aGk=")) "base64 view in")
  (is (= (att/bytes "hi") (att/bytes [104 105])) "vector of ints")
  (is (= (att/bytes "hi") (att/from-string "hi")))
  (is (= (att/from-string "aGk=" "base64") (att/bytes "hi")) "from-string with a Node encoding")
  (is (not= (att/bytes "hi") (att/bytes "ho")))
  (is (= (hash (att/bytes "hi")) (hash (att/from-base64 "aGk="))))
  (is (let [b (att/bytes "x")] (identical? b (att/bytes b))) "idempotent")
  (is (nil? (att/bytes nil)))
  (is (= "hi" (att/->string (att/bytes (tu/buffer-of "hi")))) "opaque Buffer accepted")
  (is (= :email-cljs/argument-error (util/error-type (tu/thrown #(att/bytes 42))))))

(deftest bytes-round-trip-base64
  (let [b64 "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="
        b (att/from-base64 b64)]
    (is (= b64 (att/->base64 b)) "lazy base64 out equals base64 in")
    (is (= 70 (att/size b)))
    (is (= (att/bytes "PNG") (att/slice b 1 4)) "slice")
    (is (= (att/bytes "ab") (att/concat-bytes (att/bytes "a") (att/bytes "b"))))))
