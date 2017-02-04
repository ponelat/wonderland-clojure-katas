(ns alphabet-cipher.coder
  "Alphabet-coder katana"
  (:use [clojure.string :only [join]]))

(def letter-a (int \a))
(def mod-26 #(mod % 26))

(defn !alpha
  "a => 0, b => 1, etc"
  [letter]
  (mod-26
   (- (int letter) letter-a)))

(defn alpha
  "0 => a, 1 => b, etc"
  [c]
  (char (+ letter-a (mod-26 c))))

(defn shift-letter
  "Shift the _base_ by (alpha offset)"
  [base offset]
  (alpha (+ (!alpha base) (!alpha offset))))

(defn unshift-letter
  "Unshift the _base_ by _offset_"
  [base offset]
  (alpha (- (!alpha base) (!alpha offset))))

(defn slice
  "Slice a string, from start to start + length. 
   while protecting against out-of-bounds"
  ([str len] (slice str 0 len))
  ([str start len]
   (subs str start (min (+ start len) (count str)))))
(defn multiply-str
  [a-str factor] (join (repeat factor a-str)))

(defn keyword-padding
  "returns a string of _size_ length, made from repeating _keyword_"
  [keyword size]
  (if (>= (count keyword) size)
    (slice keyword size)
    (keyword-padding (multiply-str keyword (Math/ceil (/ size (count keyword)))) size)))

(defn combine-two-strings
  [f str-a str-b]
  (join (map #(apply f %) (map vector str-a str-b))))

(def shift-string (partial combine-two-strings shift-letter))
(def unshift-string (partial combine-two-strings unshift-letter))

(defn guess-cipher
 "Can only guess, if the key is shorter than the plaintext + ciphertext"
 ([msg] (guess-cipher (subs msg 0 3) (subs msg 3)))
 ([key msg]
  (if (= key (slice msg (count key))) key
      (guess-cipher (str key (subs msg 0 1)) (subs msg 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Public API

(defn encode [keyword message]
  (let [keystr (keyword-padding keyword (count message))]
    (shift-string message keystr)))

(defn decode [keyword message]
  (let [keystr (keyword-padding keyword (count message))]
    (unshift-string message keystr)))

(defn decipher [cipher message]
  (guess-cipher (decode message cipher)))
