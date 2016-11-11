(ns alphabet-cipher.coder
  (:require [clojure.string :as string]))

(def offset (int \a))
(def alphabet-size 26)

(defn index
  [c]
  (- (int c) offset))

(defn index->char
  [i]
  (char (+ offset i)))

(defn- cipher-map 
  [keyword message f]
  (string/join
    (map f 
         (cycle (string/lower-case keyword)) 
         (string/lower-case message))))

(defn- encode-char
  [key-char plain-char] 
  (index->char
    (mod (+ (index key-char) (index plain-char))
         alphabet-size)))

(defn encode 
  [keyword message]
  (cipher-map keyword message encode-char))

(defn- decode-char
  [key-char cipher-char]
  (index->char
    (mod (+ (- alphabet-size (index key-char)) (index cipher-char))
         alphabet-size)))

(defn decode
  [keyword message]
  (cipher-map keyword message decode-char))

(defn- derepeat-cipher
  ([repeated-cipher]
   (derepeat-cipher (seq repeated-cipher) 1))
  ([repeated-cipher n]
   (let [test-cipher (take n repeated-cipher)
         length (count repeated-cipher)]
     (if (and (not (> n length))
              (= repeated-cipher
                 (take length (cycle test-cipher))))
       (string/join test-cipher)
       (recur repeated-cipher (inc n))))))

(defn decipher 
  [cipher message]
  (derepeat-cipher (cipher-map message cipher decode-char)))

