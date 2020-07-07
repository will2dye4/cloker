(ns cloker.utils
  (:require [clojure.string :as str])
  (:import (java.io EOFException Writer)))

(def enumerate (partial map-indexed vector))

(def sum (partial reduce +))

(defn consecutive-pairs [coll] (map vector coll (drop 1 coll)))

(defn rotate
  "Take a sequence and left rotates it n steps. If n is negative, the collection is rotated right."
  [n coll]
  (let [c (count coll)] (take c (drop (mod n c) (cycle coll)))))

(defn map-keys
  ([val-fn keys] (map-keys val-fn {} keys))
  ([val-fn init keys]
    (into init (map #(vector % (val-fn %)) keys))))

(defn map-vals
  ([f m]
    (map-vals f {} m))
  ([f init m]
    (reduce-kv #(assoc %1 %2 (f %3)) init m)))

(defn keyword->name [keyword]
  (-> keyword
      name
      (.replace "-" " ")
      str/capitalize))

(defn make-printable
  ([type] (make-printable type symbol))
  ([type coerce]
    (defmethod print-method type [object ^Writer writer]
      (print-method (coerce (str object)) writer))
    (defmethod print-dup type [object ^Writer writer]
      (print-ctor object (fn [o w] (print-dup (vals o) w)) writer))
    (defmethod clojure.pprint/simple-dispatch type [object]
      (.write ^Writer *out* (str object)))))

(defn value-comparator [m]
  (fn [key1 key2]
    (compare (get m key1) (get m key2))))

(defn sorted-map-by-value [m]
  (into (sorted-map-by (value-comparator m)) m))

(defn percentage [numer denom]
  (if (zero? denom)
    0.0
    (* 100.0 (/ numer denom))))

(defn repeat-char [c n]
  (apply str (repeat n c)))

(defn center-heading [heading width]
    (let [padding (- width (+ 2 (count heading)))
          left-padding (int (/ padding 2))
          right-padding (if (odd? padding) (inc left-padding) left-padding)]
      (str (repeat-char \= left-padding) \space heading \space (repeat-char \= right-padding))))

(defn input [prompt]
  (print prompt)
  (flush)
  (let [raw-input (read-line)]
    (if (nil? raw-input)
      (throw (EOFException.))
      raw-input)))
