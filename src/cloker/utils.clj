(ns cloker.utils
  (:require [clojure.string :as str])
  (:import (java.io Writer)))

(def enumerate (partial map-indexed vector))

(defn consecutive-pairs [coll] (map vector coll (drop 1 coll)))

(defn keyword->name [keyword]
  (-> keyword
      name
      (.replace "-" " ")
      str/capitalize))

(defn make-printable [type]
  (defmethod print-method type [object ^Writer writer]
    (print-method (symbol (str object)) writer))
  (defmethod print-dup type [object ^Writer writer]
    (print-ctor object (fn [o w] (print-dup (vals o) w)) writer))
  (defmethod clojure.pprint/simple-dispatch type [object]
    (.write ^Writer *out* (str object))))

(defn value-comparator [m]
  (fn [key1 key2]
    (compare (get m key1) (get m key2))))

(defn sorted-map-by-value [m]
  (into (sorted-map-by (value-comparator m)) m))
