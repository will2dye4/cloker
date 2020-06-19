(ns cloker.utils)

(defn make-printable [type printer]
  (defmethod print-method type [object ^java.io.Writer writer]
    (print-method (printer object) writer))
  (defmethod print-dup type [object ^java.io.Writer writer]
    (print-ctor object (fn [o w] (print-dup (vals o) w)) writer))
  (defmethod clojure.pprint/simple-dispatch type [object]
    (.write *out* (str (printer object)))))

(defn value-comparator [m]
  (fn [key1 key2]
    (compare (:value (get m key1)) (:value (get m key2)))))

(defn sorted-map-by-value [m]
  (into (sorted-map-by (value-comparator m)) m))
