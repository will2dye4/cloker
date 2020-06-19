(ns cloker.utils)

(defn make-printable [type printer]
  (defmethod print-method type [object ^java.io.Writer writer]
    (print-method (printer object) writer))
  (defmethod print-dup type [object ^java.io.Writer writer]
    (print-ctor object (fn [o w] (print-dup (vals o) w)) writer))
  (defmethod clojure.pprint/simple-dispatch type [object]
    (.write *out* (str (printer object)))))
