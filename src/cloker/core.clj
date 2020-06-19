(ns cloker.core
  (:require [clojure.pprint :refer [pprint]]
            [cloker.cards :refer :all])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [[hands deck] (draw-hands 3 shuffled-deck)]
    (pprint hands)))
