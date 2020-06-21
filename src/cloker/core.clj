(ns cloker.core
  (:require [clojure.pprint :refer [pprint]]
            [cloker.cards :refer :all]
            [cloker.rating :refer :all])
  (:gen-class))

(def deck (new-deck))

(def shuffled-deck (shuffle deck))

(def cards (take 10 shuffled-deck))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (sort cards))
  (pprint (rate-hand cards)))
