(ns cloker.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [cloker.cards :refer :all]
            [cloker.game :refer :all]
            [cloker.rating :refer :all]
            [cloker.stats :refer :all]
            [cloker.utils :refer :all])
  (:gen-class))

(def deck (new-deck))

(def shuffled-deck (shuffle deck))

(defn c [rank-suit]
  {:pre [(string? rank-suit) (< 1 (count rank-suit) 4)]}
  (let [[r s] (if (= 2 (count rank-suit))
                (str/split (str/upper-case rank-suit) #"")
                [(.substring rank-suit 0 2) (.substring rank-suit 2)])
        rank (->> ranks
                  vals
                  (filter #(= r (:symbol %)))
                  first)
        suit (suits ({"C" :clubs "D" :diamonds "H" :hearts "S" :spades} s))]
    (when (and rank suit)
      (->Card rank suit))))

(defn -main
  "Simulate 1,000 hands with 4 players and show a summary of the results."
  [& args]
  (run-hands))
