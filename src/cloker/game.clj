(ns cloker.game
  (:require [cloker.rating :refer [hand-rating-sort-key]]))

(defn winners [player-ratings]
  (->> player-ratings
       (sort-by :rating)
       (partition-by (comp hand-rating-sort-key :rating))
       last))
