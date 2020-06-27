(ns cloker.odds
  (:require [clojure.math.combinatorics :refer [combinations count-combinations]]
            [cloker.constants :refer [board-size]]
            [cloker.game :refer [current-board players]]
            [cloker.rating :refer [winners]]
            [cloker.utils :refer [map-keys map-vals percentage]]))

(defn- player-win-counts [players board all-candidates]
  (loop [player-wins (into {:tie 0} (map-keys (constantly 0) (keys players)))
         all-candidates all-candidates]
    (if-let [candidate-cards (first all-candidates)]
      (let [potential-board (concat board candidate-cards)
            winners (winners (vals players) potential-board)
            key (if (> (count winners) 1)
                  :tie
                  (:id (:player (first winners))))]
        (recur (update player-wins key inc) (rest all-candidates)))
      player-wins)))

(defn all-player-odds [game]
  (let [board (current-board game)
        deck (:deck game)
        n (- board-size (count board))
        all-candidates (combinations deck n)
        num-candidates (count-combinations deck n)
        win-counts (player-win-counts (:players game) board all-candidates)]
    (map-vals #(percentage % num-candidates) win-counts)))
