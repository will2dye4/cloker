(ns cloker.odds
  (:require [clojure.math.combinatorics :refer [combinations count-combinations]]
            [cloker.constants :refer [board-size]]
            [cloker.game :refer [current-board players]]
            [cloker.rating :refer [winners]]
            [cloker.utils :refer [map-keys map-vals percentage]]))

(defn- player-win-counts [players board all-candidates]
  (->> all-candidates
       (pmap #(let [winners (winners players (concat board %))]
                (if (> (count winners) 1)
                  :tie
                  (:id (:player (first winners))))))
       frequencies))

(defn all-player-odds [game]
  (let [{:keys [deck players]} game
        board (current-board game)
        n (- board-size (count board))
        all-candidates (combinations deck n)
        num-candidates (count-combinations deck n)
        win-counts (player-win-counts (vals players) board all-candidates)]
    (map-keys #(percentage (win-counts % 0) num-candidates) (conj (keys players) :tie))))

(defn player-odds [game player]
  ((all-player-odds game) (:id player)))
