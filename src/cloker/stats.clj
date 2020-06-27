(ns cloker.stats
  (:require [cloker.cards :refer [draw draw-hands new-deck]]
            [cloker.constants :refer [board-size]]
            [cloker.rating :refer [hand-types rate-hands winners]]
            [cloker.utils :refer [enumerate map-keys map-vals percentage]]))

(defn- results->outcomes [hand-results key]
  (let [outcomes (flatten (map key hand-results))]
    {:total (count outcomes)
     :hand-counts (->> outcomes
                       (group-by (comp :hand-type :rating))
                       (map-vals count))}))

(defn- hand-type->counts [stats hand-type]
  (let [total-occurrences (get-in stats [:ratings :hand-counts hand-type] 0)
        winning-occurrences (get-in stats [:winners :hand-counts hand-type] 0)]
    {:total {:occurrences total-occurrences
             :frequency (percentage total-occurrences (get-in stats [:ratings :total]))}
     :wins {:occurrences winning-occurrences
            :frequency (percentage winning-occurrences (get-in stats [:winners :total]))}
     :hand-won-frequency (percentage winning-occurrences total-occurrences)}))

(defn hand-frequencies [hand-results]
  (let [stats (map-keys (partial results->outcomes hand-results) [:ratings :winners])]
    (map-keys (partial hand-type->counts stats) (sorted-map) (vals hand-types))))

(defn run-hand [num-players]
  {:pre [(< 1 num-players 24)]}
  (let [deck (shuffle (new-deck))
        [hands deck] (draw-hands num-players deck)
        [board _] (draw board-size deck)
        players (for [[i hand] (enumerate hands)] {:id (inc i) :hand hand})
        ratings (rate-hands players board)]
    {:board board
     :ratings ratings
     :winners (winners ratings)}))

(defn run-hands [n num-players]
  {:pre [(pos? n) (< 1 num-players 24)]}
  (->> #(run-hand num-players)
       (repeatedly n)
       hand-frequencies))
