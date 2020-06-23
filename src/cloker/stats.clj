(ns cloker.stats
  (:require [cloker.cards :refer :all]
            [cloker.constants :refer [board-size]]
            [cloker.game :refer [winners]]
            [cloker.rating :refer [hand-types rate-hand]]
            [cloker.utils :refer :all]))

(def ^:const heading-width 42)

(defn- format-line [name occurrences frequency]
  (format "%16s %,8d\t\t%9s" name occurrences (format "(%.2f%%)" frequency)))

(defn- show-outcomes [heading outcomes]
  (let [flattened-outcomes (flatten outcomes)
        total-outcomes (count flattened-outcomes)
        grouped-outcomes (group-by (comp :hand-type :rating) flattened-outcomes)
        occurrence-counts (for [hand-type (vals hand-types)]
                            (let [occurrences (count (grouped-outcomes hand-type))
                                  frequency (* 100.0 (/ occurrences total-outcomes))]
                              [(:name hand-type) occurrences frequency]))
        frequency-total (reduce + (map last occurrence-counts))]
    (println (center-heading heading heading-width))
    (doseq [counts occurrence-counts]
      (println (apply format-line counts)))
    (println (repeat-char \- heading-width))
    (println (format-line "Total" total-outcomes frequency-total))))

(defn group-by-hand-type [outcomes]
  (group-by (comp :hand-type :rating) (flatten outcomes)))

(defn- show-hand-strength [hand-results]
  (let [all-outcomes (group-by-hand-type (map :ratings hand-results))
        winning-outcomes (group-by-hand-type (map :winners hand-results))
        win-counts (for [hand-type (vals hand-types)]
                     (let [total (count (all-outcomes hand-type))
                           wins (count (winning-outcomes hand-type))
                           win-freq (if (zero? total) 0.0 (* 100.0 (/ wins total)))]
                       [(:name hand-type) wins total win-freq]))]
    (println (center-heading "Hand Strength" (+ 4 heading-width)))
    (doseq [[name wins total frequency] win-counts]
      (let [ratio (format "%,d / %,d" wins total)
            freq (format "(%.2f%%)" frequency)]
        (println (format "%16s %14s\t\t%9s" name ratio freq))))))

(defn run-hand [num-players]
  (let [deck (shuffle (new-deck))
        [hands deck] (draw-hands num-players deck)
        [board _] (draw 5 deck)
        ratings (for [[i hand] (enumerate hands)]
                  {:player {:id (inc i) :hand hand}
                   :rating (rate-hand (concat hand board))})]
    {:board board
     :ratings ratings
     :winners (winners ratings)}))

(defn run-hands [& {:keys [n num-players] :or {n 1000 num-players 4}}]
  {:pre [(pos? n) (< 1 num-players 24)]}
  (let [hand-results (repeatedly n #(run-hand num-players))]
    (show-outcomes "All Outcomes" (map :ratings hand-results))
    (println)
    (show-outcomes "Winning Outcomes" (map :winners hand-results))
    (println)
    (show-hand-strength hand-results)))
