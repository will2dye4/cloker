(ns cloker.core
  (:require [clojure.pprint :refer [pprint]]
            [cloker.cards :refer :all]
            [cloker.rating :refer :all])
  (:gen-class))

(def deck (new-deck))

(def shuffled-deck (shuffle deck))

(def cards (take 10 shuffled-deck))

(defn run-hand [num-players]
  (let [deck (shuffle (new-deck))
        [hands deck] (draw-hands num-players deck)
        [board _] (draw 5 deck)]
    (for [hand hands]
      {:hand hand
       :rating (rate-hand (concat hand board))
       :board board})))

(defn run-hands [& {:keys [n num-players] :or {n 1000 num-players 4}}]
  {:pre [(pos? n) (< 1 num-players 24)]}
  (let [total (* n num-players)
        outcomes (flatten (for [_ (range n)] (run-hand num-players)))
        grouped-outcomes (group-by (comp :hand-type :rating) outcomes)
        occurrence-counts (for [hand-type (vals hand-types)]
                            (let [occurrences (count (grouped-outcomes hand-type))
                                  frequency (* 100.0 (/ occurrences total))]
                              [(:name hand-type) occurrences frequency]))
        occurrence-total (reduce + (map second occurrence-counts))
        frequency-total (reduce + (map last occurrence-counts))]
    (println "============= All Outcomes =============")
    (doseq [[name occurrences frequency] occurrence-counts]
      (println (format "%16s %,8d\t\t(%.2f%%)" name occurrences frequency)))
    (println "----------------------------------------")
    (println (format "Total            %,8d     (%.2f%%)" occurrence-total frequency-total))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (run-hands))
