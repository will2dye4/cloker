(ns cloker.stats
  (:require [cloker.cards :refer :all]
            [cloker.constants :refer [board-size]]
            [cloker.game :refer [winners]]
            [cloker.rating :refer [hand-types rate-hand]]
            [cloker.utils :refer :all]))

(def ^:const heading-width 42)

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

(def ^:private format-percentage (partial format "(%.2f%%)"))

(defn- format-line [name occurrences frequency]
  (format "%16s %,8d %16s" name occurrences (format-percentage frequency)))

(defn show-outcomes [hand-freqs key]
  (let [heading (str ({:total "All" :wins "Winning"} key) " Outcomes")
        sum-field (fn [field] (reduce + (map #(get-in (val %) [key field]) hand-freqs)))
        total-outcomes (sum-field :occurrences)
        frequency-total (sum-field :frequency)]
    (println (center-heading heading heading-width))
    (doseq [[hand-type counts] hand-freqs]
      (let [name (:name hand-type)
            occurrences (get-in counts [key :occurrences])
            frequency (get-in counts [key :frequency])]
        (println (format-line name occurrences frequency))))
    (println (repeat-char \- heading-width))
    (println (format-line "Total" total-outcomes frequency-total))))

(defn show-hand-strength [hand-freqs]
  (println (center-heading "Hand Strength" (+ 8 heading-width)))
  (doseq [[hand-type counts] hand-freqs]
    (let [name (:name hand-type)
          total (get-in counts [:total :occurrences])
          wins (get-in counts [:wins :occurrences])
          ratio (format "%,d / %,d" wins total)
          win-frequency (format-percentage (:hand-won-frequency counts))]
      (println (format "%16s %18s %14s" name ratio win-frequency)))))

(defn run-hand [num-players]
  (let [deck (shuffle (new-deck))
        [hands deck] (draw-hands num-players deck)
        [board _] (draw board-size deck)
        ratings (for [[i hand] (enumerate hands)]
                  {:player {:id (inc i) :hand hand}
                   :rating (rate-hand (concat hand board))})]
    {:board board
     :ratings ratings
     :winners (winners ratings)}))

(defn run-hands [& {:keys [n num-players] :or {n 1000 num-players 4}}]
  {:pre [(pos? n) (< 1 num-players 24)]}
  (let [hand-results (repeatedly n #(run-hand num-players))
        hand-freqs (hand-frequencies hand-results)]
    (show-outcomes hand-freqs :total)
    (println)
    (show-outcomes hand-freqs :wins)
    (println)
    (show-hand-strength hand-freqs)))
