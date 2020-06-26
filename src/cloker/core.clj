(ns cloker.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [cloker.cards :refer :all]
            [cloker.cli :refer [show-hand-strength show-outcomes]]
            [cloker.game :refer :all]
            [cloker.rating :refer :all]
            [cloker.stats :refer :all]
            [cloker.utils :refer :all])
  (:import (java.io EOFException))
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

(defn play-game []
  (println "Welcome! A new game is starting.\n")
  (try
    (let [prompt "\nWould you like to play another hand? "]
      (loop [game (new-game)]
        (let [game (play-hand game)
              response (-> (input prompt) str/trim str/lower-case)]
          (when-not (#{"n" "no"} response)
            (println)
            (recur game)))))
    (catch EOFException _ (println)))  ;; force a newline
  (println "Goodbye!"))

(defn hand-stats [& {:keys [n num-players] :or {n 1000 num-players 4}}]
  (println (format "Simulating %,d hands with %d players...\n" n num-players))
  (let [hand-freqs (run-hands n num-players)]
    (show-outcomes hand-freqs :total)
    (println)
    (show-outcomes hand-freqs :wins)
    (println)
    (show-hand-strength hand-freqs)))

(defn -main
  "Play an interactive game of poker."
  [& args]
  (play-game))
