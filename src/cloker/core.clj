(ns cloker.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [cloker.cards :refer :all]
            [cloker.cli :refer :all]
            [cloker.game :refer :all]
            [cloker.odds :refer :all]
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

(defn play-game [& {:keys [mode] :or {mode :interactive}}]
  (println "Welcome! A new game is starting.\n")
  (try
    (let [prompt "\nWould you like to play another hand? "]
      (loop [game (new-cli-game mode)]
        (let [game (play-hand game)
              players (players game)]
          (if (= 1 (count players))
            (println (str (:name (first players)) " has won the game!"))
            (when-not (#{"n" "no"} (-> (input prompt) str/trim str/lower-case))
              (println)
              (recur game))))))
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
  (condp = (vec args)
    [] (play-game)
    ["play"] (play-game)
    ["play" "auto"] (play-game :mode :auto)
    ["play" "interactive"] (play-game :mode :interactive)
    ["play" "single-player"] (play-game :mode :single-player)
    ["play" "solo"] (play-game :mode :single-player)
    ["stats"] (hand-stats)
    (println "Usage: lein run (play [auto|interactive|single-player]|stats")))
