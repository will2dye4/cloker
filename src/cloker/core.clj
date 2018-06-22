(ns cloker.core
  (:require [clojure.pprint :refer [pprint]])
  (:gen-class))

(def cards-per-hand 2)

(def suits #{:spades :hearts :clubs :diamonds})

(def values #{:ace :2 :3 :4 :5 :6 :7 :8 :9 :10 :jack :queen :king})

(defstruct card :suit :value)

(def deck (for [suit suits value values] (struct card suit value)))

(def shuffled-deck (shuffle deck))

(defn draw-hands [n deck]
  (loop [hands (vec (repeat n [])) 
         i 0
         cards-dealt 0
         deck deck]
    (if (< cards-dealt cards-per-hand)
      (if (< i n)
        (recur (assoc hands i (conj (nth hands i) (first deck))) (inc i) cards-dealt (vec (drop 1 deck)))
        (recur hands 0 (inc cards-dealt) deck))
      [hands deck]))) 

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [[hands deck] (draw-hands 3 shuffled-deck)]
    (pprint hands)))

