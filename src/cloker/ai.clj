(ns cloker.ai
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [cloker.game :refer [default-action-fn]]
            [cloker.outs :refer [draw-type->hand-type player-draws round->probability-key]]
            [cloker.rating :refer [rate-hand]])
  (:import (java.io PushbackReader)))

(def occur-frequencies
  {:high-card 13
   :pair 41
   :two-pair 27
   :three-of-a-kind 7
   :straight 5
   :flush 3
   :full-house 4
   :four-of-a-kind 0.4
   :straight-flush 0.04
   :royal-flush 0.00004})

(def win-frequencies (-> "win-frequencies.edn" io/resource io/reader PushbackReader. edn/read))

(def ^:private max-num-players (apply max (keys win-frequencies)))

(defn win-frequency [num-players hand-type]
  (get-in win-frequencies [(min num-players max-num-players) hand-type]))

(defn draw->win%
  ([draw-probability win-probability]
    (/ (* draw-probability win-probability) 100.0))
  ([draw-probability num-players hand-type]
    (draw->win% draw-probability (win-frequency num-players hand-type))))

(defn potential-win% [draw-probabilities num-players]
  (reduce +
    (for [[draw-type draw-probability] draw-probabilities
          :let [hand-type (draw-type->hand-type draw-type)]]
      (if (keyword? hand-type)
        (draw->win% draw-probability num-players hand-type)
        (let [weighted-sum (reduce + (map #(* (val %) (win-frequency num-players (key %))) hand-type))
              weighted-mean (/ weighted-sum (reduce + (map val hand-type)))]
          (draw->win% draw-probability weighted-mean))))))

(defn ai-action-fn [state]
  (let [{:keys [player board round num-players]} state
        hand (:hand player)
        cards (concat hand board)
        rating (rate-hand cards)
        hand-type (:key (:hand-type rating))
        current-win% (win-frequency num-players hand-type)
        draws (player-draws hand board)
        probability-key (round->probability-key round)
        draw-probabilities (when probability-key (into {} (map (juxt :draw-type probability-key) draws)))
        default-action (default-action-fn state)
        action (merge default-action {:hand-type hand-type
                                      :current-win% current-win%
                                      :potential-win% (potential-win% draw-probabilities num-players)
                                      :draw-probabilities draw-probabilities})]
    (prn action)
    action))
