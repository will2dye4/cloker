(ns cloker.ai.naive
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [cloker.ai.pre-flop :refer [pre-flop-action-fn]]
            [cloker.ai.utils :refer [debug-ai-actions? select-action]]
            [cloker.game :refer [default-action-fn]]
            [cloker.outs :refer [draw-type->hand-type player-draws round->probability-key]]
            [cloker.rating :refer [rate-hand]]
            [cloker.utils :refer [sum]]))

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

(def win-frequencies (-> "win-frequencies.edn" io/resource slurp edn/read-string))

(def ^:private max-num-players (apply max (keys win-frequencies)))

(defn win-frequency [num-players hand-type]
  (get-in win-frequencies [(min num-players max-num-players) hand-type]))

(defn draw->win%
  ([draw-probability win-probability]
    (/ (* draw-probability win-probability) 100.0))
  ([draw-probability num-players hand-type]
    (draw->win% draw-probability (win-frequency num-players hand-type))))

(defn potential-win% [draw-probabilities num-players]
  (sum
    (for [[draw-type draw-probability] draw-probabilities
          :let [hand-type (draw-type->hand-type draw-type)]]
      (if (keyword? hand-type)
        (draw->win% draw-probability num-players hand-type)
        (let [weighted-sum (sum (map #(* (val %) (win-frequency num-players (key %))) hand-type))
              weighted-mean (/ weighted-sum (sum (map val hand-type)))]
          (draw->win% draw-probability weighted-mean))))))

(defn fcr [overall-win%]
  (condp (fn [[min max] win%] (and (< min win%) (<= win% max))) overall-win%
    [0 20] [90 9 1]
    [20 40] [75 20 5]
    [40 50] [50 35 15]
    [50 70] [25 50 25]
    [70 90] [10 30 60]
    [90 100] [1 19 80]
    (throw (IllegalArgumentException. (str "Unexpected overall-win%: " overall-win%)))))

(defn naive-action-fn [state]
  (if (= :pre-flop (:round state))
    (pre-flop-action-fn state)
    (let [{:keys [player current-bet big-blind board round num-players allowed-actions]} state
          {hand :hand player-chips :chips} player
          rating (rate-hand (concat hand board))
          hand-type (:key (:hand-type rating))
          current-win% (win-frequency num-players hand-type)
          draws (player-draws hand board)
          probability-key (round->probability-key round)
          draw-probabilities (when probability-key (into {} (map (juxt :draw-type probability-key) draws)))
          potential-win% (potential-win% draw-probabilities num-players)
          overall-win% (min (+ current-win% potential-win%) 100)
          fcr (fcr overall-win%)
          action (select-action allowed-actions fcr current-bet big-blind player-chips)]
      (when debug-ai-actions?
        (prn (merge action {:hand-type hand-type
                            :current-win% current-win%
                            :potential-win% potential-win%
                            :overall-win% overall-win%
                            :fcr fcr
                            :draw-probabilities draw-probabilities})))
      action)))
