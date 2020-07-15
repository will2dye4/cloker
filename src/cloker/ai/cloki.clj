(ns cloker.ai.cloki
  (:require [clojure.math.combinatorics :refer [combinations]]
            [cloker.ai.pre-flop :refer [pre-flop-action-fn]]
            [cloker.ai.utils :refer [debug-ai-actions? select-action]]
            [cloker.cards :refer [new-deck]]
            [cloker.constants :refer [board-size num-hole-cards]]
            [cloker.rating :refer [excluding rate-hand]]
            [cloker.utils :refer [map-vals percentage sum]]))

;; Hand strength calculations based on the "Loki" agent and algorithms described in
;; "Opponent Modeling in Poker" (Billings, Papp, Schaeffer, & Szafron, 1998).
;; http://www.cs.virginia.edu/~evans/poker/wp-content/uploads/2011/02/opponent_modeling_in_poker_billings.pdf

(defn all-possible-remaining-cards [known-cards n]
  (-> known-cards
      (excluding (new-deck))
      vec
      (combinations n)))

(defn all-possible-opponent-hands [known-cards]
  (all-possible-remaining-cards known-cards num-hole-cards))

(defn- relative-value [hand-rating opponent-rating]
  (condp #(%1 %2 0) (compare hand-rating opponent-rating)
    < :behind
    = :tied
    > :ahead))

(def ^:private initial-hand-strengths {:ahead 0 :tied 0 :behind 0})

(defn hand-strength [hand board]
  (let [cards (concat hand board)
        hand-rating (rate-hand cards)]
    (loop [hs initial-hand-strengths all-opponent-hands (all-possible-opponent-hands cards)]
      (if-let [opponent-hand (first all-opponent-hands)]
        (let [opponent-rating (rate-hand (concat opponent-hand board))
              key (relative-value hand-rating opponent-rating)]
          (recur (update hs key inc) (rest all-opponent-hands)))
        (let [{:keys [ahead tied behind]} hs]
          (/ (+ ahead (/ tied 2)) (+ ahead tied behind)))))))

(defn adjusted-hand-strength [hand board num-players]
  (Math/pow (hand-strength hand board) (dec num-players)))

(def ^:private initial-hand-potentials (map-vals (constantly initial-hand-strengths) initial-hand-strengths))

(defn hand-potential [hand board]
  (let [cards (concat hand board)
        hand-rating (rate-hand cards)]
    (loop [hp initial-hand-potentials
           all-opponent-hands (all-possible-opponent-hands cards)]
      (if-let [opponent-hand (first all-opponent-hands)]
        (let [opponent-rating (rate-hand (concat opponent-hand board))
              key (relative-value hand-rating opponent-rating)
              num-remaining-cards (- board-size (count board))
              all-remaining-cards (all-possible-remaining-cards (concat cards opponent-hand) num-remaining-cards)
              hs (->> all-remaining-cards
                      (pmap #(relative-value (rate-hand (concat hand board %)) (rate-hand (concat opponent-hand board %))))
                      frequencies
                      (merge-with + (hp key)))]
          (recur (assoc hp key hs) (rest all-opponent-hands)))
        (let [hp-total (map-vals #(sum (map val %)) hp)
              hp-of (fn [k1 k2] (get-in hp [k1 k2]))
              behind+tied (+ (hp-total :behind) (hp-total :tied))
              ahead+tied (+ (hp-total :ahead) (hp-total :tied))
              p-pot (if (zero? behind+tied)
                      0
                      (/ (+ (hp-of :behind :ahead)
                            (/ (hp-of :behind :tied) 2)
                            (/ (hp-of :tied :ahead) 2))
                         behind+tied))
              n-pot (if (zero? ahead+tied)
                      0
                      (/ (+ (hp-of :ahead :behind)
                            (/ (hp-of :tied :behind) 2)
                            (/ (hp-of :ahead :tied) 2))
                         ahead+tied))]
          [p-pot n-pot])))))

(def effective-hand-strength
  (memoize
    (fn [hand board num-players]
      (let [hand-strength (adjusted-hand-strength hand board num-players)
            [p-pot n-pot] (hand-potential hand board)]
        (* 100 (+ (* (- 1 n-pot) hand-strength)
                  (* p-pot (- 1 hand-strength))))))))

(defn pot-odds [pot current-bet] (percentage current-bet (+ current-bet pot)))

(defn fcr [ehs pot-odds]
  (let [ehs (int ehs)]
  (if (>= ehs pot-odds)
    [0 (- 100 ehs) ehs]
    (let [half-ehs (int (/ ehs 2))] [(- 100 ehs) half-ehs half-ehs]))))

(defn cloki-action-fn [state]
  (if (= :pre-flop (:round state))
    (pre-flop-action-fn state)
    (let [{:keys [player current-bet big-blind pot board round num-players allowed-actions]} state
          {hand :hand name :name player-chips :chips} player
          _ (when (= :flop round) (println (str name " is thinking...")))
          ehs (effective-hand-strength hand board num-players)
          pot-odds (pot-odds pot current-bet)
          fcr (fcr ehs pot-odds)
          action (select-action allowed-actions fcr current-bet big-blind player-chips)]
      (when debug-ai-actions?
        (prn (merge action {:hand-type (:key (:hand-type (rate-hand (concat hand board))))
                            :ehs ehs
                            :pot-odds pot-odds
                            :fcr fcr})))
      action)))
