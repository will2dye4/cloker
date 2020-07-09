(ns cloker.ai.cloki
  (:require [clojure.math.combinatorics :refer [combinations]]
            [cloker.cards :refer [new-deck]]
            [cloker.constants :refer [board-size num-hole-cards]]
            [cloker.rating :refer [excluding rate-hand]]))

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

(defn hand-strength [hand board]
  (let [cards (concat hand board)
        hand-rating (rate-hand cards)]
    (loop [hs {:ahead 0 :tied 0 :behind 0} all-opponent-hands (all-possible-opponent-hands cards)]
      (if-let [opponent-hand (first all-opponent-hands)]
        (let [opponent-rating (rate-hand (concat opponent-hand board))
              key (relative-value hand-rating opponent-rating)]
          (recur (update hs key inc) (rest all-opponent-hands)))
        (let [{:keys [ahead tied behind]} hs]
          (/ (+ ahead (/ tied 2)) (+ ahead tied behind)))))))

(defn adjusted-hand-strength [hand board num-players]
  (* (Math/pow (hand-strength hand board) (dec num-players)) 100))

(def ^:private initial-sub-potentials {:ahead 0 :tied 0 :behind 0})

(def ^:private initial-hand-potentials {:ahead initial-sub-potentials
                                        :tied initial-sub-potentials
                                        :behind initial-sub-potentials})

;; TODO - fix this
(defn hand-potential [hand board]
  (let [cards (concat hand board)
        hand-rating (rate-hand cards)]
    (loop [hp initial-hand-potentials
           hp-total initial-sub-potentials
           all-opponent-hands (all-possible-opponent-hands cards)]
      (if-let [opponent-hand (first all-opponent-hands)]
        (let [opponent-rating (rate-hand (concat opponent-hand board))
              key (relative-value hand-rating opponent-rating)
              num-remaining-cards (- board-size (count board))
              all-remaining-cards (all-possible-remaining-cards (concat cards opponent-hand) num-remaining-cards)
              sp (loop [sp (hp key)
                        all-remaining-cards all-remaining-cards]
                   (if-let [remaining-cards (first all-remaining-cards)]
                     (let [board (concat board remaining-cards)
                           hand-rating (rate-hand (concat hand board))
                           opponent-rating (rate-hand (concat opponent-hand board))
                           key (relative-value hand-rating opponent-rating)]
                       (recur (update sp key inc) (rest all-remaining-cards)))
                     sp))]
          (recur (assoc hp key sp) (update hp-total key inc) (rest all-opponent-hands)))
        (let [hp-of (fn [k1 k2] (get-in hp [k1 k2]))
              p-pot (/ (+ (hp-of :behind :ahead)
                          (/ (hp-of :behind :tied) 2)
                          (/ (hp-of :tied :ahead) 2))
                       (+ (hp-total :behind)
                          (hp-total :tied)))
              n-pot (/ (+ (hp-of :ahead :behind)
                          (/ (hp-of :tied :behind) 2)
                          (/ (hp-of :ahead :tied) 2))
                       (+ (hp-total :ahead)
                          (hp-total :tied)))]
          [p-pot n-pot])))))
