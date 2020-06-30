(ns cloker.outs
  (:require [cloker.rating :refer :all]
            [cloker.utils :refer [map-vals]]))

(defrecord OutProbabilities [flop->turn turn->river turn+river])

(defn new-probabilities [flop->turn turn->river turn+river]
  (OutProbabilities. flop->turn turn->river turn+river))

(def ^:private probabilities
  {2 [4.3, 4.3, 8.4]
   3 [6.4, 6.5, 12.5]
   4 [8.5, 8.7, 16.5]
   5 [10.6, 10.9, 20.3]
   6 [12.8, 13.0, 24.1]
   7 [14.9, 15.2, 27.8]
   8 [17.0, 17.4, 31.5]
   9 [19.1, 19.6, 35]
   10 [21.3, 21.7, 38.4]
   12 [25.5, 26.1, 45.0]
   15 [31.9, 32.6, 54.1]})

(def out-probabilities (map-vals #(apply new-probabilities %) probabilities))

(def draw-types                                ;; Hand    Flop        Specific Outs
  {:pocket-pair->set 2                         ;; 2♦︎ 2♣   Q♦ 9♠ 4♥    2♥ 2♠
   :one-overcard 3                             ;; A♠ 8♦   9♣ 5♠ 2♦    A♣ A♦ A♥
   :inside-straight-draw 4                     ;; J♥ 9♣   Q♠ 8♦ 4♣    10?
   :two-pair->full-house 4                     ;; K♥ Q♠   K♣ Q♦ 5♠    K♠ K♦ Q♥ Q♣
   :pair->two-pair-or-trips 5                  ;; A♣ Q♦   A♦ 10♣ 3♠   A♥ A♠ Q♥ Q♠ Q♣
   :nothing->pair 6                            ;; 9♣ 7♦   J♣ 3♦ 2♠    9♥ 9♦ 9♠ 7♥ 7♠ 7♣
   :two-overcards->overpair 6                  ;; A♦ J♥   10♣ 8♦ 2♠   A♥ A♣ A♠ J♦ J♣ J♠
   :set->full-house-or-quads 7                 ;; 6♣ 6♦   J♣ 7♥ 6♠    J♣ J♦ J♥ 7♦ 7♣ 7♠ 6♥
   :open-ended-straight-draw 8                 ;; 9♣ 8♦   10♥ 7♣ 3♠   J? 6?
   :flush-draw 9                               ;; K♠ J♠   A♠ 6♠ 8♦    Q♠ 10♠ 9♠ 8♠ 7♠ 5♠ 4♠ 3♠ 2♠
   :inside-straight-draw-and-two-overcards 10  ;; A♣ K♦   Q♥ 10♣ 2♠   A♦ A♥ A♠ K♥ K♣ K♠ J?
   :inside-straight-and-flush-draw 12          ;; K♦ 9♦   J♦ Q♠ 3♦    Q♦ 10? 9♦ 8♦ 7♦ 6♦ 5♦ 4♦ 2♦
   :open-ended-straight-and-flush-draw 15})    ;; K♥ Q♥   10♥ J♠ 4♥   A? J♥ 9? 8♥ 7♥ 6♥ 5♥ 3♥ 2♥

(def all-draws
  (mapv
    (fn [[type num-draws]]
      (merge {:draw-type type :num-draws num-draws} (out-probabilities num-draws)))
    draw-types))

(defn overcards [hand board]
  (filter
    (fn [card] (every? #(pos? (compare (:rank card) (:rank %))) board))
    hand))

(defn has-open-ended-straight-draw? [cards]
  (and (has-straight-draw? cards)
       (not (has-inside-straight-draw? cards))))

(defmulti has-draw-type? (fn [draw-type hand-type hand board] draw-type))

(defmethod has-draw-type? :pocket-pair->set
  [_ _ hand board]
  (and (has-hand? :pair hand)
       (not (has-hand? :three-of-a-kind (concat hand board)))))  ;; do we even need to check that there's no trips?

(defmethod has-draw-type? :one-overcard
  [_ _ hand board] (= 1 (count (overcards hand board))))

(defmethod has-draw-type? :inside-straight-draw
  [_ _ hand board] (has-inside-straight-draw? (concat hand board)))

(defmethod has-draw-type? :two-pair->full-house
  [_ hand-type _ _] (= hand-type :two-pair))

(defmethod has-draw-type? :pair->two-pair-or-trips
  [_ hand-type _ _] (= hand-type :pair))

(defmethod has-draw-type? :nothing->pair
  [_ hand-type _ _] (= hand-type :high-card))

(defmethod has-draw-type? :two-overcards->overpair
  [_ hand-type hand board]
  (and (= hand-type :high-card)
       (= 2 (count (overcards hand board)))))

(defmethod has-draw-type? :set->full-house-or-quads
  [_ hand-type _ _] (= hand-type :three-of-a-kind))

(defmethod has-draw-type? :open-ended-straight-draw
  [_ _ hand board]
  (has-open-ended-straight-draw? (concat hand board)))

(defmethod has-draw-type? :flush-draw
  [_ _ hand board] (has-flush-draw? (concat hand board)))

(defmethod has-draw-type? :inside-straight-draw-and-two-overcards
  [_ _ hand board]
  (and (has-inside-straight-draw? (concat hand board))
       (= 2 (count (overcards hand board)))))

(defmethod has-draw-type? :inside-straight-and-flush-draw
  [_ _ hand board]
  (let [cards (concat hand board)]
    (and (has-inside-straight-draw? cards)
         (has-flush-draw? cards))))

(defmethod has-draw-type? :open-ended-straight-and-flush-draw
  [_ _ hand board]
  (let [cards (concat hand board)]
    (and (has-open-ended-straight-draw? cards)
         (has-flush-draw? cards))))

(defn player-draws [player board]
  (let [{:keys [hand]} player
        {:keys [hand-type]} (rate-hand (concat hand board))]
    (->> all-draws
         (map :draw-type)
         (filter #(has-draw-type? % hand-type hand board)))))
