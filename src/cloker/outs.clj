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

(def round->probability-key {:flop :turn+river, :turn :turn->river})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Draw Type               # Outs    Hand      Flop             Specific Outs         ;;
;; -------------------------------    ------   -----    ---------    -------------------------   ;;
;; Pocket pair --> set                    2    2♦︎ 2♣    Q♦ 9♠ 4♥     2♥ 2♠                       ;;
;; One overcard --> overpair              3    A♠ 8♦    9♣ 5♠ 2♦     A♣ A♦ A♥                    ;;
;; Inside straight draw                   4    J♥ 9♣    Q♠ 8♦ 4♣     10?                         ;;
;; Two pair --> full house                4    K♥ Q♠    K♣ Q♦ 5♠     K♠ K♦ Q♥ Q♣                 ;;
;; Pair --> two pair or trips             5    A♣ Q♦    A♦ 10♣ 3♠    A♥ A♠ Q♥ Q♠ Q♣              ;;
;; Nothing --> pair                       6    9♣ 7♦    J♣ 3♦ 2♠     9♥ 9♦ 9♠ 7♥ 7♠ 7♣           ;;
;; Two overcards --> overpair             6    A♦ J♥    10♣ 8♦ 2♠    A♥ A♣ A♠ J♦ J♣ J♠           ;;
;; Set --> full house or quads            7    6♣ 6♦    J♣ 7♥ 6♠     J♣ J♦ J♥ 7♦ 7♣ 7♠ 6♥        ;;
;; Open-ended straight draw               8    9♣ 8♦    10♥ 7♣ 3♠    J? 6?                       ;;
;; Flush draw                             9    K♠ J♠    A♠ 6♠ 8♦     Q♠ 10♠ 9♠ 8♠ 7♠ 5♠ 4♠ 3♠ 2♠ ;;
;; Inside straight and flush draw        12    K♦ 9♦    J♦ Q♠ 3♦     Q♦ 10? 9♦ 8♦ 7♦ 6♦ 5♦ 4♦ 2♦ ;;
;; Open-ended straight and flush draw    15    K♥ Q♥    10♥ J♠ 4♥    A? J♥ 9? 8♥ 7♥ 6♥ 5♥ 3♥ 2♥  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def draw-types  ;; values are ordered by preference (best to worst)
  ;; 'match' means pairs, three of a kind, etc.
  {:match [[:set->full-house-or-quads 7]
           [:two-pair->full-house 4]
           [:pair->two-pair-or-trips 5]
           [:two-overcards->overpair 6]
           [:one-overcard->overpair 3]
           [:pocket-pair->set 2]
           [:nothing->pair 6]]
   ;; 'draw' means straight and/or flush draws
   :draw [[:open-ended-straight-and-flush-draw 15]
          [:inside-straight-and-flush-draw 12]
          [:flush-draw 9]
          [:open-ended-straight-draw 8]
          [:inside-straight-draw 4]]})

(def all-draws
  (map-vals
    #(mapv
       (fn [[type num-draws]]
         (merge {:draw-type type :num-draws num-draws} (out-probabilities num-draws)))
       %)
    draw-types))

(def draw-type->hand-type
  {:pocket-pair->set :three-of-a-kind
   :one-overcard->overpair :pair
   :inside-straight-draw :straight
   :two-pair->full-house :full-house
   :pair->two-pair-or-trips {:two-pair 60 :three-of-a-kind 40}
   :nothing->pair :pair
   :two-overcards->overpair :pair
   :set->full-house-or-quads {:full-house 86 :four-of-a-kind 14}
   :open-ended-straight-draw :straight
   :flush-draw :flush
   :inside-straight-and-flush-draw {:straight 33 :flush 67}
   :open-ended-straight-and-flush-draw {:straight 53 :flush 47}})

(defn overcards [hand board]
  (filter
    (fn [card] (every? #(pos? (compare (:rank card) (:rank %))) board))
    hand))

(defn has-overpair? [hand board]
  (pos? (->> [(concat hand board) board]
             (map (comp vec sort highest-pair))
             (apply compare))))

(defn has-pocket-pair? [hand] (has-hand? :pair hand))

(defn has-open-ended-straight-draw? [cards]
  (and (has-straight-draw? cards)
       (not (has-inside-straight-draw? cards))))

(defn has-hand-from-hole-cards? [hand-type rated-hand-type board]
  (and (= hand-type rated-hand-type)
       (not (has-hand? hand-type board))))

(defmulti has-draw-type? (fn [draw-type hand-type hand board] draw-type))

(defmethod has-draw-type? :pocket-pair->set
  [_ hand-type hand _]
  (and (= hand-type :pair)
       (has-pocket-pair? hand)))

(defmethod has-draw-type? :one-overcard->overpair
  [_ _ hand board]
  (and (= 1 (count (overcards hand board)))
       (not (has-overpair? hand board))))

(defmethod has-draw-type? :inside-straight-draw
  [_ _ hand board] (has-inside-straight-draw? (concat hand board)))

(defmethod has-draw-type? :two-pair->full-house
  [_ hand-type _ board] (has-hand-from-hole-cards? :two-pair hand-type board))

(defmethod has-draw-type? :pair->two-pair-or-trips
  [_ hand-type hand board]
  (and (has-hand-from-hole-cards? :pair hand-type board)
       (not (has-pocket-pair? hand))))

(defmethod has-draw-type? :nothing->pair
  [_ hand-type hand board]
  (and (zero? (count (overcards hand board)))
       (= hand-type :high-card)))

(defmethod has-draw-type? :two-overcards->overpair
  [_ _ hand board]
  (and (= 2 (count (overcards hand board)))
       (not (has-overpair? hand board))))

(defmethod has-draw-type? :set->full-house-or-quads
  [_ hand-type _ board] (has-hand-from-hole-cards? :three-of-a-kind hand-type board))

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

(defn player-draws [hand board]
  (let [hand-type (:key (:hand-type (rate-hand (concat hand board))))
        best-draws (for [draws (vals all-draws)]
                     (->> draws
                          (filter #(has-draw-type? (:draw-type %) hand-type hand board))
                          first))]
    (->> best-draws
         flatten
         (remove nil?))))
