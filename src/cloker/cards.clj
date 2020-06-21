(ns cloker.cards
  (:require [cloker.utils :refer :all]
            [cloker.constants :refer [num-hole-cards]]))

(defrecord Suit [value symbol]
  Comparable
    (compareTo [suit other] (compare (:value suit) (:value other)))
  Object
    (toString [_] symbol))

(def suits (sorted-map-by-value {:spades (Suit. 4 "♠︎")
                                 :hearts (Suit. 3 "♥︎")
                                 :diamonds (Suit. 2 "♦︎")
                                 :clubs (Suit. 1 "♣")}))

(defrecord Rank [value symbol name]
  Comparable
    (compareTo [_ other] (compare value (:value other)))
  Object
    (toString [_] symbol))

(def ranks (sorted-map-by-value {:2 (Rank. 2 "2" "deuce")
                                 :3 (Rank. 3 "3" "three")
                                 :4 (Rank. 4 "4" "four")
                                 :5 (Rank. 5 "5" "five")
                                 :6 (Rank. 6 "6" "six")
                                 :7 (Rank. 7 "7" "seven")
                                 :8 (Rank. 8 "8" "eight")
                                 :9 (Rank. 9 "9" "nine")
                                 :10 (Rank. 10 "10" "ten")
                                 :jack (Rank. 11 "J" "jack")
                                 :queen (Rank. 12 "Q" "queen")
                                 :king (Rank. 13 "K" "king")
                                 :ace (Rank. 14 "A" "ace")}))

(defrecord Card [rank suit]
  Comparable
    (compareTo [_ other] (compare [rank suit] [(:rank other) (:suit other)]))
  Object
    (toString [_] (str rank suit)))

(make-printable Card)

(defn card [rank suit]
  (Card. (ranks rank) (suits suit)))

(defn pluralize-rank [card]
  (let [rank (:rank card)]
    (if (= rank (ranks :6))
      "sixes"
      (str (:name rank) "s"))))

(defn new-deck []
  (vec (for [suit (vals suits)
             rank (vals ranks)]
         (Card. rank suit))))

(defn draw
  ([deck] [(first deck) (vec (rest deck))])
  ([n deck] [(vec (take n deck)) (vec (drop n deck))]))

(defn draw-hands [n deck]
  (let [num-cards (* num-hole-cards n)
        [top-cards deck] (draw num-cards deck)
        hands (mapv vec (partition num-hole-cards top-cards))]
    [hands deck]))

(defn add [deck cards]
  (if (sequential? cards)
    (apply conj deck cards)
    (conj deck cards)))
