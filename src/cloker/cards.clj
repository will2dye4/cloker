(ns cloker.cards
  (:require [cloker.utils :refer :all]))

(def cards-per-hand 2)

(defrecord Suit [value symbol]
  Comparable
    (compareTo [suit other] (compare (:value suit) (:value other)))
  Object
    (toString [suit] (:symbol suit)))

(def suits (sorted-map-by-value {:spades (Suit. 4 "♠︎")
                                 :hearts (Suit. 3 "♥︎")
                                 :diamonds (Suit. 2 "♦︎")
                                 :clubs (Suit. 1 "♣")}))

(defrecord Rank [value symbol name]
  Comparable
    (compareTo [rank other] (compare (:value rank) (:value other)))
  Object
    (toString [rank] (:symbol rank)))

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
  Object
    (toString [card] (str (:rank card) (:suit card))))

(make-printable Card)

(defn new-deck [] (for [suit (vals suits) rank (vals ranks)] (Card. rank suit)))

(defn draw
  ([deck] [(first deck) (rest deck)])
  ([n deck] [(take n deck) (drop n deck)]))


(defn draw-hands [n deck]
  (let [num-cards (* cards-per-hand n)
        [top-cards deck] (draw num-cards deck)
        hands (mapv vec (partition cards-per-hand top-cards))]
    [hands deck]))
