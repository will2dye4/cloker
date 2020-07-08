(ns cloker.cards
  (:require [cloker.constants :refer [num-hole-cards]]
            [cloker.utils :refer :all]))

(defrecord Suit [value symbol]
  Comparable
    (compareTo [_ other] (compare value (:value other)))
  Object
    (toString [_] symbol))

(def suits (sorted-map-by-value {:spades (Suit. 4 "♠︎")
                                 :hearts (Suit. 3 "♥︎")
                                 :diamonds (Suit. 2 "♦︎")
                                 :clubs (Suit. 1 "♣")}))

(defrecord Rank [key value symbol name]
  Comparable
    (compareTo [_ other] (compare value (:value other)))
  Object
    (toString [_] symbol))

(def ^:private rank-data [[:2 "2" "deuce"]
                          [:3 "3" "three"]
                          [:4 "4" "four"]
                          [:5 "5" "five"]
                          [:6 "6" "six"]
                          [:7 "7" "seven"]
                          [:8 "8" "eight"]
                          [:9 "9" "nine"]
                          [:10 "10" "ten"]
                          [:jack "J" "jack"]
                          [:queen "Q" "queen"]
                          [:king "K" "king"]
                          [:ace "A" "ace"]])

(def ranks (sorted-map-by-value
             (into {} (for [[i [key symbol name]] (enumerate rank-data)]
                        [key (Rank. key (+ i 2) symbol name)]))))

(defrecord Card [rank suit]
  Comparable
    (compareTo [_ other] (compare [rank suit] [(:rank other) (:suit other)]))
  Object
    (toString [_] (str rank suit)))

(make-printable Card)

(defn sort-cards [cards]
  (-> cards sort reverse vec))

(defn card [rank suit]
  (Card. (ranks rank) (suits suit)))

(defn pluralize-rank [card]
  (when-let [rank (:rank card)]
    (if (= rank (ranks :6))
      "sixes"
      (str (:name rank) "s"))))

(defn new-deck []
  (vec (for [suit (vals suits)
             rank (vals ranks)]
         (Card. rank suit))))

(defn draw
  ([deck] [(first deck) (vec (rest deck))])
  ([n deck] (draw n deck true))
  ([n deck sort?]
   (let [cards (take n deck)
         cards (if sort? (sort-cards cards) (vec cards))
         deck (vec (drop n deck))]
     [cards deck])))

(defn draw-hands [n deck]
  (let [num-cards (* num-hole-cards n)
        [top-cards deck] (draw num-cards deck)
        hands (mapv sort-cards (partition num-hole-cards top-cards))]
    [hands deck]))

(defn add [deck cards]
  (if (sequential? cards)
    (apply conj deck cards)
    (conj deck cards)))
