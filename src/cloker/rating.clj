(ns cloker.rating
  (:require [clojure.set :as s]
            [cloker.cards :refer [ranks]]
            [cloker.constants :refer [hand-size]]
            [cloker.utils :refer :all]))

(defrecord HandType [value name]
  Comparable
    (compareTo [hand-type other] (compare (:value hand-type) (:value other)))
  Object
    (toString [hand-type] (:name hand-type)))

(def hand-types (sorted-map-by-value {:royal-flush (HandType. 10 "Royal flush")
                                      :straight-flush (HandType. 9 "Straight flush")
                                      :four-of-a-kind (HandType. 8 "Four of a kind")
                                      :full-house (HandType. 7 "Full house")
                                      :flush (HandType. 6 "Flush")
                                      :straight (HandType. 5 "Straight")
                                      :three-of-a-kind (HandType. 4 "Three of a kind")
                                      :two-pair (HandType. 3 "Two pair")
                                      :pair (HandType. 2 "Pair")
                                      :high-card (HandType. 1 "High card")}))

(defn has-enough-cards? [n cards]
  (>= (count cards) n))

(defn highest-n-of-a-kind [n cards]
  {:pre (> n 1)}
  (when (has-enough-cards? n cards)
    (let [sorted-groups (->> cards
                             (group-by :rank)
                             (filter #(= n (count (second %))))
                             sort)]
      (when-not (empty? sorted-groups)
        (val (last sorted-groups))))))

(defn has-pair? [cards]
  (boolean (highest-n-of-a-kind 2 cards)))

(defn has-two-pair? [cards]
  (if-let [top-pair (set (highest-n-of-a-kind 2 cards))]
    (->> top-pair
         (s/difference (set cards))
         has-pair?)
    false))

(defn has-three-of-a-kind? [cards]
  (boolean (highest-n-of-a-kind 3 cards)))

(defn has-four-of-a-kind? [cards]
  (boolean (highest-n-of-a-kind 4 cards)))

(defn has-full-house? [cards]
  (if-let [three-of-a-kind (set (highest-n-of-a-kind 3 cards))]
    (->> three-of-a-kind
         (s/difference (set cards))
         has-pair?)
    false))

(defn- -sorted-cards-for-straight-check [cards]
  (let [distinct-rank-cards (->> cards
                                 (sort-by :rank)
                                 (partition-by :rank)
                                 (map first))
        highest-card (last distinct-rank-cards)]
    (vec
      (if (= (:rank highest-card) (ranks :ace))
        (cons highest-card distinct-rank-cards)
        distinct-rank-cards))))

(defn best-straight [cards rank-fn]
  (let [cards (-sorted-cards-for-straight-check cards)
        consecutive-pairs (map vector cards (drop 1 cards))
        best-of-two #(if (pos? (rank-fn %1 %2)) %1 %2)]
    (loop [best-straight []
           current-straight [(first cards)]
           pairs consecutive-pairs]
      (if-let [[prev-card current-card] (first pairs)]
        (let [rank-delta (- (:value (:rank current-card)) (:value (:rank prev-card)))]
          (if (or (= rank-delta 1)
                  (and (= (:rank current-card) (ranks :two))
                       (= (:rank prev-card) (ranks :ace))))
            (recur best-straight (conj current-straight current-card) (rest pairs))
            (recur (best-of-two current-straight best-straight) [current-card] (rest pairs))))
        (best-of-two current-straight best-straight)))))

(defn highest-straight [cards]
  (best-straight cards
    (fn [current best-so-far]
      (if (or (empty? best-so-far)
              (and (= (count best-so-far) 1)
                   (= (:rank (first best-so-far)) (ranks :ace))))
        1
        (compare (:rank (first current)) (:rank (first best-so-far)))))))

(defn longest-straight [cards]
  (best-straight cards #(compare (count %1) (count %2))))

(defn has-straight? [cards]
  (>= (count (longest-straight cards)) hand-size))

(defn has-straight-draw? [cards]
  (let [num-cards-needed (dec hand-size)
        longest-straight-size (count (longest-straight cards))]
    (if (= longest-straight-size num-cards-needed)
      true  ;; open-ended straight draw
      (if (> longest-straight-size num-cards-needed)
        false  ;; already has a straight!
        (let [cards (-sorted-cards-for-straight-check cards)]
          (loop [i 0, j num-cards-needed]
            (if (> j (count cards))
              false  ;; didn't find a straight draw
              (let [current-cards (subvec cards i j)
                    gaps (loop [gaps []
                                pairs (map vector current-cards (drop 1 current-cards))]
                           (if-let [[prev-card current-card] (first pairs)]
                             (let [prev-rank-value (if (= (:rank prev-card) (ranks :ace))
                                                     1
                                                     (:value (:rank prev-card)))
                                   delta (- (:value (:rank current-card)) prev-rank-value)
                                   gaps (if (> delta 1)
                                          (conj gaps delta)
                                          gaps)]
                               (recur gaps (rest pairs)))
                             gaps))]
                (if (and (= (count gaps) 1)
                         (= (first gaps) 2))
                  true  ;; inside straight draw
                  (recur (inc i) (inc j)))))))))))

(defn biggest-flush [cards]
  (when-not (empty? cards)
    (->> cards
         (group-by :suit)
         (sort-by (comp count second))
         last
         val
         (sort-by :rank))))

(defn has-flush? [cards]
  (>= (count (biggest-flush cards)) hand-size))

(defn has-straight-flush? [cards]
  (if-let [flush (biggest-flush cards)]
      (has-straight? flush)
      false))

(defn has-flush-draw? [cards]
  (= (count (biggest-flush cards)) (dec hand-size)))

(defrecord HandRating [cards hand-type participating-cards])

;; TODO
(defn hand-rating [cards]
  (cond
    (has-straight-flush? cards)
      (let [straight-flush (-> cards biggest-flush longest-straight)])))
