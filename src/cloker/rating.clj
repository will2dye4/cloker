(ns cloker.rating
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [cloker.cards :refer [pluralize-rank ranks]]
            [cloker.constants :refer [hand-size]]
            [cloker.utils :refer :all]))

(defn excluding [unwanted cards]
  (->> unwanted
       set
       (set/difference (set cards))))

(defn highest-n-of-a-kind [n cards]
  {:pre (> n 1)}
  (when (>= (count cards) n)
    (->> cards
         (group-by :rank)
         sort
         (map val)
         (filter #(= n (count %)))
         last)))

(defn- sorted-cards-for-straight-check [cards]
  (let [distinct-rank-cards (->> cards
                                 sort
                                 (partition-by :rank)
                                 (map first))
        highest-card (last distinct-rank-cards)]
    (vec
      (if (= (:rank highest-card) (ranks :ace))
        (cons highest-card distinct-rank-cards)
        distinct-rank-cards))))

(defn best-straight [cards rank-fn]
  (let [cards (sorted-cards-for-straight-check cards)
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

(defn has-straight-draw? [cards]
  (let [num-cards-needed (dec hand-size)
        longest-straight-size (count (longest-straight cards))]
    (if (= longest-straight-size num-cards-needed)
      true  ;; open-ended straight draw
      (if (> longest-straight-size num-cards-needed)
        false  ;; already has a straight!
        (let [cards (sorted-cards-for-straight-check cards)]
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
         (map val)
         (sort-by count)
         last
         sort
         vec)))

(defn has-flush-draw? [cards]
  (= (count (biggest-flush cards)) (dec hand-size)))

(defn best-straight-flush [cards]
  (-> cards
      biggest-flush
      longest-straight))

(defn best-hand [cards]
  (let [num-cards (count cards)]
    (cond
      (= num-cards hand-size) cards
      (> num-cards hand-size) (subvec cards (- num-cards hand-size))
      :else nil)))

(defrecord HandType [key value name]
  Comparable
    (compareTo [_ other] (compare value (:value other)))
  Object
    (toString [_] name))

;; NOTE: keep these in sorted order!
(def ^:private hand-type-keys [:high-card, :pair, :two-pair, :three-of-a-kind, :straight, :flush,
                               :full-house, :four-of-a-kind, :straight-flush, :royal-flush])

(def hand-types (sorted-map-by-value
                  (into {} (for [[i key] (enumerate hand-type-keys)]
                             [key (HandType. key (inc i) (keyword->name key))]))))

(defn indefinite-form [hand-type]
  (let [needs-article #{:pair :straight :flush :full-house :straight-flush :royal-flush}
        term (str/lower-case (:name hand-type))]
    (if (needs-article (:key hand-type))
      (str "a " term)
      term)))

(def ^:private hand-type-hierarchy (-> (make-hierarchy)
                                       (derive :royal-flush :straight-flush)))

(defmulti participating-cards
          (fn [hand-type cards] hand-type)
          :hierarchy #'hand-type-hierarchy)

(defmethod participating-cards :high-card
  [_ cards]
  (when-not (empty? cards) []))

(defmethod participating-cards :pair
  [_ cards]
  (highest-n-of-a-kind 2 cards))

(defmethod participating-cards :two-pair
  [_ cards]
  (when-let [top-pair (highest-n-of-a-kind 2 cards)]
    (when-let [second-pair (->> cards
                                (excluding top-pair)
                                (highest-n-of-a-kind 2))]
      (vec (concat second-pair top-pair)))))

(defmethod participating-cards :three-of-a-kind
  [_ cards]
  (highest-n-of-a-kind 3 cards))

(defmethod participating-cards :straight
  [_ cards]
  (-> cards
      longest-straight
      best-hand))

(defmethod participating-cards :flush
  [_ cards]
  (-> cards
      biggest-flush
      best-hand))

(defmethod participating-cards :full-house
  [_ cards]
  (when-let [three-of-a-kind (highest-n-of-a-kind 3 cards)]
      (when-let [pair (->> cards
                           (excluding three-of-a-kind)
                           (highest-n-of-a-kind 2))]
        (vec (concat three-of-a-kind pair)))))

(defmethod participating-cards :four-of-a-kind
  [_ cards]
  (highest-n-of-a-kind 4 cards))

(defmethod participating-cards :straight-flush
  [hand-type cards]
  (when-let [straight-flush (best-hand (best-straight-flush cards))]
    (when (or (= hand-type :straight-flush)  ;; could also be :royal-flush
              (= (:rank (first straight-flush)) (ranks :10)))
      straight-flush)))

(defrecord HandRating [cards hand-type participating-cards kickers]
  Object
    (toString [_]
      (let [rating (indefinite-form hand-type)
            first-rank-plural (pluralize-rank (first participating-cards))
            last-rank-plural (pluralize-rank (last participating-cards))]
        (case (:key hand-type)
          :high-card "nothing"
          :pair (str rating " of " first-rank-plural)
          :three-of-a-kind (str "three " first-rank-plural)
          :four-of-a-kind (str "four " first-rank-plural)
          :two-pair (str rating " (" first-rank-plural " and " last-rank-plural ")")
          :full-house (str rating " (" first-rank-plural " full of " last-rank-plural ")")
          (str rating " " (apply list participating-cards))))))

(defn- hand-rating [hand-type cards]
  (let [participants (participating-cards (key hand-type) cards)
        kickers (->> cards
                     (excluding participants)
                     sort
                     reverse
                     vec)]
    (HandRating. cards (val hand-type) participants kickers)))

(defn rate-hand [cards]
  (->> hand-types
       reverse
       (map #(hand-rating % cards))
       (remove (comp nil? :participating-cards))
       first))

(defn has-hand? [hand-type cards]
  (boolean (participating-cards hand-type cards)))
