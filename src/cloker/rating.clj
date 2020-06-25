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

(def highest-pair (partial highest-n-of-a-kind 2))

(defn rank-delta [[prev-card current-card]]
  (let [prev-rank (if (and (= (:rank prev-card) (ranks :ace))
                           (not= (:rank current-card) (ranks :ace)))
                    1
                    (:value (:rank prev-card)))]
    (- (:value (:rank current-card)) prev-rank)))

(defn- sort-for-straight-check [cards]
  (let [distinct-rank-cards (->> cards
                                 sort
                                 (partition-by :rank)
                                 (map first))
        highest-card (last distinct-rank-cards)]
    (vec
      (if (= (:rank highest-card) (ranks :ace))
        (cons highest-card distinct-rank-cards)  ;; stash an ace at the beginning to check for a wheel
        distinct-rank-cards))))

(defn best-straight [cards rank-fn]
  (let [cards (sort-for-straight-check cards)
        pairs (consecutive-pairs cards)
        best-of-two #(if (pos? (rank-fn %1 %2)) %1 %2)]
    (loop [best-straight []
           current-straight [(first cards)]
           pairs pairs]
      (if-let [[_ current-card :as pair] (first pairs)]
        (if (= (rank-delta pair) 1)
          (recur best-straight (conj current-straight current-card) (rest pairs))
          (recur (best-of-two current-straight best-straight) [current-card] (rest pairs)))
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

(defn has-inside-straight-draw? [cards]
  (let [cards (sort-for-straight-check cards)]
    (loop [i 0, j (dec hand-size)]
      (if (> j (count cards))
        false  ;; ran out of cards
        (let [gaps (->> cards
                        (#(subvec % i j))
                        consecutive-pairs
                        (map rank-delta)
                        (filter #(> % 1))
                        vec)]
          (if (= gaps [2])
            true  ;; exactly one missing card -> inside straight draw
            (recur (inc i) (inc j))))))))

(defn has-straight-draw? [cards]
  (let [num-cards-needed (dec hand-size)
        longest-straight-size (count (longest-straight cards))]
    (cond
      (= longest-straight-size num-cards-needed) true   ;; open-ended straight draw
      (> longest-straight-size num-cards-needed) false  ;; already has a straight!
      :else (has-inside-straight-draw? cards))))

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
    (toString [_] (clojure.core/name key)))

(make-printable HandType keyword)

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
  (highest-pair cards))

(defmethod participating-cards :two-pair
  [_ cards]
  (when-let [top-pair (highest-pair cards)]
    (when-let [second-pair (->> cards
                                (excluding top-pair)
                                highest-pair)]
      (vec (concat top-pair second-pair)))))

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
                           highest-pair)]
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

(defn hand-rating-sort-key [rating]
  (let [hand-type (:hand-type rating)
        ranks-of (fn [cards]
                   (->> cards
                        (map :rank)
                        (#(if (= :flush (:key hand-type)) (reverse %) %))
                        vec))]
    [hand-type
     (ranks-of (:participating-cards rating))
     (ranks-of (:kickers rating))]))

(defrecord HandRating [cards hand-type participating-cards kickers]
  Comparable
    (compareTo [rating other]
      (compare (hand-rating-sort-key rating) (hand-rating-sort-key other)))
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
  (when-let [participants (participating-cards (key hand-type) cards)]
    (let [num-kickers (- hand-size (count participants))
          kickers (if (zero? num-kickers)
                    []
                    (->> cards
                         (excluding participants)
                         sort
                         reverse
                         (take num-kickers)
                         vec))]
      (HandRating. cards (val hand-type) participants kickers))))

(defn rate-hand [cards]
  (->> hand-types
       reverse
       (map #(hand-rating % cards))
       (remove nil?)
       first))

(defn has-hand? [hand-type cards]
  (boolean (participating-cards hand-type cards)))

(defn has-draw? [hand-type cards]
  (case hand-type
    :flush (has-flush-draw? cards)
    :straight (has-straight-draw? cards)
    false))

(def ^:private eligible-for-draw (set (take 5 hand-type-keys)))

(defn check-draws [cards hand-type]
  (if (eligible-for-draw (:key hand-type))
    (->> [:flush :straight]
         (filter #(has-draw? % cards))
         (map hand-types))
    []))
