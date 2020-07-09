(ns cloker.ai.pre-flop
  (:require [cloker.ai.utils :refer [debug-ai-actions? select-action]]
            [cloker.constants :refer [num-hole-cards]]
            [cloker.outs :refer [has-pocket-pair?]]))

;; Hand groups are based on those in Tables I and II of "Hold'em Poker for Advanced Players" (Sklansky & Malmuth).
;; See https://pdfs.semanticscholar.org/dc7a/0e5d30bffdd096ffd494a17840ffb7d39afc.pdf for more.

;; rank --> group (1-8)
(def pair-groups (->> {#{:jack :queen :king :ace} 1 #{:10} 2 #{:9} 3 #{:8} 4 #{:7} 5 #{:5 :6} 6 #{:2 :3 :4} 7}
                      (reduce (fn [m [rs g]] (apply assoc m (flatten (map #(vector % g) rs)))) {})))

;; first-rank --> second-rank --> [suited-group non-suited-group]
;; groups are 1-8, with 1 being the best and 0 indicating "below group 8"
(def non-pair-groups
  {:ace {:king [1 2] :queen [2 3] :jack [2 4] :10 [3 6] :9 [5 8] :else [5 0]}
   :king {:queen [2 4] :jack [3 5] :10 [4 6] :9 [6 8] :else [7 0]}
   :queen {:jack [3 5] :10 [4 6] :9 [5 8] :8 [7 0]}
   :jack {:10 [3 5] :9 [4 7] :8 [6 8] :7 [8 0]}
   :10 {:9 [4 7] :8 [5 8] :7 [7 0]}
   :9 {:8 [4 7] :7 [5 0] :6 [8 0]}
   :8 {:7 [5 8] :6 [6 0] :5 [8 0]}
   :7 {:6 [5 8] :5 [6 0] :4 [8 0]}
   :6 {:5 [5 8] :4 [7 0]}
   :5 {:4 [6 8] :3 [7 0]}
   :4 {:3 [7 0] :2 [8 0]}
   :3 {:2 [8 0]}})

(defn hand->group [hand]
  {:pre [(= (count hand) num-hole-cards)]}
  (if (has-pocket-pair? hand)
    (pair-groups (get-in (first hand) [:rank :key]))
    (let [ranks (->> hand sort reverse (mapv (comp :key :rank)))
          suited? (= 1 (count (set (map :suit hand))))
          idx (if suited? 0 1)]
      (or (get-in non-pair-groups [(first ranks) (second ranks) idx])
          (get-in non-pair-groups [(first ranks) :else idx])
          0))))

(def fcr {0 [95 4 1] 1 [5 35 60] 2 [15 45 40] 3 [20 50 30] 4 [30 50 20]
          5 [40 45 15] 6 [60 35 5] 7 [75 22 3] 8 [85 13 2]})

(defn pre-flop-action-fn [state]
  (let [{:keys [player current-bet big-blind allowed-actions]} state
        {hand :hand player-chips :chips} player
        group (hand->group hand)
        fcr (fcr group)
        action (select-action allowed-actions fcr current-bet big-blind player-chips)]
    (when debug-ai-actions?
      (prn (merge action {:group group :fcr fcr})))
    action))
