(ns cloker.game
  (:require [clojure.string :as str]
            [cloker.cards :refer [draw draw-hands new-deck]]
            [cloker.constants :refer [hand-size]]
            [cloker.rating :refer [check-draws hand-rating-sort-key rate-hand]]
            [cloker.utils :refer :all]))

(def all-betting-rounds {:pre-flop {:title "Pre-Flop" :num-cards 0}
                         :flop {:title "Flop" :num-cards 3}
                         :turn {:title "Turn" :num-cards 1}
                         :river {:title "River" :num-cards 1}})

(defn winners [player-ratings]
  (->> player-ratings
       (sort-by :rating)
       (partition-by (comp hand-rating-sort-key :rating))
       last))

(defrecord Hand [pot flop turn river])

(defn new-hand [] (Hand. 0 nil nil nil))

(defn board [hand] (vec (concat (:flop hand) (:turn hand) (:river hand))))

(defn new-player [id] {:id id :chips 10000 :wins 0})

(defrecord Game [ante small-blind big-blind players deck hands])

(defn ->player-map [players]
  (->> players
       (map #(vector (:id %) %))
       (into (sorted-map))))

(defn new-game [& {:keys [ante blinds players num-players]
                   :or {ante 0, blinds [100 200], players nil, num-players 4}}]
  {:pre [(int? ante) (>= ante 0)
         (sequential? blinds) (= 2 (count blinds)) (every? int? blinds) (< (first blinds) (last blinds))
         (let [n (if players (count players) num-players)] (> n 1))]}
  (let [players (->player-map (or players (map new-player (map inc (range num-players)))))
        deck (shuffle (new-deck))]
    (Game. ante (first blinds) (second blinds) players deck [])))

(defn players [game] (vals (:players game)))

(defn current-board [game] (board (:current-hand game)))

(defn current-round [game] (get-in game [:current-hand :round]))

(defn pre-flop? [game] (= :pre-flop (current-round game)))

(defn update-player [game player f & args]
  (apply update-in game [:players (:id player)] f args))

(defn update-players [game players]
  (assoc game :players (if (sequential? players)
                         (->player-map players)
                         players)))

(defn deal-hand [game]
  (let [[hands deck] (draw-hands (count (players game)) (shuffle (:deck game)))
        players (map #(assoc %1 :hand %2) (players game) hands)]
    (-> game
        (assoc :current-hand (new-hand))
        (assoc :deck deck)
        (update-players players))))

(defn deal-cards [game]
  (let [round (current-round game)
        num-cards (get-in all-betting-rounds [round :num-cards] 0)]
    (if (zero? num-cards)
      game
      (let [[cards deck] (draw num-cards (:deck game))]
        (-> game
            (assoc :deck deck)
            (assoc-in [:current-hand round] cards))))))

(defn check-bet [player amount] (<= amount (:chips player)))

(defn bet [game player amount]
  (println (str "--> Player " (:id player) " bets " amount))
  (-> game
      (update-player player update :chips - amount)
      (update-in [:current-hand :pot] + amount)))

(defn fold [game player]
  (println (str "--> Player " (:id player) " folds"))
  (update-player game player dissoc :hand))

(defn all-in-or-fold [game player]
  (if (pos? (:chips player))  ;; all in
    (bet game player (:chips player))
    (fold game player)))  ;; TODO - also remove the player entirely???

(defn bet-or-fold [game player amount]
  {:pre [(>= amount 0)]}
  (cond
    (zero? amount) game
    (check-bet player amount) (bet game player amount)
    :else (all-in-or-fold game player)))

(defn player-at-index-relative-to-dealer [relative-index game]
  (let [players (players game)
        index (mod (+ (count (:hands game)) relative-index) (count players))]
    (nth players index)))

(def dealer (partial player-at-index-relative-to-dealer 0))

(defn is-dealer? [game player] (= player (dealer game)))

(def small-blind-player (partial player-at-index-relative-to-dealer 1))

(defn is-small-blind? [game player] (= player (small-blind-player game)))

(def big-blind-player (partial player-at-index-relative-to-dealer 2))

(defn is-big-blind? [game player] (= player (big-blind-player game)))

(defn players-in-hand [game] (filter (comp not nil? :hand) (players game)))

(defn num-players-in-hand [game] (count (players-in-hand game)))

(defn collect-ante-and-blinds [game]
  (println "======== Ante / Blinds ========")
  (loop [players (players game)
         game game]
    (if-let [player (first players)]
      (let [amount-owed (+ (:ante game)
                           (cond (is-small-blind? game player) (:small-blind game)
                                 (is-big-blind? game player) (:big-blind game)
                                 :else 0))]
        (recur (rest players) (bet-or-fold game player amount-owed)))
      game)))

(defn show-player-info
  ([player] (show-player-info player nil))
  ([player extra-cards] (show-player-info player extra-cards true))
  ([player extra-cards check-for-draws?]
    (let [{:keys [hand id chips]} player
          rating (when extra-cards
                   (let [cards (concat hand extra-cards)
                         hand-type (:hand-type (rate-hand cards))
                         rating (:name hand-type)]
                     (if check-for-draws?
                       (if-let [draws (->> hand-type
                                           (check-draws cards)
                                           (map #(str % " draw"))
                                           seq)]
                         (str rating " (" (str/capitalize (str/join ", " draws)) ")")
                         rating)
                       rating)))]
      (println (format "%s\t%s\t%d\t%s" (str "Player " id) hand chips (or rating ""))))))

(defn- start-of-round-bets [game]
  (let [initial-bets (map-keys (constantly 0) (players game))
        [bets curr-bet raiser] (when (pre-flop? game)
                                 (let [bb-player (big-blind-player game)
                                       bb (:big-blind game)
                                       sb-player (small-blind-player game)
                                       sb (:small-blind game)]
                                   [{bb-player bb, sb-player sb} bb bb-player]))]
    {:player-bets (merge-with + initial-bets bets)
     :current-bet (or curr-bet 0)
     :last-raiser raiser}))

(defn round-betting-state [game]
  (let [relative-index (if (pre-flop? game) 3 1)
        players (rotate relative-index (players-in-hand game))]
    (merge {:players players, :last-caller (first players)}
           (start-of-round-bets game))))

(defn- next-step [game state i player]
  (let [pre-flop? (pre-flop? game)
        {:keys [current-bet last-caller last-raiser]} state]
    (cond
      (= 1 (num-players-in-hand game)) :return  ;; we have a winner
      (nil? (:hand player)) :recur  ;; player has folded
      (or (and pre-flop?
               (= player last-raiser)
               (> current-bet (:big-blind game)))
          (and (not pre-flop?)
               (or (= player last-raiser)
                   (and (pos? i)
                        (zero? current-bet)
                        (= player last-caller))))) :return
      :else :continue)))

(defn available-actions [game player current-bet]
  (let [{:keys [chips]} player
        actions (if (or (zero? current-bet)
                        (and (pre-flop? game)
                             (= player (big-blind-player game))
                             (= current-bet (:big-blind game))))
                  [:check :bet]
                  (map (fn [[k f]] (when (f chips current-bet) k)) {:call >=, :raise >}))]
    (apply conj #{:fold} actions)))

(defn player-position [game player]
  (condp = player
    (dealer game) :dealer
    (small-blind-player game) :small-blind
    (big-blind-player game) :big-blind
    nil))

;; TODO - why is the pre-flop betting not working when playing multiple hands with the same game?
(defn round-of-betting [game]
  (loop [game game
         state (round-betting-state game)
         players (enumerate (cycle (:players state)))]
    (let [{:keys [current-bet player-bets]} state
          [i player] (first players)
          players (rest players)]
      (case (next-step game state i player)
        :return game
        :recur (recur game state players)
        (let [check-for-draws (not= :river (current-round game))
              _ (show-player-info player (current-board game) check-for-draws)
              actions (available-actions game player current-bet)
              position (player-position game player)
              player-bet (player-bets player)
              action (or (actions :call) :check)]
          (cond
            (= :fold action) (recur (fold game player) state players)
            (= :call action) (let [game (bet game player (- current-bet player-bet))
                                   state (-> state
                                             (assoc-in [:player-bets player] current-bet)
                                             (assoc :last-caller player))]
                               (recur game state players))
            (#{:bet :raise} action) (let [bet (:big-blind game)    ;; TODO
                                               game (bet game player (- bet player-bet))
                                               state (-> state
                                                         (assoc-in [:player-bets player] current-bet)
                                                         (assoc :current-bet bet)
                                                         (assoc :last-raiser player))]
                                           (recur game state players))
            (and (= :check action)
                 (pre-flop? game)
                 (= player (big-blind-player game))) game
            :else (recur game state players)))))))

(defn show-board [game]
  (when-let [current-hand (:current-hand game)]
    (let [board (board current-hand)]
      (when-not (empty? board)
        (println (format "%-20s\tPot: %d" board (:pot current-hand))))))
  game)

(defn betting-rounds [game]
  (loop [game game
         rounds all-betting-rounds]
    (if-let [[round {:keys [title]}] (first rounds)]
      (if (> (num-players-in-hand game) 1)
        (do
          (println (str "\n========== " title " =========="))
          (let [game (-> game
                         (assoc-in [:current-hand :round] round)
                         deal-cards
                         show-board
                         round-of-betting)]
            (recur game (rest rounds))))
        game)
      game)))

(defn award-pot [game player]
  (let [pot (get-in game [:current-hand :pot])]
    (-> game
        (update-player player update :chips + pot)
        (update-player player update :wins inc))))

(defn show-winner-info [winner-ratings]
  (let [verb (if (> (count winner-ratings) 1) "ties" "wins")]
    (doseq [{:keys [player rating]} winner-ratings]
      (println (format "Player %d %s with %s" (:id player) verb rating)))))

(defn award-winnings [game]
  (if (= 1 (num-players-in-hand game))
    (let [winner (first (players-in-hand game))]
      (println "\n======== Hand Finished ========")
      (println (str "Player " (:id winner) " wins"))
      (award-pot game winner))
    (let [board (current-board game)
          ratings (for [player (players game)] {:player player :rating (rate-hand (concat (:hand player) board))})
          winner-ratings (winners ratings)]
      (println "\n======== Showdown ========")
      (show-winner-info winner-ratings)
      (if (= 1 (count winner-ratings))
        (award-pot game (:player (first winner-ratings)))
        game))))  ;; TODO - split pot in this case

(defn recycle-cards [game] game)  ;; TODO - put all hands and muck back into deck

(defn conclude-hand [game]
  (-> game
      award-winnings
      recycle-cards
      (update :hands conj (:current-hand game))
      (dissoc :current-hand)))

(defn play-hand [game]
  (-> game
      deal-hand
      collect-ante-and-blinds
      betting-rounds
      conclude-hand))
