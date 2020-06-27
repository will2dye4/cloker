(ns cloker.game
  (:require [cloker.cards :refer [add draw draw-hands new-deck]]
            [cloker.player :refer [check-bet new-player]]
            [cloker.rating :refer [check-draws rate-hand winners]]
            [cloker.utils :refer :all]))

(def ^:const cards-per-round {:pre-flop 0 :flop 3 :turn 1 :river 1})

(defrecord Hand [pot flop turn river muck])

(defn new-hand [] (Hand. 0 nil nil nil []))

(defn board [hand] (vec (concat (:flop hand) (:turn hand) (:river hand))))

(defrecord Game [ante small-blind big-blind players deck hands action-fn])

(defn default-action-fn [_ actions _ _ _]
  (->> [:check :call :fold]
       (keep #(when ((set actions) %) {:action %}))
       first))

(defn ->player-map [players]
  (->> players
       (map #(vector (:id %) %))
       (into (sorted-map))))

(defn new-game [& {:keys [ante blinds players num-players action-fn]
                   :or {ante 0, blinds [100 200], players nil, num-players 4, action-fn default-action-fn}}]
  {:pre [(int? ante) (>= ante 0)
         (sequential? blinds) (= 2 (count blinds)) (every? int? blinds) (< (first blinds) (last blinds))
         (let [n (if players (count players) num-players)] (> n 1))]}
  (let [players (->player-map (or players (map new-player (map inc (range num-players)))))
        deck (shuffle (new-deck))]
    (Game. ante (first blinds) (second blinds) players deck [] action-fn)))

(defn current-board [game] (board (:current-hand game)))

(defn current-round [game] (get-in game [:current-hand :round]))

(defn players [game] (vals (:players game)))

(defn pre-flop? [game] (= :pre-flop (current-round game)))

(defn update-player [game player f & args]
  (apply update-in game [:players (:id player)] f args))

(defn update-players [game players]
  (assoc game :players (if (sequential? players)
                         (->player-map players)
                         players)))

(defn event [event-type & attrs]
  (apply conj [event-type] attrs))

(defn register-event-handler [game handler]
  (update game :handlers conj handler))

(defn emit [game event]
  (when-let [handlers (:handlers game)]
    (doseq [handler handlers] (handler event)))
  game)

(defn deal-hand [game]
  (let [[hands deck] (draw-hands (count (players game)) (shuffle (:deck game)))
        players (map #(assoc %1 :hand %2) (players game) hands)]
    (-> game
        (assoc :current-hand (new-hand))
        (assoc :deck deck)
        (update-players players))))

(defn deal-cards [game]
  (let [round (current-round game)
        num-cards (cards-per-round round)]
    (if (zero? num-cards)
      game
      (let [[cards deck] (draw num-cards (:deck game))]
        (-> game
            (assoc :deck deck)
            (assoc-in [:current-hand round] cards))))))

(defn bet [game player amount]
  (emit game (event :bet player amount))
  (-> game
      (update-player player update :chips - amount)
      (update-in [:current-hand :pot] + amount)))

(defn fold [game player]
  (emit game (event :fold player))
  (-> game
      (update-in [:current-hand :muck] concat (:hand player))
      (update-player player dissoc :hand)))

(defn all-in-or-fold [game player]
  (let [{:keys [chips]} player]
    (if (pos? chips)  ;; all in
      (bet game player chips)
      (fold game player))))

(defn bet-or-fold [game player amount]
  {:pre [(>= amount 0)]}
  (cond
    (zero? amount) game
    (check-bet player amount) (bet game player amount)
    :else (all-in-or-fold game player)))

(defn index-relative-to-dealer [relative-index game]
  ;; use (count (:hands game)) as a counter that increments after each hand
  (mod (+ (count (:hands game)) relative-index) (count (players game))))

(defn player-id-at-index-relative-to-dealer [relative-index game]
  (:id (nth (players game) (index-relative-to-dealer relative-index game))))

(def dealer-id (partial player-id-at-index-relative-to-dealer 0))

(defn is-dealer? [game player] (= (:id player) (dealer-id game)))

(def small-blind-player-id (partial player-id-at-index-relative-to-dealer 1))

(defn is-small-blind? [game player] (= (:id player) (small-blind-player-id game)))

(def big-blind-player-id (partial player-id-at-index-relative-to-dealer 2))

(defn is-big-blind? [game player] (= (:id player) (big-blind-player-id game)))

(defn players-in-hand [game] (filter (comp not nil? :hand) (players game)))

(defn num-players-in-hand [game] (count (players-in-hand game)))

(defn collect-ante-and-blinds [game]
  (emit game (event :begin-hand))
  (loop [players (players game)
         game game]
    (if-let [player (first players)]
      (let [amount-owed (+ (:ante game)
                           (cond (is-small-blind? game player) (:small-blind game)
                                 (is-big-blind? game player) (:big-blind game)
                                 :else 0))]
        (recur (rest players) (bet-or-fold game player amount-owed)))
      game)))

(defn- start-of-round-bets [game]
  (let [initial-bets (map-keys (constantly 0) (keys (:players game)))
        [bets curr-bet raiser] (when (pre-flop? game)
                                 (let [bb-player-id (big-blind-player-id game)
                                       bb (:big-blind game)
                                       sb-player-id (small-blind-player-id game)
                                       sb (:small-blind game)]
                                   [{bb-player-id bb, sb-player-id sb} bb bb-player-id]))]
    {:player-bets (merge-with + initial-bets bets)
     :current-bet (or curr-bet 0)
     :last-raiser raiser}))

(defn round-betting-state [game]
  (let [relative-index (if (pre-flop? game) 3 1)  ;; put player to act first at the front
        player-ids (->> (players game)
                        (rotate (index-relative-to-dealer relative-index game))
                        (remove (comp nil? :hand))
                        (map :id))]
    (merge {:player-ids player-ids, :last-caller (first player-ids)}
           (start-of-round-bets game))))

(defn- next-step [game state i player]
  (let [{player-id :id hand :hand} player
        pre-flop? (pre-flop? game)
        {:keys [current-bet last-caller last-raiser]} state]
    (cond
      (= 1 (num-players-in-hand game)) :return  ;; we have a winner
      (nil? hand) :recur  ;; player has folded
      (or (and pre-flop?
               (= player-id last-raiser)
               (> current-bet (:big-blind game)))
          (and (not pre-flop?)
               (or (= player-id last-raiser)
                   (and (pos? i)
                        (zero? current-bet)
                        (= player-id last-caller))))) :return
      :else :continue)))

;; TODO - handle all in
(defn available-actions [game player current-bet]
  (let [{:keys [chips]} player
        actions (if (or (zero? current-bet)
                        (and (pos? chips)
                             (pre-flop? game)
                             (is-big-blind? game player)
                             (= current-bet (:big-blind game))))
                  [:check :bet]
                  (->> [[>= :call] [> :raise]]
                       (map (fn [[f k]] (when (f chips current-bet) k)))
                       (remove nil?)))]
    (apply conj [:fold] actions)))

(defn player-position [game player]
  (condp #(%1 game %2) player
    is-dealer? :dealer
    is-small-blind? :small-blind
    is-big-blind? :big-blind
    nil))

(defn round-of-betting [game]
  (loop [game game
         state (round-betting-state game)
         player-ids (enumerate (cycle (:player-ids state)))]
    (let [{:keys [current-bet player-bets]} state
          [i player-id] (first player-ids)
          player (get-in game [:players player-id])
          player-ids (rest player-ids)]
      (case (next-step game state i player)
        :return game
        :recur (recur game state player-ids)
        (let [_ (emit game (event :player-to-act game player))
              actions (available-actions game player current-bet)
              position (player-position game player)
              player-bet (player-bets player-id)
              {:keys [action amount]} ((:action-fn game) player actions position current-bet player-bet)]
          (cond
            (= :fold action) (recur (fold game player) state player-ids)
            (= :call action) (let [game (bet game player (- current-bet player-bet))
                                   state (-> state
                                             (assoc-in [:player-bets player-id] current-bet)
                                             (assoc :last-caller player-id))]
                               (recur game state player-ids))
            (#{:bet :raise} action) (let [game (bet game player (- amount player-bet))
                                          state (-> state
                                                    (assoc-in [:player-bets player-id] amount)
                                                    (assoc :current-bet amount)
                                                    (assoc :last-raiser player-id))]
                                      (recur game state player-ids))
            (not= :check action) (throw (IllegalStateException. (str "Unknown action: " (pr-str action))))
            (and (pre-flop? game) (is-big-blind? game player)) game
            :else (recur game state player-ids)))))))

(defn betting-rounds [game]
  (loop [game game
         rounds (keys cards-per-round)]
    (if-let [round (first rounds)]
      (if (> (num-players-in-hand game) 1)
        (let [game (-> game
                       (emit (event :begin-round round))
                       (assoc-in [:current-hand :round] round)
                       deal-cards
                       (#(emit % (event :deal-round round %)))
                       round-of-betting)]
          (recur game (rest rounds)))
        game)
      game)))

(defn award-pot [game players]
  (if (= 1 (count players))
    (let [player (first players)
          pot (get-in game [:current-hand :pot])]
        (-> game
            (update-player player update :chips + pot)
            (update-player player update :wins inc)))
    game))  ;; TODO - split pot in this case

(defn award-winnings [game]
  (let [winners (winners (players game) (current-board game))
        showdown? (> (num-players-in-hand game) 1)]
    (emit game (event :win winners showdown?))
    (-> game
        (award-pot (map :player winners))
        (assoc-in [:current-hand :winners] winners))))

(defn recycle-cards [game]
  (let [{:keys [current-hand]} game
        players (players game)
        hands (->> players
                   (map :hand)
                   (remove nil?)
                   flatten)
        cards (concat (:muck current-hand) (board current-hand) hands)]
    (-> game
        (update :deck add cards)
        (update-players (map #(dissoc %1 :hand) players)))))

(defn remove-busted-players [game]
  (update-players game (filter (comp pos? :chips) (players game))))

(defn conclude-hand [game]
  (-> game
      award-winnings
      recycle-cards
      remove-busted-players
      (#(update % :hands conj (:current-hand %)))
      (dissoc :current-hand)
      (#(emit % (event :end-hand %)))))

(defn play-hand [game]
  (-> game
      deal-hand
      collect-ante-and-blinds
      betting-rounds
      conclude-hand))
