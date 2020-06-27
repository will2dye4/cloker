(ns cloker.cli
  (:require [clojure.string :as str]
            [cloker.game :refer :all]
            [cloker.player :refer [check-bet]]
            [cloker.rating :refer [check-draws rate-hand]]
            [cloker.utils :refer [center-heading input repeat-char]]))

(defn show-board [game]
  (when-let [current-hand (:current-hand game)]
    (let [board (board current-hand)]
      (when-not (empty? board)
        (println (format "%-20s\tPot: %,d" board (:pot current-hand))))))
  game)

(defn show-player-info
  ([player] (show-player-info player nil))
  ([player extra-cards] (show-player-info player extra-cards true))
  ([player extra-cards check-for-draws?]
    (let [{:keys [hand name chips]} player
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
      (println (format "%-10s\t%-10s\t%,6d\t  %s" name (str hand) chips (or rating ""))))))

(defn show-winner-info [winner-ratings]
  (let [verb (if (> (count winner-ratings) 1) "ties" "wins")]
    (doseq [{:keys [player rating]} winner-ratings]
      (println (format "%s %s with %s" (:name player) verb rating)))))

(defn show-standings [game]
  (println)
  (doseq [{:keys [chips name wins]} (players game)
          :let [chips (format "%,8d" chips)
                wins (format "%,d %s" wins (if (= 1 wins) "win" "wins"))]]
    (println (format "%s\t%s\t%s" name chips wins)))
  (println)
  game)

(def ^:const heading-width 42)

(def ^:private format-percentage (partial format "(%.2f%%)"))

(defn- format-line [name occurrences frequency]
  (format "%16s %,8d %16s" name occurrences (format-percentage frequency)))

(defn show-outcomes [hand-freqs key]
  (let [heading (str ({:total "All" :wins "Winning"} key) " Outcomes")
        sum-field (fn [field] (reduce + (map #(get-in (val %) [key field]) hand-freqs)))
        total-outcomes (sum-field :occurrences)
        frequency-total (sum-field :frequency)]
    (println (center-heading heading heading-width))
    (doseq [[hand-type counts] hand-freqs]
      (let [name (:name hand-type)
            occurrences (get-in counts [key :occurrences])
            frequency (get-in counts [key :frequency])]
        (println (format-line name occurrences frequency))))
    (println (repeat-char \- heading-width))
    (println (format-line "Total" total-outcomes frequency-total))))

(defn show-hand-strength [hand-freqs]
  (println (center-heading "Hand Strength" (+ 8 heading-width)))
  (doseq [[hand-type counts] hand-freqs]
    (let [name (:name hand-type)
          total (get-in counts [:total :occurrences])
          wins (get-in counts [:wins :occurrences])
          ratio (format "%,d / %,d" wins total)
          win-frequency (format-percentage (:hand-won-frequency counts))]
      (println (format "%16s %18s %14s" name ratio win-frequency)))))

(defmulti cli-event-handler (fn [[event-type & attrs]] event-type))

(defmethod cli-event-handler :begin-hand
  [_] (println "============= Ante / Blinds ============="))

(defmethod cli-event-handler :begin-round
  [[_ round]]
  (let [title (if (= :pre-flop round) "Pre-Flop" (str/capitalize (name round)))]
    (println (str "\n================== " title " =================="))))

(defmethod cli-event-handler :bet
  [[_ player amount]]
  (let [action (if (= amount (:chips player))
                 "is all in"
                 (format "bets %,d" amount))]
    (println (format "--> %s %s" (:name player) action))))

(defmethod cli-event-handler :deal-round
  [[_ _ game]] (show-board game))

(defmethod cli-event-handler :end-hand
  [[_ game]] (show-standings game))

(defmethod cli-event-handler :fold
  [[_ player]] (println (format "--> %s folds" (:name player))))

(defmethod cli-event-handler :player-to-act
  [[_ game player]]
  (let [check-for-draws? (#{:flop :turn} (current-round game))]
    (show-player-info player (current-board game) check-for-draws?)))

(defmethod cli-event-handler :win
  [[_ winners showdown?]]
  (if showdown?
    (do
      (println "\n================ Showdown ================")
      (show-winner-info winners))
    (do
      (println "\n============= Hand Finished =============")
      (println (str (:name (:player (first winners))) " wins")))))

(defmethod cli-event-handler :default [_])  ;; do nothing

(def ^:private position-strs {:dealer "D" :big-blind "BB" :small-blind "SB"})

(defn- parse-bet-or-raise-command [player current-bet player-bet args]
  (if (< (count args) 2)
    (println "You must specify an amount!")
    (let [verb (first args)
          raise-to? (= ["raise" "to"] [verb (or (second args) "")])]
      (when-let [amount (try
                          (Integer/parseInt (str/replace (last args) "," ""))
                          (catch NumberFormatException _ (println "Invalid amount - must be an integer")))]
        (let [amount (+ amount (if raise-to? 0 current-bet))]
          (cond
            (not (pos? amount)) (println "Invalid amount - must be positive")
            (and raise-to? (<= amount current-bet)) (println (format "Invalid amount - must be higher than the current bet (%,d)" current-bet))
            (not (check-bet player (- amount player-bet))) (println (format "Invalid amount - %s only has %,d chips" (:name player) (:chips player)))
            :else {:action (keyword verb) :amount amount}))))))

(defn get-player-action [player allowed-actions position current-bet player-bet]
  (let [annotation (if position (str " (" (position-strs position) ")") "")
        action-names (map name allowed-actions)
        actions (set action-names)
        prompt (format "[%,d] %s%s may %s: " current-bet (:name player) annotation (str/join ", " action-names))]
    (loop []
      (let [args (str/split (input prompt) #"\s+")
            verb (first args)]
        (cond
          (not (actions verb)) (do (println "Invalid action!") (recur))
          (#{"bet" "raise"} verb) (if-let [action (parse-bet-or-raise-command player current-bet player-bet args)]
                                    action
                                    (recur))
          :else {:action (keyword verb)})))))

(defn single-player-action-fn [& args]
  (let [f (if (= 1 (:id (first args))) get-player-action default-action-fn)]
    (apply f args)))

(defn new-cli-game
  ([] (new-cli-game :interactive))
  ([mode]
    (let [action-fn (case mode
                      :auto default-action-fn
                      :interactive get-player-action
                      :single-player single-player-action-fn
                      (throw (IllegalArgumentException. (str "Unknown mode: " (pr-str mode)))))]
      (-> (new-game :action-fn action-fn)
          (register-event-handler cli-event-handler)))))
