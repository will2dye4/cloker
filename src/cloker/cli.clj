(ns cloker.cli
  (:require [clojure.string :as str]
            [cloker.player :refer [check-bet]]
            [cloker.rating :refer [check-draws rate-hand]]
            [cloker.utils :refer [input]]))

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
