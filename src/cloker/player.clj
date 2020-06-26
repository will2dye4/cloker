(ns cloker.player)

(defrecord Player [id name chips wins])

(defn new-player
  ([id] (new-player id 10000))
  ([id chips] (Player. id (str "Player " id) chips 0)))

(defn check-bet [player amount] (<= amount (:chips player)))
