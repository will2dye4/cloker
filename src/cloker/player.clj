(ns cloker.player)

(def ^:const default-chips 10000)

(defrecord Player [id name chips wins])

(defn new-player
  ([id] (new-player id default-chips))
  ([id chips] (Player. id (str "Player " id) chips 0)))

(defn check-bet [player amount] (<= amount (:chips player)))
