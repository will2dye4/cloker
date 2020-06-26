(ns cloker.player)

(defn check-bet [player amount] (<= amount (:chips player)))
