(ns cloker.ai.utils
  (:require [clojure.set :as set]
            [cloker.utils :refer [sum]]))

(def debug-ai-actions? false)

(defn wrand
  "Given a vector of slice sizes, returns the index of a slice given a
   random spin of a roulette wheel with compartments proportional to slices."
  [slices]
  (let [total (sum slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))

(defn select-action [allowed-actions fcr current-bet min-raise max-raise]
  (let [actions (set allowed-actions)
        [fold% call% raise%] fcr]
    (if (= 1 (count actions))
      {:action (first actions)}
      (let [raise-actions (set/intersection actions #{:bet :raise})]
        (if (and (empty? raise-actions) (pos? raise%))
          (let [increment (int (/ raise% 2))
                fcr [(+ fold% increment) (+ call% increment) 0]]
            (select-action allowed-actions fcr current-bet min-raise max-raise))
          (let [fold-action (or (actions :check) :fold)
                call-action (first (set/intersection actions #{:call :check}))
                raise-action (first raise-actions)
                selected-action ([fold-action call-action raise-action] (wrand fcr))
                action {:action selected-action}]
            (if (raise-actions selected-action)
              (assoc action :amount (min (+ current-bet min-raise) max-raise))  ;; TODO - vary amount
              action)))))))
