(ns cloker.ai
  (:require [cloker.ai.cloki :refer [cloki-action-fn]]
            [cloker.ai.naive :refer [naive-action-fn]]))

(def ai-action-fns {:cloki cloki-action-fn
                    :naive naive-action-fn})

(def ^:const default-ai-mode :cloki)

(def ai-action-fn (ai-action-fns default-ai-mode))
