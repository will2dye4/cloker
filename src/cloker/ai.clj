(ns cloker.ai
  (:require [cloker.ai.naive :refer [naive-action-fn]]))

(def ai-action-fns {:naive naive-action-fn})

(def ^:const default-ai-mode :naive)

(def ai-action-fn (ai-action-fns default-ai-mode))
