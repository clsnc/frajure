(ns frajure.state
  (:require [reagent.core :as r]))

(def state-atom (r/atom "5 8 sum"))

(defn replace-text! [new-text]
  (reset! state-atom new-text))