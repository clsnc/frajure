(ns frajure.state
  (:require [reagent.core :as r]))

(def state-atom (r/atom "some code"))

(defn replace-text! [new-text]
  (reset! state-atom new-text))