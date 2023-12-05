(ns frajure.editor
  (:require [frajure.state :as state]))

(defn frame
  "A Frajure frame."
  [text]
  [:div.frame
   [:textarea.frame-input {:value text
               :onChange #(state/replace-text! (.-value (.-target %)))}]
   [:div.frame-output text]])