(ns frajure.editor
  (:require [frajure.code.eval :as eval]
            [frajure.code.values :as vals]
            [frajure.state :as state]))

(defn frame
  "A Frajure frame."
  [text]
  [:div.frame
   [:textarea.frame-input {:value text
                           :onChange #(state/replace-text! (.-value (.-target %)))}]
   [:div.frame-output (str (vals/frj->clj (eval/eval-clj-str-of-frj-expr text)))]])