(ns frajure.core
  (:require [frajure.editor :as editor]
            [frajure.state :as state]
            [reagent.dom :as d]))

(defn home-page []
  [editor/frame @state/state-atom])

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))