(ns frajure.utils
  (:require [clojure.core.rrb-vector :as v]
            [clojure.test :refer [is]]))

(defn in?
  "Returns whether an element el exists in a collection coll. Takes at least O(n)."
  {:test (fn []
           (is (in? (list 1 2 3 4) 4))
           (is (not (in? (list 5 6 7 8) 10)))
           (is (in? [1 2 nil 4 5] nil))
           (is (not (in? [6 7 false 8] nil)))
           (is (in? [1 3 false 5] false))
           (is (not (in? [8 4 nil 3 5] false))))}
  [coll el]
  (some #(= % el) coll))

(defn split-vec-at-last
  "Returns a vector of form [v-without-last-element last-element-of-v]."
  {:test (fn []
           (is (= (split-vec-at-last [3 5]) [[3] 5]))
           (is (= (split-vec-at-last [1 2 3]) [[1 2] 3]))
           (is (= (split-vec-at-last [6]) [[] 6])))}
  [v]
  [(v/subvec v 0 (dec (count v))) (peek v)])

(defn update-vec-last
  "Applies update to the last element of a vector."
  {:test (fn []
           (is (= (update-vec-last [1 2 3] inc) [1 2 4]))
           (is (= (update-vec-last [1 2 3] + 9 14) [1 2 26]))
           (is (= (update-vec-last [1] dec) [0])))}
  [v f & args]
  (apply update v (dec (count v)) f args))