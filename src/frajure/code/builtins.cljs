(ns frajure.code.builtins
  (:require [clojure.test :refer [is]]
            [frajure.code.db :as cdb]
            [frajure.code.values :as vals]))

(defn- eval-pane
  "Returns the evaluation of a pane."
  {:test (fn []
           (is (= (eval-pane #(vals/clj-int->frj-int 15) #(vals/clj-int->frj-int 10))
                  (vals/clj-int->frj-int 10)))
           (is (nil? (eval-pane))))}
  [& subexpr-clj-eval-funcs]
  (let [eval-func (last subexpr-clj-eval-funcs)]
    (when eval-func (eval-func))))

(defn- sum-frj-ints
  "Sums 2 Frajure integers to return another Frajure integer."
  {:test (fn []
           (is (= (sum-frj-ints #(vals/clj-int->frj-int 2)) (vals/clj-int->frj-int 2)))
           (is (= (sum-frj-ints #(vals/clj-int->frj-int 8) #(vals/clj-int->frj-int -17))
                  (vals/clj-int->frj-int -9)))
           (is (= (sum-frj-ints #(vals/clj-int->frj-int 3) #(vals/clj-int->frj-int 5) #(vals/clj-int->frj-int 9))
                  (vals/clj-int->frj-int 17))))}
  [& frj-int-eval-funcs]
  (vals/clj-int->frj-int (apply + (map #(vals/frj-int->clj-int (%)) frj-int-eval-funcs))))

(def frj-pane (vals/clj-func->frj-func eval-pane nil))
(def frj-sum (vals/clj-func->frj-func sum-frj-ints 2))

(def default-context
  {::cdb/pane (fn [] frj-pane)
   "sum" (fn [] frj-sum)})