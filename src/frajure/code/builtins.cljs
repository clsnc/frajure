(ns frajure.code.builtins
  (:require [clojure.test :refer [is]]
            [frajure.code.values :as vals]))

(defn- sum-2-frj-ints
  "Sums 2 Frajure integers to return another Frajure integer."
  {:test (fn []
           (is (= (sum-2-frj-ints #(vals/clj-int->frj-int 8) #(vals/clj-int->frj-int -17))
                  (vals/clj-int->frj-int -9))))}
  [frj-int1-eval-func frj-int2-eval-func]
  (vals/clj-int->frj-int (+ (vals/frj-int->clj-int (frj-int1-eval-func))
                            (vals/frj-int->clj-int (frj-int2-eval-func)))))

(def frj-sum (vals/clj-func->frj-func sum-2-frj-ints 2))

(def default-context
  {"sum" (fn [] frj-sum)})