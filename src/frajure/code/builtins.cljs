(ns frajure.code.builtins
  (:require [clojure.test :refer [is]]
            [frajure.code.values :as vals]))

(defn frj-sum
  "Sums 2 Frajure integers to return another Frajure integer."
  {:test (fn []
           (is (= (frj-sum #(vals/clj-int->frj-int 8) #(vals/clj-int->frj-int -17))
                  (vals/clj-int->frj-int -9))))}
  [frj-int1-eval-func frj-int2-eval-func]
  (vals/clj-int->frj-int (+ (vals/frj-int->clj-int (frj-int1-eval-func))
                            (vals/frj-int->clj-int (frj-int2-eval-func)))))

(def default-context
  {(vals/clj-str->frj-sym "sum") (fn [] frj-sum)})