(ns frajure.code.values
  (:require [clojure.test :refer [is]]))

(declare frj->clj)

(defn clj-func->frj-func
  "Converts a Clojure function to a Frajure function. The resulting function does  
   not convert inputs to Frajure values."
  {:test (fn []
           (let [clj-func #(+ %1 %2)]
             (is (= (clj-func->frj-func clj-func 2) {::type ::func
                                                     ::arity 2
                                                     ::val clj-func}))))}
  [f arity]
  {::type ::func
   ::arity arity
   ::val f})

(defn clj-int->frj-int
  "Converts a Clojure integer to a Frajure integer."
  {:test (fn []
           (is (= (clj-int->frj-int 12) {::type ::int
                                         ::val 12})))}
  [clj-int]
  {::type ::int
   ::val clj-int})

(defn clj-str->frj-sym
  "Converts a Clojure string to a Frajure symbol."
  {:test (fn []
           (is (= (clj-str->frj-sym "sym->[test]") {::type ::sym
                                                    ::val "sym->[test]"})))}
  [clj-str]
  {::type ::sym
   ::val clj-str})

(defn clj-seq-of-frj-vals->frj-arr
  "Converts a Clojure sequence of Frajure objects to a Frajure array."
  {:test (fn []
           (let [sym1 (clj-str->frj-sym "sym1")
                 sym2 (clj-str->frj-sym "sym2")
                 int1 (clj-int->frj-int 1)
                 int2 (clj-int->frj-int 2)
                 arr1 (clj-seq-of-frj-vals->frj-arr [sym2 int1])]
             (is (= (clj-seq-of-frj-vals->frj-arr [sym1 int2 arr1]) {::type ::arr
                                                                     ::val [sym1 int2 arr1]}))
             (is (= (clj-seq-of-frj-vals->frj-arr (list int1 int2 sym1)) {::type ::arr
                                                                          ::val [int1 int2 sym1]}))
             (is (= (clj-seq-of-frj-vals->frj-arr []) {::type ::arr
                                                       ::val []}))))}
  [clj-seq]
  {::type ::arr
   ::val (vec clj-seq)})

(defn- frj-type?
  "Returns whether v is of Frajure type t."
  {:test (fn []
           (let [int1 (clj-int->frj-int 1)
                 sym1 (clj-str->frj-sym "sym1")
                 arr1 (clj-seq-of-frj-vals->frj-arr [int1 sym1])]
             (is (frj-type? ::int int1))
             (is (not (frj-type? ::int sym1)))
             (is (frj-type? ::arr arr1))
             (is (not (frj-type? ::sym arr1)))))}
  [t v]
  (and (map? v) (= (::type v) t)))

(defn frj-arr?
  "Returns whether v is a Frajure array."
  {:test (fn []
           (is (frj-arr? (clj-seq-of-frj-vals->frj-arr [(clj-int->frj-int 7)])))
           (is (not (frj-arr? [7]))))}
  [v]
  (frj-type? ::arr v))

(defn frj-func?
  "Returns whether v is a Frajure function."
  {:test (fn []
           (is (frj-func? (clj-func->frj-func #(%1 %2) 2)))
           (is (not (frj-func? (clj-int->frj-int 5)))))}
  [v]
  (frj-type? ::func v))

(defn frj-sym?
  "Returns whether v is a Frajure symbol."
  {:test (fn []
           (is (frj-sym? (clj-str->frj-sym "sym")))
           (is (not (frj-sym? (clj-int->frj-int 5)))))}
  [v]
  (frj-type? ::sym v))

(defn frj-func->clj-func
  "Converts a Frajure function to a Clojure function. The resulting function does 
   not convert inputs to Clojure values."
  {:test (fn []
           (let [f #(+ %1 %2)]
             (is (= (frj-func->clj-func (clj-func->frj-func f 2)) f))))}
  [frj-func]
  (::val frj-func))

(defn frj-int->clj-int
  "Converts a Frajure integer to a Clojure integer."
  {:test (fn []
           (is (= (frj-int->clj-int (clj-int->frj-int 65)) 65))
           (is (= (frj-int->clj-int (clj-int->frj-int -108)) -108)))}
  [frj-int]
  (::val frj-int))

(defn shallow-frj-arr->clj-vec
  "Converts a Frajure array to a Clojure vector. Does not convert array contents."
  {:test (fn []
           (let [int1 (clj-int->frj-int 1)
                 sym1 (clj-str->frj-sym "sym1")
                 arr (clj-seq-of-frj-vals->frj-arr [int1 sym1])]
             (is (= (shallow-frj-arr->clj-vec arr) [int1 sym1]))
             (is (= (shallow-frj-arr->clj-vec (clj-seq-of-frj-vals->frj-arr [])) []))))}
  [frj-arr]
  (::val frj-arr))

(defn deep-frj-arr->clj-vec
  "Recursively converts a Frajure array to a Clojure vector."
  {:test (fn []
           (let [int1 (clj-int->frj-int 1)
                 int2 (clj-int->frj-int 2)
                 int3 (clj-int->frj-int 3)
                 arr1 (clj-seq-of-frj-vals->frj-arr [int1 int2])
                 arr2 (clj-seq-of-frj-vals->frj-arr [int3 arr1])]
             (is (= (deep-frj-arr->clj-vec arr2) [3 [1 2]]))))}
  [frj-arr]
  (mapv frj->clj (shallow-frj-arr->clj-vec frj-arr)))

(defn frj->clj
  "Converts a Frajure value to a Clojure value."
  {:test (fn []
           (is (= (frj->clj (clj-int->frj-int -5)) -5))
           (is (= (frj->clj (clj-seq-of-frj-vals->frj-arr [(clj-int->frj-int 4)
                                                           (clj-seq-of-frj-vals->frj-arr [(clj-int->frj-int 5)])]))
                  [4 [5]]))
           (is (nil? (frj->clj nil))))}
  [v]
  (if (frj-arr? v)
    (deep-frj-arr->clj-vec v)
    (::val v)))