(ns frajure.code.expressions
  (:require [clojure.test :refer [is]]))

(defn op-element
  "Given a sequence that represents a Frajure expression (each element represents 
   a subexpression), return the one associated with the operation. The type of the 
   subexpressions does not matter."
  {:test (fn []
           (is (= (op-element ["a" "b" "c"]) "a")))}
  [expr-elements]
  (first expr-elements))

(defn arg-elements
  "Given a sequence that represents a Frajure expression (each element represents 
   a subexpression), return a sequence of the ones associted with arguments. The 
   type of the subexpressions does not matter."
  {:test (fn []
           (is (= (vec (arg-elements ["d" "e" "f"])) ["e" "f"])))}
  [expr-elements]
  (rest expr-elements))