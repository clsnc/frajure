(ns frajure.code.eval
  (:require [clojure.test :refer [is]]
            [frajure.code.builtins :as builtins]
            [frajure.code.parse :as parse]
            [frajure.code.tokenize :as tok]
            [frajure.code.values :as vals]
            [frajure.utils :as u]))

(declare frj-expr-tree->clj-eval-func)

(defn- frj-context+sym->clj-eval-func
  "Resolves a Frajure symbol to a Clojure evaluation function in a given context."
  {:test (fn []
           (is (= ((frj-context+sym->clj-eval-func builtins/default-context (vals/clj-str->frj-sym "sum"))) builtins/frj-sum))
           (is (nil? (frj-context+sym->clj-eval-func builtins/default-context (vals/clj-str->frj-sym "not-real")))))}
  [context sym]
  (context sym))

(defn clj-eval-funcs->expr-clj-eval-func
  "Takes a vector of Clojure functions that return evaluations of Frajure expressions and 
   returns another Clojure function that returns the evaluation of that vector as a Frajure 
   expression. If the operator in the expression is not a Frajure function or is the wrong 
   arity for the number of parameters in the expression, returns nil."
  {:test (fn []
           (let [frj-sum-func (fn [] builtins/frj-sum)
                 int10-func #(vals/clj-int->frj-int 10)
                 int7-func #(vals/clj-int->frj-int 7)
                 int3-func #(vals/clj-int->frj-int 3)]
             (is (= ((clj-eval-funcs->expr-clj-eval-func [int10-func int7-func frj-sum-func]))
                    (vals/clj-int->frj-int 17)))
             (is (nil? ((clj-eval-funcs->expr-clj-eval-func [int7-func frj-sum-func]))))
             (is (nil? ((clj-eval-funcs->expr-clj-eval-func [int7-func int10-func int3-func frj-sum-func]))))
             (is (nil? ((clj-eval-funcs->expr-clj-eval-func [int3-func int7-func]))))))}
  [fs]
  ;; Notice that this whole block is a function that will be returned.
  #(let [[param-funcs op-func] (u/split-vec-at-last fs)
         frj-op (op-func)]
     (when (and (vals/frj-func? frj-op) (= (::vals/arity frj-op) (count param-funcs)))
       (apply (vals/frj-func->clj-func frj-op) param-funcs))))

(defn- frj-expr-arr->clj-eval-func
  "Returns a Clojure function that returns the evaluation of a Frajure expression array."
  {:test (fn []
           (let [frj-arr-tree1 (parse/clj-str-tree->frj-arr-tree ["1" ["8" "5" "sum"] "sum"])
                 frj-arr-tree2 (parse/clj-str-tree->frj-arr-tree ["3"])
                 frj-arr-tree3 (parse/clj-str-tree->frj-arr-tree ["4" "5"])
                 frj-arr-tree4 (parse/clj-str-tree->frj-arr-tree ["3" "2" "unresolvable-sym"])
                 clj-eval-func (frj-expr-arr->clj-eval-func frj-arr-tree1)]
             (is (= (clj-eval-func) (vals/clj-int->frj-int 14)))
             (is (= ((frj-expr-arr->clj-eval-func frj-arr-tree2)) (vals/clj-int->frj-int 3)))
             (is (nil? (frj-expr-arr->clj-eval-func (parse/clj-str-tree->frj-arr-tree []))))
             ;; Notice that the following 2 tests are different: the 1st returns a function that returns nil, but the 2nd 
             ;; returns nil directly. This is because in the first, the operator is a valid expression, though it isn't 
             ;; a valid function. In the second, the operator is not a valid expression because it contains a symbol that 
             ;; cannot be resolved.
             (is (nil? ((frj-expr-arr->clj-eval-func frj-arr-tree3))))
             (is (nil? (frj-expr-arr->clj-eval-func frj-arr-tree4)))))}
  [frj-arr]
  (let [frj-subexprs (vals/shallow-frj-arr->clj-vec frj-arr)]
    (if (= (count frj-subexprs) 1)
      (frj-expr-tree->clj-eval-func (first frj-subexprs)) ;; A single nested element should be unnested before evaluation.
      (let [subexpr-clj-eval-funcs (mapv frj-expr-tree->clj-eval-func frj-subexprs)]
        (when-not (or (empty? subexpr-clj-eval-funcs) (u/in? subexpr-clj-eval-funcs nil))
          (clj-eval-funcs->expr-clj-eval-func subexpr-clj-eval-funcs))))))

(defn frj-expr-tree->clj-eval-func
  "Returns a function that returns the evaluation of a Frajure expression tree."
  {:test (fn []
           (let [int1 (vals/clj-int->frj-int 1)
                 frj-tree (parse/clj-str-tree->frj-arr-tree [["4" ["6" "8" "sum"] "sum"] "2" "sum"])]
             (is (= ((frj-expr-tree->clj-eval-func int1)) int1))
             (is (= ((frj-expr-tree->clj-eval-func frj-tree)) (vals/clj-int->frj-int 20)))))}
  [frj-tree]
  (cond
    (vals/frj-arr? frj-tree) (frj-expr-arr->clj-eval-func frj-tree)
    (vals/frj-sym? frj-tree) (frj-context+sym->clj-eval-func builtins/default-context frj-tree)
    :else (fn [] frj-tree)))

(defn eval-clj-str-of-frj-expr
  "Evaluates a Clojure string of a Frajure expression and returns a Frajure value."
  {:test (fn []
           (is (= (eval-clj-str-of-frj-expr "3") (vals/clj-int->frj-int 3)))
           (is (= (eval-clj-str-of-frj-expr "5 19 sum") (vals/clj-int->frj-int 24)))
           (is (= (eval-clj-str-of-frj-expr "(2 3 sum) (6 (1 7 sum) sum) sum") (vals/clj-int->frj-int 19)))
           (is (nil? (eval-clj-str-of-frj-expr ""))))}
  [clj-str]
  (let [eval-func (-> clj-str
                      (tok/tokenize-line)
                      (parse/tokens->parse-tree)
                      (parse/clj-str-tree->frj-arr-tree)
                      (frj-expr-tree->clj-eval-func))]
    (when eval-func
      (eval-func))))