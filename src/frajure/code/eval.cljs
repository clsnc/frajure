(ns frajure.code.eval
  (:require [clojure.test :refer [is]]
            [frajure.code.builtins :as builtins]
            [frajure.code.parse :as parse]
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
        (when-not (u/in? subexpr-clj-eval-funcs nil)
          (let [[param-eval-funcs op-eval-func] (u/split-vec-at-last subexpr-clj-eval-funcs)]
            #(apply (op-eval-func) param-eval-funcs)))))))

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
           (is (= (eval-clj-str-of-frj-expr "5 19 sum") (vals/clj-int->frj-int 24))))}
  [clj-str]
  (let [eval-func (-> clj-str
                      (parse/str->parse-tree)
                      (parse/clj-str-tree->frj-arr-tree)
                      (frj-expr-tree->clj-eval-func))]
    (when eval-func
      (eval-func))))