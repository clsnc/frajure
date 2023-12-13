(ns frajure.code.eval
  (:require [clojure.test :refer [is]]
            [frajure.code.builtins :as builtins]
            [frajure.code.db :as cdb]
            [frajure.code.expressions :as exprs]
            [frajure.code.parse :as parse]
            [frajure.code.tokenize :as tok]
            [frajure.code.values :as vals]
            [frajure.utils :as u]))

(declare frj-expr-id->clj-eval-func)

(defn- sym-expr-id->clj-eval-func
  "Given a database and a symbol expression ID, returns a function that evaluates the symbol."
  {:test (fn []
           (let [db (cdb/parse-tree->db ["sum"])]
             (is (= ((sym-expr-id->clj-eval-func db 2)) builtins/frj-sum))))}
  [db expr-id]
  (builtins/default-context (cdb/term-expr-id->term-text db expr-id)))

(defn- term-expr-id->clj-eval-func
  "Resolves a term expression to an evaluation function."
  {:test (fn []
           (let [db (cdb/parse-tree->db ["a" "2" "sum"])]
             (is (= ((term-expr-id->clj-eval-func db 4)) builtins/frj-sum))
             (is (= ((term-expr-id->clj-eval-func db 3)) (vals/clj-int->frj-int 2)))
             (is (nil? (term-expr-id->clj-eval-func db 2)))))}
  [db expr-id]
  (let [text (cdb/term-expr-id->term-text db expr-id)
        parsed-int (parse/clj-str->clj-int text)]
    (if parsed-int
      #(vals/clj-int->frj-int parsed-int)
      (sym-expr-id->clj-eval-func db expr-id))))

(defn clj-eval-funcs->expr-clj-eval-func
  "Takes a vector of Clojure functions that return evaluations of Frajure expressions and 
   returns another Clojure function that returns the evaluation of that vector as a Frajure 
   expression. If the operator in the expression is not a Frajure function or is the wrong 
   arity for the number of arguments in the expression, returns nil."
  {:test (fn []
           (let [frj-sum-func (fn [] builtins/frj-sum)
                 int10-func #(vals/clj-int->frj-int 10)
                 int7-func #(vals/clj-int->frj-int 7)
                 int3-func #(vals/clj-int->frj-int 3)]
             (is (= ((clj-eval-funcs->expr-clj-eval-func [frj-sum-func int10-func int7-func]))
                    (vals/clj-int->frj-int 17)))
             (is (nil? ((clj-eval-funcs->expr-clj-eval-func [frj-sum-func int7-func]))))
             (is (nil? ((clj-eval-funcs->expr-clj-eval-func [frj-sum-func int7-func int10-func int3-func]))))
             (is (nil? ((clj-eval-funcs->expr-clj-eval-func [int3-func int7-func]))))))}
  [fs]
  ;; Notice that this whole block is a function that will be returned.
  #(let [op-func (exprs/op-element fs)
         arg-funcs (exprs/arg-elements fs)
         frj-op (op-func)]
     (when (and (vals/frj-func? frj-op) (= (::vals/arity frj-op) (count arg-funcs)))
       (apply (vals/frj-func->clj-func frj-op) arg-funcs))))

(defn- frj-cmpd-expr-id->clj-eval-func
  "Returns a Clojure function that returns the evaluation of a Frajure expression array."
  {:test (fn []
           (let [db1 (cdb/parse-tree->db ["sum" "1" ["sum" "8" "5"]])
                 db2 (cdb/parse-tree->db ["3"])
                 db3 (cdb/parse-tree->db ["4" "5"])
                 db4 (cdb/parse-tree->db ["unresolvable-sym" "3" "2"])
                 clj-eval-func (frj-cmpd-expr-id->clj-eval-func db1 1)]
             (is (= (clj-eval-func) (vals/clj-int->frj-int 14)))
             (is (= ((frj-cmpd-expr-id->clj-eval-func db2 1)) (vals/clj-int->frj-int 3)))
             (is (nil? (frj-cmpd-expr-id->clj-eval-func (cdb/parse-tree->db []) 1)))
             ;; Notice that the following 2 tests are different: the 1st returns a function that returns nil, but the 2nd 
             ;; returns nil directly. This is because in the first, the operator is a valid expression, though it isn't 
             ;; a valid function. In the second, the operator is not a valid expression because it contains a symbol that 
             ;; cannot be resolved.
             (is (nil? ((frj-cmpd-expr-id->clj-eval-func db3 1))))
             (is (nil? (frj-cmpd-expr-id->clj-eval-func db4 1)))
             (is (= ((frj-cmpd-expr-id->clj-eval-func (cdb/parse-tree->db ["sum" "1" "2" ["def" "5" "a"]]) 1)) (vals/clj-int->frj-int 3)))))}
  [db expr-id]
  (let [nondef-subexpr-ids (cdb/expr-id->ordered-nondef-subexpr-ids db expr-id)]
    (if (= (count nondef-subexpr-ids) 1)
      (frj-expr-id->clj-eval-func db (first nondef-subexpr-ids)) ;; A single nested element should be unnested before evaluation.
      (let [subexpr-clj-eval-funcs (mapv #(frj-expr-id->clj-eval-func db %) nondef-subexpr-ids)]
        (when-not (or (empty? subexpr-clj-eval-funcs) (u/in? subexpr-clj-eval-funcs nil))
          (clj-eval-funcs->expr-clj-eval-func subexpr-clj-eval-funcs))))))

(defn frj-expr-id->clj-eval-func
  "Returns a function that returns the evaluation of a Frajure expression."
  {:test (fn []
           (let [db (cdb/parse-tree->db ["sum" ["sum" "4" ["sum" "6" "8"]] "2"])]
             (is (= ((frj-expr-id->clj-eval-func db 5)) (vals/clj-int->frj-int 4)))
             (is (= ((frj-expr-id->clj-eval-func db 1)) (vals/clj-int->frj-int 20)))))}
  [db expr-id]
  (if (cdb/term-expr-id? db expr-id)
    (term-expr-id->clj-eval-func db expr-id)
    (frj-cmpd-expr-id->clj-eval-func db expr-id)))

(defn eval-clj-str-of-frj-expr
  "Evaluates a Clojure string of a Frajure expression and returns a Frajure value."
  {:test (fn []
           (is (= (eval-clj-str-of-frj-expr "3") (vals/clj-int->frj-int 3)))
           (is (= (eval-clj-str-of-frj-expr "sum 5 19") (vals/clj-int->frj-int 24)))
           (is (= (eval-clj-str-of-frj-expr "sum (sum 2 3) (sum 6 (sum 1 7))") (vals/clj-int->frj-int 19)))
           (is (nil? (eval-clj-str-of-frj-expr ""))))}
  [clj-str]
  (let [eval-func (-> clj-str
                      (tok/tokenize-line)
                      (parse/tokens->parse-tree)
                      (cdb/parse-tree->db)
                      (frj-expr-id->clj-eval-func 1))]
    (when eval-func
      (eval-func))))