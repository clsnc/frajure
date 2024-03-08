(ns frajure.code.eval
  (:require [clojure.test :refer [is]]
            [frajure.code.builtins :as builtins]
            [frajure.code.db :as cdb]
            [frajure.code.expressions :as exprs]
            [frajure.code.parse :as parse]
            [frajure.code.values :as vals]))

(declare frj-expr-id->clj-eval-func)

(defn- sym-expr-id->defined-eval-func
  "Returns a function that evaluates a symbol based on definitions in its context. Does not use built-in definitions. Returns 
   nil if no definition is found."
  {:test (fn []
           (is (= ((sym-expr-id->defined-eval-func (cdb/parse-tree->db ["sum" "1" "a" ["def" "a" "14"]]) 4)) (vals/clj-int->frj-int 14)))
           (is (= ((sym-expr-id->defined-eval-func
                    (cdb/parse-tree->db [["def" "a" "1" "9"] ["2" ["3" "a" "b"] "c"]]) 11))
                  (vals/clj-int->frj-int 9)))
           (is (nil? (sym-expr-id->defined-eval-func (cdb/parse-tree->db ["sum" ["def" "a" "1"] [["b"]] "8"]) 9))))}
  [db expr-id]
  (let [sym-name (cdb/term-expr-id->term-text db expr-id)]
    (loop [search-expr-id expr-id]
      (when search-expr-id
        (let [local-definition-expr-id (cdb/expr-id->sym-def-subexpr-id db search-expr-id sym-name)]
          (if local-definition-expr-id
            (frj-expr-id->clj-eval-func db (cdb/expr-id->last-nondef-subexpr-id db local-definition-expr-id)) ;; use the last subexpression
            (recur (cdb/expr-id->parent-expr-id db search-expr-id))))))))

(defn- sym-expr-id->clj-eval-func
  "Given a database and a symbol expression ID, returns a function that evaluates the symbol."
  {:test (fn []
           (let [db (cdb/parse-tree->db ["sum"])]
             (is (= ((sym-expr-id->clj-eval-func db 2)) builtins/frj-sum))
             (is (= ((sym-expr-id->clj-eval-func (cdb/parse-tree->db ["sum" ["def" "a" "3"] "a" "a"]) 7)) (vals/clj-int->frj-int 3)))))}
  [db expr-id]
  (let [found-def-eval-func (sym-expr-id->defined-eval-func db expr-id)]
    (if found-def-eval-func
      found-def-eval-func
      (builtins/default-context (cdb/term-expr-id->term-text db expr-id)))))

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

(defn frj-expr-ids->expr-clj-eval-func
  "Takes a database and a coll of subexpression IDs and returns a Clojure evaluation function for the 
   Frajure expression consisting of the referenced subexpressions."
  {:test (fn []
           (let [db (cdb/parse-tree->db ["sum" "2" "5"])
                 eval-func1 (frj-expr-ids->expr-clj-eval-func db [2 3 4])
                 eval-func2 (frj-expr-ids->expr-clj-eval-func db [3 4])]
             (is (= (eval-func1) (vals/clj-int->frj-int 7)))
             (is (nil? (eval-func2)))))}
  [db expr-ids]
  (let [op-expr-id (exprs/op-element expr-ids)
        arg-expr-ids (exprs/arg-elements expr-ids)
        op-eval-func (frj-expr-id->clj-eval-func db op-expr-id)]
    (when op-eval-func
      (fn [] (let [frj-op (op-eval-func)]
               (when (and (vals/frj-op? frj-op) (vals/frj-op-accepts-arity? frj-op (count arg-expr-ids)))
                 (apply (vals/frj-op->clj-func frj-op) #(frj-expr-id->clj-eval-func db %) arg-expr-ids)))))))

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
    (condp = (count nondef-subexpr-ids)
      0 nil
      1 (frj-expr-id->clj-eval-func db (first nondef-subexpr-ids)) ;; A single nested element should be unnested before evaluation.
      (frj-expr-ids->expr-clj-eval-func db nondef-subexpr-ids))))

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
                      (parse/pane-text->parse-trees)
                      (cdb/parse-tree->db)
                      (frj-expr-id->clj-eval-func 1))]
    (when eval-func
      (eval-func))))