(ns frajure.code.db
  (:require [clojure.core.rrb-vector :as v]
            [clojure.test :refer [is]]
            [datascript.core :as d]
            [medley.core :as m]))

(declare expr-tx-items term-expr-id? term-expr-id->term-text)

(def db-schema {::type {:db/cardinality :db.cardinality/one}
                ::parent {:db/type :db.type/ref
                          :db/cardinality :db.cardinality/one}
                ::pos {:db/cardinality :db.cardinality/one}
                ::text {:db/cardinality :db.cardinality/one}})

(def default-db (d/empty-db db-schema))

(defn- base-tx-item
  "Returns a transaction item with basic expression data. Only includes ::parent and ::pos if parent-id is not nil."
  {:test (fn []
           (is (= (base-tx-item -3 ::term-expr -5 1) {:db/id -3
                                                      ::type ::term-expr
                                                      ::parent -5
                                                      ::pos 1}))
           (is (= (base-tx-item -6 ::cmpd-expr nil nil) {:db/id -6
                                                         ::type ::cmpd-expr})))}
  [id type parent-id pos]
  (let [tx-item {:db/id id
                 ::type type}]
    (if parent-id
      (assoc tx-item
             ::parent parent-id
             ::pos pos)
      tx-item)))

(defn- term-expr-tx-items
  "Returns a map of the form {::next-temp-id <next available temporary ID>,
   ::tx-items <vector of transaction items for the term expression>}."
  {:test (fn []
           (is (= (term-expr-tx-items "sym" 7 -3 -5) {::next-temp-id -6
                                                      ::tx-items [{:db/id -5
                                                                   ::type ::term-expr
                                                                   ::parent -3
                                                                   ::pos 7
                                                                   ::text "sym"}]})))}
  [term pos parent-temp-id next-temp-id]
  {::next-temp-id (dec next-temp-id)
   ::tx-items [(assoc (base-tx-item next-temp-id ::term-expr parent-temp-id pos) ::text term)]})

(defn- cmpd-expr-tx-items
  "Returns a map of the form {::next-temp-id <next available temporary ID>,
   ::tx-items <vector of transaction items for the compound expression and 
   it subexpressions>}."
  {:test (fn []
           (is (= (cmpd-expr-tx-items ["3" "a" ["4"]] 4 -16 -18) {::next-temp-id -23
                                                                  ::tx-items [{:db/id -18
                                                                               ::type ::cmpd-expr
                                                                               ::parent -16
                                                                               ::pos 4}
                                                                              {:db/id -19
                                                                               ::type ::term-expr
                                                                               ::parent -18
                                                                               ::pos 0
                                                                               ::text "3"}
                                                                              {:db/id -20
                                                                               ::type ::term-expr
                                                                               ::parent -18
                                                                               ::pos 1
                                                                               ::text "a"}
                                                                              {:db/id -21
                                                                               ::type ::cmpd-expr
                                                                               ::parent -18
                                                                               ::pos 2}
                                                                              {:db/id -22
                                                                               ::type ::term-expr
                                                                               ::parent -21
                                                                               ::pos 0
                                                                               ::text "4"}]})))}
  [expr pos parent-temp-id next-temp-id]
  (let [expr-temp-id next-temp-id
        expr-tx-item (base-tx-item expr-temp-id ::cmpd-expr parent-temp-id pos)
        next-temp-id (dec next-temp-id)]
    (loop [rest-indexed-subexprs (m/indexed expr)
           next-temp-id next-temp-id
           tx-items [expr-tx-item]]
      (if (empty? rest-indexed-subexprs)
        {::next-temp-id next-temp-id
         ::tx-items tx-items}
        (let [[[subexpr-pos subexpr] & rest-indexed-subexprs] rest-indexed-subexprs
              {next-temp-id ::next-temp-id
               subexpr-tx-items ::tx-items} (expr-tx-items subexpr subexpr-pos expr-temp-id next-temp-id)]
          (recur rest-indexed-subexprs next-temp-id (v/catvec tx-items subexpr-tx-items)))))))

(defn- expr-tx-items
  "Returns a map of the form {::next-temp-id <next available temporary ID>,
   ::tx-items <vector of transaction items for the expression and any subexpressions>}."
  {:test (fn []
           (is (= (expr-tx-items "sym" 3 -10 -14) {::next-temp-id -15
                                                   ::tx-items [{:db/id -14
                                                                ::type ::term-expr
                                                                ::parent -10
                                                                ::pos 3
                                                                ::text "sym"}]}))
           (is (= (expr-tx-items [["a" "b"] "c"] 0 -8 -9) {::next-temp-id -14
                                                           ::tx-items [{:db/id -9
                                                                        ::type ::cmpd-expr
                                                                        ::parent -8
                                                                        ::pos 0}
                                                                       {:db/id -10
                                                                        ::type ::cmpd-expr
                                                                        ::parent -9
                                                                        ::pos 0}
                                                                       {:db/id -11
                                                                        ::type ::term-expr
                                                                        ::parent -10
                                                                        ::pos 0
                                                                        ::text "a"}
                                                                       {:db/id -12
                                                                        ::type ::term-expr
                                                                        ::parent -10
                                                                        ::pos 1
                                                                        ::text "b"}
                                                                       {:db/id -13
                                                                        ::type ::term-expr
                                                                        ::parent -9
                                                                        ::pos 1
                                                                        ::text "c"}]})))}
  [expr pos parent-temp-id next-temp-id]
  ((if (string? expr)
     term-expr-tx-items
     cmpd-expr-tx-items) expr pos parent-temp-id next-temp-id))

(defn db-add-expr-from-parse-tree
  "Returns db updated to include parse-tree."
  {:test (fn []
           (let [db (db-add-expr-from-parse-tree default-db ["1" ["2"] ["3" "a"]])]
             (is (not (term-expr-id? db 1)))
             (is (not (term-expr-id? db 3)))
             (is (= (term-expr-id->term-text db 4) "2"))
             (is (not (term-expr-id? db 5)))
             (is (= (term-expr-id->term-text db 6) "3"))
             (is (= (term-expr-id->term-text db 7) "a"))))}
  [db parse-tree]
  (d/db-with db (::tx-items (expr-tx-items parse-tree nil nil -1))))

(defn parse-tree->db
  "Returns a new code database containing parse-tree."
  {:test (fn []
           (is (= (term-expr-id->term-text (parse-tree->db ["1"]) 2) "1")))}
  [parse-tree]
  (db-add-expr-from-parse-tree default-db parse-tree))

(defn expr-id->ordered-subexpr-ids
  "Returns an expression's subexpression IDs in order of their position in the expression."
  {:test (fn []
           (is (= (vec (expr-id->ordered-subexpr-ids (parse-tree->db ["a" "b" "c"]) 1)) [2 3 4])))}
  [db expr-id]
  (let [subexpr-ids (map first (d/q `[:find ?sid :where
                                      [?sid ::parent ~expr-id]]
                                    db))
        subexprs (map #(d/entity db %) subexpr-ids)
        ordered-subexprs (sort-by ::pos subexprs)]
    (map :db/id ordered-subexprs)))

(defn expr-id->expr-type
  "Given a database and an expression ID, returns the expression type."
  {:test (fn []
           (let [db (db-add-expr-from-parse-tree default-db ["a" ["b"]])]
             (is (= (expr-id->expr-type db 1) ::cmpd-expr))
             (is (= (expr-id->expr-type db 2) ::term-expr))
             (is (= (expr-id->expr-type db 3) ::cmpd-expr))))}
  [db expr-id]
  (::type (d/entity db expr-id)))

(defn term-expr-id?
  "Returns whether the expression with ID expr-id in db is a term."
  {:test (fn []
           (let [db (db-add-expr-from-parse-tree default-db ["1"])]
             (is (not (term-expr-id? db 1)))
             (is (term-expr-id? db 2))))}
  [db expr-id]
  (= (expr-id->expr-type db expr-id) ::term-expr))

(defn term-expr-id->term-text
  "Given a database and a term expression ID, returns term text."
  {:test (fn []
           (is (= (term-expr-id->term-text (db-add-expr-from-parse-tree default-db "1") 1) "1")))}
  [db expr-id]
  (::text (d/entity db expr-id)))