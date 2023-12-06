(ns frajure.code.parse
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [frajure.code.values :as vals]))

(defn str->parse-tree
  "Parses a Clojure string into a tree of strings."
  {:test (fn []
           (is (= (str->parse-tree "these are    some\twords") ["these" "are" "some" "words"])))}
  [text]
  (vec (str/split text #"[ \t]+"))) ;; split on tabs or spaces

(defn- clj-str->clj-int
  "Converts text to an integer. Returns nil if the text is not a valid integer."
  {:test (fn []
           (is (= (clj-str->clj-int "58") 58))
           (is (= (clj-str->clj-int "-123") -123))
           (is (nil? (clj-str->clj-int "abc")))
           (is (nil? (clj-str->clj-int "")))
           (is (nil? (clj-str->clj-int " \t"))))}
  [text]
  (let [parsed-int (js->clj (js/parseInt text))]
    (when-not (NaN? parsed-int)
      parsed-int)))

(defn- clj-str->frj-val
  "Converts a Clojure string to a Frajure value (integer or symbol)."
  {:test (fn []
           (is (= (clj-str->frj-val "-88") (vals/clj-int->frj-int -88)))
           (is (= (clj-str->frj-val "?test-sym") (vals/clj-str->frj-sym "?test-sym"))))}
  [text]
  (let [parsed-int (clj-str->clj-int text)]
    (if parsed-int
      (vals/clj-int->frj-int parsed-int)
      (vals/clj-str->frj-sym text))))

(defn clj-str-tree->frj-arr-tree
  "Converts a Clojure string tree (nested Clojure sequences of Clojure strings) to a Frajure 
   array tree (nested Frajure arrays of Frajure values)."
  {:test (fn []
           (is (=
                (clj-str-tree->frj-arr-tree ["abc" ["123" [["nested-deep"]]] "end"])
                (vals/clj-seq-of-frj-vals->frj-arr
                 [(clj-str->frj-val "abc")
                  (vals/clj-seq-of-frj-vals->frj-arr
                   [(clj-str->frj-val "123")
                    (vals/clj-seq-of-frj-vals->frj-arr
                     [(vals/clj-seq-of-frj-vals->frj-arr
                       [(clj-str->frj-val "nested-deep")])])])
                  (clj-str->frj-val "end")]))))}
  [clj-str-tree]
  (if (string? clj-str-tree)
    (clj-str->frj-val clj-str-tree)
    (vals/clj-seq-of-frj-vals->frj-arr (map clj-str-tree->frj-arr-tree clj-str-tree))))