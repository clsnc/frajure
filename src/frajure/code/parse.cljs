(ns frajure.code.parse
  (:require [clojure.core.rrb-vector :as v]
            [clojure.string :as str]
            [clojure.test :refer [is]]
            [frajure.code.db :as cdb]
            [frajure.code.tokenize :as tok]
            [frajure.code.values :as vals]
            [frajure.utils :as u]))

(defn- text->indent-depth
  "Returns the number of tabs at the beginning of a string."
  {:test (fn []
           (is (= (text->indent-depth "\t\t\ttext") 3))
           (is (zero? (text->indent-depth "text")))
           (is (zero? (text->indent-depth ""))))}
  [text]
  (count (take-while #(= \tab %) text)))

(defn- text->indent-trees
  "Returns trees of lines of text, with the tree structures derived from line indentations."
  {:test (fn []
           (is (= (text->indent-trees "text") ["text"]))
           (is (= (text->indent-trees "\n\n\na\n\nb\n") ["a" "b"]))
           (is (= (text->indent-trees "a\n\tb\n\tc") ["a" ["b" "c"]]))
           (is (= (text->indent-trees "a\n\tb\nc") ["a" ["b"] "c"]))
           (is (= (text->indent-trees "a\n\tb\n\t\tc\n\td\ne") ["a" ["b" ["c"] "d"] "e"]))
           (is (= (text->indent-trees "a\n\tb\n\t\tc\nd") ["a" ["b" ["c"]] "d"]))
           (is (= (text->indent-trees "a\n\tb\n\t\tc\n\t\t\td") ["a" ["b" ["c" ["d"]]]]))
           (is (nil? (text->indent-trees "a\n\t\tb")))
           (is (nil? (text->indent-trees "\ta\nb"))))}
  [text]
  (let [indented-lines (filter #(not (str/blank? %)) (str/split-lines text))
        indents (mapv text->indent-depth indented-lines)
        lines (map str/triml indented-lines)]
    (loop [els lines
           indents (conj indents -1 -1) ;; two -1s at the end allow handling of the last line being indented
           prev-indent -1
           curr-expr []
           expr-stack []]
      (let [[el & rest-els] els
            [indent & rest-indents] indents
            indent-change (- indent prev-indent)]
        (cond
          (= indent prev-indent -1) (first curr-expr) ;; pull the first because we started at -1 indentation
          (= indent-change 1) (recur rest-els rest-indents indent [el] (conj expr-stack curr-expr))
          (= indent-change 0) (recur rest-els rest-indents indent (conj curr-expr el) expr-stack)
          (< indent-change 0) (recur els indents (dec prev-indent) (conj (peek expr-stack) curr-expr) (pop expr-stack)))))))

(defn tokens->parse-tree
  "Converts a sequence of tokens to a parse tree."
  {:test (fn []
           (is (= (tokens->parse-tree (tok/tokenize-line "a test expr")) ["a" "test" "expr"]))
           (is (= (tokens->parse-tree (tok/tokenize-line "some (parens (close) properly)")) ["some" ["parens" ["close"] "properly"]]))
           (is (= (tokens->parse-tree (tok/tokenize-line "(extra nesting)")) [["extra" "nesting"]]))
           (is (nil? (tokens->parse-tree (tok/tokenize-line "some (parens (close) improperly"))))
           (is (nil? (tokens->parse-tree (tok/tokenize-line "too (much (closing)))")))))}
  [tokens]
  (loop [tokens tokens
         curr-expr []
         expr-stack []]
    (let [[curr-token & rest-tokens] tokens]
      (if curr-token
        (cond
          (tok/whitespace-token? curr-token) (recur rest-tokens curr-expr expr-stack)
          (tok/open-paren-token? curr-token) (recur rest-tokens [] (conj expr-stack curr-expr))
          (tok/close-paren-token? curr-token) (when (not-empty expr-stack)
                                                (recur rest-tokens (conj (peek expr-stack) curr-expr) (pop expr-stack)))
          :else (recur rest-tokens (conj curr-expr curr-token) expr-stack))
        (when (empty? expr-stack)
          curr-expr)))))

(defn- line->parse-tree
  "Returns a parse tree constructed from a line of text."
  {:test (fn []
           (is (= (line->parse-tree "a (b c d) e") ["a" ["b" "c" "d"] "e"])))}
  [line]
  (tokens->parse-tree (tok/tokenize-line line)))

(defn- indent-trees->parse-trees
  "Converts a sequence of index trees to a vector of parse trees."
  {:test (fn []
           (is (= (indent-trees->parse-trees []) []))
           (is (= (indent-trees->parse-trees ["a"]) [["a"]]))
           (is (= (indent-trees->parse-trees ["a" "b"]) [["a"] ["b"]]))
           (is (= (indent-trees->parse-trees ["a" ["b"] "c"]) [["a" ["b"]] ["c"]]))
           (is (= (indent-trees->parse-trees ["a" ["b" "c"]]) [["a" ["b"] ["c"]]]))
           (is (= (indent-trees->parse-trees ["a" ["b" ["c" ["d" "e"]] "f"]]) [["a" ["b" ["c" ["d"] ["e"]]] ["f"]]]))
           (is (= (indent-trees->parse-trees ["a" ["b (c d e) f" ["g"]]]) [["a" ["b" ["c" "d" "e"] "f" ["g"]]]]))
           (is (nil? (indent-trees->parse-trees ["a" ["b c" ["d (e"]] "f"]))))}
  [indent-trees]
  (loop [indent-trees indent-trees
         parse-trees []]
    (if (empty? indent-trees)
      parse-trees
      (let [[indent-tree & rest-indent-trees] indent-trees]
        (if (string? indent-tree)
          (let [line-parse-tree (line->parse-tree indent-tree)]
            (when line-parse-tree (recur rest-indent-trees (conj parse-trees line-parse-tree))))
          (let [parse-subtree (indent-trees->parse-trees indent-tree)]
            (when parse-subtree (recur rest-indent-trees (u/update-vec-last parse-trees v/catvec parse-subtree)))))))))

(defn pane-text->parse-trees
  "Given the text of a pane, returns the parse tree for that pane."
  {:test (fn []
           (is (= (pane-text->parse-trees "a b c\ne (f g)") [::cdb/pane ["a" "b" "c"] ["e" ["f" "g"]]]))
           (is (= (pane-text->parse-trees "a\n\nb\n") [::cdb/pane ["a"] ["b"]]))
           (is (= (pane-text->parse-trees "a b\n\tc (d e f)\n\t\tg h\ni") [::cdb/pane ["a" "b" ["c" ["d" "e" "f"] ["g" "h"]]] ["i"]]))
           (is (nil? (pane-text->parse-trees "a\nb (\nc")))
           (is (nil? (pane-text->parse-trees ""))))}
  [pane-text]
  (indent-trees->parse-trees (text->indent-trees pane-text))
  (let [parse-trees (indent-trees->parse-trees (text->indent-trees pane-text))]
    (when (not-empty parse-trees) (v/catvec [::cdb/pane] parse-trees))))

(defn clj-str->clj-int
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
                  (clj-str->frj-val "end")])))
           (is (= (clj-str-tree->frj-arr-tree []) (vals/clj-seq-of-frj-vals->frj-arr []))))}
  [clj-str-tree]
  (if (string? clj-str-tree)
    (clj-str->frj-val clj-str-tree)
    (vals/clj-seq-of-frj-vals->frj-arr (map clj-str-tree->frj-arr-tree clj-str-tree))))