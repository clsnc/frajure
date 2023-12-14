(ns frajure.code.tokenize
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))

(defn tokenize-line
  "Parses a Clojure string into a tree of strings."
  {:test (fn []
           (is (= (vec (tokenize-line "these are (  ( ) \t\t)   (some)\twords "))
                  ["these" " " "are" " " "(" "  " "(" " " ")" " \t\t" ")" "   " "(" "some" ")" "\t" "words" " "])))}
  [text]
  (filter not-empty (str/split text #"([ \t]+|[(]|[)])")))

(defn close-paren-token?
  "Returns whether a token is a closing parenthesis."
  {:test (fn []
           (is (close-paren-token? ")"))
           (is (not (close-paren-token? "(")))
           (is (not (close-paren-token? "\t \t")))
           (is (not (close-paren-token? "sym"))))}
  [token]
  (= token ")"))

(defn newline-token?
  "Returns whether a token is a newline token."
  {:test (fn []
           (is (newline-token? "\n"))
           (is (not (newline-token? "\t"))))}
  [token]
  (= token "\n"))

(defn open-paren-token?
  "Returns whether a token is an opening parenthesis."
  {:test (fn []
           (is (open-paren-token? "("))
           (is (not (open-paren-token? ")")))
           (is (not (open-paren-token? "\t \t")))
           (is (not (open-paren-token? "sym"))))}
  [token]
  (= token "("))

(defn whitespace-token?
  "Returns whether a token is a whitespace token."
  {:test (fn []
           (is (whitespace-token? " "))
           (is (whitespace-token? "\t"))
           (is (whitespace-token? " \t"))
           (is (not (whitespace-token? "(")))
           (is (not (whitespace-token? "sym"))))}
  [token]
  (let [first-char (first token)]
    (boolean (or (= first-char " ") (= first-char "\t")))))

(defn sym-token?
  "Returns whether a token is a symbol."
  {:test (fn []
           (is (sym-token? "sym"))
           (is (not (sym-token? " ")))
           (is (not (sym-token? "(")))
           (is (not (sym-token? ")"))))}
  [token]
  (not (or (open-paren-token? token) (close-paren-token? token) (whitespace-token? token))))