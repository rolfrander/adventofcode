(ns advent2015.day19
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)

(def testdata "H => HO
H => OH
O => HH

HOH")

(conj nil 42)

(defn parse-line [substitutions line]
  (if-let [m (re-matches #"([^ ]+) => (.*)" line)]
    [(update substitutions (nth m 1) conj (nth m 2)) nil]
    (if (empty? line)
      [substitutions nil]
      [substitutions line])))

(defn parse [input]
  (reduce (fn [[substitutions start] line]
            (parse-line substitutions line))
          [{} nil]
          input))

(defn parse-file [filename]
  (with-open [in (clojure.java.io/reader filename)]
    (doall (parse (line-seq in)))))

(defn split-molecule 
  ([subst mol] (split-molecule subst mol 0))
  ([subst mol start]
   (let [first-1 (.substring mol start (+ start 1))
         first-2 (.substring mol start (+ start 2))]
     (cond (contains? subst first-1)
           (cons first-1 (split-molecule subst mol (+ start 1)))
           
           (contains? subst first-2)
           (cons first-2 (split-molecule subst mol (+ start 2)))
           
           :else
           ))))

(parse-file "resources/2015/day19.txt")