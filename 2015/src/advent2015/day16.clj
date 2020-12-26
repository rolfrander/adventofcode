(ns advent2015.day16
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]
            [clojure.data.json :as json]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)

(def sender-has
  {"children" 3
   "cats" 7
   "samoyeds" 2
   "pomeranians" 3
   "akitas" 0
   "vizslas" 0
   "goldfish" 5 
   "trees" 3
   "cars" 2
   "perfumes" 1})

(def sender-predicate
  {"children" =
   "cats" <
   "samoyeds" =
   "pomeranians" >
   "akitas" =
   "vizslas" =
   "goldfish" >
   "trees" <
   "cars" =
   "perfumes" =})

(defn match-sender [line]
  (let [[_sue has] (str/split line #": " 2)]
    (->> (str/split has #", ")
         (map #(str/split % #": "))
         (every? (fn [[element cnt]]
                   (when (not (contains? sender-predicate element))
                     (throw (IllegalStateException. (str "illegal element " element))))
                   ((get sender-predicate element) (get sender-has element)
                                                   (Long/parseLong cnt)))))))

(->> (slurp "resources/2015/day16.txt")
     str/split-lines
     (filter match-sender))
;; => ("Sue 323: perfumes: 1, trees: 6, goldfish: 0")



;; => ("Sue 213: children: 3, goldfish: 5, vizslas: 0")
