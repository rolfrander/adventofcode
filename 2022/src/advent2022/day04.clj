(ns advent2022.day04 
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn parse [input]
  (->> (str/split-lines input)
       (map (partial re-seq #"[0-9]+"))
       (map (partial map puzzle/str->long))))

(defn task-1 [input]
  (let [check-line (fn [[a b c d]]
                     (or (and (>= a c)
                              (<= b d))
                         (and (<= a c)
                              (>= b d))))]
    (->> (parse input)
         (filter check-line)
         count)))

(defn task-2 [input]
  (let [check-line (fn [[a b c d]]
                     (or (<= a c b)
                         (<= a d b)
                         (<= c a d)
                         (<= c b d)))]
    (->> (parse input)
         (filter check-line)
         count)))


(task-1 testdata)
(task-1 (puzzle/get-data 2022 4))

(task-2 testdata)
(task-2 (puzzle/get-data 2022 4))

