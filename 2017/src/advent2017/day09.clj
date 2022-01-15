(ns advent2017.day09
  (:require [rolfrander.puzzle-lib :refer [get-data]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is are run-all-tests]]))

(def parse-line seq)

(defn parse [input]
  (map parse-line (str/split-lines input)))

(def ^:dynamic *debug* false)

(defn debug [input]
  (when *debug* (println input))
  input)

(defn ignore-garbage [data]
  (loop [data data]
    (case (first data)
      nil nil
      \> (rest data)
      \! (recur (rest (rest data)))
      
      (recur (rest data)))))

(defn count-garbage [data]
  (loop [data data
         cnt 0]
    (case (first data)
      nil nil
      \> [(rest data) cnt]
      \! (recur (rest (rest data)) cnt)

      (recur (rest data) (inc cnt)))))

(defn score
  ([data] (score data 0))

  ([data depth]
   (loop [data data
          running-score 0]
     (case (first data)
       nil running-score
       \{ (let [[data subscore] (score (rest data) (inc depth))]
            (recur data (+ running-score subscore)))
       
       \} (debug [(rest data) (+ depth running-score)])
       
       \< (recur (ignore-garbage data) running-score)

       (recur (rest data) running-score)
       ))))

(deftest score-test
  (are [input res] (= res (score (parse-line input)))
    "{}" 1
    "{{{}}}" 6
    "{{}{}}" 5
    "{{{},{},{{}}}}" 16
    "{<a>,<a>,<a>,<a>}" 1
    "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
    "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
    "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3
    "{}{}" 2))

(defn task-1 [input]
  (reduce + (map score (parse input))))

(defn task-2 [input]
  (loop [data (parse-line input)
         cnt 0]
    (case (first data)
      nil cnt
      \< (let [[data cnt2] (count-garbage (rest data))]
           (recur data (+ cnt cnt2)))
      (recur (rest data) cnt))))

(deftest data-2-test
  (are [input res] (= res (task-2 input))
    "<>" 0
    "<random characters>" 17
    "<<<<>" 3
    "<{!>}>" 2
    "<!!>" 0
    "<!!!>>" 0
    "<{o\"i!a,<{i<a>" 10))


(task-1 (get-data 2017 9))
;; => 12396

(task-2 (get-data 2017 9))
;; => 6346


(run-all-tests #"advent2017.day09")