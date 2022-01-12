(ns advent2016.day18
  (:require [rolfrander.puzzle-lib :as puzzle]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn is-trap [[left center right]]
  (or (and left center (not right))
      (and (not left) center right)
      (and left (not center) (not right))
      (and (not left) (not center) right)))

(defn parse [input]
  (map (partial = \^) (str/trim-newline input)))

(test/deftest parse-test
  (test/is (= [false false true true false] (parse "..^^."))))

(defn next-line [line]
  (map is-trap (partition 3 1 (concat [false] line [false]))))

(test/deftest next-line-test
  (test/are [in out] (= (parse out) (next-line (parse in)))
    "..^^." ".^^^^"
    ".^^^^" "^^..^"))

(defn task-1 [input rows]
  (letfn [(count-safe [tiles] (count (remove true? tiles)))]
    (loop [current input
           safe-cnt 0
           line 0]
      (if (>= line rows)
        safe-cnt
        (recur (next-line current)
               (+ safe-cnt (count-safe current))
               (inc line))))))

(def data (parse (puzzle/get-data 2016 18)))

(test/deftest task-1-test
  (test/is (= 38 (task-1 (parse ".^^.^.^^^^") 10))))

(task-1 data 40)
;; => 1978

(task-1 data 400000)
;; => 20003246


(test/run-all-tests #"advent2016.day18")