(ns advent2017.day05
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.test :refer [deftest is run-all-tests]]))

(defn parse [input]
  (mapv safe-parse-number (re-seq #"[-0-9]+" input)))

(defn simulate [program]
  (loop [cnt 0
         ip 0
         p program]
    ;(println p)
    (if-not (< -1 ip (count program))
      cnt
      (recur (inc cnt)
             (+ ip (p ip))
             (update p ip inc)))))

(defn simulate-2 [program]
  (loop [cnt 0
         ip 0
         p program]
    ;(println ip p)
    (if-not (< -1 ip (count program))
      cnt
      (let [offset (p ip)]
        (recur (inc cnt)
               (+ ip offset)
               (update p ip (if (>= offset 3) dec inc)))))))

(defn task-1 [input]
  (simulate (parse input)))

(defn task-2 [input]
  (simulate-2 (parse input)))

(def testdata "0
3
0
1
-3")

(deftest task-1-test
  (is (= 5 (task-1 testdata))))

(deftest task-2-test
  (is (= 10 (task-2 testdata))))

(task-2 (get-data 2017 05)) ; 26395586

(run-all-tests #"advent2017.day05")