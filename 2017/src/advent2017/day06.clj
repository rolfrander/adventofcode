(ns advent2017.day06
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.test :refer [deftest is run-all-tests]]))

(defn parse [input]
  (mapv safe-parse-number (re-seq #"[-0-9]+" input)))


(defn simulate [blocks]
  (let [w (count blocks)]
    (loop [b blocks
           cnt 0
           previous {}]
      ;(println b)
      (cond (previous b)
            [cnt (- cnt (previous b))]

            (> cnt 100000)
            -1

            :else
            (let [max-bank (apply max-key b (range (dec w) -1 -1)) 
                  val (b max-bank)
                  rounds (quot val w)
                  left (mod val w)
                  ; increase all by "rounds"
                  new-b (reduce #(update %1 %2 (partial + rounds))
                                (assoc b max-bank 0)
                                (range w))
                  ; then increase the rest with 1
                  new-b (reduce #(update %1 (mod (+ %2 max-bank 1) w) inc)
                                new-b
                                (range left))
                  ]
              (recur new-b (inc cnt) (assoc previous b cnt)))))))

(defn task-1 [input]
  (simulate (parse input)))

(def testdata "0 2 7 0")

(apply max-key [1 2 3 1] (range 4))

(deftest task-1-test
  (is (= [5 4] (task-1 testdata))))

(run-all-tests #"advent2017.day06")

(task-1 (get-data 2017 06))

