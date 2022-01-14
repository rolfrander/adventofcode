(ns advent2017.day02
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.string :as str]
            [clojure.test :refer [deftest are is run-all-tests]]))

(defn parse [input]
  (->> (str/split-lines input)
       (map #(map safe-parse-number (str/split % #"[ \t]+")))))

(def testdata "5 1 9 5
7 5 3
2 4 6 8")

(def testdata2 "5 9 2 8
9 4 7 3
3 8 6 5")

(defn task-1 [input]
  (->> (parse input)
       (map (juxt (partial apply max) (partial apply min)))
       (map (partial apply -))
       (reduce +)))

(deftest task-1-test
  (is (= 18 (task-1 testdata))))

(defn find-even-division [coll]
  (first (for [a coll
               b coll
               :when (not= a b)
               :when (zero? (mod a b))]
           (quot a b))))

(defn task-2 [input]
  (->> (parse input)
       (map find-even-division)
       (reduce +))
  )

(map find-even-division (parse testdata2))

(deftest task-2-test
  (is (= 3 (find-even-division [5 9 2 8])))
  (is (= 9 (task-2 testdata2))))

(task-1 (get-data 2017 2))
;; => 46402

(task-2 (get-data 2017 2))
;; => 265


(run-all-tests #"advent2017.day02")