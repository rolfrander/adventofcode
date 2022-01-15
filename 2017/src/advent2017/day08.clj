(ns advent2017.day08
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-all-tests]]))

(defn parse-line [line]
  (let [[target operation amount _if register cmp cmpval] (map safe-parse-number (str/split line #" +"))]
    [target
     (fnil (case operation "inc" #(+ % amount) "dec" #(- % amount))
           0)
     register
     (fnil (case cmp
             ">"  #(> % cmpval)
             "<"  #(< % cmpval)
             "==" #(= % cmpval)
             ">=" #(>= % cmpval)
             "<=" #(<= % cmpval)
             "!=" #(not= % cmpval))
           0)]))

(defn parse [input]
  (map parse-line (str/split-lines input)))

(defn cpu [state [target op reg cmp]]
  (if (cmp (state reg)) 
    (update state target op)
    state))

(defn interpreter [program]
  (reduce cpu {} program))

(defn task-1 [input]
  (apply max (vals (interpreter (parse input)))))

(defn task-2 [input]
  (let [program (parse input)]
    (first (reduce (fn [[m state] instruction]
                     (let [state (cpu state instruction)]
                       [(max m (apply max 0 (vals state)))
                        state]))
                   [0 {}] program))))

(def testdata "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

(deftest task-1-test
  (is (= 1 (task-1 testdata))))

(deftest task-2-test
  (is (= 10 (task-2 testdata))))

(task-2 (get-data 2017 8))

(run-all-tests #"advent2017.day08")