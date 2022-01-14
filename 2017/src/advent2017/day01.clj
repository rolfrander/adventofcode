(ns advent2017.day01
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.test :refer [deftest is are run-all-tests]]
            [clojure.string :as str]))

(defn validator
  "Create a version of a predicate that only tests its output for truthiness,
  returning the original input value if the predicate evaluates to anything
  truthy, and nil otherwise. ((validator even?) 10) => 10, even though
  (even? 10) is true."
  [pred]
  (fn [x]
    (when (pred x)
      x)))

(defn char->digit [c]
  (- (int c) (int \0)))

(defn task-1 [input]
  (->> (concat (seq input) )
       (partition 2 1 [(first input)])
       (keep (validator (partial apply =)))
       (map first)
       (map char->digit)
       (reduce +)))

(deftest task-1-test
  (are [in out] (= out (task-1 in))
    "1122" 3
    "1111" 4
    "1234" 0
    "91212129" 9
    "31813174349235972159811869755166" 12))

(defn task-2 [^String input]
  (let [len (.length input)
        half (/ len 2)]
    (->> (range half)
         (keep #(let [a (.charAt input %)
                      b (.charAt input (+ half %))]
                  (if (= a b) a)))
         (map char->digit)
         (reduce +)
         (* 2))))

(deftest task-2-test
  (are [in out] (= out (task-2 in))
    "1212" 6
    "1221" 0
    "123425" 4
    "123123" 12
    "12131415" 4))


(task-1 (str/trim-newline (get-data 2017 1)))
(task-2 (str/trim-newline (get-data 2017 1)))

(run-all-tests #"advent2017.day01")