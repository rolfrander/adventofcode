(ns advent2024.day03
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def testdata2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(def data (puzzle/get-data 2024 03))

(defn parse [in]
  (->> (re-seq #"mul\(([0-9]+),([0-9]+)\)|do\(\)|don't\(\)" in)
       (map (fn [[ins op-a op-b]] [ins
                                   (puzzle/str->long op-a)
                                   (puzzle/str->long op-b)]))))

;(parse testdata2)

(defn solve-1 [in]
  (->> (parse in)
       (filter #(str/starts-with? (first %) "mul"))
       (map (comp (partial apply *) rest))
       (reduce +)))

(defn solve-2 [in]
  (loop [[ip & mem] (parse in)
         do-mul true
         result 0]
    (if (seq ip)
      (case (first ip)
        "do()" (recur mem true result)
        "don't()" (recur mem false result)
        (if do-mul
          (recur mem do-mul (+ result (apply * (rest ip))))
          (recur mem do-mul result)))
      result)))

(solve-1 testdata2)
(solve-1 data)
;;=> 175615763


(solve-2 testdata2)
(solve-2 data)
;;=> 74361272
