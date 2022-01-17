(ns advent2017.day10
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.test :refer [deftest are run-all-tests]]
            [clojure.string :as str]))

(defn parse [input]
  (map safe-parse-number (re-seq #"\d+" input)))

(defn parse2 [input]
  (map int (str/trim input)))

(def testdata "3, 4, 1, 5")

(defn reverse-and-accumulate
  "Pre-processing of the list of input-lengths.
   Returning list of start-pos and length in reversed order. 

   * `s` is original list of widths, 
   * `iv` is lengths of input-vector"
  [s iv]

  (loop [s s
         pos 0
         skip 0
         ret '()]
    (if (empty? s)
      ret
      (let [width (first s)
            next-pos (mod (+ pos skip width) iv)]
        (recur (rest s) next-pos (inc skip) (cons [pos width] ret))))))

(defn shift-pos [iv [start-pos length] pos]
  (if (<= length 1)
    pos
    (let [end-pos (+ start-pos length)
          extended-pos (if (< pos start-pos)
                         (+ pos iv)
                         pos)
          offset (- extended-pos start-pos)]
      (if (and (<= start-pos extended-pos) (< extended-pos end-pos))
        (mod (+ start-pos (- (dec length) offset))
             iv)
        pos))))

(defn get-values-at [iv skip-list coll]
  (loop [skip-list skip-list
         values coll]
    (if (empty? skip-list)
      values
      (recur (rest skip-list)
             (doall (map (partial shift-pos iv (first skip-list)) values))))))

(defn prepare-for-task-2 [input]
  (->> (concat (parse2 input) [17, 31, 73, 47, 23])
       (repeat 64)
       flatten))

(defn task-1 [input iv]
  (let [data (parse input)
        skip-list (reverse-and-accumulate data iv)]
    (apply * (get-values-at iv skip-list [0 1]))))

(defn knot-hash [input]
  (let [data (doall (prepare-for-task-2 input))
        skip-list (doall (reverse-and-accumulate data 256))]
    (->> (get-values-at 256 skip-list (range 256))
         (partition 16)
         (map (partial apply bit-xor))
         )))

(defn task-2 [input iv]
  (->> (knot-hash input)
       (map (partial format "%02x"))
       (apply str)))

;(task-1 testdata 5)
;(task-1 (get-data 2017 10) 256)

(deftest task-2-test
  (are [in out] (= out (task-2 in 256))
    ""         "a2582a3a0e66e6e86e3812dcb672a272"
    "AoC 2017" "33efeb34ea91902bb2f59c9920caa6cd"
    "1,2,3"    "3efbe78a8d82f29979031a4aa0b16a9d"
    "1,2,4"    "63960835bcdc130f0b66d7ff4f6a5a8e"))

(task-2 (get-data 2017 10) 256)

;(prepare-for-task-2 "1,2,3")
;; => (49 44 50 44 51 17 31 73 47 23)
;;     49,44,50,44,51,17,31,73,47,23

(run-all-tests #"advent2017.day10")