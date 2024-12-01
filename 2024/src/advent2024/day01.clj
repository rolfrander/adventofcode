(ns advent2024.day01
  (:require [rolfrander.puzzle-lib :as puzzle]))

(def data (puzzle/get-data 2024 01))

(def testdata "3   4
4   3
2   5
1   3
3   9
3   3")

(defn parse [input]
  (->> (re-seq #"[0-9]+" input)
       (map puzzle/str->long)
       (partition 2)
       (apply mapv vector)))

(defn solve [in]
  (let [[a b] (parse in)
        a (sort a)
        b (sort b)]
    (->> (map #(Math/abs (- %1 %2)) a b)
         (reduce +))))

(defn solve-2 [in]
  (let [[a b] (parse in)
        b-freq (frequencies b)]
    (->> (map #(* % (get b-freq % 0)) a)
         (reduce +))))

(solve testdata)
(solve data)
;;=> 1603498

(solve-2 testdata)
(solve-2 data)
;;=> 25574739
