(ns advent2020.day10
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def testdata "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(defn parse-data [input]
  (map #(Integer/parseInt %) (str/split-lines input)))

(def sorted-data
  (sort (parse-data (slurp "day10.txt"))))

(def sorted-data-test
  (sort (parse-data testdata)))

(let [data sorted-data]
  (let [[cnt1 cnt3 last-level]
        (->> data
             (reduce (fn [[diff1 diff3 prev] element]
                       (case (- element prev)
                         1 [(inc diff1) diff3 element]
                         3 [diff1 (inc diff3) element]
                         :else [diff1 diff3 element]))
                     [0 0 0])
             )]
    (* cnt1 (inc cnt3))))
;; => 2263

(def combinations
  (memoize
   (fn [^long count]
     (let [result (case count
                    0 [1 0 0] ; pathological case
                    1 [1 1 0]
                    2 [2 2 0]
                    3 [4 2 1]
                    (let [[a b c] (combinations (dec count))]
                      [(+ a b c) a b]))]
       result))))

(def combinations-mem (memoize combinations))

(time 
 (->> sorted-data
      (reductions (fn [[part val] next]
                    [(if (= 3 (- next val)) (inc part) part) next])
                  [0 0])
      (partition-by first)
      (map count)
      ;(map #(if (> % 2) (apply + (combinations (- % 2))) 1))
      ;(apply *)
      ))
;; => 396857386627072



(time (apply + (combinations 60)))

((comp (partial + 3 ) (partial * 2)) 5)

(def xf (comp (filter odd?) (take 10)))

((xf conj) (range))