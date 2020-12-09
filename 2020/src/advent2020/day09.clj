(ns advent2020.day09
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def testdata "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(defn parse [input]
  (let [data (str/split-lines input)
        values (long-array (count data))]
    (reduce (fn [i element]
              (aset-long values (int i) (Long/parseLong (str element)))
              (inc i))
            0
            data)
    values))

(defn check-number [^longs data num preamble]
  (let [find (aget data num)]
    ;(printf "look for d[%d] = %d\n" num find)
    (some #(= % find)
          (for [i (range (- num preamble) num)
                j (range (- num preamble) num)
                :while (not= i j)]
            (let [sum (+ (aget data i) (aget data j))]
              sum)))))

(def data (parse (slurp "day09.txt")))

(let [preamble 25
      d data]
  (some (fn [i]
          (if (check-number d i preamble)
            nil
            [i (aget d i)]))
        (range preamble (count d))))
;; => [646 1038347917]

(defn find-target [data target]
  (loop [accum (aget data 0)
         i 0
         j 0]
    (cond
      (> accum target) (recur (- accum (aget data i))
                              (inc i)
                              j)
      (< accum target) (recur (+ accum (aget data (inc j)))
                              i
                              (inc j))
      :else (let [[min max] (reduce (fn [[a b] current]
                                      (let [aa (min a (aget data current))
                                            bb (max b (aget data current))]
                                        [aa bb]))
                                    [(aget data i) (aget data j)]
                                    (range i (inc j)))
                  minmax (+ min max)] 
              {:i i, :j j, :di (aget data i), :dj (aget data j), :min-plus-max minmax})
      )))

(find-target data 1038347917)
;; => {:i 529, :j 545, :di 44615102, :dj 71185283, :min-plus-max 137394018}


(some #(if % % false) '(nil false true 3 nil))


(some #(do (println %) (not (nil? %)))
     (for [i (range 0 5)
           j (range 0 5)
           :while (not= i j)]
       (do (println i j)
           (if (= 5 (+ i j)) [i j] nil))))