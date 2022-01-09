(ns advent2021.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def testdata (string/split-lines "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"))

(- (int (first "1")) (int \0))

(defn parse [bits]
  (mapv #(- (int %) (int \0)) bits))

(defn bin-to-num [a]
  (reduce (fn [res n]
            (bit-or (bit-shift-left res 1)
                    n))
          0 a))

(bin-to-num [1 0 1 0 0])

(defn task-1 [data]
  (let [gamma-bits (->> (rest data)
                        (reduce (fn [counts line]
                                  (map + counts (parse line)))
                                (parse (first data)))
                        (map #(if (>= % (/ (count data) 2)) 1 0)))
        gamma (bin-to-num gamma-bits)
        epsilon-bits (mapv #(- 1 %) gamma-bits)
        epsilon (bin-to-num epsilon-bits)]
    [epsilon gamma gamma-bits (* epsilon gamma)]))

(task-1 testdata)
(task-1 (line-seq (io/reader "resources/day03.txt")))

(defn task-2-filter [compare data]
  (loop [input data
         i 0]
    (if (= 1 (count input))
      (first input)
      (let [f (group-by #(nth % i) input)]
        (if (compare (count (get f 0))
                     (count (get f 1)))
          (recur (get f 0) (inc i))
          (recur (get f 1) (inc i)))))))

(defn task-2 [data]
  (let [parsed-data (map parse data)]
    (->> [(task-2-filter > parsed-data)
          (task-2-filter <= parsed-data)]
         (map bin-to-num)
         (apply *))))

(task-2 testdata)
(task-2 (line-seq (io/reader "resources/day03.txt")))