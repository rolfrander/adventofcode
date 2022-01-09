(ns advent2021.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def test "199
200
208
210
200
207
240
269
260
263")

(defn depth-seq-test [test]
  (string/split-lines test))

(defn parse-num [input]
  (map #(Long/parseLong %) input))

(defn count-increase [input]
  (reduce +
          (map #(if (< %1 %2) 1 0) input (rest input))))

(count-increase (parse-num (depth-seq-test test)))

(count-increase (parse-num (line-seq (io/reader "resources/day01.txt"))))

(defn calc-sliding-window [input]
  (let [input2 (rest input)
        input3 (rest input2)]
    (map + input input2 input3)))

(count-increase (calc-sliding-window (parse-num (line-seq (io/reader "resources/day01.txt")))))
