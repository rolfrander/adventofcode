(ns advent2021.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def testdata "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn parse [datalines]
  (mapv (fn [line] (mapv #(- (int %) (int \0)) line)) 
        datalines)
  )

(parse (string/split-lines testdata))

(def data (parse (string/split-lines (slurp "resources/day09.txt"))))

(def neighbours [[-1 0] [1 0]
                 [0 -1] [0 1]])

(defn list-lowpoints [data]
  (let [max-i (count data)
        max-j (count (first data))
        get-neighbours (fn [i j]
                         (keep (fn [[di dj]]
                                 (let [x (+ j dj)
                                       y (+ i di)]
                                   (if (and (>= x 0) (< x max-j)
                                            (>= y 0) (< y max-i))
                                     (get-in data [y x])
                                     nil)))
                               neighbours))]
    (keep (fn [[element neighbours]]
            (if (every? #(< element %) neighbours)
              element
              nil))
     (for [i (range max-i)
           j (range max-j)]
       [(get-in data [i j])
        (get-neighbours i j)])))
  )

;; task-1
(let [low (list-lowpoints data)]
  (+ (apply + low)
     (count low)))

;; task-2

(let [grid (mapv #(into-array Byte/TYPE %) data)
      max-i (count grid)
      max-j (count (first grid))
      e (fn [i j] (aget (get grid i) j))]
  (letfn [(flood-fill-count [i j]
            (if (or (< i 0)
                    (< j 0)
                    (>= i max-i)
                    (>= j max-j)
                    (= 9 (e i j)))
              0
              (do
                (aset (get grid i) j (byte 9))
                (reduce (fn [sum [di dj]]
                          (+ sum (flood-fill-count (+ i di)
                                                   (+ j dj))))
                        1
                        neighbours))))]
    (->> (doall (filter #(> % 0)
                        (for [i (range max-i)
                              j (range max-j)]
                          (flood-fill-count i j))))
         (sort >)
         (take 3)
         (apply *))))

(let [grid (mapv #(into-array Byte/TYPE %) (parse (string/split-lines testdata)))]
  (aget (get grid 1) 0))
