(ns advent2021.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def testdata "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn parse-coords [coords]
  (let [[x y] (string/split coords #",")]
    [(Long/parseLong x)
     (Long/parseLong y)]))

(defn parse-line [line]
  (let [[from to] (string/split line #" -> ")]
    [(parse-coords from)
     (parse-coords to)]))

(parse-line "123,345 -> 456,567")

(defn parse [input-lines]
  (map parse-line input-lines))

(defn flexible-range [a b]
  (cond (= a b) (repeat a)
        (> a b) (range a (dec b) -1)
        (< a b) (range a (inc b))))

(defn extend-hv [[[x1 y1][x2 y2]]]
  (cond (= x1 x2) (map #(vector x1 %) (flexible-range y1 y2))
        (= y1 y2) (map #(vector % y1) (flexible-range x1 x2))
        :else []))

(defn extend [[[x1 y1][x2 y2]]]
  (map #(vector %1 %2) 
       (flexible-range x1 x2)
       (flexible-range y1 y2)))


; (extend [[0 9] [5 9]])
(extend [[8 0] [0 0]])

(defn task-1 [input-lines]
  (letfn [(myinc [i] (if (number? i) (inc i) 1))]
    (->> (mapcat extend-hv (parse input-lines))
         (reduce #(update %1 %2 myinc) {})
         )))

(defn task-2 [input-lines]
  (letfn [(myinc [i] (if (number? i) (inc i) 1))]
    (->> (mapcat extend (parse input-lines))
         (reduce #(update %1 %2 myinc) {})
         )))

(defn filter-and-count [board]
  (->> board
       (filter (fn [[k v]] (> v 1)))
       count))

(defn print-board [board w h]
  (dotimes [i h]
    (dotimes [j w]
      (if (contains? board [j i])
        (print (get board [j i]))
        (print ".")))
    (println)))

(print-board (task-2 (string/split-lines testdata))
             10 10)

(print-board (task-2 (line-seq (io/reader "resources/day05.txt")))
             30 30)

(filter-and-count (task-2 (string/split-lines testdata)))

(filter-and-count (task-2 (line-seq (io/reader "resources/day05.txt"))))
