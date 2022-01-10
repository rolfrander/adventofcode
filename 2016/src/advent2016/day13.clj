(ns advent2016.day13
    (:require [rolfrander.puzzle-lib :refer [dijkstra]]))

(def testdata 10)
(def data 1358)

(defn count-bits [^long i]
  (let [i (- i (bit-and (bit-shift-right i 1)  0x55555555))
        i (+   (bit-and                  i     0x33333333)
               (bit-and (bit-shift-right i 2)  0x33333333))
        i (bit-and (+ i (bit-shift-right i 4)) 0x0F0F0F0F)
        i (bit-shift-right                (* i 0x01010101) 24)]
    i))

;(count-bits 128)

(defn generate-board [w h magic]
  (->> (for [x (range w)
             y (range h)
             :when (even? (count-bits (+ (* x x)
                                         (* 3 x)
                                         (* 2 x y)
                                         y
                                         (* y y)
                                         magic)))]
         [x y])
       (into #{})))

(defn print-board [board]
  (let [w (inc (apply max (map first board)))
        h (inc (apply max (map second board)))]
    (dotimes [y h]
      (dotimes [x w]
        (if (board [x y])
          (print ".")
          (print "#")))
      (newline))))

;(print-board (generate-board 10 10 10))

(defn neighbours [board]
  (let [w (inc (apply max (map first board)))
        h (inc (apply max (map second board)))
        find-neighbours (fn [[x y]]
                          [[x y] (filter board [[(inc x) y]
                                              [(dec x) y]
                                              [x (inc y)]
                                              [x (dec y)]])])]
    (->> board
         (map find-neighbours)
         (into {}))))

(defn task-1 [w h magic start dest]
  (let [nodes (generate-board w h magic)
        map (neighbours nodes)]
    (dijkstra nodes start map :dest? (partial = dest) :result-type :dist)))

(defn task-2 [w h magic start]
  (->> (task-1 w h magic start [-1 -1])
       (filter (comp (partial >= 50) second))
       count))

(comment
  (task-1 10 10 10 [1 1] [7 4])

  (task-1 100 100 1358 [1 1] [31 39])

  (task-2 100 100 1358 [1 1])
  )

