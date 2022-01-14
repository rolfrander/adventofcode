(ns advent2017.day03
  (:require [rolfrander.puzzle-lib :ref [get-data]]))

(defn get-y-larger-than [input n]
  (loop [i n
         y 1 ; ring number, couting from 0
         last-ring 2; one side of last ring
         diff (dec n)]
    (if (> i input)
      [i y last-ring]
      (let [diff (+ diff 8)]
        (recur (+ i diff)
               (inc y)
               (+ 2 last-ring)
               diff)))))

(get-y-larger-than 277678 2)

(get-y-larger-than 27 8)

(def data 277678)

(let [[i ring-no ring-side] (get-y-larger-than data 2)]
  (loop [j i
         x 0
         y (- ring-no)
         magic [[-1 -1] [-1 0] [-1 1] [0 1] [1 1] [1 0]]]
    (if (or (empty? magic) (< j input))
      [j x y]
      (recur (- j ring-no)
             (* ring-no (ffirst magic))
             (* ring-no (second (first magic)))
             (rest magic)))))

(- 277993 263 data)

(+ 263 (- 264 52))

; n = 1

; increase x n times
; increase y n times
; increase n
; decrease x n times
; decrease y n times
; increase n

; increase x n times
; increase y n times

(defn neighbours [x y]
  (map #(vector (+ x (first %))
                (+ y (second %)))
       [[0 1] [0 -1]  [1 0] [-1  0]
        [1 1] [-1 -1] [1 -1] [-1 1]]))

(defn value-at [board [x y]]
  (->> (neighbours x y)
       (keep board)
       (apply +)))

(def ^:dynamic *value* 806)

(defn apply-to [pred key coll]
  (apply pred (map key coll)))

(defn print-board [board]
  (let [pos (keys board)
        [min-x max-x] (apply-to (juxt min max) first pos)
        [min-y max-y] (apply-to (juxt min max) second pos)]
    (doseq [y (range max-y (dec min-y) -1)]
      (doseq [x (range min-x (inc max-x))]
        (if-let [n (board [x y])]
          (printf "%4d" n)
          (printf "    ")))
      (newline))))

(defn walk-side [board i j n step key]
  (loop [i (step i)
         n (dec n)
         board board]
    (let [next-val (value-at board (key i j))
          board (assoc board (key i j) next-val)]
      (cond (>= next-val *value*) [:end next-val (key i j)]
            
            (> n 0) (recur (step i) (dec n) board)

            :else [:cont board (key i j)]))))

(defn task-2 []
  (let [move-x #(vector %1 %2)
        move-y #(vector %2 %1)
        op (cycle [[move-x inc] [move-y inc] [move-x dec] [move-y dec]])
        [answer x y] (loop [n 1
                           x 0
                           y 0
                           shift-n false
                           board {[0 0] 1}
                           op op]
                      (let [[move step] (first op)
                            [res board [x y]] (walk-side board
                                                         (if shift-n y x)
                                                         (if shift-n x y)
                                                         n
                                                         step
                                                         move)]
                        (if (= :end res)
                          [board x y]
                          (recur (if shift-n (inc n) n)
                                 x
                                 y
                                 (not shift-n)
                                 board
                                 (rest op)))))]
    [answer x y]))

(binding [*value* data] (task-2))