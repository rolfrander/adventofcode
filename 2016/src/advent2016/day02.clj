(ns advent2016.day02
  (:require [advent2016.core :as core]
            [clojure.string :as string]))

(def testdata "ULL
RRDDD
LURDL
UUUUD")

(def keypad [[1 2 3]
             [4 5 6]
             [7 8 9]])

(defn pos-to-num [pos]
  (get-in keypad pos))

(def keypad2 [[nil nil \1 nil nil]
              [nil  \2 \3  \4 nil]
              [ \5  \6 \7  \8  \9]
              [nil  \A \B  \C nil]
              [nil nil \D nil nil]])

(defn pos-to-num2 [pos]
  (get-in keypad2 pos))

(defn parse [input]
  (->> (string/split-lines input)
       (map #(string/split % #""))))

(defn task-1 [data]
  (letfn [(next-pos [cur-pos instructions]
                    (reduce (fn [[row col] i]
                              (case i
                                "U" [(max (dec row) 0) col]
                                "D" [(min (inc row) 2) col]
                                "L" [row (max (dec col) 0)]
                                "R" [row (min (inc col) 2)]))
                            cur-pos instructions))]
    (->> (reduce (fn [[code pos] instructions]
                   (let [pos (next-pos pos instructions)]
                     [(conj code (pos-to-num pos)) pos]))
                 [[] [1 1]]
                 data)
         first
         string/join)))

(defn task-2 [data]
  (letfn [(safe-move [pos direction]
                     (let [next-pos (map + pos direction)]
                       (if (or (> (apply max next-pos) 4)
                               (< (apply min next-pos) 0)
                               (nil? (pos-to-num2 next-pos)))
                         pos
                         next-pos)))
          (next-pos [cur-pos instructions]
                    (reduce (fn [pos i]
                              (case i
                                "U" (safe-move pos [-1 0])
                                "D" (safe-move pos [ 1 0])
                                "L" (safe-move pos [ 0 -1])
                                "R" (safe-move pos [ 0  1])))
                            cur-pos instructions))]
    (->> (reduce (fn [[code pos] instructions]
                   (let [pos (next-pos pos instructions)]
                     [(conj code (pos-to-num2 pos)) pos]))
                 [[] [2 0]]
                 data)
         first
         string/join
         )))

(task-1 (parse testdata)) ; 1985
(task-1 (parse (core/get-data 2016 2)))

(task-2 (parse testdata)) ; 5DB3
(task-2 (parse (core/get-data 2016 2))) ; B3C27
