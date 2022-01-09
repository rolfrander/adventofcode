(ns advent2021.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def testdata "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(defn board-new [numbers]
  (partition 5 numbers))

(defn board-place [board number]
  (map (fn [row]
         (map (fn [item]
                (if (= item number)
                  nil
                  item))
              row))
       board))

(defn board-score [board]
  (let [check-row (fn [row] (every? nil? row))]
    (if (or (some true? (map check-row board))
            (some true? (map check-row (apply map list board))))
      (apply + (filter number? (flatten board))))))

(defn transform-tree [tx tree]
  (if (seq? tree)
    (for [x tree] (transform-tree tx x))
    (tx tree)))

(defn board-parse [text-lines]
(let [data (->> (re-seq #"(?!\n)(\d| |,|\n(?!\n))+" text-lines)
                (map first))
      input (first data)
      boards (rest data)
      parsed-boards  (->> boards
                          (transform-tree string/split-lines)
                          (transform-tree (fn [line] (map #(re-seq #"\d+" %) line)))
                          (transform-tree #(Long/parseLong %))
                          )
      parsed-input (map #(Long/parseLong %) (re-seq #"\d+" input))]
  [parsed-input parsed-boards])
)


(defn task-1 [data]
  (let [[winner last-call]
        (loop [[input boards] (board-parse data)
               last-call 0]
          (if-let [winners (seq (filter board-score boards))]
            [winners last-call]
            (recur [(rest input)
                    (map #(board-place % (first input)) boards)]
                   (first input))))]
    (map #(* (board-score %) last-call) winner)))

(defn task-2 [data]
  (let [[winner last-call]
        (loop [[input boards] (board-parse data)
               last-call 0]
          (let [winners (seq (remove board-score boards))]
            (if (= 0 (count winners))
              [boards last-call]
              (recur [(rest input)
                      (map #(board-place % (first input)) winners)]
                     (first input)))))]
    (map #(* (board-score %) last-call) winner)
    ))

;(board-parse testdata)
(task-2 testdata)
(task-2 (slurp "resources/day04.txt"))

(nil? (or nil nil nil))

(board-score (board-new [nil 2 3 4 5 nil 7 8 9 10 nil 12 13 14 15]))