(ns advent2021.day02
 (:require [clojure.java.io :as io]
           [clojure.string :as string]))

(def testdata "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn parse [input]
  (map #(let [[c a] (string/split % #" ")]
          [c (Long/parseLong a)]) 
       input))

(parse (string/split-lines testdata))
(first (line-seq (io/reader "resources/day02.txt")))

(defn task-1 [input]
  (let [data (parse input)
        [hor dep]
        (reduce (fn [[hor dep] [cmd amount]]
                  (case cmd
                    "down" [hor (+ dep amount)]
                    "up" [hor (- dep amount)]
                    "forward" [(+ hor amount) dep]))
                [0 0] data)]
    (* hor dep)))

(defn task-2 [input]
  (let [data (parse input)
        [hor dep]
        (reduce (fn [[hor dep aim] [cmd amount]]
                  (case cmd
                    "down" [hor dep (+ aim amount)]
                    "up" [hor dep (- aim amount)]
                    "forward" [(+ hor amount) (+ dep (* amount aim)) aim]))
                [0 0 0] data)]
    (* hor dep)))

(task-1 (line-seq (io/reader "resources/day02.txt")))
(task-2 (string/split-lines testdata))
(task-2 (line-seq (io/reader "resources/day02.txt")))

