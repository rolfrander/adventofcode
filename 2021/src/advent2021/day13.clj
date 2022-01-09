(ns advent2021.day13
  (:require [clj-http.client :as http]
            [clojure.string :as string]))

(def data
  (->> (http/get "https://adventofcode.com/2021/day/13/input"
                 {:cookies {"session" {:value session}}})
       :body
       string/split-lines))

(def data (->> "resources/day13.txt"
               slurp
               string/split-lines))

(def testdata (string/split-lines "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"))

(count data)

(defn parse [input]
  (letfn [(parse-function [line]
            (let [[_ axis value] (re-matches #"fold along ([xy])=([0-9]+)" line)
                  fold-line (Long/parseLong value)
                  fold-over (* 2 fold-line)]
              (if (= axis "x")
                (fn [[x y]] (if (> x fold-line) [(- fold-over x) y] [x y]))
                (fn [[x y]] (if (> y fold-line) [x (- fold-over y)] [x y])))))
          (parse-data [line]
            (map #(Long/parseLong %) (string/split line #",")))]
    (reduce (fn [ret line]
              (cond (.startsWith line "fold along ")
                    (update ret :folding #(conj % (parse-function line)))
                    
                    (= line "") ret

                    :else
                    (update ret :data #(conj % (parse-data line)))))
            {:data #{} :folding []}
            input))
  )

(defn task-1 [input]
  (let [d (parse input)
        f (first (:folding d))]
    {:data (reduce (fn [new-data element]
                     (conj new-data (f element)))
                   #{}
                   (:data d))
     :folding (rest (:folding d))}))

(defn task-2 [input]
  (let [d (parse input)]
    {:data (reduce (fn [new-data element]
                     (conj new-data
                           (reduce (fn [new-element f]
                                     (f new-element))
                                   element
                                   (:folding d))))
                   #{}
                   (:data d))
     :folding []}))

(defn print-paper [input w h]
  (dotimes [y h]
    (dotimes [x w]
      (if (contains? (:data input) [x y])
        (print "#")
        (print " ")))
    (newline)))

(print-paper (task-2 testdata) 6 6)

(count (:data (task-1 data)))

(print-paper (task-2 data) 70 7)