(ns advent2021.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def testdata "3,4,3,1,2")

(def data "2,1,2,1,5,1,5,1,2,2,1,1,5,1,4,4,4,3,1,2,2,3,4,1,1,5,1,1,4,2,5,5,5,1,1,4,5,4,1,1,4,2,1,4,1,2,2,5,1,1,5,1,1,3,4,4,1,2,3,1,5,5,4,1,4,1,2,1,5,1,1,1,3,4,1,1,5,1,5,1,1,5,1,1,4,3,2,4,1,4,1,5,3,3,1,5,1,3,1,1,4,1,4,5,2,3,1,1,1,1,3,1,2,1,5,1,1,5,1,1,1,1,4,1,4,3,1,5,1,1,5,4,4,2,1,4,5,1,1,3,3,1,1,4,2,5,5,2,4,1,4,5,4,5,3,1,4,1,5,2,4,5,3,1,3,2,4,5,4,4,1,5,1,5,1,2,2,1,4,1,1,4,2,2,2,4,1,1,5,3,1,1,5,4,4,1,5,1,3,1,3,2,2,1,1,4,1,4,1,2,2,1,1,3,5,1,2,1,3,1,4,5,1,3,4,1,1,1,1,4,3,3,4,5,1,1,1,1,1,2,4,5,3,4,2,1,1,1,3,3,1,4,1,1,4,2,1,5,1,1,2,3,4,2,5,1,1,1,5,1,1,4,1,2,4,1,1,2,4,3,4,2,3,1,1,2,1,5,4,2,3,5,1,2,3,1,2,2,1,4")

(defn parse [data]
  (map #(Long/parseLong %)
       (string/split data #",")))

(defn simulate [d generations]
  (loop [state (parse d)
         days-left generations]
    (if (= 0 days-left)
      state
      (recur
       (loop [[input & state] state
              output []
              new 0]
         (if (nil? input)
           (concat output (repeat new 8))
           (if (= 0 input)
             (recur state
                    (conj output 6)
                    (inc new))
             (recur state
                    (conj output (dec input))
                    new))))
       (dec days-left)))))

(defn state-count [d]
  (reduce #(update %1 %2 inc)
          (vec (repeat 9 0))
          (parse d)))

;(state-count data)

(defn simulate-count [d generations]
  (loop [days-left generations
         state (state-count d)]
    (if (= 0 days-left)
      (apply + state)
      (let [new (get state 0)
            rest (subvec state 1)
            rest (update rest 6 #(+ % new))]
        (recur (dec days-left)
               (conj rest new))))))

(defn task-1 [d] (count (simulate d 80)))


(simulate-count testdata 256)
(simulate-count data 256)

(task-1 testdata)
(task-1 data)
