(ns advent2016.day01
  (:require [advent2016.core :as core]))

(def data (core/get-data 2016 1))

(defn parse [input]
  (->> (re-seq #"([RL])([0-9]+)" input)
       (map (juxt second (comp (partial core/safe-parse-number) peek)))))

(def directions [[0 1]
                 [1 0]
                 [0 -1]
                 [-1 0]])

(def north 0)

(defn turn [current-direction left-or-right]
  (case left-or-right
    "L" (mod (+ current-direction 3) 4)
    "R" (mod (+ current-direction 1) 4)))

(defn move [pos direction amount]
  (map + pos
       (map (partial * amount) (directions direction))))

(defn taxi-distance [pos]
  (apply + (map #(Math/abs %) pos)))

(defn task-1 [data]
  (->> (reduce (fn [[pos dir] [t amount]]
                 (let [dir (turn dir t)]
                   [(move pos dir amount) dir]))
               [[0 0] 0]
               (parse data))
       first
       ))

(defn task-2 [data]
  (->> (reduce (fn [[pos dir previous] [t amount]]
                 (let [dir (turn dir t)]
                   (loop [pos (move pos dir 1)
                          i (dec amount) ; dec because we already moved 1
                          previous previous]
                     (cond (previous pos)
                           (reduced pos)

                           (= i 0)
                           [pos dir (conj previous pos)]
                           
                           :else
                           (recur (move pos dir 1)
                                  (dec i)
                                  (conj previous pos))))))
               [[0 0] 0 #{}]
               (parse data))
       ))

(taxi-distance (task-1 data))

(taxi-distance (task-2 "R8, R4, R4, R8"))
(taxi-distance (task-2 data))