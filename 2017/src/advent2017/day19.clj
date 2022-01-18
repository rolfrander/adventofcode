(ns advent2017.day19
  (:require [rolfrander.puzzle-lib :refer [get-data]]
            [clojure.string :as str]))

(defn parse [input]
  (str/split-lines input))

(defn path-at [data [row col]]
  (if (and (<= 0 row (dec (count data)))
           (<= 0 col (dec (count (get data row)))))
    (.charAt (get data row) col)
    \space))

(defn find-start [data]
  (let [i (.indexOf (first data) "|")]
    (if (>= i 0)
      [0 i]
      (throw (RuntimeException. "did not find entrypoint in map")))))

(def directions [[[-1 0] :up-down]
                 [[1 0]  :up-down]
                 [[0 -1] :left-right]
                 [[0 1]  :left-right]])

(def dir->sym {:up-down \|
               :left-right \-})

(def sym->dir {\| :up-down
               \- :left-right})

(defn plus? [c] (= c \+))

(defn letter? [c] (if (nil? c) 
                    false
                    (and (char? c) (Character/isAlphabetic (int c)))))

(defn connection? [c] (or (= c \|)
                          (= c \-)))

(defn add-pos [[r1 c1] [r2 c2]]
  [(+ r1 r2) (+ c1 c2)])

(defn other-directions [current-direction]
  (->> directions
       (remove #(= (second %) current-direction))
       ))

(def testdata "     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ 
")

(defn find-path [data]
  (loop [pos (find-start data)
         direction :up-down
         delta [1 0]
         current-path-length 0
         letters []]
    (let [path (path-at data pos)
          next-path (path-at data (add-pos pos delta))
          next-direction (sym->dir next-path)]
      (cond (and (plus? path) (not= direction next-direction))
            (let [[delta direction] (some (fn [[delta _direction :as r]]
                                            (let [sym (path-at data (add-pos pos delta))]
                                              (when (or (connection? sym)
                                                        (letter? sym))
                                                r)))
                                          (other-directions direction))]
              (recur (add-pos pos delta) direction delta 0 (conj letters current-path-length \+)))

            (connection? path)
            (recur (add-pos pos delta) direction delta (inc current-path-length) letters)

            (letter? path)
            (recur (add-pos pos delta) direction delta 0 (conj letters current-path-length path))

            :else letters))))

(defn task-1 [input]
  (let [letters (find-path (parse input))]
    (str/join (filter letter? letters))))

(defn task-2 [input]
  (let [letters (find-path (parse input))]
    (reduce + (map #(if (number? %) % 1) letters))))


;(find-path (parse testdata))

(task-2 testdata)
(task-2 (get-data 2017 19))

(map #(path-at testdata (add-pos [5 5] (first %)))
     (other-directions testdata :up-down [5 5]))