(ns advent2020.day15
  (:require [clojure.string :as str]))

(defn prepare-data [input]
  (->> input 
       (map-indexed (fn [idx num] [num idx]))
       (into {})))

(defn game [input end]
  (let [next (peek input)
        data (prepare-data (pop input))]
  (second
   (reduce (fn [[previous last] i]
             (let [next (if (not (contains? previous last))
                          0
                          (- i (get previous last)))]
               [(assoc previous last i) next]))
           [data next]
           (range (count data) (dec end))))))

(game [0 3 6] 10)

(game [0 3 6] 2020)
(game [1 3 2] 2020)
(game [2 1 3] 2020)
(game [1 2 3] 2020)

(game [0 14 1 3 7 9] 2020)
;; => 763

(game [0 3 6] 30000000)

(game [0 14 1 3 7 9] 30000000)
;; => 1876406

