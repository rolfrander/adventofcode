(ns advent2017.day11
  (:require [rolfrander.puzzle-lib :refer [get-data move-fn neighbours-fn a-star dijkstra]]
            [clojure.string :as str]))

(defn parse [input]
  (str/split (str/trim input) #","))

(def testdata [["ne,ne,ne" 3]
               ["ne,ne,sw,sw" 0]
               ["ne,ne,s,s" 2]
               ["se,sw,se,sw,sw" 3]])

(def move (move-fn :hex-ns-str :infinite))
(def neighbours (neighbours-fn :hex-ns-str :infinite))

(defn abs [x] (Math/abs x))

(defn sign [x] (cond (= x 0) 0
                     (> x 0) 1
                     :else -1))

(defn hexagon-manhattan-distance 
  "manhattan distance in a hex grid.
   https://stackoverflow.com/questions/5084801/manhattan-distance-between-tiles-in-a-hexagonal-grid"
  [[x y]]
  (if (= (sign x) (sign y))
    (abs (+ x y))
    (max (abs x) (abs y))))

(defn move-and-measure [movements]
  (->> (reduce move [0 0] movements)
       (hexagon-manhattan-distance)))

(defn move-and-measure-all [movements]
  (->> (reductions move [0 0] movements)
       (map hexagon-manhattan-distance)))

(defn task-1 [input]
  (move-and-measure (parse input)))

(defn task-2 [input]
  (->> (parse input)
       move-and-measure-all
       (reduce max)))

(->> testdata
     (map first)
     (map task-1)
     ;(= (map second testdata))
     )

(task-1 (get-data 2017 11))

(task-2 (get-data 2017 11))