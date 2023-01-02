(ns advent2022.day08
  (:require [rolfrander.puzzle-lib :as puzzle]
            [clojure.string :as str]))

(def testdata "30373
25512
65332
33549
35390")

(def data (puzzle/get-data 2022 8))

(defn parse [input]
  (->> (str/split-lines input)
       (mapv #(mapv puzzle/char->digit %))))

(parse testdata)

(defn count-in-direction [matrix start direction]
  (->> (case direction
         :right (get matrix start)
         :left  (rseq (get matrix start))
         :down  (map #(get % start) matrix)
         :up    (map #(get % start) (rseq matrix)))
       (reduce (fn [[vis-state cur-height] next]
                 (if (>= next cur-height)
                   [(conj vis-state 1) (inc next)]
                   [(conj vis-state 0) cur-height]))
               [[] 0])
       first))

(defn count-until [limit s]
  (loop [i 0
         [c & s] s]
    (cond (nil? c) i
          (>= c limit) (inc i)
          :else (recur (inc i) s))))

(defn scenic-score [matrix row col]
  (let [mytree (get-in matrix [row col])
        view-in-direction (fn [direction]
                            (->> (case direction
                                   :right (subvec (get matrix row) (inc col))
                                   :left  (rseq (subvec (get matrix row) 0 col))
                                   :down  (map #(get % col) (subvec matrix (inc row)))
                                   :up    (map #(get % col) (rseq (subvec matrix 0 row))))
                                 (count-until mytree)
                                 ))]
    (->> (map view-in-direction [:up :left :right :down])
         (reduce *)
         )))

(defn task-1 [data]
  (let [d (parse data)
        right (map #(count-in-direction d % :right) (range 1 (dec (count d))))
        left (map #(count-in-direction d % :left) (range 1 (dec (count d))))
        combine (fn [a b] (mapv #(mapv (partial +) %1 (rseq %2)) a b))
        left-right (combine right left)
        down (map #(count-in-direction d % :down) (range 1 (dec (count (first d)))))
        up (map #(count-in-direction d % :up) (range 1 (dec (count (first d)))))
        up-down (combine down up)]
    (->>
     (map-indexed (fn [y-1 row]
                    (map-indexed (fn [x tree-cnt]
                                   (if (or (> tree-cnt 0)
                                           (> (get-in up-down [(dec x) (inc y-1)]) 0))
                                     1
                                     0))
                                 row))
                  left-right)
     (map #(reduce + %))
     (reduce +)
     (+ (* 2 (count (first d)))) ; f;rste og siste rad synes alltid
     )))

(defn task-2 [input]
  (let [data (parse input)]
    (->> (for [row (range 1 (dec (count data)))
               col (range 1 (dec (count (first data))))]
           (scenic-score data row col))
         (reduce max)
         )))

(task-1 testdata)
(task-1 data)

(task-2 testdata)
(task-2 data)

(let [d (parse testdata)]
  (scenic-score d 1 3))
