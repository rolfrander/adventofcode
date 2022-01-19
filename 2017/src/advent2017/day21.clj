(ns advent2017.day21
  (:require [rolfrander.puzzle-lib :refer [get-data]]
            [clojure.string :as str]
            [clojure.core.matrix :as mat]))

(defn str->matrix [in]
  (->> (str/split in #"/")
       (map #(map {\# 1 \. 0} %))
       (mat/matrix)))

(defn parse-line [line]
  (let [[from to] (str/split line #" => ")]
    [(str->matrix from)
     (str->matrix to)]))

(defn parse [input]
  (map parse-line (str/split-lines input)))

(def testdata "../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#")

(parse testdata)

(def rot-matrix
  {2 (mat/matrix [[0 1] [1 0]])
   3 (mat/matrix [[0 0 1] [0 1 0] [1 0 0]])})

(defn turn [m]
  (mat/mmul (mat/transpose m)
            (rot-matrix (mat/dimension-count m 0))))

(defn prepare-transforms [data]
  (letfn [(rotations [m] (let [turns (take 4 (iterate turn m))] 
                           (-> #{}
                               (into turns)
                               (into (map mat/transpose turns)))))]
    (->> data
         (map (fn [[from to]]
                {:size (mat/dimension-count from 0)
                 :match (rotations from)
                 :replace to}))
         (group-by :size))))

(defn find-transform [transforms matrix]
  (let [dim (mat/dimension-count matrix 0)]
    (some #(when (contains? (:match %) matrix) (:replace %))
          (get transforms dim))))

;(prepare-transforms (parse testdata))

(defn sub-squares [m]
  (let [size (mat/dimension-count m 0)
        sub-size (cond (= 0 (mod size 2)) 2
                       (= 0 (mod size 3)) 3
                       :else             (throw (RuntimeException. (str "unexpected dim: " size))))]
    (for [x (range 0 size sub-size)]
      (for [y (range 0 size sub-size)]
        (mat/submatrix m [[x sub-size]
                          [y sub-size]])))))


(defn expand [transforms m]
  (let [sq-list (sub-squares m)]
    (if (= 1 (count sq-list))
      (find-transform transforms m)
      (->> sq-list
           (map #(apply mat/join-along 1 (map (partial find-transform transforms) %)))
           (apply mat/join)))))

(defn task-1 [input start iterations]
  (let [transforms (prepare-transforms (parse input))
        matrix-after-n (-> (iterate (partial expand transforms) start)
                           (nth iterations))
        zero-cnt (mat/zero-count matrix-after-n)]
    (- (apply * (mat/shape matrix-after-n))
       zero-cnt)))

(mat/set-current-implementation :vectorz)

(def start-pattern (str->matrix ".#./..#/###"))

(task-1 testdata start-pattern 2)
(task-1 (get-data 2017 21) start-pattern 5)
(task-1 (get-data 2017 21) start-pattern 18); 1782917

