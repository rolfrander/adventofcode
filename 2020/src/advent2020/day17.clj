(ns advent2020.day17
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def testdata ".#.
..#
###")

(def data "#....#.#
..##.##.
#..#..#.
.#..#..#
.#..#...
##.#####
#..#..#.
##.##..#")

(defn move [[x y z w] [dx dy dz dw]]
  [(+ x dx) (+ y dy) (+ z dz) (+ w dw)])

(def neighbours3
  (for [x (range -1 2)
        y (range -1 2)
        z (range -1 2)
        :when (not-every? (partial = 0) [x y z])]
    [x y z 0]))

(def neighbours4
  (for [x (range -1 2)
        y (range -1 2)
        z (range -1 2)
        w (range -1 2)
        :when (not-every? (partial = 0) [x y z w])]
    [x y z w]))

(def ^:dynamic *neighbours*)

(defn parse [input]
  (->> (for [[y line] (zipmap (range) (str/split-lines input))
             [x char] (zipmap (range) line)
             :when (= char \#)]
         [x y 0 0])
       (into #{})))

(defn get-x [[_x _y _z _w]] _x)
(defn get-y [[_x _y _z _w]] _y)
(defn get-z [[_x _y _z _w]] _z)
(defn get-w [[_x _y _z _w]] _w)

(defn get-dim [function set]
  (mapv #(function (apply % function set))
        [min-key max-key]))

(defn display [board]
  (let [r (fn [dim] (let [[a b] (get-dim dim board)]
                      (range a (inc b))))]
    (doseq [z (r get-z)]
      (println)
      (println "z =" z)
      (doseq [y (r get-y)]
        (->> (r get-x)
             (map #(if (contains? board [% y z 0]) \# \.))
             str/join
             println)))))


;;;; task 1
(defn find-neighbours [point]
  (map (partial move point) *neighbours*))

(defn evolve [board]
  (->> (mapcat find-neighbours board)
       (frequencies)
       (filter (fn [[k v]]
                 (or (= v 3)
                     (and (= v 2) (contains? board k)))))
       (map first)
       (into #{})))

(defn count-neighbours
  "count number of neighbours to 'point' at 'board', but count no longer than 'upper'"
  [board point upper]
  (loop [[p & r] (find-neighbours point)
         i 0]
    (cond (empty? r) i
          (>= i upper) i
          (contains? board p) (recur r (inc i))
          :else (recur r i))))

(binding [*neighbours* neighbours4]
  (->> data
       parse
       (iterate evolve)
       (drop 6)
       first
       count))