(ns advent2024.day08
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn inner-loop [m calc-antinodes]
  (->> (loop [a (keys (m :markings))
              antinodes #{}]
         (if (empty? a)
           antinodes
           (recur (rest a)
                  (->> (get-in m [:markings (first a)])
                       (puzzle/map-pairs calc-antinodes)
                       (apply concat)
                       (into antinodes)))))
       count))

(defn solve-1 [in]
  (let [m (puzzle/parse-map in)
        calc-antinodes (fn [[ax ay] [bx by]]
                         (let [max-x (:width m)
                               max-y (:height m)]
                           (->> (list [(- ax (- bx ax)) (- ay (- by ay))]
                                      [(+ bx (- bx ax)) (+ by (- by ay))])
                                (filter (fn [[x y]] (and (< -1 x max-x)
                                                         (< -1 y max-y)))))))]
    (inner-loop m calc-antinodes)))

(defn solve-2 [in]
  (let [m (puzzle/parse-map in)
        calc-antinodes (fn [[ax ay] [bx by]]
                         (let [max-x (:width m)
                               max-y (:height m)
                               is-in-map (fn [[x y]] (and (< -1 x max-x)
                                                          (< -1 y max-y)))
                               delta-x (- bx ax)
                               delta-y (- by ay)

                               generator-fn (fn [[f start-x start-y]]
                                              (iterate (fn [[x y]] [(f x delta-x) (f y delta-y)]) [start-x start-y]))]
                           (concat (take-while is-in-map (generator-fn [- ax ay]))
                                   (take-while is-in-map (generator-fn [+ bx by])))))]
    (inner-loop m calc-antinodes)))

(solve-1 testdata)
;;=> 14
(solve-1 (puzzle/get-data 2024 8))
;;=> 214

(solve-2 testdata)
;;=> 34
(solve-2 (puzzle/get-data 2024 8))
;;=> 809
