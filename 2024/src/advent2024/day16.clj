(ns advent2024.day16
  (:require
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]))

(def testdata "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")

(def testdata2 "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################")

(def data (puzzle/get-data 2024 16))

(def turn-left {"n" "e"
                "e" "s"
                "s" "w"
                "w" "n"
                })

(def turn-right {"n" "w"
                 "e" "n"
                 "s" "e"
                 "w" "s"
                 })

(defn solve-1 [in]
  (let [m (puzzle/parse-map in)
        end-point (first (get-in m [:markings \E]))
        start-point (first (get-in m [:markings \S]))
        nodes (into #{} (mapcat #(vector {:point % :dir "n"}
                                         {:point % :dir "s"}
                                         {:point % :dir "e"}
                                         {:point % :dir "w"})
                                (conj (get-in m [:markings \.])
                                      end-point
                                      start-point)))
        start {:point start-point
               :dir "e"}
        move (puzzle/move-fn  :sq-4 :infinite)
        neighbours (fn [pos] (let [turn [(update pos :dir turn-left)
                                         (update pos :dir turn-right)]
                                   new-pos (update pos :point #(move % (:dir pos)))]
                               (if (contains? nodes new-pos)
                                 (conj turn new-pos)
                                 turn)))
        weight (fn [p1 p2] (if (= (:dir p1) (:dir p2))
                             1
                             1000))
        check-end (fn [pos] (= (:point pos) end-point))]
    (puzzle/dijkstra nodes start neighbours
                     :weight-fn weight
                     :dest? check-end
                     :result-type :dist)))

(solve-1 testdata)
;;=> 7036
(solve-1 testdata2)
;;=> 11048
(solve-1 data)
;;=> 88468