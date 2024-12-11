(ns advent2024.day06
    (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defn parse-row [row y]
  (->> (map-indexed #(and (= %2 \#) [%1 y]) row)
       (remove false?)))

(defn parse [in]
  (loop [[row & rows] (str/split-lines in)
         y 0
         map-coord #{}
         startpos nil
         width 0]
    (if (nil? row)
      {:map map-coord
       :startpos startpos
       :width width
       :height y}
      (recur rows (inc y)
             (into map-coord (parse-row row y))
             (or startpos (when-let [x (str/index-of row \^)] [x y]))
             (count row)))))

; (parse testdata)

(def directions (cycle [[0 -1] [1 0] [0 1] [-1 0]]))

(defn walk [config]
  (let [obstacles (:map config)
        width (:width config)
        height (:height config)]
    (loop [pos (:startpos config)
           visited {pos (first directions)}
           [dir & dirs] directions]
      (let [newpos (map + pos dir)]
        (cond (= (visited newpos) dir) true ; means circle
              
              (not (and (< -1 (first newpos) width)
                        (< -1 (second newpos) height)))
              visited

              :else
              (if (obstacles newpos)
                (recur pos visited dirs)
                (recur newpos (assoc visited newpos dir) (cons dir dirs))))))))

(defn solve-1 [data]
  (count (walk (parse data))))

(defn solve-2 [data]
  (let [config (parse data)
        visited (walk config)
        check-for-circle (fn [new-obstacle]
                           (true? (walk (update config :map #(conj % new-obstacle)))))]
    (count (filter check-for-circle (keys visited)))))

(solve-1 testdata)
(solve-1 (puzzle/get-data 2024 6))
;;=> 5145

(solve-2 testdata)
;;=> 6
(solve-2 (puzzle/get-data 2024 6)) ; 1523
