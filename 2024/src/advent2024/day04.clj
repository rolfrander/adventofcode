(ns advent2024.day04
    (:require [clojure.string :as str]
              [rolfrander.puzzle-lib :as puzzle]))

(def testdata "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(def data (puzzle/get-data 2024 04))

(defn parse [in]
 (-> (str/split-lines in)
     vec))

(defn enum-across [in]
  (->> (mapcat #(map str/join (partition 4 1 %)) in)
       (filter #(or (= % "XMAS") (= % "SAMX")))))

(defn enum-down [in]
  (->> (apply map vector in)
       (map str/join)
       enum-across))

(defn enum-diag-down [in]
  (->> (for [y (range 0 (- (count in) 3))
             x (range 0 (- (count (first in)) 3))]
         (str/join (map #(get-in in [(+ y %) (+ x %)]) (range 4))))
       (filter #(or (= % "XMAS") (= % "SAMX")))))

(defn enum-diag-up [in]
  (->> (for [y (range 3 (count in))
             x (range 0 (- (count (first in)) 3))]
         (str/join (map #(get-in in [(- y %) (+ x %)]) (range 4))))
       (filter #(or (= % "XMAS") (= % "SAMX")))
       ))

(defn find-x-mas [in]
  (->> (for [y (range 0 (- (count in) 2))
             x (range 0 (- (count (first in)) 2))
             :when (= \A (get-in in [(+ y 1) (+ x 1)]))]
         (str/join (map (fn [[dx dy]] (get-in in [(+ y dy) (+ x dx)]))
                        [[0 0] [2 0] [2 2] [0 2]])))
       (filter #(or (= % "MMSS")
                    (= % "MSSM")
                    (= % "SSMM")
                    (= % "SMMS")))
       count))

(find-x-mas (parse testdata))

(defn solve-1 [data]
  (let [d (parse data)]
    (->> (map #(count (% d)) [enum-down enum-across enum-diag-down enum-diag-up])
         (reduce +))))

(defn solve-2 [data] (find-x-mas (parse data)))

(solve-1 testdata)
;;=> 18
(solve-1 data)
;;=> 2613
(solve-2 testdata)
;;=> 9
(solve-2 data)
;;=> 1905