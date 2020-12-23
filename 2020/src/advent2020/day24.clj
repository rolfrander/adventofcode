(ns advent2020.day24
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)


;; y=2   -2  -1  0   1   2
;; y=1     -2  -1  0   1   2
;; y=0   -2  -1  0   1   2
;; y=-1    -2  -1  0   1   2
;; y=-2  -2  -1  0   1   2

(def neighbours-even [[-1  1] [0  1]
                      [-1  0] [1  0]
                      [-1 -1] [0 -1]])

;; (map #(move [0 0] %) (re-seq #"e|w|ne|se|nw|sw" "nwneweswse"))
;; => ([-1 1] [0 1] [-1 0] [1 0] [-1 -1] [0 -1])
(def neighbours-even [[-1  1] [0  1]
                      [-1  0] [1  0]
                      [-1 -1] [0 -1]])

(map #(let [[x y] (move [0 1] %)] [x (dec y)]) (re-seq #"e|w|ne|se|nw|sw" "nwneweswse"))
;; => ([0 1] [1 1] [-1 0] [1 0] [0 -1] [1 -1])
(def neighbours-odd [[ 0  1] [1  1] 
                     [-1  0] [1  0]
                     [ 0 -1] [1 -1]])



(defn get-neighbours [[x y]]
  (->> (if (odd? y) neighbours-odd neighbours-even)
       (map (fn [[xx yy]] [(+ x xx) (+ y yy)]))))

(defn move [[x y] direction]
  (let [y-1 (dec y)
        y+1 (inc y)]
    (case direction
      "ne" [(+ x (mod y   2)) y+1]
      "nw" [(- x (mod y-1 2)) y+1]
      "e"  [(inc x)           y]
      "w"  [(dec x)           y]
      "se" [(+ x (mod y   2)) y-1]
      "sw" [(- x (mod y+1 2)) y-1])))

(defn exec-line [line]
  (reduce move [0 0] (re-seq #"e|w|ne|se|nw|sw" line)))

(def testdata "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew")

(defn parse-and-execute [input]
  (->> input
       str/split-lines
       (map exec-line)
       frequencies
       (filter #(odd? (second %)))
       (map first)
       set))

(defn initial-black-tiles [input]
  (->> input
       str/split-lines
       (map exec-line)
       frequencies
       (filter #(odd? (second %)))))

(defn count-black-neighbours
  "only three answers are relevant: 0, many (>2), other, but will identify 0, 1, 2 and many (=3)"
  [black-tiles point]
  (->> (filter black-tiles (get-neighbours point))
       (take 3)
       count))

(defn evolve [black-tiles]
  (let [black-to-white (->> black-tiles
                            (map #(vector % (count-black-neighbours black-tiles %)))
                            (filter #(#{0,3} (second %)))
                            (map first)
                            set)
        white-to-black (->> (mapcat get-neighbours black-tiles)
                            frequencies
                            (filter #(= 2 (second %)))
                            (map first)
                            (filter #(not (black-tiles %)))
                            set)]
    (when *debug*
      (println "input" black-tiles)
      (println "to white" black-to-white)
      (println "to black" white-to-black))
    (-> black-tiles
        (set/difference black-to-white)
        (set/union white-to-black))))

(defn task-1 [input]
(->> (parse-and-execute input)
     count))

(defn task-2 [input rounds]
  (count (nth (iterate evolve (set (parse-and-execute input)))
              rounds)))

(task-1 testdata)
;; => 10

(task-1 (slurp "day24.txt"))
;; => 266



(binding [*debug* false]
  (task-2 testdata 2))

(map #(task-2 testdata %)
     (concat (range 1 11) (range 10 110 10)))
;; => (15 12 25 14 23 28 41 37 49 37 37 132 259 406 566 788 1106 1373 1844 2208)


(task-2 (slurp "day24.txt") 100)
;; => 3627

