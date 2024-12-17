(ns advent2024.day15
  (:require
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]))

(def testdata1 "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

(def testdata2 "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

(def data (puzzle/get-data 2024 15))

(def move-map (zipmap [">" "<" "^" "v"]
                      ["e" "w" "s" "n"]))

(defn parse-moves [in]
  (->> (re-seq #"[<>^v]" in)
       (map move-map)))

(defn parse [in]
  (let [[map moves] (str/split in #"\n\n")]
    (-> (puzzle/parse-map map)
        (assoc :moves (parse-moves moves)
               ))))

(def move (puzzle/move-fn :sq-4 :infinite))

(defn move-objects [pos object wall direction]
  (cond (contains? wall pos)
        nil

        (contains? object pos)
        (let [relocated-object (move pos direction)]
          (when-let [other-objects-moved (move-objects relocated-object
                                                     (disj object pos)
                                                     wall direction)]
            (conj other-objects-moved relocated-object)))
        
        :else
        object))

(defn widen [m]
  {:wall
   (->> (get-in m [:markings \#])
        (mapcat (fn [[x y]] [[(* 2 x) y]
                             [(inc (* 2 x)) y]]) )
        (into #{}))
   :object
   (->> (get-in m [:markings \O])
        (map (fn [[x y]] [[(* 2 x) y][(inc (* 2 x)) y]]))
        (into #{}))
   
   :pos
   (->> (get-in m [:markings \@])
        (map (fn [[x y]] [[(* 2 x) y]]))
        first)}
  )

(widen (parse testdata1))

(inc')

(defn solve-1 [in]
  (let [d (parse in)]
    (->> (loop [[m & moves] (:moves d)
                wall (into #{} (get-in d [:markings \#]))
                object (into #{} (get-in d [:markings \O]))
                pos (first (get-in d [:markings \@]))]
           (if (nil? m)
             object
             (let [new-pos (move pos m)
                   new-objects (move-objects new-pos object wall m)]
               (if (nil? new-objects)
                 (recur moves wall object pos)
                 (recur moves wall new-objects new-pos )))
             ))
         (map #(+ (* 100 (second %)) (first %)))
         (reduce +)
         )
    ))

(solve-1 testdata1)
(solve-1 testdata2)
(solve-1 data)
;;=> 1383666