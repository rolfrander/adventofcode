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

(def testdata3 "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^")

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

(let [dir (puzzle/directions-maps :sq-4)]
  (defn move-double [pos direction]
    (map #(map + % (dir direction)) pos)))

(move-double [[5 4][6 4]] "w")

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
  ;:wall
  (-> m
      (update-in [:markings \#]
                 #(mapcat (fn [[x y]] [[(* 2 x) y]
                                      [(inc (* 2 x)) y]]) %))

  ;:space
      (update-in [:markings \.]
                 #(mapcat (fn [[x y]] [[(* 2 x) y]
                                      [(inc (* 2 x)) y]]) %))

  ;:object
      (update-in [:markings \O]
                 #(map (fn [[x y]] [[(* 2 x) y] [(inc (* 2 x)) y]]) %))


  ;:pos
      (update-in [:markings \@]
                 #(map (fn [[x y]] [[(* 2 x) y]]) %))
      
      (update-in [:width] * 2)))



(defn move-wide-objects [pos object wall direction]
  (let [overlap (fn [p1 p2]
                  (some true? (for [a p1 b p2] (= a b))))]
    (if (some (partial wall) pos)
          nil

          (let [affected-objects (filter (partial overlap pos) object)]
            (if (seq affected-objects)
              (let [relocated-objects (map #(move-double % direction) affected-objects)
                    objects-removed (apply disj object affected-objects)
                    other-objects-moved
                    (reduce #(into %1 (move-wide-objects %2
                                                         objects-removed
                                                         wall direction))
                            #{} relocated-objects)]
                (when (seq other-objects-moved)
                  (into other-objects-moved relocated-objects)))

              object)))))

(defn draw-wide-map [m]
  (-> m
      (assoc-in [:markings \[] (map first (get-in m [:markings \O])))
      (assoc-in [:markings \]] (map second (get-in m [:markings \O])))
      (assoc-in [:markings \O] nil)
      puzzle/draw-map))

;(draw-wide-map (widen (parse data)))

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

(defn solve-2 [in]
  (let [d (widen (parse in))]
    (print (str (char 27) "[2J")) ; clear screen
    (->> (loop [[m & moves] (:moves d)
                wall (into #{} (get-in d [:markings \#]))
                object (into #{} (get-in d [:markings \O]))
                pos (first (get-in d [:markings \@]))
                cnt (count (:moves d))]
           
           (println (str (char 27) "[;H")) ; move to upper left corner
           (println "Count:" cnt "   ")
           (draw-wide-map (-> d
                              (assoc-in [:markings \O] object)
                              (assoc-in [:markings \@] pos)))
           (if (nil? m)
             object
             (let [new-pos (move-double pos m)
                   new-objects (move-wide-objects new-pos object wall m)]
               (if (nil? new-objects)
                 (recur moves wall object pos (dec cnt))
                 (recur moves wall new-objects new-pos (dec cnt))))))
         
         (map first)
         (map #(+ (* 100 (second %)) (first %)))
         (reduce +)
         ;draw-wide-map
         )))

;(solve-1 testdata1)
;(solve-1 testdata2)
;(solve-1 data)
;;=> 1383666

(solve-2 testdata3)
;(solve-2 data)
;;=> 
