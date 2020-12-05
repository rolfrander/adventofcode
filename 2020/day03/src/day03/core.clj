(ns day03.core
  (:gen-class))

(defn load-data [filename]
  (with-open [in (clojure.java.io/reader filename)]
    (reduce conj [] (line-seq in))))

(def data (load-data "input.txt"))

(def testdata (reduce conj [] 
                      (clojure.string/split-lines "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")))

(defn tree-crash [data right down]
  (->> data
       (map-indexed (fn [idx line] [idx line]))
       (filter #(= 0 (mod (first %) down)))
       ; the first map-indexed added index which we need to destructure away to add a new index
       (map-indexed (fn [i [_ forest]]
                      (nth forest (mod (* right i) (count forest)))))
       (reduce #(case %2
                  \# (inc %1)
                  %1)
               0)))

(tree-crash data 3 1)

(tree-crash testdata 3 1)

(reduce (fn [product [down right]]
          (* product
             (tree-crash data down right)))
        1
        '([1 1]
          [3 1]
          [5 1]
          [7 1]
          [1 2]))

(let [data testdata right 3 down 1]
  (->> data
       (map-indexed (fn [idx line] [idx line]))
       (filter #(= 0 (mod (first %) down)))
       ; the first map-indexed added index which we need to destructure away to add a new index
       (map-indexed (fn [i [_ forest]]
                      (nth forest (mod (* right i) (count forest)))))
       (reduce #(case %2 \# (inc %1) %1) 0)
       ))
