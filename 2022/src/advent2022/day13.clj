(ns advent2022.day13
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(defn task-1-parse [input]
  (->> (str/split-lines input)
       (puzzle/split-by #(= "" %))
       (map #(remove (partial = "") %))
       (map #(map read-string %))))

(defn task-2-parse [input]
  (->> (str/split-lines input)
       (remove (partial = ""))
       (map read-string)))

(defn elf-compare [a b]
  (let [ret (if (= (type a) (type b))
              (cond
                (instance? Long a)
                (compare a b)

                (instance? clojure.lang.PersistentVector a)
                (or (first (filter (partial not= 0) (map elf-compare a b)))
                    (compare (count a) (count b))))
              (if (= (type a) java.lang.Long)
                (elf-compare [a] b)
                (elf-compare a [b])))]
    ;(println a b "=>" ret)
    ret))

(elf-compare [9] [[8 7 6]])
(elf-compare 9 [8])

(defn task-1 [input]
  (->> (task-1-parse input) 
       (map-indexed #(if (<= (apply elf-compare %2) 0)
                                     (inc %1)
                                     nil))
       (remove nil?)
       (reduce +)))

(defn task-2 [input]
  (->> (task-2-parse input)
       (cons [[2]])
       (cons [[6]])
       (sort elf-compare)
       (map-indexed #(if (or (= %2 [[2]])
                             (= %2 [[6]]))
                       (inc %1)
                       1))
       (reduce *)))

(task-1 testdata)
(task-1 (puzzle/get-data 2022 13))

(task-2 testdata)
(task-2 (puzzle/get-data 2022 13))
