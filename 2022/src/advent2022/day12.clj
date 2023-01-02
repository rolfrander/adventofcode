(ns advent2022.day12
    (:require [clojure.set :as set]
              [clojure.string :as str]
              [rolfrander.puzzle-lib :as puzzle]))

(def testdata "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn parse [input]
  (let [data (->> (str/split-lines input)
                  (map-indexed
                   (fn [i row]
                     (map-indexed
                      (fn [j cell]
                        [[j i] cell])
                      row)))
                  (apply concat)
                  (group-by #(Character/isUpperCase (second %))))
        heights (get data false)
        [poi-a poi-b] (get data true)
        start (if (= \S (second poi-a)) (first poi-a) (first poi-b))
        end (if (= \E (second poi-a)) (first poi-a) (first poi-b))
        heights (-> (into {} heights)
                 (assoc start \a)
                 (assoc end \z))
        ]
    [heights start end]
    ))

(defn- find-path [height-map start end]
  (let [get-neighbours (puzzle/neighbours-fn :sq-4 :infinite)]
      (->> (puzzle/dijkstra (keys height-map)
                            end
                            (fn [pos]
                              (->> (get-neighbours pos)
                                   (filter (partial contains? height-map))
                                   (filter #(>= (- (int (get height-map %))
                                                   (int (get height-map pos)))
                                                -1))))
                            ;:result-type :dist
                            ;:dest? #(= % start)
                            )
           )))

(defn task-1 [input]
  (let [[height-map start end] (parse input) 
        distances (first (find-path height-map start end))]
    (distances start)))

(defn task-2 [input]
  (let [[height-map start end] (parse input)]
    (->> (find-path height-map start end)
         first
         (filter #(= (height-map (first %)) \a))
         (sort-by second)
         (first)
         (second))))

(task-1 testdata)
(task-1 (puzzle/get-data 2022 12))

(task-2 testdata)
(task-2 (puzzle/get-data 2022 12))

