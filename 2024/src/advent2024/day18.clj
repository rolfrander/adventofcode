(ns advent2024.day18
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")

(def data (puzzle/get-data 2024 18))

(defn parse [in]
  (->> (re-seq #"\d+" in)
       (map puzzle/str->long)
       (partition 2)
       ;(into #{})
       ))

(defn solve-1 [in t dim]
  (let [neighbour (puzzle/neighbours-fn :sq-4 :ignore :max-dim [dim dim])
        errors (into #{} (take t (parse in)))
        nodes (for [a (range (inc dim))
                    b (range (inc dim))
                    :let [pos [a b]]
                    :when (not (errors pos))]
                pos)]
    (puzzle/dijkstra nodes [0 0] neighbour 
                     :result-type :dist
                     :dest? #(= % [dim dim])
                     )
    ))

(defn solve-2 [in t dim]
  (let [neighbour (puzzle/neighbours-fn :sq-4 :ignore :max-dim [dim dim])
        [errors-1 errors-2] (split-at t (parse in))
        nodes (into #{} (for [a (range (inc dim))
                              b (range (inc dim))
                              :let [pos [a b]]]
                          pos))]
    (loop [[e & err] errors-2
           nodes (reduce disj nodes errors-1)]
      (if (nil? e)
        "found none"
        (let [nodes (disj nodes e)]
          (if (< (puzzle/dijkstra nodes [0 0] neighbour
                                  :result-type :dist
                                  :dest? #(= % [dim dim]))
                 5000)
            (recur err nodes)
            e))))))

(solve-1 testdata 12 6)
(solve-1 testdata 22 6)

(solve-1 data 1024 70)
;;=> 316

(solve-2 testdata 12 6)

(solve-2 data 1024 70)
