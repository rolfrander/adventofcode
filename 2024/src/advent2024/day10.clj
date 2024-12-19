(ns advent2024.day10
  (:require
   [rolfrander.puzzle-lib :as puzzle]))

(def testdata "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(puzzle/parse-map testdata)

(let [neighbours (puzzle/neighbours-fn :sq-4 :ignore :max-dim [9 9])]
  (into #{} (neighbours [2 0])))

(defn solve-1 [in]
  (let [data (puzzle/parse-map in)
        m (->> data
               :markings
               (map (fn [[k v]] [(puzzle/char->digit k) (into #{} v)]))
               (into {}))
        neighbours (puzzle/neighbours-fn :sq-4 :ignore :max-dim [(data :width) (data :height)])]
    (letfn [(solve [nextlevel startpoint]
              (if (= 10 nextlevel)
                (list startpoint)
                (let [n (into #{} (neighbours startpoint))]
                  (->> (m nextlevel)
                       (filter n)
                       (map (partial solve (inc nextlevel)))
                       (reduce into #{})))))]
      (->> (map (partial solve 1) (m 0))
           (map count)
           (reduce +)))))

(defn solve-2 [in]
  (let [data (puzzle/parse-map in)
        m (->> data
               :markings
               (map (fn [[k v]] [(puzzle/char->digit k) (into #{} v)]))
               (into {}))
        neighbours (puzzle/neighbours-fn :sq-4 :ignore :max-dim [(data :width) (data :height)])]
    (letfn [(solve [nextlevel startpoint]
              (if (= 10 nextlevel)
                1
                (let [n (into #{} (neighbours startpoint))
                      paths (m nextlevel)
                      pathcount (count paths)]
                  (->> paths
                       (filter n)
                       (map (partial solve (inc nextlevel)))
                       (reduce +)))))]
      (->> (map (partial solve 1) (m 0))
           (reduce +)))))

(solve-1 testdata)
;;=> 36
(solve-1 (puzzle/get-data 2024 10))
;;=> 646
(solve-2 testdata)
;;=> 81
(solve-2 (puzzle/get-data 2024 10))
;;=> 1494
(ns advent2024.day10
  (:require
   [rolfrander.puzzle-lib :as puzzle]))

