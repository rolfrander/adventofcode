(ns advent2024.day20
  (:require
   [rolfrander.puzzle-lib :as puzzle]))

(def testdata "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

(def data (puzzle/get-data 2024 20))

(defn parse [in]
  (let [m (puzzle/parse-map in)]
    (-> m
        ;(update-in [:markings \.] conj (first (get-in m [:markings \S])))
        ;(update-in [:markings \.] conj (first (get-in m [:markings \E])))
    )))

(defn get-path 
  ([prev dest]
   (get-path prev dest []))

  ([prev dest p]
   (if (contains? prev dest)
     (recur prev (prev dest) (conj p dest))
     (conj p dest))))

(let [m (parse testdata)

      start (first (get-in m [:markings \S]))
      end (first (get-in m [:markings \E]))
      track (into #{} (get-in m [:markings \.]))
      track (conj track end)
      neighbours (let [n (puzzle/neighbours-fn :sq-4 :infinite)]
                   (fn [p] (filter track (n p))))

      path (puzzle/dijkstra track start neighbours
                            :result-type :prev)]

  (-> (get-path path end)))

(defn solve-1-debug [in save]
  (let [m (parse in)
        start (first (get-in m [:markings \S]))
        end (first (get-in m [:markings \E]))
        track (into #{} (get-in m [:markings \.]))
        track (conj track end)
        neighbours (let [n (puzzle/neighbours-fn :sq-4 :infinite)]
                     (fn [p] (filter track (n p))))

        path (get-path (puzzle/dijkstra track start neighbours
                                        :result-type :prev)
                       end
                       [])
        min-save (+ save 2)

        straight-line-distance (fn [[x1 y1] [x2 y2]] (cond (= x1 x2) (abs (- y2 y1))
                                                           (= y1 y2) (abs (- x2 x1))
                                                           :else 99))]

    (->> (map
          (juxt identity #(loop [i 0
                                 j (+ 2 %)
                                 cheats []]
                            (if (>= (+ i % 2) (count path))
                              cheats
                              (recur (inc i) (inc j) (if (<= (straight-line-distance (path i)
                                                                                     (path j))
                                                             2)
                                                       (conj cheats (path i))
                                                       cheats)))))
          (range save (count path)))
         (remove #(empty? (second %))))))

(defn solve-1 [in save & {:keys [max-cheat] :or {max-cheat 2}}]
  (let [m (parse in)
        start (first (get-in m [:markings \S]))
        end (first (get-in m [:markings \E]))
        track (into #{} (get-in m [:markings \.]))
        track (conj track end)
        neighbours (let [n (puzzle/neighbours-fn :sq-4 :infinite)]
                     (fn [p] (filter track (n p))))

        path (get-path (puzzle/dijkstra track start neighbours
                                        :result-type :prev)
                       end
                       [])
        min-save (+ save 2)

        manhattan-distance (fn [[x1 y1] [x2 y2]] (+ (abs (- x2 x1))
                                                    (abs (- y2 y1))))]

    (loop [i 0
           j min-save
           cheat-cnt {}]
      (if (>= (+ i min-save) (count path))
        (apply + (vals cheat-cnt))
        (if (>= j (count path)) 
          (recur (inc i) (+ i min-save) cheat-cnt)
          (let [cheat-distance (manhattan-distance (path i)
                                                   (path j))
                cheat-savings (- j i cheat-distance)]
            
            (recur i (inc j) (if (and (<= cheat-distance max-cheat)
                                      (>= cheat-savings save))
                               (update cheat-cnt cheat-savings #(if % (inc %) 1))
                               cheat-cnt))))))))

(sort (solve-1 testdata 2))

(solve-1 data 100)

(solve-1 testdata 76 :max-cheat 20)
(solve-1 data 100 :max-cheat 20)

