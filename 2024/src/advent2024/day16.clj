(ns advent2024.day16
  (:require
   [clojure.string :as str]
   [clojure.data.priority-map :refer [priority-map priority-map-by]]
   [rolfrander.puzzle-lib :as puzzle]))

(def testdata "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")

(def testdata2 "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################")

(def data (puzzle/get-data 2024 16))

(def turn-left {"n" "e"
                "e" "s"
                "s" "w"
                "w" "n"
                })

(def turn-right {"n" "w"
                 "e" "n"
                 "s" "e"
                 "w" "s"
                 })

(def move (puzzle/move-fn  :sq-4 :infinite))

(defn solve-1 [in]
  (let [m (puzzle/parse-map in)
        end-point (first (get-in m [:markings \E]))
        start-point (first (get-in m [:markings \S]))
        nodes (into #{} (mapcat #(vector {:point % :dir "n"}
                                         {:point % :dir "s"}
                                         {:point % :dir "e"}
                                         {:point % :dir "w"})
                                (conj (get-in m [:markings \.])
                                      end-point
                                      start-point)))
        start {:point start-point
               :dir "e"}
        move (puzzle/move-fn  :sq-4 :infinite)
        neighbours (fn [pos] (let [turn [(update pos :dir turn-left)
                                         (update pos :dir turn-right)]
                                   new-pos (update pos :point #(move % (:dir pos)))]
                               (if (contains? nodes new-pos)
                                 (conj turn new-pos)
                                 turn)))
        weight (fn [p1 p2] (if (= (:dir p1) (:dir p2))
                             1
                             1000))
        check-end (fn [pos] (= (:point pos) end-point))]
    (puzzle/dijkstra nodes start neighbours
                     :weight-fn weight
                     :dest? check-end
                     :result-type :dist)))

(defn recursive-find-path [pos max-len end? weight neighbours]
  (letfn [(find-path-internal [visited pos max-len]
            (cond (and (end? pos) (= max-len 0))
                  (conj #{} pos)

                  (<= max-len 0)
                  nil

                  :else
                  (loop [n (->> (neighbours pos)
                                (remove #(contains? visited (:point %))))
                         paths #{}]
                    (if (empty? n)
                      (if (empty? paths)
                        nil
                        paths)
                      (if-let [path (find-path-internal (conj visited (:point pos))
                                                        (first n)
                                                        (- max-len (weight pos (first n))))]
                        (recur (rest n) (into (conj paths pos) path))
                        (recur (rest n) paths))))))]
    (find-path-internal #{} 
                        pos 
                        max-len)))

(defn solve-2 [in max]
  (let [m (puzzle/parse-map in)
        end-point (first (get-in m [:markings \E]))
        start-point (first (get-in m [:markings \S]))
        nodes (into #{} (mapcat #(vector {:point % :dir "n"}
                                         {:point % :dir "s"}
                                         {:point % :dir "e"}
                                         {:point % :dir "w"})
                                (conj (get-in m [:markings \.])
                                      end-point
                                      start-point)))
        start {:point start-point
               :dir "e"}
        move (puzzle/move-fn :sq-4 :infinite)
        neighbours (fn [pos] (let [turn [(update pos :dir turn-left)
                                         (update pos :dir turn-right)]
                                   new-pos (update pos :point #(move % (:dir pos)))]
                               (if (contains? nodes new-pos)
                                 (conj turn new-pos)
                                 turn)))
        weight (fn [p1 p2] (if (= (:dir p1) (:dir p2))
                             1
                             1000))
        check-end (fn [pos] (= (:point pos) end-point))

        paths
        (->> (recursive-find-path start max check-end weight neighbours)
             (map :point)
             (into #{}))]
    (puzzle/draw-map (assoc-in m [:markings \O] paths))
    (count paths)))

(defn dijkstra-special
  [nodes source neighbour-fn & {:keys [weight-fn result-type dest?]
                                :or {weight-fn (constantly 1)
                                     result-type :both
                                     dest? (constantly false)}}]
  ; implemented from wikipedia https://en.wikipedia.org/wiki/Dijkstra's_algorithm
  (letfn [(get-path [prev dest] (if (contains? prev dest)
                                  (cons dest (get-path prev (prev dest)))
                                  '()))]
    (loop [dist (-> (zipmap nodes (repeat 99999999))
                    (assoc source 0))
           prev {}
           Q (into (priority-map) dist)]
      (if (empty? Q)
        (case result-type
          :dist dist
          :prev prev
          :both [dist prev])
        (let [u (first (peek Q))
              Q (pop Q)]
          (if (dest? u)
            (case result-type
              :dist (dist u)
              :prev (get-path prev u)
              :both [(dist u) (get-path prev u)])
            (let [[new-dist new-prev new-q]
                  (->> (filter Q (neighbour-fn u)) ; for each neighbour v of u, still in Q
                       (reduce (fn [[new-dist new-prev q] v]
                                 (let [alt (+ (new-dist u) (weight-fn u v))] ; alt <- dist[u] + length(u,v)
                                   (if (<= alt (new-dist v))                ; if alt < dist[v]
                                     [(assoc new-dist v alt)               ;   dist[v] <- alt
                                      (if (= alt (new-dist v))
                                        ; if equal to earlier paths, add to list
                                        (update-in new-prev [v :come-from] conj u)
                                        (assoc new-prev v {:come-from (list u)}))                 ;   prev[v] <- u
                                      (assoc q v alt)]                     ;   Q.decrease_priority(v, alt)
                                     [new-dist new-prev q])))
                               [dist prev Q]))]
              (recur new-dist new-prev new-q))))))))

(defn solve-2b [in]
  (let [m (puzzle/parse-map in)
        end-point (first (get-in m [:markings \E]))
        start-point (first (get-in m [:markings \S]))
        nodes (into #{} (mapcat #(vector {:point % :dir "n"}
                                         {:point % :dir "s"}
                                         {:point % :dir "e"}
                                         {:point % :dir "w"})
                                (conj (get-in m [:markings \.])
                                      end-point
                                      start-point)))
        start {:point start-point
               :dir "e"}
        move (puzzle/move-fn  :sq-4 :infinite)
        neighbours (fn [pos] (let [turn [(update pos :dir turn-left)
                                         (update pos :dir turn-right)]
                                   new-pos (update pos :point #(move % (:dir pos)))]
                               (if (contains? nodes new-pos)
                                 (conj turn new-pos)
                                 turn)))
        weight (fn [p1 p2] (if (= (:dir p1) (:dir p2))
                             1
                             1000))
        check-end (fn [pos] (= (:point pos) end-point))

        prev (dijkstra-special nodes start neighbours
                               :weight-fn weight
                                        ;:dest? check-end
                               :result-type :prev)
        dest (first (filter check-end (keys prev)))]
    (letfn [(get-path [prev dest] (if (contains? prev dest)
                                    (let [p (:come-from (prev dest))]
                                      (if (> (count p) 1)
                                        (cons dest (list (mapv (partial get-path prev) p)))
                                        (cons dest (get-path prev (first p)))))
                                    '()))]
      (let [point-set (->> (get-path prev dest)
                           flatten

                           (map :point)
                           (into #{}))]
        
        (puzzle/draw-map (assoc-in m [:markings \O] point-set))
        (count point-set)))))

(solve-1 testdata)
;;=> 7036
(solve-1 testdata2)
;;=> 11048
(solve-1 data)
;;=> 88468

(solve-2b testdata)
;;=> 45
(solve-2b testdata2)
;;=> 64
(solve-2b data)

(puzzle/draw-map (puzzle/parse-map data))