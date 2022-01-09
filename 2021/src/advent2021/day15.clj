(ns advent2021.day15
  (:require [clojure.string :as string]
            [clojure.data.priority-map :refer [priority-map]]))

(def testdata "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(def data (advent2021.core/load-data 15 "53616c7465645f5f5a4604206c8513eae045f4ad5f28de5f922c27403a8b27dc3c61b859b662ac43879c8aaed36c456f"))

(defn neighbours [[x y] max-x max-y]
  (filter (fn [[x y]] (and (>= x 0)
                           (>= y 0)
                           (< x max-x)
                           (< y max-y)))
          (map (fn [[dx dy]]
                 [(+ x dx) (+ y dy)])
               [[1 0] [-1 0] [0 1] [0 -1]])))

(defn parse [d]
  (let [input-lines (string/split-lines d)
        max-x (count (first input-lines))
        max-y (count input-lines)
        create-vertex (fn [x y c] {:pos [x y]
                                   :weight (- (int c) (int \0))
                                   :neighbours (neighbours [x y] max-x max-y)})
        vertex-map (->> (map-indexed
                         (fn [y line]
                           (map-indexed #(create-vertex %1 y %2) line))
                         input-lines)
                        flatten
                        (reduce (fn [s e]
                                  (assoc s (:pos e) e))
                                {}))]
    {:vertex-map vertex-map
     :max-x max-x
     :max-y max-y}))

(defn print-vertex [{:keys [vertex-map max-x max-y]}]
  (dotimes [y max-y]
    (dotimes [x max-x]
      (print (:weight (vertex-map [x y]))))
    (newline)))

(defn print-directions [{:keys [max-x max-y]} prev]
  (dotimes [y max-y]
    (dotimes [x max-x]
      (let [[px py] (or (prev [x y]) [-1 -1])
            dx (- px x)
            dy (- py y)]
        (print (case [dx dy]
                 [-1 0] "<"
                 [1 0]  ">"
                 [0 -1] "^"
                 [0 1]  "v"
                 "o"))))
    (newline)))

;(print-vertex (parse testdata))

(defn expand [{:keys [vertex-map max-x max-y]} count-x count-y]
  (let [new-max-x (* count-x max-x)
        new-max-y (* count-y max-y)]
    (letfn [(recount-neighbours [cave]
                                (reduce (fn [cave v]
                                          (assoc-in cave [v :neighbours]
                                                    (neighbours v new-max-x new-max-y)))
                                        cave
                                        (keys cave)))
            (increase-vertex [v [dx dy]]
              (let [weight (:weight v)
                    [x y] (:pos v)
                    new-x (+ x (* max-x dx))
                    new-y (+ y (* max-y dy))]
                (assoc v
                       :weight (inc (mod weight 9))
                       :pos [new-x new-y]
                       :neighbours (neighbours [new-x new-y] new-max-x new-max-y))))
            (increase-x [v] (increase-vertex v [1 0]))
            (increase-y [v] (increase-vertex v [0 1]))
            (increase [cave count increase-function]
              (reduce (fn [cave-1 vertex]
                        (->> (increase-function vertex)
                             (iterate increase-function)
                             (take (dec count))
                             (reduce #(assoc %1 (:pos %2) %2)
                                     cave-1)))
                      cave
                      (vals cave)))]
      {:vertex-map (-> vertex-map
                       recount-neighbours
                       (increase count-x increase-x)
                       (increase count-y increase-y))
       :max-x new-max-x
       :max-y new-max-y})))

; (print-vertex (expand (parse testdata) 5 5))

(defn debug [x]
  (println x)
  x)

(defn dijkstra [vertex-map source destination]
  (loop [Q vertex-map
         dist (-> Q
                  (as-> d (reduce (fn [ret v] (assoc ret v 999999)) {} (keys d)))
                  (assoc source 0))
         prev {}]
    (if (empty? Q)
      [dist prev]
      (let [u (apply min-key dist (keys Q))]
        (if (= u destination)
          (dist u)
          (let [Q (dissoc Q u)
                [new-dist new-prev] (reduce (fn [[new-dist new-prev] v]
                                              (let [alt (+ (new-dist u) (:weight (vertex-map v)))]
                                                (println u v alt)
                                                (if (< alt (new-dist v))
                                                  [(assoc new-dist v alt)
                                                   (assoc new-prev v u)]
                                                  [new-dist new-prev])))
                                            [dist prev]
                                            (filter Q (:neighbours (vertex-map u))))]
            (recur Q new-dist new-prev)))))))

(defn dijkstra-pri [vertex-map source destination]
  (loop [dist (-> vertex-map
                  (as-> d (reduce (fn [ret v] (assoc ret v 99999999)) {} (keys d)))
                  (assoc source 0))
         prev {}
         Q (into (priority-map) dist)]
    (if (empty? Q)
      [dist prev]
      (let [u (first (peek Q))
            Q (pop Q)]
        (when (Q u) (throw (RuntimeException. (str u " is still in Q"))))
        (if (= u destination)
          (dist u)
          (let [[new-dist new-prev new-q]
                (->> (filter Q (:neighbours (vertex-map u))) ; for each neighbour v of u, still in Q
                     (reduce (fn [[new-dist new-prev q] v]
                               (let [alt (+ (new-dist u) (:weight (vertex-map v)))] ; alt <- dist[u] + length(u,v)
                                 (if (< alt (new-dist v))                           ; if alt < dist[v]
                                   [(assoc new-dist v alt)                          ;   dist[v] <- alt
                                    (assoc new-prev v u)                            ;   prev[v] <- u
                                    (assoc q v alt)]                        ;   Q.decrease_priority(v, alt)
                                   [new-dist new-prev q])))
                             [dist prev Q]))]
            (recur new-dist new-prev new-q)))))))

; (apply min-key :dist (vals (assoc-in (parse testdata) [[0 0] :dist] 0)))

(defn task-1 [data]
  (let [{:keys [vertex-map max-x max-y]} (parse data)]
    (dijkstra-pri vertex-map [0 0] [(dec max-x) (dec max-y)])))

(defn task-2 [data]
  (let [{:keys [vertex-map max-x max-y]} (expand (parse data) 5 5)]
    (dijkstra-pri vertex-map [0 0] [(dec max-x) (dec max-y)])))


;(dijkstra (:vertex-map (parse testdata)))

(get (:vertex-map (parse testdata)) [0 2])

(dijkstra-pri (:vertex-map (parse testdata)) [0 0] [9 9])

(let [d (expand (parse testdata) 5 5)]
  (print-directions d (second (dijkstra-pri (:vertex-map d) [0 0] nil))))

(task-1 data) ; 583
(task-2 data) ; 2927

;(+ 1 2)