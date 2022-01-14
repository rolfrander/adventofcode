(ns advent2016.day24
  (:require [rolfrander.puzzle-lib :refer [get-data dijkstra *debug*]]
            [clojure.string :as str]))

(def testdata "###########
#0.1.....2#
#.#######.#
#4.......3#
###########")

(def testdata2 "
###########
#0.1.....2#
#########.#
#4........#
#.#######.#
#........6#
#.#########
#7.......8#
###########")

(defn node [row col goal]
  {:pos [row col]
   :goal (when-not (= goal \.)
           (- (int goal) (int \0)))
   :neighbours '()})

(defn index-by [key coll]
  (reduce #(assoc %1 (key %2) %2) {} coll))

(defn parse-line [rowno line]
  (->> (map-indexed #(when (not= %2 \#)
                       (node rowno %1 %2))
                    (seq line))
       (remove nil?)))

(defn parse [input]
  (->> (map-indexed parse-line (str/split-lines input))
       flatten
       (index-by :pos)))

(defn find-neighbours [grid pos]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
       (map #(mapv + pos %))
       (filter (partial contains? grid))))

(defn add-neighbours [grid]
  (loop [p (keys grid)
         newgrid {}]
    (if (empty? p)
      newgrid
      (let [cur (first p)]
        (recur (rest p)
               (->> (find-neighbours grid cur)
                    (assoc (grid cur) :neighbours)
                    (assoc newgrid cur)))))))

(defn core-nodes [grid]
  (->> (filter #(or (:goal %) (> (count (:neighbours %)) 2)) (vals grid))
       (index-by :pos)))

(defn find-simple-route [grid targets start-pos step-pos]
  (loop [cur-pos step-pos
         seen #{start-pos step-pos}
         dist 1] ; assume distance from start-pos to step-pos is 1
    (cond (nil? cur-pos) nil

          (contains? targets cur-pos) [cur-pos dist]

          :else
          (let [neighbour-list (remove seen (find-neighbours grid cur-pos))
                n (first neighbour-list)]
            (recur n
                   (conj seen n)
                   (inc dist))))))

(defn extend-neighbours [grid core-nodes cur-node]
  (let [pos (:pos cur-node)
        neighbours (:neighbours cur-node)]
    (keep (partial find-simple-route
                   grid core-nodes pos)
          neighbours)))

(defn simplify [grid]
  (let [nodes (core-nodes grid)]
    (loop [n (keys nodes)
           res {}
           dist {}]
      (if (empty? n)
        [res dist]
        (let [cur-pos (first n)
              cur-node (nodes cur-pos)
              extended-neighbours (extend-neighbours grid nodes cur-node)
              dist (reduce (fn [d [n w]]
                             (if (get-in d [cur-pos n])
                               d
                               (-> d
                                   (assoc-in [cur-pos n] w)
                                   (assoc-in [n cur-pos] w))))
                           dist extended-neighbours)]
          (recur (rest n)
                 (->> (assoc cur-node :neighbours (map first extended-neighbours))
                      (assoc res cur-pos))
                 dist))))))

(comment
  (let [nodes (core-nodes data)
        cur-pos [21 121]]
  ;(:neighbours (nodes cur-pos))
  ;(extend-neighbours data nodes (nodes cur-pos))
  ; infinite loop:
    (find-simple-route data nodes cur-pos [22 121])
  ;(:neighbours (data [22 121]))
    ))


(defn distance-between-goals [grid]
  (let [[nodes dist] (simplify grid)
        neighbours (fn [pos] (:neighbours (nodes pos)))
        all-goals (->> (vals nodes)
                       (filter :goal)
                       (map :pos)
                       (into #{}))
        intermediate (->> (vals nodes)
                          (remove :goal)
                          (map :pos))]
    (reduce (fn [super-dist src]
              (assoc super-dist src (-> (dijkstra (keys nodes) src neighbours
                                                  :weight-fn #((dist %1) %2)
                                                  :result-type :dist)
                                        (dissoc src)
                                        (as-> x (apply dissoc x intermediate)))))
            {} all-goals)))

(defn debug [input]
  (prn input)
  input)

(defn shortest-path [grid]
  (let [dist (distance-between-goals grid)
        ordered-nodes (->> (vals grid)
                           (filter :goal)
                           (sort-by :goal)
                           (map :pos))
        start (first ordered-nodes)]
    (letfn [(update-dist-by [delta]
              (fn [[pos dist]] [pos (+ dist delta)]))

            (search [current-node node-set current-dist best]
              (if (empty? node-set)
                ; return final distance and a list of nodes ending with current
                [(+ current-dist ((dist current-node) start)) (cons current-node '())]

                ; recursively try all other nodes looking for a path better than "best"
                ; next-distance is a list of other nodes with distances from current to each
                (let [next-distance (->> (map #(vector % ((dist current-node) %)) node-set) ; find distance from this to other nodes
                                         (sort-by second) ; sort by distance (start with the nearest)
                                         (map (update-dist-by current-dist)) ; combined distance from start to first-node and from first-node to next
                                         )]
                  (reduce (fn [[best path] [next dist-to-next]]
                            (if (< best dist-to-next)
                              [best path] ; prune
                              (let [[d p] (search next (disj node-set next) dist-to-next best)]
                                (if (< d best)
                                  [d (cons next p)]
                                  [best path]))))
                          [best nil] next-distance))))]
      (search start
              (set (rest ordered-nodes))
              0
              9999999999))))


(defn task-1 [input]
  (shortest-path (add-neighbours (parse input))))

(def data (add-neighbours (parse (get-data 2016 24))))

(defn get-map []
  (vals data))

(defn max-by [key coll]
  (apply max (map key coll)))

(defn get-paths []
  (let [[task-2 grand-path] (shortest-path data)
        grid data
        origin (->> (vals grid)
                    (some #(when (= (:goal %) 0) %))
                    :pos)
        neighbours (fn [pos] (:neighbours (grid pos)))]
    (map (fn [[src dest]] (dijkstra (keys grid) src neighbours
                                    :dest? #(= dest %)
                                    :result-type :prev))
         (partition 2 1 (concat [origin] grand-path [origin])))))

(defn get-grid-data []
  (let [path-data (get-paths)
        path-step (quot 360 (count path-data))
        max-x (max-by second (keys data))
        max-y (max-by first  (keys data))
        junction? (fn [pos] (> (count (:neighbours (data pos))) 2))
        ret {:xrange [1 max-x]
             :yrange [1 max-y]
             :data nil}
        basic-grid (reduce (fn [res pos]
                             (let [[y x] pos]
                               (assoc res pos {:x x :y y :class (if (junction? pos)
                                                                  "room junction"
                                                                  "room plain")
                                               :info (format "%d %d" x y)})))
                           {} (keys data))
        grid-with-paths (first (reduce (fn [[res i] path-list]
                                         [(reduce #(assoc-in %1 [%2 :style]
                                                             (format "fill: hsl(%d,100%%,40%%)" (* i path-step)))
                                                  res path-list)
                                          (inc i)])
                                       [basic-grid 0]
                                       path-data))
        grid-with-goals (reduce (fn [res pos]
                                  (if (:goal (data pos))
                                    (assoc-in res [pos :class] "room goal")
                                    res))
                                grid-with-paths (vals data))]
    (assoc ret :data (vals grid-with-goals))))

(keys (get-grid-data))

(defn dispatch [function params]
  (case function
    "map" (get-map)
    "paths" (get-paths)
    "data" (get-grid-data)))

;(dispatch "paths" nil)

(task-1 testdata)
(task-1 (get-data 2016 24))


