(ns advent2016.day22
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number dijkstra]]
            [clojure.string :as str]
            [clojure.test :as test]))

(def testdata "root@ebhq-gridcenter# df -h
Filesystem              Size  Used  Avail  Use%
/dev/grid/node-x0-y0     94T   65T    29T   69%")

(def testdata2 "# df -h
Filesystem            Size  Used  Avail  Use%
/dev/grid/node-x0-y0   10T    8T     2T   80%
/dev/grid/node-x0-y1   11T    6T     5T   54%
/dev/grid/node-x0-y2   32T   28T     4T   87%
/dev/grid/node-x1-y0    9T    7T     2T   77%
/dev/grid/node-x1-y1    8T    0T     8T    0%
/dev/grid/node-x1-y2   11T    7T     4T   63%
/dev/grid/node-x2-y0   10T    6T     4T   60%
/dev/grid/node-x2-y1    9T    8T     1T   88%
/dev/grid/node-x2-y2    9T    6T     3T   66%")

(defn parse-line [line]
  (if-let [tokens (re-matches #"/dev/grid/(node-x([0-9]+)-y([0-9]+)) +([0-9]+)T +([0-9]+)T +([0-9]+)T +([0-9]+)%" line)]
    (zipmap [:name :x :y :size :used :avail :use%] (map safe-parse-number (rest tokens)))
    (throw (RuntimeException. (str "unexpected format for line: " line)))))

(defn parse [input]
  (->> (str/split-lines input)
       rest
       rest
       (map parse-line)))

(test/deftest parse-test
  (test/is (= {:name "node-x0-y1", :x 0, :y 1, :size 94, :used 65, :avail 29, :use% 69}
              (parse-line "/dev/grid/node-x0-y1     94T   65T    29T   69%")))
  (test/is (= '({:name "node-x0-y0", :x 0, :y 0, :size 94, :used 65, :avail 29, :use% 69})
              (parse testdata))))

(defn rearrange [grid-data]
  (->> grid-data
       (map (fn [g] [[(:x g) (:y g)] {:pos [(:x g) (:y g)]
                                      :size (:size g)
                                      :used (:used g)}]))
       (into {})))

(defn avail [node]
  (- (:size node) (:used node)))

(defn viable-pair [a b]
  (and (not= a b)
       (not (zero? (:used a)))
       (<= (:used a) (avail b))))

(test/deftest viable-pair-test
  (test/is (viable-pair {:size 94  :used 65}
                        {:size 131 :used 65})))

(defn neighbours
  "which neighbours can the node move its data to inside the grid"
  [grid node]
  (let [[x y] (:pos node)]
    (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
          :let [n [(+ x dx) (+ y dy)]]
          :when (and (contains? grid n)
                     (viable-pair node (grid n)))]
      n)))

(def testgrid (rearrange (parse testdata2)))

(test/deftest neighbours-test
  (test/is (= (neighbours testgrid (testgrid [1 0]))
              '([1 1]))))

(defn move-data [grid from to]
  (let [from-node (grid from)
        volume (:used from-node)
        is-goal (= from (:goal grid))
        grid (-> grid
                 (assoc :lastmove (second to))
                 (update from #(assoc  % :used 0))
                 (update to   #(update % :used (partial + volume))))]
    (if is-goal
      (assoc grid :goal to)
      grid)))

(test/deftest move-test
  (test/is (= 7 (:used ((move-data testgrid [1 0] [1 1]) [1 1]))))
  (test/is (= 0 (:used ((move-data testgrid [1 0] [1 1]) [1 0])))))

(defn all-possible-moves [grid]
  (->> (vals grid)
       (filter map?)
       (mapcat #(when-let [n (seq (neighbours grid %))]
                  (map (partial vector (:pos %)) n)))))

;(all-possible-moves testgrid)

(defn max-by [key coll]
  (apply max (map key coll)))

(defn all-viable-pairs [data]
  (for [a data
        b data
        :when (viable-pair a b)]
    [a b]))

(defn task-1 [data]
  (count (all-viable-pairs data)))

(defn set-goal [data]
  (let [max-x (max-by first (keys data))]
    (assoc data
           :goal [max-x 0]
           :lastmove 0)))


(defn pred-pred [pred x]
  (pred x))

(defn clamp [a x b]
  (min (max a x) b))

(defn print-grid [grid]
  (let [coords (remove #{:goal :lastmove} (keys grid))
        max-x (max-by first coords)
        max-y (max-by second coords)
        empty? #(= 0 (:used %))
        full? #(and (> (:size %) 20) (> (/ (:used %) (:size %)) 0.85))
        goal? #(= (:goal grid) %)
        not-moveable (fn [pos] (->> [[-1 0] [1 0] [0 -1] [0 1]]
                                    (map #(map + pos %))
                                    (filter (partial contains? grid))
                                    (every? #(> (:used (grid pos)) (:size (grid %))))))]
    (dotimes [y (inc max-y)]
      (dotimes [x (inc max-x)]
        (let [node (grid [x y])]
          (print (str " " (if (nil? node)
                            "."
                            (char (+ 0x2581 (clamp 0 (int (/ (- 80 (avail node)) 3)) 7))))))))
      (newline))))

;(print-grid (assoc-in testgrid [[2 0] :goal] true))

(test/deftest task-2-test
  (test/is (= 7 (task-2 testgrid)
              )))

(def data (parse (get-data 2016 22)))
(def data2 (rearrange data))

;sanity-check on data
(test/deftest sanity-test
  (test/is (= '() (keep #(when (not= 0 (- (:size %) (:avail %) (:used %)))
                           (:name %)) data))))


;(binding [rolfrander.puzzle-lib/*debug* true]
;  (task-2 (rearrange data)))

(let [has-room (into #{} (map (comp (juxt :x :y) second) (all-viable-pairs data)))
      max-x (max-by :x data)
      max-y (max-by :y data)]
  (dotimes [y (inc max-y)]
    (dotimes [x (inc max-x)]
      (if (< (:used (data2 [x y])) 80)
        (print ".")
        (print "X")))
    (newline)))

(def grid (rearrange data))

(let [available-coords (->> (keys grid)
                            (remove #(> (:used (grid %)) 100))
                            (into #{}))
      start (some #(when (= (:used %) 0) (:pos %)) (vals grid))
      goal-x (max-by first available-coords)
      goal-y 0
      goal [goal-x goal-y]
      subgoal [(dec goal-x) goal-y]
      neighbours-fn (fn [[x y]] (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
                                      :let [n [(+ x dx) (+ y dy)]]
                                      :when (available-coords n)]
                                  n))]
    ; move available space to subgoal, one left of goal
  (+ (dijkstra available-coords start neighbours-fn
               :dest? (partial = subgoal)
               :result-type :dist)
     ; move goal one to the left
     1
     ; move space around goal, then goal, times the distance
     (* 5 (dec goal-x)))
  
  )

;(frequencies (map (comp #(quot % 10) :used) (vals grid)))

;(print-grid (set-goal grid))


(test/run-all-tests #"advent2016.day22")