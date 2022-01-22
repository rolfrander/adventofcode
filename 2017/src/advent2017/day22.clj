(ns advent2017.day22
  (:require [rolfrander.puzzle-lib :refer [move-fn get-data abs]]
            [clojure.string :as str]))

(def testdata "..#
#..
...")

(defn parse-line [y line]
  (keep-indexed #(when (= \# %2) [%1 y]) (seq line)))

(defn parse [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        center-x (quot width 2)
        center-y (quot height 2)]
    (->> (map-indexed parse-line lines)
         (apply concat)
         (map (fn [[x y]] [(- x center-x) (- center-y y)])))))

(def move (move-fn :sq-4 :infinite))

(def turn-left (zipmap ["n" "e" "s" "w"]
                       ["w" "n" "e" "s"]))

(def turn-right (zipmap ["w" "n" "e" "s"]
                        ["n" "e" "s" "w"]))

(def turn-back (zipmap ["n" "e" "s" "w"]
                       ["s" "w" "n" "e"]))

(def points->grid (partial into #{}))

(defn points->grid2 [points]
  (zipmap points (repeat :infected)))

(def start-state {:grid nil
                  :pos [0 0]
                  :dir "n"
                  :max-dim 0
                  :counter 0
                  :infected 0})

;(defn abs [x] (Math/abs x))

(defn max-dim [state]
  (let [grid (:grid state)
        grid (if (associative? grid) (keys grid) grid)
        max (reduce #(max %1 (abs (first %2)) (abs (second %2))) 0 grid)]
    (assoc state :max-dim max)))

(defn step [{:keys [grid pos dir max-dim counter]}]
  (let [dir ((if (grid pos) turn-left turn-right) dir)
        counter (if (grid pos) counter (inc counter))
        grid ((if (grid pos) disj conj) grid pos)
        [x y :as pos] (move pos dir)]
    {:grid grid
     :pos pos
     :dir dir
     :max-dim (max max-dim (abs x) (abs y))
     :counter counter}))

(defn step-2 [{:keys [grid pos dir max-dim counter infected]}]
  (let [dir (case (get grid pos :clean)
              :clean    (turn-left dir)
              :weakened dir
              :infected (turn-right dir)
              :flagged  (turn-back dir))
        grid (case (get grid pos :clean)
               :clean    (assoc grid pos :weakened)
               :weakened (assoc grid pos :infected)
               :infected (assoc grid pos :flagged)
               :flagged  (dissoc grid pos))
        infected (if (= (get grid pos) :infected)
                   (inc infected)
                   infected)
        [x y :as pos] (move pos dir)
        max-dim (max max-dim (inc (abs x)) (inc (abs y)))
        counter (inc counter)]
    {:grid grid
     :pos pos
     :dir dir
     :max-dim max-dim
     :infected infected
     :counter counter}))

(defn print-grid [state]
  (let [max-dim (:max-dim state)
        grid (:grid state)
        pos (:pos state)]
    (println "direction:" (:dir state))
    (println "count:" (:counter state))
    (println "infections:" (:infected state))
    (doseq [y (range max-dim (- max-dim) -1)]
      (doseq [x (range (- max-dim) (inc max-dim))]
        (printf (cond (= pos [x y]) "[%s"
                      (= pos [(dec x) y]) "]%s"
                      :else " %s") (case (grid [x y])
                                     true "#"
                                     :infected "#"
                                     :weakened "W"
                                     :flagged "F"
                                     ".")))
      (newline))))

(defn get-grid []
  (->> (get-data 2017 22)
       parse))

(defn dispatch [f-name _params]
  (case f-name
    "data" (get-grid)))

(defn task-1 [input]
  (let [points (parse input)]
    (->> (assoc start-state :grid (points->grid points))
         max-dim
         (iterate step)
         (drop 10000)
         first
         :counter)))

(defn task-2 [input cnt]
  (let [points (parse input)]
    (->> (assoc start-state :grid (points->grid2 points))
         max-dim
         (iterate step-2)
         (drop cnt)
         first
         :infected)))

;(time (task-2 (get-data 2017 22) 10000000))

;(time (task-1 (get-data 2017 22)))
;