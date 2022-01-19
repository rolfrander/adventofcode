(ns advent2017-viz.day22
  (:require [advent2017-viz.grid :as grid]))

(defn move [[x y] dir]
  (case dir
    "n" [x (inc y)]
    "e" [(inc x) y]
    "s" [x (dec y)]
    "w" [(dec x) y]))

; turning is mirrored, our grid goes down, move-fn assumes up
(def turn-left (zipmap ["w" "n" "e" "s"]
                       ["n" "e" "s" "w"]))

(def turn-right (zipmap ["n" "e" "s" "w"]
                        ["w" "n" "e" "s"]))

(def turn-back (zipmap ["n" "e" "s" "w"]
                       ["s" "w" "n" "e"]))

(def start-state {:grid nil
                  :pos [0 0]
                  :dir "n"
                  :max-dim 15
                  :infected 0
                  :counter 0
                  :xrange [-15 15]
                  :yrange [-15 15]})

(defn abs [x] (Math/abs x))

(defn make-point [[x y] state]
  {:x x :y y :class state})

(defn step [{:keys [grid pos dir max-dim counter infected]}]
  (let [dir (case (get-in grid [pos :class] "clean")
              "clean"    (turn-left dir)
              "weakened" dir
              "infected" (turn-right dir)
              "flagged"  (turn-back dir))
        grid (case (get-in grid [pos :class] "clean")
               "clean"    (assoc grid pos (make-point pos "weakened"))
               "weakened" (assoc-in grid [pos :class] "infected")
               "infected" (assoc-in grid [pos :class] "flagged")
               "flagged"  (dissoc grid pos))
        infected (if (= (get-in grid [pos :class]) "infected")
                   (inc infected)
                   infected)
        [x y :as pos] (move pos dir)
        max-dim (max max-dim (abs x) (abs y))
        counter (inc counter)]
    {:grid grid
     :data (vals (assoc grid pos (make-point pos "pos")))
     :pos pos
     :dir dir
     :max-dim max-dim
     :xrange [(- max-dim) max-dim]
     :yrange [(- max-dim) max-dim]
     :infected infected
     :counter counter
     :debug (str "counter: " counter "\r\n"
                 "infected: " infected "\r\n")}))

(defn prepare-state [data]
  (let [state (->> data
                   (reduce #(assoc %1 %2 (make-point %2 "infected"))
                           {})
                   (assoc start-state :grid))]
    (assoc state :data (vals (:grid (assoc-in state [:grid [0 0]] (make-point [0 0] "pos")))))))

(defn get-grid-nodes [data]
  (vals (:grid data)))

(grid/set-simulate-fn step)
(grid/set-prepare-fn prepare-state)
(grid/set-data-fn get-grid-nodes)