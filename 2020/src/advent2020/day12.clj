(ns advent2020.day12
  (:require [clojure.string :as str]))

(def testdata "F10
N3
F7
R90
F11")

(defn parse [input]
  (let [m (re-matcher #"([A-Z])([0-9]+)" input)]
    (take-while #(not (nil? %))
                (repeatedly (partial re-find m)))))

(def direction {"N" [0 1]
                "S" [0 -1]
                "E" [1 0]
                "W" [-1 0]
                })

(defn rot [[dir-x dir-y] deg]
  (case deg
    90 [(- dir-y) dir-x]
    180 [(- dir-x) (- dir-y)]
    270 [dir-y (- dir-x)])
  )

(defn navigate [steps]
  (reduce (fn [[x y dir] [_ instr cnt]]
            (let [cnt (Integer/parseInt cnt)]
              (case instr
                "N" [x (+ y cnt) dir]
                "S" [x (- y cnt) dir]
                "E" [(+ x cnt) y dir]
                "W" [(- x cnt) y dir]
                "R" [x y (rot dir (- 360 cnt))]
                "L" [x y (rot dir cnt)]
                "F" [(+ x (* (first dir) cnt))
                     (+ y (* (second dir) cnt))
                     dir])))
          [0 0 [1 0]]
          steps))

(defn navigate-2 [steps]
  (reduce (fn [[x y wp] [_ instr cnt]]
            (let [[wx wy] wp
                  cnt (Integer/parseInt cnt)]
              (case instr
                "N" [x y [wx (+ wy cnt)]]
                "S" [x y [wx (- wy cnt)]]
                "E" [x y [(+ wx cnt) wy]]
                "W" [x y [(- wx cnt) wy]]
                "R" [x y (rot wp (- 360 cnt))]
                "L" [x y (rot wp cnt)]
                "F" [(+ x (* wx cnt))
                     (+ y (* wy cnt))
                     wp])))
          [0 0 [10 1]]
          steps))

(defn manhattan [[x y _dir]]
  (+ (Math/abs x)
     (Math/abs y)))

(->> ;testdata
     (slurp "day12.txt")
     parse
     navigate-2
     manhattan)
;; => 178986


(rot [1 0] 270)