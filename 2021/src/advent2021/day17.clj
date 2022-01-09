(ns advent2021.day17)

(def testdata "target area: x=20..30, y=-10..-5")
(def data (advent2021.core/load-data 17 "53616c7465645f5f5a4604206c8513eae045f4ad5f28de5f922c27403a8b27dc3c61b859b662ac43879c8aaed36c456f"))

(defn parse-long-all [list-of-strings]
  (map #(Long/parseLong %) list-of-strings))

(defn parse [input]
  (->> (re-matches #"target area: x=(-?[0-9]+)..(-?[0-9]+), y=(-?[0-9]+)..(-?[0-9]+)[\r\n]*" input)
       rest
       parse-long-all
       (map vector [:x1 :x2 :y1 :y2])
       (into {})
       ))

;(parse data)

(defn max-y-velocity [target-area]
  ;; virker kanskje bare hvis y1 og y2 er negativ
  (dec (Math/abs (min (:y2 target-area)
                      (:y1 target-area)))))

(defn min-y-velocity [target-area]
  ; virker bare hvis y1 er negativ og mindre enn y2
  (:y1 target-area))

(defn max-y [target-area]
  (let [initial-y-velocity (max-y-velocity target-area)
        max-y-pos (/ (* initial-y-velocity (inc initial-y-velocity)) 2)]
    max-y-pos)
  )

(defn count-steps [target-area y-velocity]
  ; kan l;ses mer elegant matematisk...
  (loop [y 0
         v y-velocity
         i 0]
    (cond (< y (:y1 target-area)) nil
          (<= y (:y2 target-area)) i
          :else (recur (+ y v) (dec v) (inc i)))))

(defn task-1 [input]
  (max-y (parse input )))

(defn min-x-velocity [target-area]
  (inc (long (/ (+ -1 (Math/sqrt (+ 1 (* 8 (:x1 target-area))))) 2))))

(defn max-x-velocity [target-area]
  (:x2 target-area))

(defn next-step [[x y dx dy]]
  [(+ x dx) (+ y dy)
   (cond (> dx 0) (dec dx)
         (< dx 0) (inc dx)
         :else 0)
   (dec dy)])

(defn debug [x] (println x) x)

(defn task-2 [input]
  (let [target-area (parse input)]
    (letfn [(in-target
              ([[x y _dx _dy]] (in-target x y))
              ([x y]
               (and (>= (:x2 target-area) x (:x1 target-area))
                    (>= (:y2 target-area) y (:y1 target-area)))))
            (in-target-after-n-steps [x-vel y-vel steps]
              (some in-target (take (+ 4 steps) (iterate next-step [0 0 x-vel y-vel]))))]
      (count (for [y-vel (range (min-y-velocity target-area) (inc (max-y-velocity target-area)))
                   x-vel (range (min-x-velocity target-area) (inc (max-x-velocity target-area)))
                   :let [steps (count-steps target-area y-vel)]
                   :when (and (not (nil? steps))
                              (in-target-after-n-steps x-vel y-vel steps))]
               [x-vel y-vel])))))

(def expected-results-testdata
  (->> (re-seq #"(-?[0-9]+),(-?[0-9+])" "23,-10  25,-9   27,-5   29,-6   22,-6   21,-7   9,0     27,-7   24,-5
25,-7   26,-6   25,-5   6,8     11,-2   20,-5   29,-10  6,3     28,-7
8,0     30,-6   29,-8   20,-10  6,7     6,4     6,1     14,-4   21,-6
26,-10  7,-1    7,7     8,-1    21,-9   6,2     20,-7   30,-10  14,-3
20,-8   13,-2   7,3     28,-8   29,-9   15,-3   22,-5   26,-8   25,-8
25,-6   15,-4   9,-2    15,-2   12,-2   28,-9   12,-3   24,-6   23,-7
25,-10  7,8     11,-3   26,-7   7,1     23,-9   6,0     22,-10  27,-6
8,1     22,-8   13,-4   7,6     28,-6   11,-4   12,-4   26,-9   7,4
24,-10  23,-8   30,-8   7,0     9,-1    10,-1   26,-5   22,-9   6,5
7,5     23,-6   28,-10  10,-2   11,-1   20,-9   14,-2   29,-7   13,-3
23,-5   24,-8   27,-9   30,-7   28,-5   21,-10  7,9     6,6     21,-5
27,-10  7,2     30,-9   21,-8   22,-7   24,-9   20,-6   6,9     29,-5
8,-2    27,-8   30,-5   24,-7")
       (map (comp parse-long-all rest))))

(let [target-area (parse testdata)]
  (map (comp (partial count-steps target-area) second) expected-results-testdata))

(count-steps (parse testdata) -5)

(some #(> (first %) 20) (take 8 (iterate next-step [0 0 7 2])))

(not (nil? (count-steps (parse testdata) 4)))

(take 2 (iterate next-step [0 0 23 -10]))

(task-1 data)
(task-2 data)


