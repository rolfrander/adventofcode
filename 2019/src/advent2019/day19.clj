(ns advent2019.day19
  (:require
   [clojure.string :as str]
   [advent2019.intcode :as ic]
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.core.async :refer [>!! <!! chan thread pipe close!]]
   [flatland.useful.seq :refer [prefix-of?]])
  (:import
   [java.io File]
   [java.awt Color]
   [java.awt.image BufferedImage]
   [javax.imageio ImageIO]))

(def program (ic/parse (puzzle/get-data 2019 19)))

(defn run [x y]
  (ic/interpret-with-input program [x y]))

(doseq [y (range 50)
        x (range 50)]
  (when (= x 0) (println))
  (print ([\. \#] (first (run x y)))))

(defn solve-1 []
  (let [run (fn [x y] (ic/interpret-with-input program [x y]))]
    (->> (for [x (range 50)
               y (range 50)
               :when (= 1 (first (run x y)))]
           true)
         count)))

(defn print-png [data w h filename]
  (let [bi (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics bi)
        color (fn [c] (case c
                        :white Color/WHITE
                        :black Color/BLACK
                        :grey  Color/GRAY
                        ))]
    (.setColor g Color/BLACK)
    (.fillRect g 0 0 w h)
    (doseq [[x y c] data]
      (.setColor g (color c))
      (.fillRect g x y 1 1))
    (ImageIO/write bi "png" (File. (format "%s.png" filename)))))

(def subset-4-50 (for [y (range 5 45)
                       x (range 3 60)]
                   [x y (first (run x y))]))

(def subset-sq-100
  (let [[_prev slopevals-left slopevals-right]
        (->> subset-4-50
             (reduce (fn [[prev slope-1 slope-2] [x y val]]
                       (if (= val prev)
                         [val slope-1 slope-2]
                         (if (= val 1)
                           [val (conj slope-1 (float (/ y x))) slope-2]
                           [val slope-1 (conj slope-2 (float (/ y (dec x))))])))
                     [0 [] []]))
        slope-left (apply max slopevals-left)
        slope-right (apply min slopevals-right)

        delta-y-above-box (* 100 slope-right)

        box-x (int (/ (+ 100 delta-y-above-box) (- slope-left slope-right))) ; = 1508
        box-y (int (+ (* box-x slope-right) delta-y-above-box))

        margin 30
        collect-data (fn [coll x1 x2 slope]
                       (->> (for [x (range x1 x2)
                                  y (range (int (- (* x slope) margin))
                                           (int (+ (* x slope) margin)))]
                              [x y (if (= (first (run x y)) 1) :white :grey)])
                            (into coll)))

        data (-> []
                 (collect-data (- box-x margin) (+ box-x margin 100) slope-left)
                 (collect-data (- box-x margin) (+ box-x margin 100) slope-right))
        ;data (map (fn [[x y c]]
        ;            [(- x (- box-x margin))
        ;             (- y (- box-y margin))
        ;             c]) data)
        ]
    data
    ;[box-x box-y]
    ))

(take 10 subset-sq-100)

(def dataset (reduce (fn [res [x y c]]
                       (assoc res [x y] c))
                     {} subset-sq-100))

(dataset [1560 1450])

(for [x (range 1500 1700)
      y (range 1300 1500)
      :when (and (= (dataset [(+ x 99) y]) :white)
                 (= (dataset [(+ x 100) y]) :grey)
                 (= (dataset [x (+ y 99)]) :white)
                 (= (dataset [x (+ y 100)]) :grey))]
  [x y (+ (* x x) (* y y))])
