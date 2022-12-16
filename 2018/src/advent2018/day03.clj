(ns advent2018.day03
  (:require [rolfrander.puzzle-lib :refer [get-data str->long]]
            [clojure.string :as str]))

(defn parse-line
  [line]
  (let [[_all x y w h] (map str->long (re-seq #"\d+" line))]
    {:x1 x :x2 (+ x w) :y1 y :y2 (+ y w)
     ;:width w :height h
     
     }))

(defn parse [input]
  (map parse-line (str/split-lines input)))

(def testdata "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2")

(def testdata2 "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 4,4: 2x2
#4 @ 1,1: 8x8")


;(parse testdata)

(defn overlap-dimension? [a1 a2 b1 b2]
  (or (and (<= a1 b1) (< b1 a2))
      (and (<= b1 a1) (< a1 b2))))

(defn overlap? [cl1 cl2]
  (and (overlap-dimension? (:x1 cl1) (:x2 cl1) (:x1 cl2) (:x2 cl2))
       (overlap-dimension? (:y1 cl1) (:y2 cl1) (:y1 cl2) (:y2 cl2)))
  )

(defn overlap-area [cl1 cl2]
  (let [x1 (max (:x1 cl1) (:x1 cl2))
        x2 (min (:x2 cl1) (:x2 cl2))
        y1 (max (:y1 cl1) (:y1 cl2))
        y2 (min (:y2 cl1) (:y2 cl2))]
    (when (and (< x1 x2) (< y1 y2))
      {:x1 x1 :x2 x2 :y1 y1 :y2 y2})))

(defn area [c]
  (* (- (:x1 c) (:x2 c))
     (- (:y1 c) (:y2 c))))

(defn area-list [[head & others]]  
  (if (nil? head)
    0
    (+ (area head)
       (area-list others)
       (- (area-list (keep (partial overlap-area head) others))))))

(defn map-pairs [f coll]
  (for [a-list (take-while (complement empty?) (iterate rest coll))
        :let [a (first a-list)]
        b (rest a-list)]
    (f a b)))

;(map-pairs vector (range 1 10))

(defn task-1 [input]
  (->> (parse input)
       (map-pairs overlap-area)
       (remove nil?)
       area-list))


;; (only-overlaps (parse testdata))

(task-1 (get-data 2018 3))
(clojure.pprint/pprint (map-pairs overlap-area (parse testdata2)))