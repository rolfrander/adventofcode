(ns advent2022.day14
    (:require [clojure.set :as set]
              [clojure.string :as str]
              [rolfrander.puzzle-lib :as puzzle]))

(def ^:dynamic *debug* false)

(def testdata "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(def data (puzzle/get-data 2022 14))

(defn parse-line [line]
  (->> (re-seq #"[0-9]+" line)
       (map puzzle/str->long)
       (partition 2)))

(defn parse [input]
  (->> (str/split-lines input)
       (map parse-line)))

(update {512 (sorted-set 43)} 512 #(if (nil? %) 
                  (sorted-set 42)
                  (conj % 42)))

(defn add-point [cave col depth]
  (update cave col #(if (nil? %)
                      (sorted-set depth)
                      (conj % depth))))

(defn draw-line [line-map line-spec]
  (loop [[point & line-spec] line-spec
         lines line-map]
    (if (empty? line-spec)
      (apply add-point lines point)
      (recur line-spec
             (let [[x1 y1] point
                   [x2 y2] (first line-spec)]
               (if (= x1 x2)
                 (reduce #(add-point %1 x1 %2) lines (range y1 y2 (puzzle/sign (- y2 y1))))
                 (reduce #(add-point %1 %2 y1) lines (range x1 x2 (puzzle/sign (- x2 x1))))))))))

(defn create-map [input]
  (->> (parse input)
       (reduce draw-line {})))

(defn draw-map [rock sand]
  (let [x-r (sort (keys rock))
        x-s (sort (keys sand))
        min-x (min (first x-r) (first x-s))
        max-x (max (last x-r) (last x-s))
        min-y (reduce min (map first (vals rock)))
        max-y (reduce max (map last (vals rock)))]
    (dotimes [row (inc max-y)]
      (printf "%2d " row)
      (doseq [col (range min-x (inc max-x))]
        (cond (contains? (rock col) row)
              (print "#")
              
              (contains? (sand col) row)
              (print "o")
              
              :else
              (print ".")))
      (println))))

(defn drop-sand [cave drop-x drop-y floor]
  (let [find-below (fn [x y] (some->> (cave x)
                                      (filter (partial < y))
                                      first
                                      dec))
        depth (find-below drop-x drop-y)
        left (dec drop-x)
        right (inc drop-x)]
    (cond (nil? depth)
          (if (nil? floor)
            nil
            (add-point cave drop-x (dec floor)))

          (not (contains? (cave left) (inc depth)))
          (drop-sand cave left (inc depth) floor)

          (not (contains? (cave right) (inc depth)))
          (drop-sand cave right (inc depth) floor)

          :else
          (add-point cave drop-x depth))))

(defn simulate [cave floor]
  (loop [sand cave
         prev-sand nil
         count 0]
    (cond  (nil? sand) 
           (do
             (when *debug* (draw-map cave prev-sand))
             (dec count))
           
           (contains? (sand 500) 0)
           (do
             (when *debug* (draw-map cave sand))
             count)
           
           :else
           (recur (drop-sand sand 500 -1 floor)
                  sand
                  (inc count)))))

(defn task-1 [input]
  (let [cave (create-map input)]
    (simulate cave nil)))

(defn task-2 [input]
  (let [cave (create-map input)
        max-y (reduce max (map last (vals cave)))]
    (simulate cave (+ max-y 2))))

(task-1 testdata)
(task-1 (puzzle/get-data 2022 14))

(binding [*debug* true]
  (task-2 testdata))

(task-2 (puzzle/get-data 2022 14))

