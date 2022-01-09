(ns advent2016.day06
  (:require [advent2016.core :as core]
            [clojure.string :as string]))

(def data (core/get-data 2016 6))

(def testdata "eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar")

(defn decode [data sort-comparator]
  (let [f (repeat (count (first data)) {})
        inc* (fnil inc 0)]
    (->> (reduce (fn [freq line]
                   (map #(update %1 %2 inc*) freq line))
                 f
                 data)
         (map (partial sort-by second sort-comparator))
         (map ffirst)
         (string/join))))

(defn task-1 [data]
  (decode (string/split-lines data) >))

(defn task-2 [data]
  (decode (string/split-lines data) <))

(task-1 data)
(task-2 data)