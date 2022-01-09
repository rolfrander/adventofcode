(ns advent2016.day03  
  (:require [advent2016.core :as core]
            [clojure.string :as string]))

(def data (core/get-data 2016 3))

(def testdata "5 10 25
30 40 50
1 2 40")

(defn parse-line [line]
  (map core/safe-parse-number (re-seq #"[0-9]+" line)))

(defn parse [input]
  (->> (string/split-lines input)
       (map parse-line)))

(defn task-1 [data]
  (letfn [(check-triangle [[a b c]]
            (and (> (+ a b) c)
                 (> (+ b c) a)
                 (> (+ a c) b)))]
    (->> data
         (filter check-triangle)
         count)))

(defn transpose [m]
  (apply map vector m))

(defn task-2 [data]
  (->> (partition 3 data)
       (mapcat transpose)
       task-1))

(task-1 (parse testdata)) ; 1
(task-1 (parse data)) ; 1032

(task-2 (parse testdata)) ; 1
(task-2 (parse data)) ; 1838