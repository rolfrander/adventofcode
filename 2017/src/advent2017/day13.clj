(ns advent2017.day13
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.string :as str]))

(def testdata "0: 3
1: 2
4: 4
6: 4")

(defn parse-line [line]
  (mapv safe-parse-number (re-seq #"\d+" line)))

(defn parse [input]
  (map parse-line (str/split-lines input)))

(defn task-1 [input]
  (let [data (parse input)
        caught (fn [pos fw-depth] (= 0 (mod pos (* 2 (dec fw-depth)))))]
    (->> (map #(if (apply caught %) (apply * %) 0) data)
         (reduce +))))

(defn task-2 [input]
  (let [data (parse input)
        caught (fn [delay pos fw-depth] (= 0 (mod (+ delay pos) (* 2 (dec fw-depth)))))]
    (some #(when-not (some (partial apply caught %) data)
             %)
          (range))))

(task-1 testdata) ; 24
(task-1 (get-data 2017 13))

(task-2 testdata)
(task-2 (get-data 2017 13)) ; 3873662
