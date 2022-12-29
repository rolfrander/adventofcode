(ns advent2022.day03 
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def priorities
  (let [char-range (fn [start end] (map char (range (int start) (inc (int end)))))]
    (zipmap (concat (char-range \a \z)
                    (char-range \A \Z))
            (range 1 53))))

(def testdata "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn find-error [line]
  (let [size (/ (count line) 2)
        items (into #{} (take size line))]
    (first (filter items (drop size line)))))

(defn task-1 [input]
  (->> (str/split-lines input)
       (map find-error)
       (map priorities)
       (reduce +)))

(defn task-2 [input]
  (->> (str/split-lines input)
       (partition 3)
       (map (partial map (partial into #{})))
       (map (partial apply concat))
       (map frequencies)
       (map (fn [freq] (filter #(= 3 (second %)) freq)))
       (map ffirst)
       (map priorities)
       (reduce +)))



(task-1 (puzzle/get-data 2022 03))
(task-2 (puzzle/get-data 2022 03))
