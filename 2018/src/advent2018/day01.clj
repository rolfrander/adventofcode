(ns advent2018.day01
  (:require [rolfrander.puzzle-lib :refer [get-data str->long]]
            [clojure.string :as str]))

(def testdata (map str->long (str/split "+3, +3, +4, -2, -4" #", ")))

(def data (->> (get-data 2018 1)
               str/split-lines
               (map str->long)))

(defn task-2 [data]
  (loop [d (rest (cycle data))
         running-sum (first data)
         seen #{}]
    (if (seen running-sum)
      running-sum
      (recur (next d)
             (+ running-sum (first d))
             (conj seen running-sum)))))

(task-2 data)