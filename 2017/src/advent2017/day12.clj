(ns advent2017.day12
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.string :as str]))

(def testdata "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5")

(defn parse-line [line]
  (let [[program & connects-to] (map safe-parse-number (re-seq #"\d+" line))]
    [program connects-to]))

(defn parse [input]
  (->> (map parse-line (str/split-lines input))
       (into {})))

(defn dfs [data start]
  (letfn [(dfs- [seen start]
            (let [children (remove seen (data start))
                  seen (into seen children)]
              (reduce dfs- seen children)))]
    (dfs- #{} start)))

(defn task-1 [input]
  (count (dfs (parse input) 0)))

(defn task-2 [input]
  (let [data (parse input)]
    (loop [remaining (into #{} (keys data))
           group-cnt 0]
      (if (empty? remaining)
        group-cnt
        (let [in-group (dfs data (first remaining))]
          (recur (remove in-group remaining)
                 (inc group-cnt)))))))

(task-1 testdata) ; 6
(task-1 (get-data 2017 12)) ; 380

(task-2 testdata) ; 2
(task-2 (get-data 2017 12)) ; 181
