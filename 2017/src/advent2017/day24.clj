(ns advent2017.day24
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.string :as str]))

(defn parse-line [line]
  (map safe-parse-number (str/split line #"/")))

(defn parse [input]
  (->> (map parse-line (str/split-lines input))
       (into #{})))

(def testdata "0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10")

(parse testdata)

(defn debug [t x]
  (printf "%s %s" t)
  x)

(defn dfs [data start]
  (if (empty? data)
    0
    (let [candidates (filter (fn [[a b]] (or (= start a) (= start b)))
                             data)
          find-out-port (fn [[a b]] (if (= start a) b a))
          strength (fn [[a b]] (+ a b))]
      (->> (map #(+ (strength %) (dfs (disj data %) (find-out-port %))) candidates)
           (reduce max 0)))))

(defn dfs-2 [data start]
  (if (empty? data)
    [0 0]
    (let [candidates (filter (fn [[a b]] (or (= start a) (= start b)))
                             data)
          find-out-port (fn [[a b]] (if (= start a) b a))
          strength (fn [[a b]] (+ a b))]
      (->> (map #(let [[sub-strength sub-length] (dfs-2 (disj data %) (find-out-port %))]
                   [(+ (strength %) sub-strength)
                    (inc sub-length)]) candidates)
           (reduce (fn [[max-strength max-length] [s l]]
                     (cond (> l max-length)
                           [s l]
                           
                           (and (= l max-length) (> s max-strength))
                           [s l]
                           
                           :else
                           [max-strength max-length]))
                   [0 0])))))

(defn task-1 [input]
  (dfs (parse input) 0))

(defn task-2 [input]
  (dfs-2 (parse input) 0))


(task-2 (get-data 2017 24))