(ns advent2015.day24 
  (:require [advent2015.core :as core]
            [clojure.string :as string]))

(defn parse [input]
  (->> (string/split-lines input)
       (map core/safe-parse-number)))

(def data (parse (core/get-data 2015 24)))

(defn permutations-set [packets max-w]
  (if (= max-w 0)
    [#{}]
    (if (empty? packets)
      nil
      (let [d-set (set packets)
            sum-rest (reduce + d-set)
            head (first d-set)]
        (cond (< sum-rest max-w)
              nil

              (= sum-rest max-w)
              [d-set]

              :else
              (let [with-head (map #(conj % head) (permutations-set (disj d-set head) (- max-w head)))
                    without-head (permutations-set (disj d-set head) max-w)
                    res (concat with-head without-head)]
                res))))))


(defn permutations [d max-w max-c]
  (cond (= max-w 0)
        ['()]

        (or (= max-c 0) (empty? d))
        nil

        :else
        (let [head (first d)
              with-head (map #(conj % head) (permutations (rest d) (- max-w head) (dec max-c)))
              without-head (permutations (rest d) max-w max-c)
              res (concat with-head without-head)]
          res)))

(permutations-set (concat (range 1 6)(range 7 12)) 20)

(defn task [data groups]
  (let [w-per-group (quot (reduce + data) groups)
        qe (partial reduce *)]
    (->> (range 2 10)
         (map (partial permutations data w-per-group))
         ;first
         (remove empty?)
         (first)
         (map (juxt identity qe))
         (sort-by second)
         first
         )))

(task (concat (range 1 6)(range 7 12)) 3)

(task data 4)