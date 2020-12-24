(ns advent2015.day09
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)

(def testdata "London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141")

(defn parse [input]
  (reduce (fn [distances [_line from to distance-str]]
            (let [distance (Integer/parseInt distance-str)]
              (-> distances
                  (assoc-in [from to] distance)
                  (assoc-in [to from] distance))))
          {}
          (re-seq #"([A-Za-z]+) to ([A-Za-z]+) = ([0-9]+)" input)))

(defn permute [destinations]
  ;; functional version of 
  ;; https://en.wikipedia.org/wiki/Heap%27s_algorithm
  (letfn [(generate [available chosen]
                    (if (empty? available)
                      (list chosen)
                      (loop [[c & k] available
                             used '()
                             results '()]
                        (if (not c)
                          results 
                          (let [r (generate (concat used k)
                                            (conj chosen c))]
                            (recur k (conj used c) (concat results r)))))))]
    (generate destinations '())))

;(permute '(1 2 3 4))

(binding [*debug* false]
  (let [data (parse (slurp "resources/2015/day09.txt"))
        routes (permute (keys data))
        calc-distance (fn [route]
                        (reduce (fn [[acc prevdest] dest]
                                  (if prevdest
                                    [(+ acc (get-in data [prevdest dest])) dest]
                                    [acc dest]))
                                [0 nil]
                                route))]
  (->> routes
       (map #(vector % (calc-distance %)))
       (apply max-key #(first (second %))))
    ))

