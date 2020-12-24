(ns advent2015.day10
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)

(def input "1113222113")

(defn round [input]
  (->> input
       (partition-by identity)
       (map #(vector (count %) (first %)))
       flatten
       str/join))

(defn round-x [input]
  (let [[result count last]
        (reduce (fn [[result count last] cur]
                  (if (= last cur)
                    [result (inc count) cur]
                    [(conj (conj result count) last) 1 cur]))
                [[] 1 (first input)]
                (rest input))]
    (str/join (conj (conj result count) last))))

(take 6 (iterate round-x "1"))

(time (count (nth (iterate round-x input) 50)))

(time (count (nth (iterate round input) 50)))
;; => 3579328

;; => 252594
