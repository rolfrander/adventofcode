(ns advent2015.day13
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)

(defn parse [file]
  (with-open [in (clojure.java.io/reader file)]
    (->> (line-seq in)
         (map (fn [line]
                (let [[_line from g-or-l value-str to]
                      (re-matches #"([^ ]+) would (gain|lose) ([0-9]+) happiness units by sitting next to ([^.]+)." line)
                      value (Long/parseLong value-str)
                      value (if (= g-or-l "gain")
                              value
                              (- value))]
                  [from to value])))
         (reduce (fn [res [from to value]]
                   (assoc-in res [from to] value))
                 {})
         doall)))

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

(defn add-me [data]
  (reduce (fn [res name]
            (-> res
                (assoc-in ["me" name] 0)
                (assoc-in [name "me"] 0)))
          data
          (keys data)))

(let [data (parse "resources/2015/day13.txt")
      net-happiness (fn [a b] (+ (get-in data [a b])
                                 (get-in data [b a])))
      calculate-happiness (fn [seating]
                            (let [[result last]
                                  (reduce (fn [[result prev] cur]
                                            [(+ result (net-happiness prev cur)) 
                                             cur])
                                          [0 (first seating)]
                                          (rest seating))]
                              ;[seating (+ result (net-happiness (first seating) last))]
                              ; placing myself at the end:
                              [seating result]
                              ))]
  (->> data
       keys
       permute
       (map calculate-happiness)
       (apply max-key second)
       ))
;; => task 2: [("George" "David" "Eric" "Carol" "Frank" "Bob" "Alice" "Mallory") 725]


;; => task 1: [("George" "David" "Eric" "Carol" "Frank" "Bob" "Alice" "Mallory") 733]




;; => {"Alice" {"Bob" 54, "Carol" -79, "David" -2},
;;     "Bob" {"Alice" 83, "Carol" -7, "David" -63},
;;     "Carol" {"Alice" -62, "Bob" 60, "David" 55},
;;     "David" {"Alice" 46, "Bob" -7, "Carol" 41}}



