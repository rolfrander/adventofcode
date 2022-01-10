(ns advent2016.day15
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.string :as string]))

;Disc #1 has 13 positions; at time=0, it is at position 11.  i1 = 11+t (mod 13)
;Disc #2 has 5 positions; at time=0, it is at position 0.    i2 = 0+t (mod 5)
;Disc #3 has 17 positions; at time=0, it is at position 11.
;Disc #4 has 3 positions; at time=0, it is at position 0.
;Disc #5 has 7 positions; at time=0, it is at position 2.
;Disc #6 has 19 positions; at time=0, it is at position 17.

(defn parse-line [line]
  (->> (re-matches #"Disc #([0-9]) has ([0-9]+) positions; at time=0, it is at position ([0-9]+)." line)
       rest
       (map safe-parse-number)))

(defn parse [data time-sym]
  (->> (string/split-lines data)
       (map parse-line)
       (map (fn [[pos modulus start]] `(mod (+ ~pos ~start ~time-sym) ~modulus)))
       (cons '+)))

(parse (get-data 2016 15) 't)

(loop [t 1]
  (if (= 0 (+
            (clojure.core/mod (clojure.core/+ 1 11 t) 13)
            (clojure.core/mod (clojure.core/+ 2 0 t) 5)
            (clojure.core/mod (clojure.core/+ 3 11 t) 17)
            (clojure.core/mod (clojure.core/+ 4 0 t) 3)
            (clojure.core/mod (clojure.core/+ 5 2 t) 7)
            (clojure.core/mod (clojure.core/+ 6 17 t) 19)
            (mod (+ 7 t) 11)))
    t
    (recur (inc t))))