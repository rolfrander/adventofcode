(ns advent2017.day15
  (:require [rolfrander.puzzle-lib :refer [get-data]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^long generator-a 16807)
(def ^long generator-b 48271)
(def ^long modulus 2147483647)

(def ^long start-a 679)
(def ^long start-b 771)

(defn next-fn [^long generator]
  (fn [^long prev] (mod (unchecked-multiply generator prev) modulus)))

(defn match [^long a ^long b]
  (= (bit-and a 0xffff)
     (bit-and b 0xffff)))

(defn task-1 [^long start-a ^long start-b cnt]
  (->> (map match
            (take cnt (iterate (next-fn generator-a) start-a))
            (take cnt (iterate (next-fn generator-b) start-b)))
       (filter true?)
       count))

(defn mod-4 [^long i]
  (= (bit-and i 3) 0))

(defn mod-8 [^long i]
  (= (bit-and i 7) 0))

(defn task-2 [^long start-a ^long start-b cnt]
  (->> (map match
            (filter mod-4 (iterate (next-fn generator-a) start-a))
            (filter mod-8 (iterate (next-fn generator-b) start-b)))
       (take cnt)
       (filter true?)
       count))

(task-1 1092455 430625591 5)
(task-1 start-a start-b 40000000)
;; => 626

(task-2 1092455 430625591 5000000)
(task-2 start-a start-b 5000000) ; 306

