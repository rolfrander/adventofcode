(ns advent2020.day25
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)
(def ^:dynamic *divisor* 20201227)
(def ^:dynamic *ground* 7)

(defn pow-mod [^long base ^long exp]
  (loop [x base
         result 1
         exp exp]
    (if (= 0 exp)
      result
      (let [x-squared (mod (* x x) *divisor*)
            next-exp (bit-shift-right exp 1)]
        (if (bit-test exp 0)
          (recur x-squared (mod (* x result) *divisor*) next-exp)
          (recur x-squared result next-exp))))))

(defn crack-key [^long input ^long expected]
  (loop [i 0
         v input]
    (if (= v expected)
      i
      (recur (inc i) (mod (* v *ground*) *divisor*)))))

(defn task-1 [door-pub key-pub]
  (let [door-secret (crack-key 1 door-pub)
        key-secret (crack-key 1 key-pub)]
    [door-secret key-secret 
     (pow-mod door-pub key-secret)
     (pow-mod key-pub door-secret)]))

(task-1 17807724 5764801)

(pow-mod 7 21)

(task-1 
 335121
 363891)
;; => [8156519 5062092 9420461 9420461]

