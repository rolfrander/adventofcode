(ns advent2016.day16
  (:require [rolfrander.puzzle-lib :as puzzle]
            [clojure.string :as str]))


(defn parse-line [line]
  (->> (seq line)
       (map int)
       (map #(- % (int \0)))))


(defn parse [input]
  (parse-line (first (str/split-lines input))))

(defn step [input]
  (->> input
       reverse
       (map (partial - 1))
       (concat input [0])))

(defn calc-chk [s]
 (let [chk-2 {'(1 0) 0
              '(0 1) 0
              '(0 0) 1
              '(1 1) 1}]
   (->> (partition 2 s)
        (map chk-2))))

(defn chksum 
  ([input] (chksum input (count input)))
  ([input cnt]
   (let []
     (if (even? cnt)
       (calc-chk (chksum input (/ cnt 2)))
       input))))


(defn task-1 [input space]
  (loop [fill (parse input)]
    (if (>= (count fill) space)
      (str/join (chksum (take space fill) space))
      (recur (step fill)))))

(defn chksum-xf [rf]
  (let [previous (java.util.ArrayList. 1)]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (if (= 1 (.size previous))
         (let [chksum (if (= input (.get previous 0)) 1 0)]
           (.clear previous)
           (rf result chksum))
         (do
           (.add previous input)
           result))))))

(defn combine-xf [xf i]
  (if (= 1 i)
    xf
    (comp xf (combine-xf xf (dec i)))))

(defn rev-invert [input]
  (map (partial - 1) (reverse input)))

(defn padding-step [prev]
  (let [cont (cons 0 (rev-invert prev))]
    (lazy-cat cont (padding-step (concat prev cont)))))

(def padding
  (lazy-cat [0] (padding-step [0])))

(defn iteration-count [space]
  (loop [s space
         i 0]
    (if (odd? s) i (recur (/ s 2) (inc i)))))

(defn pad-sequence [input]
  ((fn step [a b pad]
     (lazy-cat a (cons (first pad) (step b a (rest pad)))))
   input (rev-invert input) padding))

(defn task-2 [input space]
  (let [xf (combine-xf chksum-xf (iteration-count space))]
    (->> (parse input) 
         (pad-sequence)
         (take space)
         (sequence xf)
         (map str)
         str/join)))

(def task-1-space 272)
(def task-2-space 35651584)
(def data "10001110011110000")

(task-1 data task-1-space)
(task-2 data task-2-space)

(iteration-count task-2-space)

