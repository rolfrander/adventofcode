(ns advent2016.day10
  (:require [advent2016.core :as core]
            [clojure.string :as string]))

(def testdata "value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2")

(def data (core/get-data 2016 10))

(defn parse-line [line]
  (if-let [value (re-matches #"value ([0-9]+) goes to (.*)" line)]
    [:value (core/safe-parse-number (nth value 1)) (nth value 2)]
    (let [give (re-matches #"(bot [0-9]+) gives low to ([a-z]+ [0-9]+) and high to ([a-z]+ [0-9]+)" line)]
      (conj (rest give) :give))))

(defn parse [input]
  (map parse-line (string/split-lines input)))

(defn compile-give [lo-target hi-target]
  (let [state (atom nil)]
    (fn [v]
      (if (compare-and-set! state nil v)
        nil
        (let [u @state]
          (println "send" (min u v) "to" lo-target "and" (max u v) "to" hi-target)
          (reset! state nil)
          [[lo-target (min u v)]
           [hi-target (max u v)]])))))

(defn evaluate [programs find-lo find-hi]
  (let [robots (reduce (fn [r [_give name lo-target hi-target]]
                         (assoc r name (compile-give lo-target hi-target)))
                       {}
                       (filter #(= (first %) :give) programs))
        events (reduce (fn [r [_value v target]]
                         (conj r [target v]))
                       (clojure.lang.PersistentQueue/EMPTY)
                       (filter #(= (first %) :value) programs))]
    (loop [events events
           outputs {}]
      (if (empty? events)
        outputs
        (let [[target value] (peek events)
              r (robots target)]
          (if (nil? r)
            (recur (pop events) (assoc outputs target value))
            (let [[lo hi] (r value)]
              (if (and (= find-lo (second lo))
                       (= find-hi (second hi)))
                target
                (recur (conj (pop events) lo hi) outputs)))))))))

(defn task-1 [data]
  (evaluate (parse data) 17 61))

(defn task-2 [data]
  (let [outputs (evaluate (parse data) -1 -1)]
       (reduce * (map outputs ["output 0" "output 1" "output 2"]))))

(task-2 data)

(take 5 (parse data))
