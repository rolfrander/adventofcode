(ns advent2015.day14
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]
            [clojure.data.json :as json]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)

(def testdata "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")

(defn parse [input]
  (->> (re-seq #"([^ ]+) can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds.\n?" input)
       (map (fn [[_line name speed flytime resttime]]
              {:name name, 
               :speed (Long/parseLong speed), 
               :flytime (Long/parseLong flytime), 
               :resttime (Long/parseLong resttime)}))))

(defn add-cycletime [reindeer]
  (assoc reindeer :cycletime (+ (:flytime reindeer)
                                (:resttime reindeer))))

(defn status-after-time [reindeer seconds]
  (let [dist-per-cycle (* (:speed reindeer) (:flytime reindeer))
        whole-cycles (quot seconds (:cycletime reindeer))
        part-cycle (min (mod seconds (:cycletime reindeer)) (:flytime reindeer))]
    (+ (* dist-per-cycle whole-cycles)
       (* (:speed reindeer) part-cycle))))

(parse testdata)
;;({:name "Comet", :speed 14, :flytime 10, :resttime 127} 
;; {:name "Dancer", :speed 16, :flytime 11, :resttime 162})

(defn task-1 [data seconds]
  (->> data
       parse
       (map add-cycletime)
       (map #(vector (:name %) (status-after-time % seconds)))
       (apply max-key second)
       ))

(defn task-2 [data seconds]
  (let [winner-by-second (->> data
                              parse
                              (map add-cycletime)
                              (map (fn [reindeer] (map #(status-after-time reindeer %) (range 1 (inc seconds)))))
                              (apply map (fn [& distances]
                                           (let [winner (apply max distances)]
                                             (map #(if (= % winner) 1 0) distances))))
                              )]
    (loop [points (first winner-by-second)
           data (rest winner-by-second)]
      (if (empty? data)
        points
        (recur (mapv + points (first data))
               (rest data))))))

(reduce (partial map +) (list [0 1] [0 1] [0 1] [1 0]))

(task-1 (slurp "resources/2015/day14.txt") 2503)
;; => ["Cupid" 2696]

(task-2 (slurp "resources/2015/day14.txt") 2503)
;; => [1084 838 24 277 0 121 0 13 199]

