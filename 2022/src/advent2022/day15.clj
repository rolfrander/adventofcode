(ns advent2022.day15
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(def data (puzzle/get-data 2022 15))

(defn parse [input]
  (->> (re-seq #"-?[0-9]+" input)
       (map puzzle/str->long)
       (partition 2)
       (partition 2)
       (map #(hash-map :sensor (first %) :beacon (second %)))))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (puzzle/abs (- x2 x1))
     (puzzle/abs (- y2 y1))))

(defn points-on-row [[x y] row dist]
  (let [d (- dist (puzzle/abs (- row y)))]
    (if (< d 0)
      '()
      (range (- x d) (+ x d 1)))))

(defn range-on-row [[x y] row dist]
  (let [d (- dist (puzzle/abs (- row y)))]
    (if (< d 0)
      nil
      [(- x d) (+ x d 1)])))

(defn merge-rages [r]
  (let [r (sort-by first r)]
    (reduce (fn [s [r1 r2 :as next]]
              (let [[t1 t2] (first s)]
                (if (<= r1 t2)
                  (cons [t1 (max r2 t2)] (rest s))
                  (cons next s))))
            (cons (first r) '())
            (rest r))))

(defn scan-radius [m]
  (manhattan-distance (:sensor m)
                      (:beacon m)))

(defn get-sensors [input]
  (->> (parse input)
       (map #(assoc % :scan-radius (scan-radius %)))
       (map-indexed #(assoc %2 :id %1))))

(defn scale-sensors [s factor]
  (map (fn [sensor]
         (-> sensor
             (update :sensor #(vector (quot (first %) factor)
                                      (quot (second %) factor)))
             (dissoc :beacon)
             (update :scan-radius #(dec (quot % factor)))))
       s)
  )

(defn task-1 [input row]
  (let [data (get-sensors input)
        not-beacon (->> data
                        (map #(points-on-row (:sensor %)
                                             row
                                             (:scan-radius %)))
                        (remove empty?)
                        (reduce into (sorted-set)))
        not-beacon (->> (map :beacon data)
                        (keep #(when (= (second %) row)
                                 (first %)))
                        (reduce disj not-beacon))]
    (count not-beacon)))

(defn find-taken-ranges [data row]
  (->> data
       (map #(range-on-row (:sensor %)
                           row
                           (:scan-radius %)))
       (remove nil?)
       (merge-rages)))

(defn task-1b [input row]
  (let [data (get-sensors input)
        not-beacon-ranges (find-taken-ranges data row)
        total-range-size (reduce + (map (fn [[f t]] (- t f))
                                        not-beacon-ranges))
        beacons-on-row (->> (map :beacon data)
                            (keep #(when (= (second %) row)
                                     (first %)))
                            (into #{}))
        not-beacon-cnt (- total-range-size
                          (count (for [[f t] not-beacon-ranges
                                       b beacons-on-row
                                       :when (and (<= f b)
                                                  (< b t))]
                                   true)))]
    not-beacon-cnt
    ))

(defn find-gap-in-ranges [r]
  (let [[[a1 a2] [b1 b2]] (sort-by first r)]
    a2))

(defn find-gaps [ranges]
  (let [r (sort-by first ranges)]
    (map (fn [[[_a1 a2] [b1 _b2]]] 
           [a2 b1])
         (partition 2 1 r))))

(defn tuning-freq [maxdim [x y]]
  (+ (* x maxdim) y))

(defn scan [data maxdim]
  (->> (range 0 (inc maxdim))
       (map-indexed #(vector %1 (find-taken-ranges data %2)))
       (keep (fn [[idx [[h1 h2] & range-t :as ranges]]]
               (when-not (and (empty? range-t)
                              (<= h1 0)
                              (>= h2 maxdim))
                 [(find-gaps ranges) idx])))))

(defn approx-areas [sensors scale maxdim]
  (let [reset-scale (fn [scale [x-ranges y]]
                      (let [y-min (* (dec y) scale)
                            y-max (* (inc y) scale)]
                        (map (fn [[x1 x2]] [(* (dec x1) scale) y-min
                                            (* (inc x2) scale) y-max])
                             x-ranges)))]
    (mapcat #(reset-scale scale %) (-> sensors
                                    (scale-sensors scale)
                                    (scan (/ maxdim scale))))))

(defn scan-range [sensors [x1 y1 x2 y2]]
  (->> (range y1 (inc y2))
       (map-indexed #(vector %1 (find-taken-ranges sensors %2)))
       (keep (fn [[idx [[h1 h2] & range-t :as ranges]]]
               (when-not (and (empty? range-t)
                              (<= h1 x1)
                              (>= h2 x2))
                 [(find-gap-in-ranges ranges) (+ idx y1)])))))

;(scan-range (get-sensors testdata) [13 10 15 12])

(defn task-2b [input scaling maxdim]
  (let [s (get-sensors input)
        a (approx-areas s scaling maxdim)]
    (->> (mapcat #(scan-range s %) a)
         (remove #(or (< (first %) 0)
                      (< (second %) 0)
                      (> (first %) maxdim)
                      (> (second %) maxdim)))
         (into #{})
         (map (partial tuning-freq 4000000)))))

(defn task-2 [input maxdim]
  (let [data (get-sensors input)]
    (->> (range 0 (inc maxdim))
         (map-indexed #(vector %1 (find-taken-ranges data %2)))
         (keep (fn [[idx [[h1 h2] & range-t :as ranges]]]
                 (when-not (and (empty? range-t)
                                (<= h1 0)
                                (>= h2 maxdim))
                   [(find-gap-in-ranges ranges) idx])))
         first
         (tuning-freq 4000000)))
  
  )



(task-1b testdata 10)
(task-1b data 2000000)

(task-2b testdata 2 20)

(task-2b data 1000 4000000)

