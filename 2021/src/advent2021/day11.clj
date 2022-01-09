(ns advent2021.day11
  (:require [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math*  :warn-on-boxed)


(def testdata "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(def data (advent2021.core/load-data 11 session))

(defn parse [input]
  (let [width (count (re-find #"^[0-9]+" input))]
    {:flashes 0
     :width width
     :data (into-array Byte/TYPE
                       (for [c (re-seq #"[0-9]" input)]
                         (- (int (first c)) (int \0))))}))

(defn print-array [data]
  (print "; ")
  (dotimes [i (count (:data data))]
    (let [v (aget (:data data) i)]
      (print (if (> v 9) 
               (char (+ (- v 10) (int \A)))
               (char (+ v (int \0)))
               )))
    (when (= (mod i (:width data)) (dec (:width data))) 
      (newline)
      (print "; ")))
  (newline)
  (flush)
  data)

(defn increase-energy [data]
  (dotimes [i (count (:data data))]
    (let [prev-val (aget ^bytes (:data data) i)]
      (aset-byte (:data data) i (inc prev-val))))
  data)

(defn blink [data]
  (let [find-tens (fn [^bytes byte-array]
                     (for [i (range (count byte-array))
                           :when (= (aget byte-array i) 10)]
                       i))
        ^long w (:width data)
        #^bytes d (:data data)
        l (count d)
        neighbours [(- 0 w 1) (- 0 w) (- 1 w)
                    (- 1)             1
                    (- w 1)   w       (+ w 1)]
        neigh-left [(- 0 w) (- 1 w)
                            1
                    w       (+ w 1)]
        neigh-right [(- 0 w 1) (- 0 w)
                     (- 1)
                     (- w 1)   w      ]
        
        ]
    (loop [^long f (:flashes data)
           [b & blinking] (doall (find-tens d))
           ]
      (if (nil? b)
        (assoc data :flashes f)
        (recur
         (inc f)
         (reduce (fn [new-blink ^long i]
                   (if (and (>= i 0) (< i l))
                     (let [val (inc (aget ^bytes d i))]
                       (aset-byte d i val)
                       (if (= val 10)
                         (conj new-blink i)
                         new-blink))
                     new-blink))
                 blinking
                 (map #(+ b %) (condp = (mod b w)
                                 0 neigh-left
                                 (dec w) neigh-right
                                 neighbours)))))
      )))

(defn set-to-zero [data]
  (let [d ^bytes (:data data)]
    (dotimes [i (count d)]
      (when (> (aget d i) 9)
        (aset-byte d i 0))))
  data)

(defn step [data]
  (->> data
       increase-energy
       blink
       set-to-zero))

(defn repeat-steps [steps data]
  (loop [i steps
         d data]
    (if (= i 0)
      d
      (recur (dec i) (step d)))))

(print-array (step (step (parse "11111
19991
19191
19991
11111"))))

(def data
  (->> (http/get "https://adventofcode.com/2021/day/11/input"
                 {:cookies {"session" {:value session}}})
       :body))

(->> data
     parse
     (repeat-steps 100)
     print-array
     :flashes)

(loop [d (parse data)
       i 0]
  (if (= (:flashes d) 100)
    i
    (recur (step (assoc d :flashes 0))
           (inc i))))