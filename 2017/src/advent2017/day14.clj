(ns advent2017.day14
  (:require [rolfrander.puzzle-lib :refer [get-data neighbours-fn]]
            [clojure.core.matrix :as mat]
            [advent2017.day10 :refer [knot-hash]]
            [clojure.string :as str]))

(def testdata "flqrgnkx")
(def data "vbqugkhl")

(def bits (take-while (partial not= 0) (iterate #(bit-shift-right % 1) 128)))

(defn count-bits [^long i]
  (let [i (- i (bit-and (bit-shift-right i 1)  0x55555555))
        i (+   (bit-and                  i     0x33333333)
               (bit-and (bit-shift-right i 2)  0x33333333))
        i (bit-and (+ i (bit-shift-right i 4)) 0x0F0F0F0F)
        i (bit-shift-right                (* i 0x01010101) 24)]
    i))

(defn map-row [hash]
  (let [a (int-array 128 0)]
    (->> (for [h hash
               b (map (partial bit-and h) bits)]
           (if (> b 0) 0 1)) 
         (map-indexed #(aset-int a %1 %2))
         doall)
    a))

(defn create-disk-bitmap [input]
  (->> (map (partial str input "-") (range 128))
       (map knot-hash)))

(defn task-1 [input]
  (->> (create-disk-bitmap input)
       flatten
       (map count-bits)
       (reduce +)))

(defn task-2 [input]
  (let [disk (create-disk-bitmap input)
        neighbours (neighbours-fn :sq-4 :ignore :max-dim [127 127])
        img (mapv map-row disk)] 
    (letfn [(paint [[row col :as pos] buffer]
              (when (zero? (aget (buffer row) col))
                (aset-int (buffer row) col 1)
                (doseq [npos (neighbours pos)]
                  (paint npos buffer))))]
      (loop [row 0
             col 0
             cnt 0]
        (cond (> col 127)
              (recur (inc row) 0 cnt)

              (> row 127)
              cnt

              (zero? (aget (img row) col))
              (do (paint [row col] img)
                  (recur row (inc col) (inc cnt)))

              :else
              (recur row (inc col) cnt))))))

(task-1 testdata)
(task-1 data) ; 8148

(task-2 testdata) ; 1242
(task-2 data)
;; => 1180
