(ns advent2024.day07
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn parse [in]
  (->> (str/split-lines in)
       (map #(map puzzle/str->long (re-seq #"[0-9]+" %)))))

;(parse testdata)

(defn bitstring [num]
  (loop [t num
         s []]
    (if (= 0 t)
      s
      (recur (bit-shift-right t 1)
             (conj s (bit-and t 1))))))

;(bitstring 11)

(defn || [a b]
  (loop [mult 10
         a (* a 10)]
    (if (> mult b)
      (+ a b)
      (recur (* mult 10)
             (* a 10)))))

(defn rope-eval [numbers oper-map]
  (loop [total (first numbers)
         [n & num] (rest numbers)
         [o & opers] oper-map]
    (if (nil? n)
      total
      (recur (case o
               0 (+ total n)
               1 (* total n)
               2 (|| total n))
             num
             opers))))

;(rope-eval [2 3 4 5] 3)

(defn check-sequence [[total & elements]]
  (let [max-bitstring (bit-shift-left 1 (count elements))]
    (some #(= total (rope-eval elements (concat (bitstring %) (repeat 0)))) 
          (range max-bitstring))))

(defn check-sequence-2 [[total & elements]]
  (let [c (count elements)]
    (letfn [(build-sequence [i oper-map]
                            (if (= i c)
                              (= total (rope-eval elements oper-map))
                              (some #(build-sequence (inc i) (cons % oper-map)) (range 3))))]
      (build-sequence 0 '()))))

(vec (repeat 5 0))

(defn solve [in check-fn]
  (->> (parse in)
       (filter check-fn)
       (map first)
       (reduce +)))

(defn solve-1 [in]
  (solve in check-sequence))

(defn solve-2 [in]
  (solve in check-sequence-2))


(solve-1 testdata)
;;=> 3749
(solve-1 (puzzle/get-data 2024 7))
;;=> 4122618559853

(solve-2 testdata)
;;=> 11387
(solve-2 (puzzle/get-data 2024 7))
;;=> 227615740238334

