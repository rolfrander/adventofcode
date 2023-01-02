(ns advent2022.day06 
  (:require [rolfrander.puzzle-lib :as puzzle]))

(def testdata
  [["mjqjpqmgbljsphdztnvjfqwrcgsmlb" 7 19]
   ["bvwbjplbgvbhsrlpgdmjqwftvncz" 5 23]
   ["nppdvjthqldpwncqszvftbrmjlhg" 6 23]
   ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10 29]
   ["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 11 26]])

(defn different? [chunk]
  (let [[a b c d] chunk]
    (and (not= a b)
         (not= a c)
         (not= a d)
         (not= b c)
         (not= b d)
         (not= c d))))

(defn find-bop [input]
  (loop [i 4
         [chunk & data] (partition 4 1 input)]
    (cond (nil? chunk) nil
          (different? chunk) i
          :else (recur (inc i) data))))

; test
(every? #(= (find-bop (first %)) (second %)) testdata)

;task 1
(find-bop (puzzle/get-data 2022 6))

(defn different-from [input start cnt]
  (let [data (take cnt (drop start input))
        [cand & tail] data]
    (loop [i 1
           [x & tail] tail]
      (cond (nil? x) nil
            (= x cand) i
            :else (recur (inc i) tail)))))

(def ^:dynamic *debug* false)

;(different-from "abcdefghijklmnbpqrstuvwx" 2 14)
(defn find-uniq [data marker-length]
  (let [offsets (map #(different-from data % marker-length) (range (- (count data) (dec marker-length))))]
  ;offsets)
    (loop [i marker-length
           cnt marker-length
           offsets offsets]
      (when *debug* (println i cnt offsets))
      (cond (or (empty? offsets)
                (= cnt 0)) (- (+ cnt i) marker-length)
            (or (nil? (first offsets))
                (<= cnt (first offsets))) (recur (inc i) (dec cnt) (rest offsets))
            :else (recur (inc i)
                         marker-length
                         (rest offsets))))))

; task 1, test
;(every? #(= (find-uniq (first %) 4) (second %)) testdata)

; task 2, test
;(every? #(= (find-uniq (first %) 14) (nth % 2)) testdata)

; task 1
(find-uniq (puzzle/get-data 2022 6) 4)
; task 2
(find-uniq (puzzle/get-data 2022 6) 14)
