(ns advent2020.day13
  (:require [clojure.string :as str]))

(def testdata-time 939)
(def testdata "7,13,x,x,59,x,31,19")

(def data-time 1006605)
(def data "19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,883,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,x,x,797,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29")

(defn parse [input]
  (let [m (re-matcher #"[0-9]+" input)]
    (map #(Integer/parseInt %)
         (take-while #(not (nil? %))
                     (repeatedly (partial re-find m))))))

(defn wait-prod [d dt]
  (->> d
       parse
       (map (fn [id]
              (let [m (mod dt id)]
                (if (= m 0) 
                  [id 0]
                  [id (- id m)]))))
       (reduce (fn [[id wait] [next-id next-wait]]
                 (if (< next-wait wait)
                   [next-id next-wait]
                   [id wait]))
               [-1 9999])
       (apply *)))

(wait-prod data data-time)

(defn parse-2 [input]
  (->> (str/split input #",")
       (map-indexed #(if (= %2 "x") nil [%1 (Integer/parseInt %2)]))
       (remove nil?)))

(defn gcd [a b]
  (if (= b 0) a
      (recur b (mod a b))))

(defn all-gcd [ints]
  (for [a ints 
        b ints
        :when (not (= a b))]
    (gcd a b)))

(defn normalize [data]
  (map (fn [[a b]] [(mod a b) b])
       data))

(->> (parse-2 data)
     (map second)
     all-gcd
     (remove #(= 1 %)))

(defn chinese-reminder [input]
  (let [input (->> input normalize (sort-by second) reverse)
        upper (apply * (map second input))]
    (loop [[a1 n1] (first input)
           [[a2 n2] & rest] (rest input)
           start a1
           step n1]
      (println start step)
      (let [result (some #(if (= (mod % n2) a2) % false) (range start upper step))]
        (if rest (recur [a2 n2] rest result (* n2 step))
            result)))))

(chinese-reminder [[0 3][3 4][4 5]])

(defn switch [[a n]]
  [(- n a) n])

(defn find-time [input]
  (chinese-reminder (map switch (parse-2 input))))

(find-time "17,x,13,19")
(find-time "67,7,59,61")
(find-time "67,x,7,59,61")
(find-time "67,7,x,59,61")
(find-time "1789,37,47,1889")
(time (find-time data))
;; => 1118684865113056

;; => 2990035291265711

(->> data
     parse-2
     (map #(second))
     (apply *))

;; => ([0 7] [1 13] [4 59] [6 31] [7 19])

