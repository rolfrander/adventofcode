(ns advent2020.day23
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:dynamic *debug* false)
(def ^:dynamic *max-number* 1000000)


(defn parse [input]
  (map #(- (int %) (int \0)) (seq input)))

(defn parse-and-extend [input]
  (let [buffer (int-array *max-number*)]
    (loop [[i & input] (parse input)
           n 0
           j 0]
      (cond (= j *max-number*)
            [buffer 0]

            (nil? i)
            (do (aset-int buffer j (inc n))
                (recur nil (inc n) (inc j)))

            :else
            (do (aset-int buffer j i)
                (recur input (max n i) (inc j)))))))

(defn parse-to-ll [input]
  (let [input (parse input)
        buffer (int-array (inc *max-number*))
        first-after-input (inc (count input))
        head (first input)
        last (loop [f head
                    [n & r] (concat input
                                    (range first-after-input (inc *max-number*)))]
               (if n
                 (do (aset-int buffer f n)
                     (recur n r))
                 f))]
    ; bite the tail
    (aset-int buffer last head)
    [buffer head]))

;;; **********************************


(defn max-less-than [n cand]
  (loop [[c & cand] cand
         pos 1
         cur-max 0
         cur-max-below 0
         cur-max-pos nil
         cur-max-below-pos nil]
    (when *debug* (println n c cur-max-pos cur-max-below-pos))
    (cond (nil? c)
          (or cur-max-below-pos cur-max-pos)

          (= c (dec n))
          pos

          (and (>= c n) (> c cur-max))
          (recur cand (inc pos) c cur-max-below pos cur-max-below-pos)

          (and (< c n) (> c cur-max-below))
          (recur cand (inc pos) cur-max c cur-max-pos pos)

          :else
          (recur cand (inc pos) cur-max cur-max-below cur-max-pos cur-max-below-pos))))

(defn highest-below [n except]
  (loop [n (dec n)]
    (cond (= n 0) (recur *max-number*)
          (not-any? #(= n %) except) n
          :else (recur (dec n)))))

(defn round [numbers]
  (let [[current & remaining] numbers
        moves (take 3 remaining)
        remaining (nthrest remaining 3)
        ;cutpoint (max-less-than current remaining)
        cutpoint (highest-below current moves)
        ;[start end] (split-at cutpoint remaining)
        [start end] (split-with #(not= % cutpoint) remaining)]
    (remove nil? (flatten [start (first end) moves (rest end) current]))))

(defn round-array [[^ints numbers ^long start]]
  (let [m (count numbers)
        current (aget numbers start)
        moves (->> (range (inc start) (+ start 4))
                   (map #(aget numbers ^int (mod % m))))
        cutpoint (highest-below current moves)
        new-start (loop [i (mod (inc start) m)]
                    (let [x (aget numbers (mod (+ i 3) m))]
                      (aset-int numbers i x)
                      (let [next-i (mod (inc i) m)]
                        (if (= x cutpoint)
                          next-i
                          (recur next-i)))))]
    (when *debug* (println "destination" new-start))
    (doall (map (fn [i move]
                  (aset-int numbers (mod i m) move))
                (range new-start (+ 3 new-start))
                moves))

    [numbers (mod (inc start) m)]))

(defn ll-as-seq [[^ints buffer ^long current]]
  (iterate (partial aget buffer) current))


(defn round-ll [[^ints buffer ^long current :as input]]
  (let [[m1 _m2 m3 :as moves] (take 3 (rest (ll-as-seq input)))
        cutpoint (highest-below current moves)]
    (aset buffer current (aget buffer m3))
    (aset buffer m3 (aget buffer cutpoint))
    (aset buffer cutpoint m1)
    [buffer (aget buffer current)]))

(defn play [input rounds]
  (as-> input $
    (iterate round $)
    (nth $ rounds)
    (split-with #(> % 1) $)
    (concat (rest (second $)) (first $))
    (str/join $)))

(defn play-array [input rounds]
  (as-> input $
    (iterate round-array $)
    (nth $ rounds)
    (first $)
    (split-with #(> % 1) $)
    (concat (rest (second $)) (first $))
    (str/join $)
    ))

(defn play-ll [input rounds]
  (as-> input $
    (iterate round-ll $)
    (nth $ rounds)
    (ll-as-seq [(first $) 1])
    (take *max-number* $)
    (doall $)
    (str/join $)
    ))

(defn play-ll-task-2 [input rounds]
  (let [[buffer _cur] (as-> input $
                        (iterate round-ll $)
                        (nth $ rounds)
                        )
        num1 (aget buffer 1)
        num2 (aget buffer num1)]
    [num1 num2 (* num1 num2)]
    ))

(binding [*max-number* 9]
  (doall (take 10 (iterate round (parse "389125467")))))

(binding [*max-number* 9]
  (highest-below 2 [8 9 1]))

(binding [*max-number* 9]
  [(= (play (parse "389125467") 10)
      "92658374")

   (= (play (parse "389125467") 100)
      "67384529")

   (= (play (parse "712643589") 100)
      "29385746")])

(binding [*max-number* 9]
  (play-array (parse-and-extend "389125467") 10))

(time 
 (binding [*max-number* 1000000]
   (play-ll-task-2 (parse-to-ll "712643589") 10000000)))
;; => [837921 812052 680435423892]


(time 
 (binding [*max-number* 1000000]
   (play-ll-task-2 (parse-to-ll "389125467") 10000000)))
;; => [934001 159792 149245887792]



(highest-below 3 [8 9 1])

(binding [*debug* true
          *max-number* 9]
  (round-array [(parse-and-extend "389125467") 0]))
;; => [[3, 2, 8, 9, 1, 5, 4, 6, 7] 1]


(binding [*max-number* 9]
  (loop [status (parse-to-ll "389125467")
         collect []
         i 10]
    (if (>= i 0)
      (let [r (doall (take *max-number* (ll-as-seq status)))]
        (recur (round-ll status)
               (conj collect r)
               (dec i)))
      collect)))

(binding [*max-number* 9]
  (as-> (parse-to-ll "389125467") $
    (iterate round-ll $)
       ;(take 1)
       ;(map #(take *max-number* (ll-as-seq %)))
    (nth)
    (take (+ 4 *max-number*))
    doall))



(binding [*debug* true
          *max-number* 10000]
  (let [input (time (parse-and-extend "389125467"))
        [array pos] (time (play-array [input 0] 10))
        num1 (aget ^ints array (mod (+ pos 1) *max-number*))
        num2 (aget ^ints array (mod (+ pos 2) *max-number*))]
    [num1 num2 (* num1 num2)]))

(round (parse "389125467"))
;; => (2 8 9 1 5 4 6 7 3)

