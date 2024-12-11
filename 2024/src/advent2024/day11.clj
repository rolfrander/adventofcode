(ns advent2024.day11 
  (:require
    [rolfrander.puzzle-lib :as puzzle]))

(def testdata "125 17")

(defn parse [in]
  (map puzzle/str->long (re-seq #"\d+" in)))

(defn digits [n]
  (if (< n 10)
    [n]
    (conj (digits (quot n 10))
          (mod n 10))))

(defn digits->long [d]
  (reduce #(+ (* %1 10) %2)
          0 d))

(defn blink-stone [stone]
  "- If the stone is engraved with the number 0, it is replaced by a 
  stone engraved with the number 1.
- If the stone is engraved with a number that has an even number of 
  digits, it is replaced by two stones. The left half of the digits
  are engraved on the new left stone, and the right half of the
  digits are engraved on the new right stone. (The new numbers 
  don't keep extra leading zeroes: 1000 would become stones 10
  and 0.)
- If none of the other rules apply, the stone is replaced by a new
  stone; the old stone's number multiplied by 2024 is engraved on 
  the new stone."
  (if (= stone 0) [1]
      (let [d (digits stone)]
        (if (even? (count d))
          [(digits->long (subvec d 0 (/ (count d) 2)))
           (digits->long (subvec d (/ (count d) 2)))]
          [(* stone 2024)]))))

(defn blink [stones]
  (mapcat blink-stone stones))

(defn stones-after-n-blinks [start n] 
  (count (nth (iterate blink start) n)))

(defn stones-after-n-blinks-debug [start n]
  (let [starttime (System/currentTimeMillis)]
    (loop [start start
           i 0]
      (let [c (count start)]
        (if (= i 75)
          c
          (do (println i c (format "(%d s)" (quot (- (System/currentTimeMillis) starttime) 1000)))
              (recur (blink start) (inc i))))))))

;; datastructure
;; each stone has
;; - value
;; - list of sub-stones (1 or 2)
;; current state
;; - list of stone-id and count

;(blink-stone 17) => 1 7

(defn stones-2 [start rounds]
  (let [maybe+ (fn [a b]
                 (cond (= a nil) b
                       (= b nil) a
                       :else (+ a b)))]
    (loop [stones (reduce #(assoc %1 %2 1) {} start)
           i 0]
      (if (= i rounds)
        stones
        (recur (reduce (fn [ret [stone cnt]]
                         (reduce #(update %1 %2 maybe+ cnt)
                                 ret
                                 (blink-stone stone)))
                       {}
                       stones)
               (inc i))))))

(defn solve-1 [in]
  (stones-after-n-blinks (parse in) 25))

(defn solve-2 [in]
  (->> (stones-2 (parse in) 75)
       (map second)
       (reduce +)))

(loop [stones [1]
       i 0]
  (when (< i 10)
    (println i stones)
    (recur (blink stones) (inc i))))

(def data (puzzle/get-data 2024 11))
(solve-1 testdata)
(solve-1 data)
;;=> 183248

(solve-2 data)
;;=> 218811774248729

