(ns advent2023.day03
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(def num-or-empty (set ".0123456789"))

(defn parse-row [row y]
  (->> (map-indexed #(and (not (num-or-empty %2)) [[%1 y] %2]) row)
       (remove false?)))

;(into {} (parse-row "..#..*..%.." 2))

(defn parse-components [in]
  (loop [[row & rows] (str/split-lines in)
         y 0
         map-coord {}]
    (if (nil? row)
      map-coord
      (recur rows (inc y)
             (into map-coord (parse-row row y))))))

(defn solve-1 [in]
  (let [c (parse-components in)]
    (loop [[l & lines] (str/split-lines in)
           y 0
           total 0]
      (if (nil? l)
        total
        (let [line-total (loop [[p & line-parts] (re-seq #"\d+|[^\d]+" l)
                                x 0
                                line-counter 0]
                           (if (nil? p)
                             line-counter
                             (if (Character/isDigit (first p))
                               (let [num-len (count p)
                                     walk-around (concat [[(dec x) y] [(+ x num-len) y]]
                                                         (map #(vector % (dec y)) (range (dec x) (+ x num-len 1)))
                                                         (map #(vector % (inc y)) (range (dec x) (+ x num-len 1))))]
                                 (if (some c walk-around)
                                   (recur line-parts (+ x num-len) (+ line-counter (puzzle/str->long p)))
                                   (recur line-parts (+ x num-len) line-counter)))
                               (recur line-parts (+ x (count p)) line-counter))))]
          (recur lines (inc y) (+ total line-total)))))))

(defn solve-2 [in]
  (let [c (parse-components in)
        possible-matches
        (loop [[l & lines] (str/split-lines in)
               y 0
               possible-gear-nums {}]
          (if (nil? l)
            possible-gear-nums
            (let [line-total (loop [[p & line-parts] (re-seq #"\d+|[^\d]+" l)
                                    x 0
                                    possible-gear-nums {}]
                               (if (nil? p)
                                 possible-gear-nums
                                 (if (Character/isDigit (first p))
                                   (let [num-len (count p)
                                         walk-around (concat [[(dec x) y] [(+ x num-len) y]]
                                                             (map #(vector % (dec y)) (range (dec x) (+ x num-len 1)))
                                                             (map #(vector % (inc y)) (range (dec x) (+ x num-len 1))))
                                         close-component (some #(find c %) walk-around)]
                                     (if (= (second close-component) \*)
                                       (recur line-parts (+ x num-len) (update possible-gear-nums (first close-component) conj (puzzle/str->long p)))
                                       (recur line-parts (+ x num-len) possible-gear-nums)))
                                   (recur line-parts (+ x (count p)) possible-gear-nums))))]
              (recur lines (inc y) (merge-with concat possible-gear-nums line-total)))))]
    (->> (filter #(> (count %) 1) (vals possible-matches))
         (map #(apply * %))
         (reduce +))))

(second nil)
(some #(find (parse-components testdata) %) [[3 4]])

(solve-1 testdata)
;;=> 4361
(solve-1 (puzzle/get-data 2023 3))
;;=> 525911

(solve-2 testdata)
;;=> 467835
(solve-2 (puzzle/get-data 2023 3))
;;=> 75805607
