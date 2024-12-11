(ns advent2023.day01
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(def testdata2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(defn parse-line [l]
  ((juxt first last) (re-seq #"\d" l)))

(def str->num {"on" "1"
               "tw" "2"
               "thre" "3"
               "four" "4"
               "fiv" "5"
               "six" "6"
               "seven" "7"
               "eigh" "8"
               "nin" "9"})

(defn parse-line-2 [l]
  (->> (re-seq #"\d|on(?=e)|tw(?=o)|thre(?=e)|four|fiv(?=e)|six|seven|eigh(?=t)|nin(?=e)" l)
       ((juxt first last))
       (map #(or (str->num %) %))))

;(parse-line-2 "2nineightnin")

(defn solve-1 [data]
  (->> (str/split-lines data)
       (map parse-line)
       (map str/join)
       (map puzzle/str->long)
       (reduce +)))

(defn solve-2 [data]
  (->> (str/split-lines data)
       (map parse-line-2)
       (map str/join)
       (map puzzle/str->long)
       (reduce +)
       ))
;;=> 55902

(solve-1 testdata)
(solve-1 (puzzle/get-data 2023 1))
(solve-2 testdata)
(solve-2 (puzzle/get-data 2023 1))
