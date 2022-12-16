(ns advent2018.day02
  (:require [rolfrander.puzzle-lib :refer [get-data]]
            [clojure.string :as str]))

(def testdata "abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab")

(def testdata2 "abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz")

(def parse str/split-lines)

(defn reverse-map [m]
  (reduce (fn [ret [k v]]
            (update ret v conj k))
          {}
          m))

(defn count-if [pred coll]
  (count (filter pred coll)))

(defn count-match [str1 str2 upto]
  (count (take upto (filter true? (map not= str1 str2)))))

(defn find-same [str1 str2]
  (str/join (map #(if (= %1 %2) %1 "") str1 str2)))

(defn task-1 [input]
  (->> (parse input)
       (map frequencies)
       (map reverse-map)
       ((juxt (partial count-if #(contains? % 2))
              (partial count-if #(contains? % 3))))
       (apply *)))

(defn task-2 [input]
  (let [data (parse input)]
    (for [a-list (take-while (complement empty?) (iterate rest data))
          :let [a (first a-list)]
          b (rest a-list)
          :when (= 1 (count-match a b 2))]
      (find-same a b))))

(task-2 (get-data 2018 2))
(task-2 testdata2)