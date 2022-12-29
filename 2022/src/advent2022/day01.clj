(ns advent2022.day01
  (:require [rolfrander.puzzle-lib :as puzzle] 
            [clojure.string :as str]))

(def data (puzzle/get-data 2022 01)) 
(def testdata "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn parse [input]
  (->> (str/split input #"\n\n")
       (map str/split-lines)
       (map #(map puzzle/str->long %))))

(->> (parse data)
     (map #(reduce + %))
     (sort >)
     (take 3)
     (reduce +))