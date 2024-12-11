(ns advent2023.day05 
  (:require
    [clojure.string :as str]
    [rolfrander.puzzle-lib :as puzzle]))

(def testdata "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn parse-map [m]
  (let [[_ign from to] (re-find #"(\w+)-to-(\w+) map:" (first m))]
    {:from from
     :to to
     :offsets (reduce (fn [r e]
                    (conj r
                          (let [[dest src length] (map puzzle/str->long (str/split e #" "))]
                            {:det dest
                             :src src
                             :len length})))
                  []
                  (rest m))}))

(defn parse [in]
  (let [[seed & maps] (->> (str/split-lines in)
                           (puzzle/split-by #(not= % "")))
        maps (map (comp parse-map rest) maps)]
    {:seed (map puzzle/str->long (re-seq #"\d+" (first seed)))
     :maps maps}))

(defn offset-map [m src]
  (cond (and (< src (:src m))
             (< src (:dst m))) src
        
        (< src (:src m)) (+ src (:len m))

        (< src (:dst m))
        ))