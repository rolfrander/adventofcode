(ns advent2017.day04
  (:require [rolfrander.puzzle-lib :refer [get-data]]
            [clojure.string :as str]))

(defn valid-passphrase [line]
  (->> (str/split line #" +")
       frequencies
       vals
       (reduce max)
       (= 1)))

(defn valid-passphrase2 [line]
  (->> (str/split line #" +")
       (map seq)
       (map sort)
       (map str/join)
       frequencies
       vals
       (reduce max)
       (= 1)))

;(valid-passphrase "aa bb cc dd aaa")

;(valid-passphrase2 "abcde xyz ecdab")

(def testdata "aa bb cc dd ee
aa bb cc dd aa
aa bb cc dd aaa")

(let [data (get-data 2017 04)]
  (->> (str/split-lines data)
       (filter valid-passphrase2)
       count))