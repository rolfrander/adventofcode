(ns advent2016.day04
  (:require [advent2016.core :as core]
            [clojure.string :as string]))

(def data (core/get-data 2016 04))

(def testdata "aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]")

(def alphabet (vec "abcdefghijklmnopqrstuvwxyz "))
(def ord (into {} (map-indexed #(vector %2 %1) alphabet)))

(defn parse-line [line]
  (->> (re-matches #"([a-z-]*)-([0-9]+)\[([a-z]+)\]" line)
       (rest)
       (map core/safe-parse-number)))

(defn parse [input]
  (map parse-line (string/split-lines input)))

(defn checksum-room [name]
  (letfn [(compare-room [[n1 f1] [n2 f2]]
            (let [f (compare f2 f1)]
              (if (not= f 0)
                f
                (compare n1 n2))))]
    (->> (dissoc (frequencies name) \-)
         (sort compare-room)
         (map first)
         (take 5)
         string/join)))

(defn check-room [[name _sector checksum]]
  (= (checksum-room name) checksum))

(defn decrypt-name [[name sector _checksum]]
  (letfn [(decrypt-char [x]
            (if (= x \-) 26
                (mod (+ (ord x) sector) 26)))]
    (->> (seq name)
         (map decrypt-char)
         (map alphabet)
         string/join
         )))

(defn task-1 [data]
  (->> data
       (filter check-room)
       (map second)
       (reduce +)))

(defn task-2 [data]
  (->> data
       (map (juxt decrypt-name second))
       (filter (comp (partial re-matches #".*north.*")
                     first))))

(task-1 (parse testdata)) ; 1514
(task-1 (parse data)) ;; => 158835

(task-2 (parse data))
