(ns advent2022.day02 
  (:require [rolfrander.puzzle-lib :as puzzle]))

(def data (puzzle/get-data 2022 02))
(def testdata "A Y
B X
C Z")

(defn parse [input]
  (partition 2 (re-seq #"[A-Z]" input)))

(def round-1 {"A" :rock
              "B" :paper
              "C" :scissors
              "X" :rock
              "Y" :paper
              "Z" :scissors})

(def shape-points {:rock 1
                   :paper 2
                   :scissors 3})

(def win {:rock :scissors
          :scissors :paper
          :paper :rock})

(def loose {:scissors :rock
            :paper :scissors
            :rock :paper})

(defn play-2 [opponent my]
  (let [opponent (round-1 opponent)]
    (case my
      "X" (shape-points (win opponent))
      "Y" (+ 3 (shape-points opponent))
      "Z" (+ 6 (shape-points (loose opponent))))))

(defn play-1 [opponent my]
  (let [sp (shape-points (round-1 my))
        my (round-1 my)
        opponent (round-1 opponent)]
    (cond (= my opponent) (+ 3 sp)
          (= (win my) opponent) (+ 6 sp)
          :else sp)))

(defn part-1 [data]
  (->> (parse data)
       (map (partial apply play-1))
       (reduce +)))

;(part-1 data)

(defn part-2 [data]
  (->> (parse data)
       (map (partial apply play-2))
       (reduce +)
       ))

(part-2 data)