(ns advent2017.day09
  (:require [rolfrander.puzzle-lib :refer [get-data]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is are run-all-tests]]))

(def parse-line seq)

(defn parse [input]
  (map parse-line (str/split-lines input)))

(def ^:dynamic *debug* false)

(defn debug [input]
  (when *debug* (println input))
  input)

(defn ignore-garbage [data]
  (loop [data data]
    (case (first data)
      nil nil
      \> (rest data)
      \! (recur (rest (rest data)))
      
      (recur (rest data)))))

(defn count-garbage [data]
  (loop [data data
         cnt 0]
    (case (first data)
      nil nil
      \> [(rest data) cnt]
      \! (recur (rest (rest data)) cnt)

      (recur (rest data) (inc cnt)))))

(defn score
  ([data] (score data 0))

  ([data depth]
   (loop [data data
          running-score 0]
     (case (first data)
       nil running-score
       \{ (let [[data subscore] (score (rest data) (inc depth))]
            (recur data (+ running-score subscore)))

       \} [(rest data) (+ depth running-score)]

       \< (recur (ignore-garbage data) running-score)

       (recur (rest data) running-score)))))

(defn score-2 [^String input]
  (loop [i 0
         score-cnt 0
         garbage-cnt 0
         depth 0
         in-garbage false]
    (if (>= i (.length input))
      [score-cnt garbage-cnt]
      (case (.charAt input i)
        \{ (recur (inc i) score-cnt garbage-cnt (inc depth) in-garbage)
        \} (if in-garbage
             (recur (inc i) score-cnt (inc garbage-cnt) depth in-garbage)
             (recur (inc i) (+ score-cnt depth) garbage-cnt (dec depth) in-garbage))
        \! (recur (+ i 2) score-cnt garbage-cnt depth in-garbage)
        \< (recur (inc i) score-cnt garbage-cnt depth true)
        \> (recur (inc i) score-cnt garbage-cnt depth false)
        (if in-garbage
          (recur (inc i) score-cnt (inc garbage-cnt) depth in-garbage)
          (recur (inc i) score-cnt garbage-cnt (dec depth) in-garbage))))))

(def testvectors (partition 2 ["{}" 1
                               "{{{}}}" 6
                               "{{}{}}" 5
                               "{{{},{},{{}}}}" 16
                               "{<a>,<a>,<a>,<a>}" 1
                               "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
                               "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
                               "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3
                               "{}{}" 2]))

(def testvectors2 (partition 2 ["<>" 0
                                "<random characters>" 17
                                "<<<<>" 3
                                "<{!>}>" 2
                                "<!!>" 0
                                "<!!!>>" 0
                                "<{o\"i!a,<{i<a>" 10]))

(defn count-garbage-b [line]
  (->> (re-seq #"^<|>$|(?:!.|([^!>]))" line)
       (map second)
       (remove nil?)
       count))

(defn task-2-b [input]
  (reduce (fn [ret element]
            (case element
              "{" (update ret :depth inc)
              "}" (-> ret
                      (update :groups (partial + (:depth ret)))
                      (update :depth dec))
              (update ret :garbage (partial + (count-garbage-b element)))))
          {:groups 0 :garbage 0 :depth 0} (re-seq #"<(?:!.|[^!>])*>|(?:[{}])" input)))
  
(comment
  (->> testvectors
       (map first)
       (map task-2-b)
       (map vector testvectors))
  )


(deftest score-test
  (are [input res] (= res (first (score-2 input)))
    "{}" 1
    "{{{}}}" 6
    "{{}{}}" 5
    "{{{},{},{{}}}}" 16
    "{<a>,<a>,<a>,<a>}" 1
    "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
    "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
    "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3
    "{}{}" 2))

(defn task-1 [input]
  (reduce + (map score (parse input))))

(defn task-2 [input]
  (loop [data (parse-line input)
         cnt 0]
    (case (first data)
      nil cnt
      \< (let [[data cnt2] (count-garbage (rest data))]
           (recur data (+ cnt cnt2)))
      (recur (rest data) cnt))))

(deftest data-2-test
  (are [input res] (= res (task-2 input))
    "<>" 0
    "<random characters>" 17
    "<<<<>" 3
    "<{!>}>" 2
    "<!!>" 0
    "<!!!>>" 0
    "<{o\"i!a,<{i<a>" 10))


(task-1 (get-data 2017 9))
;; => 12396

(task-2 (get-data 2017 9))
;; => 6346


(run-all-tests #"advent2017.day09")