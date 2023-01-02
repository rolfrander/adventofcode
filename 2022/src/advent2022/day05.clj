(ns advent2022.day05 
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(def data (puzzle/get-data 2022 5))

(def is-num (into #{} "0123456789"))

(defn parse [input]
  (let [[stacks instructions] (str/split input #"\n\n")
        clean-stack (fn [s]
                      (loop [[i & s] s
                             stack []]
                        (cond (nil? i) (throw (RuntimeException. "empty?"))
                              (is-num i) [(- (int i) (int \0)) (seq stack)]
                              (= i \space) (recur s stack)
                              :else (recur s (conj stack i)))))
        ]
    [(->> (str/split-lines stacks)
          (apply map list)
          rest
          (partition 1 4)
          (map first)
          (map clean-stack)
          (into (sorted-map))
          )
     
     (->> (re-seq #"[0-9]+" instructions)
          (map puzzle/str->long)
          (partition 3))
     ]))

(defn execute [stacks [cnt from to]]
  (if (= cnt 0)
    stacks
    (let [m (first (stacks from))]
      (if (nil? m)
        (throw (RuntimeException. "empty stack"))
        (execute (-> stacks
                     (update from rest)
                     (update to conj m))
                 [(dec cnt) from to])))))

(defn execute-9001 [stacks [cnt from to]]
  (let [m (take cnt (stacks from))]
    (-> stacks
        (update from (partial drop cnt))
        (update to (partial concat m)))))

(defn task-1 [input]
  (let [[s i] (parse input)]
    (->> (reduce execute s i)
         vals
         (map first)
         str/join)))

(defn task-2 [input]
  (let [[s i] (parse input)]
    (->> (reduce execute-9001 s i)
         vals
         (map first)
         str/join)))

(print (first (str/split data #"\n\n")))

(task-1 testdata)
(task-1 data)

(task-2 testdata)
(task-2 data)

