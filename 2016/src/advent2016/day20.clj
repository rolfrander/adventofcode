(ns advent2016.day20
  (:require [rolfrander.puzzle-lib :as puzzle]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn parse-line [line]
  (map puzzle/safe-parse-number (re-seq #"[0-9]+" line)))

(defn parse [input]
  (->> (str/split-lines input)
       (map parse-line)))

(test/deftest parse-test
  (test/is (= [[5 8] [0 2] [4 7]] (parse "5-8
0-2
4-7"))))

(def testdata (parse "5-8
0-2
4-7"))

(def data (parse (puzzle/get-data 2016 20)))

(def max-ip 4294967295)

(defn task-1 [data]
  (reduce (fn [cur-min [block-lo block-hi]]
            (if (and (>= cur-min block-lo)
                     (<= cur-min block-hi))
              (inc block-hi)
              cur-min))
          0
          (sort-by second data)))

(test/deftest task-1-test
  (test/is (= 3 (task-1 testdata))))

(defn count-blocked [data]
  (letfn [(overlap [[a1 a2] [b1 b2]]
            (let [o1 (max a1 b1)
                  o2 (min a2 b2)]
              (if (<= o1 o2) [o1 o2] nil)))
          (ip-range [[a1 a2]] (inc (- a2 a1)))]
    (if (empty? data)
      0
      (+ (ip-range (first data))
         (- (count-blocked (keep (partial overlap (first data)) (rest data))))
         (count-blocked (rest data))))))

(task-1 data)
;; => 32259706

(count-blocked testdata)

(defn task-2 [data m]
  (- m (count-blocked data)))

(test/deftest task-2-test
  (test/is (= 2 (task-2 testdata 10))))

(task-2 data (inc max-ip))

(test/run-all-tests #"advent2016.day20")