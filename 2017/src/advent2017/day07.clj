(ns advent2017.day07
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-all-tests]]))

(def ^:dynamic *debug* false)

(defn debug [input]
  (when *debug* (println input))
  input)

(defn parse-line [input]
  (try
    (let [[program number & supporting] (re-seq #"\w+|\d+" input)]
      [program {:weight (safe-parse-number number) :supports supporting}])
    (catch Exception e
      (throw (RuntimeException. (str "Offending line: " input))))))

(defn parse [input]
  (->> input
       str/split-lines
       (map parse-line)
       (into {})))

(deftest parse-test
  (is (= ["aa" {:weight 12 :supports ["bb" "cc"]}] (parse-line "aa (12) -> bb, cc"))))

(defn rearrange [data root]
  (-> (data root)
      (assoc :name root)
      (update :supports (partial map (partial rearrange data)))))

(defn find-root [data]
  (let [not-bottom (into #{} (apply concat (map :supports (vals data))))]
    (first (remove not-bottom (keys data)))))

(defn total-weight [tree]
  (as-> tree t
      (update t :supports (partial map total-weight))
      (assoc t :total-weight (+ (:weight t)
                                (reduce + (map :total-weight (:supports t)))))))

(defn parse-and-get-total-weights [input]
  (let [data (parse input)
        root (find-root data)]
    (total-weight (rearrange data root))))

(defn find-unbalanced [tree]
  (when-let [subtrees (seq (:supports tree))]
    (let [weight-groups (group-by :total-weight subtrees)]
      (if (= 1 (count weight-groups))
        (seq (keep find-unbalanced (first (vals weight-groups))))
        ; tree is unbalanced
        (let [[unbalanced-weight unbalanced-subtree-list] (first (filter #(= 1 (count (second %))) weight-groups))
              [balanced-weights _]  (first (filter #(< 1 (count (second %))) weight-groups))
              sub-balance (find-unbalanced (first unbalanced-subtree-list))]
          (or sub-balance
              (+ (:weight (first unbalanced-subtree-list))
                 (- balanced-weights unbalanced-weight))
              ))))))

(defn task-1 [input]
  (find-root (parse input)))

(defn task-2 [input]
  (let [data (parse-and-get-total-weights input)]
    (find-unbalanced data)))

(def testdata "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")

(binding [*debug* true]
  (task-2 testdata))

(deftest task-1-test
  (is (= "tknk" (task-1 testdata))))

(deftest task-2-test
  (is (= 60 (task-2 testdata))))

(task-2 (get-data 2017 7))

(run-all-tests #"advent2017.day07")