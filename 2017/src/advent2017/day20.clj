(ns advent2017.day20
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.string :as str]
            [clojure.core.matrix :as mat]
            [clojure.data.priority-map :refer [priority-map-keyfn-by]]))

(defn dist [point]
  (reduce #(+ %1 (Math/abs %2)) 0 (:p point)))

(defn accel [point]
  (reduce #(+ %1 (Math/abs %2)) 0 (:a point)))

(defn parse-line [idx line]
  (let [p (->> (partition 3 (map safe-parse-number (re-seq #"-?\d+" line)))
               (map vec)
               (zipmap [:p :v :a]))]
    (assoc p :id idx)))


(defn parse [input]
  (map-indexed parse-line (str/split-lines input)))

(defn into-priority-queue [data]
  (reduce #(assoc %1 (:id %2) %2)
          (priority-map-keyfn-by accel <)
          data))

(def testdata "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>")

(defn iterate-position [data]
  (map (fn [p] (as-> p p
                 (update p :v (partial mat/add (:a p)))
                 (update p :p (partial mat/add (:v p)))))
       data))

(defn task-1 [input]
  (let [data (into-priority-queue (parse input))]
    (peek data)))

(defn task-2 [input cnt]
  (loop [data (parse input)
         iter cnt]
    (let [newdata (iterate-position data)
          duplicates (->> (frequencies (map :p newdata))
                          (keep #(when (> (second %) 1)
                                   (first %)))
                          (into #{}))]
      (if (zero? iter)
        (count newdata)
        (recur (remove (comp duplicates :p) newdata)
               (dec iter))))))

(task-2 (get-data 2017 20) 500)