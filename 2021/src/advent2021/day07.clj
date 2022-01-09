(ns advent2021.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [oz.core :as oz]))

(def data (slurp "resources/day07.txt"))
(def testdata "16,1,2,0,4,2,7,1,2,14")

(defn parse [d] (map #(Long/parseLong %) (string/split d #",")))

;; brute force
(defn task-1 [positions alignment]
  (reduce #(+ %1 (Math/abs (- %2 alignment)))
          0
          positions))

; 1 2 3 4 5 6 7 8

(defn arit-sum [n]
  (/ (* n (inc n)) 2))

(defn task-2 [positions alignment]
  (reduce #(+ %1 (arit-sum (Math/abs (- %2 alignment))))
          0
          positions))

(let [crabs (parse testdata)]
  (reduce (fn [[best-fuel best-align] align]
            (let [t (task-1 crabs align)]
              (if (< t best-fuel)
                [t align]
                [best-fuel best-align])))
          [999999 1]
          (range 1 20)))

(oz/start-server!)

(let [f task-1]
  (f (parse testdata) 37))

(defn graph-data [positions alignments]
  (for [i alignments]
    {:alignment i 
     :task-1 (task-1 positions i)
     :task-2 (task-2 positions i)}))

(def values (graph-data (parse data) (range 200 600)))

(let [d (parse data)
      n (count d)
      total (reduce + 0 d)]
  (float (/ total n)))

(let [d (sort (parse data))
      n (count d)]
  (if (odd? n)
    (nth d (/ (dec n) 2))
    (/ (+ (nth d (/ n 2))
          (nth d (dec (/ n 2))))
       2)))

(task-1 (parse data) 459)

(oz/view!
 [:div
  [:vega-lite
   {:width "container"
    :height "container"

    :repeat ["task-1" "task-2"]
    :columns 1
    :spec {:data {:name "task" :values values}

           :encoding {:x {:field "alignment" :type "quantitative"}
                      :y {:field {:repeat "repeat"} :type "quantitative"
                          :scale {:zero false :nice true}
                          :axis {:format "s"}}}
           :mark "line"}}
   {:width 800 :height 40}]])