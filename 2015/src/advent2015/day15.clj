(ns advent2015.day15
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)

(def data "Sprinkles: capacity 2, durability 0, flavor -2, texture 0, calories 3
Butterscotch: capacity 0, durability 5, flavor -3, texture 0, calories 3
Chocolate: capacity 0, durability 0, flavor 5, texture -1, calories 8
Candy: capacity 0, durability -1, flavor 0, texture 5, calories 8")

(defn parse [input]
  (->> (re-seq #"([a-zA-Z]+): capacity (-?[0-9]+), durability (-?[0-9]+), flavor (-?[0-9]+), texture (-?[0-9]+), calories (-?[0-9]+)\n?" input)
       (map (fn [[_line name capacity durability flavor texture calories]]
              [name {:capacity (Long/parseLong capacity)
                     :durability (Long/parseLong durability)
                     :flavor (Long/parseLong flavor)
                     :texture (Long/parseLong texture)
                     :calories (Long/parseLong calories)}]))
       (into {})))

(def start {:capacity 0
            :durability 0
            :flavor 0
            :texture 0
            :calories 0})

(def ingredients (parse data))

(def sprinkles (get ingredients "Sprinkles"))
(def butterscotch (get ingredients "Butterscotch"))
(def chocolate (get ingredients "Chocolate"))
(def candy (get ingredients "Candy"))

(defn score [values]
  (->> [:capacity :durability :flavor :texture]
       (map #(max 0 (% values)))
       (reduce *)))

(defn add-capabilities 
  ([a b]
   (merge-with + a b))
  ([a b cnt]
   (merge-with #(+ %1 (* cnt %2)) a b)))

(defn remove-capabilities [a b]
  (merge-with - a b))

(defn add-one-ingredient [current-score]
  (->> ingredients
       (map (fn [[_name values]]
              (let [combined-capabilities (add-capabilities current-score values)
                    combined-score (score combined-capabilities)]
                [combined-capabilities combined-score])))
       (apply max-key second)
       first))


(map (fn [[_name values]]
       (let [combined-capabilities (add-capabilities start values)
             combined-score (score combined-capabilities)]
         [combined-capabilities combined-score]))
     ingredients)


(def start-after-5
  (-> start
      (add-capabilities (get ingredients "Sprinkles"))
      (add-capabilities (get ingredients "Butterscotch"))
      (add-capabilities (get ingredients "Chocolate"))
      (add-capabilities (get ingredients "Chocolate"))
      (add-capabilities (get ingredients "Candy"))
      ))

(score 
 (nth (iterate add-one-ingredient start-after-5) 95))
;; => 21367368

;; 3x + 8y = 500
;; x + y = 100
;; => 5y = 200 
;; => y = 40
;; => x = 60

;; hill-climbing algorithm:
;; start with a recepie with maximum sprinkles and chocolate
;; replace ingredients while the score increases

(let [start-500 (-> start
                    (add-capabilities sprinkles 58)
                    (add-capabilities butterscotch 2)
                    (add-capabilities chocolate 33)
                    (add-capabilities candy 7))]
  (loop [current-values start-500]
    (let [replace-sprinkles (-> current-values
                                (remove-capabilities sprinkles)
                                (add-capabilities butterscotch))
          replace-chocolate (-> current-values
                                (remove-capabilities chocolate)
                                (add-capabilities candy))
          replace-both (-> replace-sprinkles
                           (remove-capabilities chocolate)
                           (add-capabilities candy))]
      (cond (> (score replace-both)
               (score current-values))
            (do (println "replace both")
                (recur replace-both))
            
            (> (score replace-sprinkles)
               (score current-values))
            (do (println "replace sprinkles")
                (recur replace-sprinkles))

            (> (score replace-chocolate)
               (score current-values))
            (do (println "replace chocolate")
                (recur replace-chocolate))

            :else (score current-values)))))


;; => 1216000
;; => 1619352 (for lav)

(reduce max
        (for [num-sprinkles (range 1 60)
              num-chocolate (range 1 40)]
          (-> start
              (add-capabilities sprinkles num-sprinkles)
              (add-capabilities butterscotch (- 60 num-sprinkles))
              (add-capabilities chocolate num-chocolate)
              (add-capabilities candy (- 40 num-chocolate))
              score)))
;; => 1766400 (riktig)
