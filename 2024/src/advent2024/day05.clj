(ns advent2024.day05
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn parse [input]
  (loop [[l & lines] (str/split-lines input)
         phase :rules
         rules []
         pages []]
    (cond (nil? l) [rules pages]
          (= l "") (recur lines :pages rules pages)
          (= phase :rules) (let [[from to] (map puzzle/str->long (re-seq #"[0-9]+" l))]
                             (recur lines phase (conj rules [from to]) pages))
          :else (let [p (map puzzle/str->long (re-seq #"[0-9]+" l))]
                  (recur lines phase rules (conj pages p))))))

(defn organize-rules [rules]
  (let [r {}]
    (reduce (fn [r [from to]]
              (if (contains? r from)
                (update r from #(conj % to))
                (assoc r from [to])))
            r rules)))

(defn check-rules [rules p]
  (->> (for [r (take-while seq (iterate rest p))
             to (rest r)
             :while (not (nil? to))
             :let [from (first r)]]
         [from to])
       (some #(not (rules %)))
       not))

(defn middle [p] (loop [a (rest p)
                        b p]
                   (if (empty? a) (first b)
                       (recur (rest (rest a)) (rest b)))))

(defn solve-1 [d]
  (let [[rules pages] (parse d)
        rules (set rules)]
    (->> (filter (partial check-rules rules) pages)
         (map middle)
         (reduce +))))

(defn solve-2 [d]
  (let [[rules pages] (parse d)
        rules (set rules)
        before (fn [a b] (if (contains? rules [a b]) -1 1))]
    (->> (remove (partial check-rules rules) pages)
         (map (partial sort before))
         (map middle)
         (reduce +))))

(let [[rules _pages] (parse testdata)
      rules (set rules)
      p [75 53 47 29 61]
      before (fn [a b] (if (contains? rules [a b]) -1 1))]
  (sort before p))

(solve-1 testdata)
;;=> 143
(solve-1 (puzzle/get-data 2024 05))
;;=> 4996
(solve-2 testdata)
;;=> 123
(solve-2 (puzzle/get-data 2024 05))
;;=> 6311
