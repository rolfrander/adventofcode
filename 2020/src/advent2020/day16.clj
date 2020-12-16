(ns advent2020.day16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def testdata "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

(def testdata2 "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9")

(defn parse
  "returns [rules myticket list-of-nearby-tickets]"
  [input]
  (let [int-split (fn [str re] (mapv #(Integer/parseInt %) (str/split str re)))
        parse-ticket (fn [l] (int-split l #","))
        [rule-data _ my-data _ nearby-data] (->> (str/split-lines input)
                                                 (partition-by #(= 0 (count %))))]
    [(loop [[l & r] rule-data
            rules {}]
       (if (not (nil? l))
         (let [[_ name ranges] (re-matches #"([a-z ]+): (.+)" l)]
           (recur r
                  (assoc rules name (->> (str/split ranges #" or ")
                                         (mapv #(int-split % #"-"))))))
         rules))

     (parse-ticket (second my-data))

     (loop [[l & r] (rest nearby-data)
            list-of-nearby-tickets []]
       (if (not (nil? l))
         (recur r (conj list-of-nearby-tickets
                        (parse-ticket l)))
         list-of-nearby-tickets))]))

(defn is-num-in-range [ranges num]
  (some (fn [[from to]] (and (>= num from) (<= num to)))
        ranges))

(defn merge-rules [rules]
  (let [sorted-rules  (->> (mapcat identity (vals rules))
                           (sort-by first))
        reduce-rules (reduce (fn [[prev curr] next]
                               (if (>= (second curr)
                                       (dec (first next)))
                                 [prev [(first curr) (max (second curr) (second next))]]
                                 [(conj prev curr) next]))
                             [[] (first sorted-rules)]
                             (rest sorted-rules))]
    (apply conj reduce-rules)))

(defn task-1 [input]
  (let [[rules _my nearby] (parse input)
        rules (merge-rules rules)]
    (apply + (remove (partial is-num-in-range rules) (flatten nearby)))))

(defn find-applicable-rules [rules my nearby]
  (let [reduced-rules (merge-rules rules)
        valid-tickets (conj (remove (fn [ticket]
                                      (some #(not (is-num-in-range reduced-rules %)) ticket))
                                    nearby)
                            my)
        combined-rules (vec (repeat (count rules) rules))]
    (reduce (fn [rules ticket]
              (map (fn [ruleset field]
                     (remove (fn [rule] (not (is-num-in-range (val rule) field)))
                             ruleset))
                   rules
                   ticket))
            combined-rules
            valid-tickets)))


(task-1 (slurp "day16.txt"))

(defn reduce-rules [rules-per-column])

(defn display [rulenames columns]
  (let [[ruleletters _] (reduce
                         (fn [[l i] r]
                           [(assoc l r (char (+ (int \A) i))) (inc i)])
                         [{} 0]
                         rulenames)]
    (doseq [c columns]
      (->> (map (fn [[rulename ruleletter]]
                  (if (contains? c rulename)
                    ruleletter
                    \_))
                ruleletters)
           str/join
           println))))

(defn solve [x]
  (letfn [(place [[cur & colset]]
            (if (nil? cur)
              ()
              (if (empty? cur)
                false
                (some
                 (fn [candidate]
                   (let [filtered (mapv (fn [el] (remove #(= candidate %) el))
                                        colset)]
                     (if-let [recurred (place filtered)]
                       (conj recurred candidate)
                       false)))
                 cur))))]
    (let [y (sort-by #(count (first %)) (map-indexed #(vector %2 %1) x))
          z (vec (place (mapv first y)))]
      (first
       (reduce (fn [[result cnt] i]
                 [(assoc result (second i) (get z cnt)) (inc cnt)])
               [(vec (range (count y))) 0]
               y)))))

(defn task-2 [data]
  (let [[rules my nearby] (parse data)
        r (find-applicable-rules rules my nearby)
        rules-per-col (map #(into #{} (map first %)) r)]

    (->> (solve rules-per-col)
         (map-indexed
          (fn [idx name]
            [name (get my idx)]))
         (filter #(str/starts-with? (first %) "departure"))
         (map second)
         (apply *))
    ))

(task-2 (slurp "day16.txt"))
;; => 362974212989
