(ns advent2022.day11
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defn parse-monkey [[_total id items oper oper-param div true-throw false-throw]]
  {:id (puzzle/str->long id)
   :items (into clojure.lang.PersistentQueue/EMPTY
                (map puzzle/str->long (str/split items #", ")))
   :oper (if (= oper-param "old")
           (case oper
             "+" (fn [w] (+ w w))
             "*" (fn [w] (* w w)))
           (let [p (puzzle/str->long oper-param)]
             (case oper
               "+" (fn [w] (+ w p))
               "*" (fn [w] (* w p)))))
   :div (puzzle/str->long div)
   :true (puzzle/str->long true-throw)
   :false (puzzle/str->long false-throw)
   :counter 0})

(defn parse [input]
  (->> input
       (re-seq #"Monkey ([0-9]+):
  Starting items: ([0-9 ,]+)
  Operation: new = old (.) (old|[0-9]+)
  Test: divisible by ([0-9]+)
    If true: throw to monkey ([0-9]+)
    If false: throw to monkey ([0-9]+)")
       (map parse-monkey)
       ;(map (juxt :id identity))
       (into [])))

;(parse testdata)
;(parse (puzzle/get-data 2022 11))

(def ^:dynamic *worry-reduction* 3)

(defn eval-monkey [monkey]
  (let [item (first (:items monkey))
        worry (quot ((:oper monkey) item) *worry-reduction*)]
    [(-> monkey
         (update :items pop)
         (update :counter inc))
     worry
     (if (= 0 (mod worry (:div monkey)))
       (:true monkey)
       (:false monkey))]))

(defn round [monkeys]
  (loop [monkeys monkeys
         i 0]
    (if (> i (count monkeys))
      monkeys
      (recur (loop [monkeys monkeys]
               (let [monkey (get monkeys i)]
                 (if (empty? (:items monkey))
                   monkeys
                   (let [[monkey item to] (eval-monkey monkey)]
                     (recur (-> monkeys
                                (assoc i monkey)
                                (update-in [to :items] conj item)))))))
             (inc i)))))

(defn eval-rounds [rounds data]
  (nth (iterate round data)
       rounds))

(defn calc-monkey-business [data]
  (->> (map :counter data)
       (sort >)
       (take 2)
       (apply *)))

(defn task-1 [input]
  (->> (parse input)
       (eval-rounds 20)
       (calc-monkey-business)))

;(eval-monkey (first (vals (parse testdata))))
(task-1 testdata)
(task-1 (puzzle/get-data 2022 11))

(binding [*worry-reduction* 1]
  (->> (parse testdata)
       (eval-rounds 20)
       (map :counter)))
