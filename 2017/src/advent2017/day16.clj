(ns advent2017.day16
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.test :refer [deftest is are run-all-tests]]
            [clojure.string :as str]))

(def testdata "s1,x3/4,pe/b")

(defn parse [input]
  (->> (re-seq #"([sxp])(\d+|\w)(?:/(\d+|\w+))?" input)
       (map rest)
       (map #(map safe-parse-number %))))

(defn spin [^String input ^long x]
  (let [split (- (count input) x)]
    (str (.substring input split) (.substring input 0 split))))

(deftest spin-test
  (are [in out] (= out (spin "abcde" in))
    0 "abcde"
    1 "eabcd"
    4 "bcdea"))

(defn exchange [^String input ^long a ^long b]
  (->> (map #(condp = %1
               a (.charAt input b)
               b (.charAt input a)
               (.charAt input %1))
            (range (.length input)))
       str/join))

(deftest exchange-test
  (are [a b out] (= out (exchange "abcde" a b))
    3 4 "abced"
    0 3 "dbcae"))

(defn partner [^String input a b]
  (let [ai (.indexOf input a)
        bi (.indexOf input b)]
    (exchange input ai bi)))

(deftest partner-test
  (are [a b out] (= out (partner "abcde" a b))
    "b" "e" "aecdb"
    "a" "b" "bacde"))

(defn dance-step [input [move a b]]
  (case move
    "s" (spin input a)
    "x" (exchange input a b)
    "p" (partner input a b)))

(defn execute-dance [input dance]
  (reduce dance-step input dance))

(defn task-1 [d start-vector]
  (execute-dance start-vector d))

(def dance (parse (get-data 2017 16)))
(def start "abcdefghijklmnop")

(task-1 dance "abcdefghijklmnop")

(->> start
     (task-1 dance)
     (task-1 dance))

(keep-indexed #(when (= (first %2) "p") [%1 %2]) dance)

(defn task-2 [dance start-vector iterations]
  (let [find-cycle-length (fn [d] (->> (iterate (partial task-1 d) start-vector)
                                       (keep-indexed #(when (= %2 start-vector) %1))
                                       second))
        only-p (filter #(= (first %) "p") dance)
        x-and-s (remove #(= (first %) "p") dance)
        cycle (* (find-cycle-length only-p)
                 (find-cycle-length x-and-s))
        remaining-cycles (mod iterations cycle)]
    (nth (iterate (partial task-1 dance) start-vector) 
         remaining-cycles)))

(task-2 (parse testdata) "abcde" 2)
(task-2 dance start 1000000000)

(time (nth (iterate (partial task-1 dance) start) 1000))

(run-all-tests #"advent2017.day16")