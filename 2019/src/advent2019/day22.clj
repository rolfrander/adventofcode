(ns advent2019.day22
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]))

(def data (puzzle/get-data 2019 22))

(def testdata "deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1")

;cut 6
;deal with increment 7
;deal into new stack

(defn parse-line [l]
  (if (= l "deal into new stack")
    [:new-stack]
    (if-let [[_ num] (re-matches #"deal with increment (\d+)" l)]
      [:increment (puzzle/str->long num)]
      (if-let [[_ num] (re-matches #"cut (-?\d+)" l)]
        [:cut (puzzle/str->long num)]))))

(defn parse [in]
  (map parse-line (str/split-lines in)))

(defn do-shuffle [stack shuf]
  (let [cnt (count stack)]
    
    (case (first shuf)
      :new-stack (vec (rseq stack))
    
      :increment (let [jump (second shuf)]
                   (loop [newstack (vec (repeat cnt nil))
                          from 0
                          to 0]
                     (if (= from cnt)
                       newstack
                       (recur (assoc newstack to (stack from))
                              (inc from)
                              (mod (+ to jump) cnt)))))
    
      :cut (let [cut (second shuf)
                 cut (if (neg? cut) (+ cnt cut) cut)
                 [stack-1 stack-2] (split-at cut stack)]
             (vec (concat stack-2 stack-1)))
      )))

(defn solve-1 [in find-card stack-size]
  (let [shuffle-spec (parse in)
        stack (vec (range stack-size))
        stack (reduce do-shuffle stack shuffle-spec)]
    (reduce #(when (= (stack %2) find-card)
               (reduced %2))
            -1 (range (count stack)))
    ))

(solve-1 testdata 4 10)
;;=> 5
(solve-1 data 2019 10007)
;;=> 4649


