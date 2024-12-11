(ns advent2023.day04 
  (:require
    [rolfrander.puzzle-lib :as puzzle]
    [clojure.string :as str]))

(def testdata "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn parse-line [l]
  (loop [[t & tokens] (re-seq #"\d+|:|\|" l)
         state :card
         card {:card nil :winning #{} :have '()}]
    (if (nil? t)
      card
      (case t
        ":" (recur tokens :winning card)
        "|" (recur tokens :have card)
        (recur tokens state (let [n (puzzle/str->long t)]
                              (case state
                                :card    (assoc card :card n)
                                :winning (update card :winning conj n)
                                :have    (update card :have conj n))))))))

(defn parse [in]
  (map parse-line (str/split-lines in)))

(defn count-winnings [c]
  (let [w (:winning c)
        h (:have c)
        win-cnt (reduce #(if (w %2) (inc %1) %1)
                        0 h)]
    win-cnt))

(defn calc-winnings [c]
  (let [win-cnt (count-winnings c)]
    (if (> win-cnt 0)
      (bit-shift-left 1 (dec win-cnt))
      0)))

;(map calc-winnings (parse testdata))

(defn solve-1 [data]
  (->> (parse data)
       (map calc-winnings)
       (reduce +)))

(defn solve-2 [data]
  (loop [cards (reduce #(assoc %1 (:card %2) (assoc %2 :cnt 1))
                       {}
                       (parse data))
         i 1]
    (if (not (contains? cards i))
      (reduce + (map :cnt (vals cards)))
      (let [card (cards i)
            w (count-winnings card)
            mult (:cnt card)
            next-i (inc i)
            new-cards (reduce #(if (contains? %1 %2)
                                 (update-in %1 [%2 :cnt] + mult)
                                 %1)
                              cards (range next-i (+ next-i w)))]
        ;(println i w mult (map (juxt :card :cnt) (vals cards)))
        (recur new-cards next-i)))))

(solve-1 testdata)
;;=> 13
(solve-1 (puzzle/get-data 2023 4))
;;=> 27845


(solve-2 testdata)
;;=> 30
(solve-2 (puzzle/get-data 2023 4))
;;=> 9496801
