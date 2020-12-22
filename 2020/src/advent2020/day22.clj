(ns advent2020.day22
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)

(def testdata "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn parse [input]
  (->> (str/split-lines input)
       (partition-by empty?)
       (remove #(= % (list "")))
       (map (fn [playerlist]
              (queue (map #(Integer/parseInt %) (rest playerlist)))))
       doall))

(defn highest-index [v]
  (first (apply max-key second (map-indexed vector v))))

(defn has-cards-left [decks]
  (->> decks
       (map-indexed vector)
       (some (fn [[playerno deck]]
               (and (seq deck) playerno)))))

(defn play [player-decks]
  (loop [decks player-decks]
    (if (some empty? decks)
      decks
      (let [round (map peek decks)
            winner (highest-index round)
            add-cards (sort > round)]
        (recur (vec (map-indexed (fn [playerno playerdeck]
                                   (pop (if (= playerno winner)
                                          (into playerdeck add-cards)
                                          playerdeck)))
                                 decks)))
        ))
    ))

(defn run-recursive? [decks]
  (try
    (not (some #(>= (peek %) (count %)) decks))
    (catch ClassCastException e
      (println "Exception in run-recursive on" decks)
      (throw (RuntimeException. "abort")))))

(defn copy-first [decks]
  (mapv #(queue (take (peek %) (pop %))) decks))

(defn play-recursive [player-decks]
  (if *debug* (println player-decks))
  (let [give-cards-to-winner (fn [decks winner add-cards]
                               (vec (map-indexed (fn [playerno playerdeck]
                                                   (pop (if (= playerno winner)
                                                          (into playerdeck add-cards)
                                                          playerdeck)))
                                                 decks)))]
    (loop [decks player-decks
           previous-decks (vec (take (count decks) (repeat #{})))]
      (if *debug* (println "   " decks previous-decks))
      (if (some empty? decks)  
        decks
        (if (some true? (map (fn [p-set d]
                               (contains? p-set d))
                             previous-decks
                             decks))
          [(first decks) (queue)] ; special rule, first player always wins
          (let [winner (if (run-recursive? decks)
                         (-> decks
                             copy-first
                             play-recursive
                             has-cards-left)
                         (highest-index (map peek decks)))
                add-cards [(peek (nth decks winner)) ; only works for 2 players
                           (peek (nth decks (- 1 winner)))]]
            (recur (give-cards-to-winner decks winner add-cards)
                   (mapv (fn [p-set d]
                           (conj p-set d))
                         previous-decks
                         decks))))))))

(defn points [deck]
  (->> deck
       seq
       reverse
       (cons 0)
       (map-indexed *)
       (reduce +)))

(defn score [decks]
  (map points decks))

(defn task-1 [input]
  (->> (parse input)
       play
       score
       ))

(task-1 (slurp "day22.txt"))
(task-1 testdata)

;(def transpose (partial apply mapv #(vec %&)))

(defn task-2 [input]
  (->> (parse input)
       play-recursive
       score))

(task-2 testdata)

(binding [*debug* false]
  (task-2 "Player 1:
43
19

Player 2:
2
29
14"))

(task-2 (slurp "day22.txt"))


(->> [(queue [5 7 9 3 1 11]) (queue [2 4 6 8 10])]
     copy-first
     )

(mapv #(queue (take (peek %) (pop %)))
      [(queue [5 1 2 3 4 5 6 7 8])
       (queue [2 1 2 3 4])])

(seq (queue [1]))

