(ns advent2022.day09
    (:require [clojure.set :as set]
              [clojure.string :as str]
              [rolfrander.puzzle-lib :as puzzle]))

(def testdata "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def directions {\R "e"
                 \L "w"
                 \U "n"
                 \D "s"})

(defn parse [input]
  (->> (str/split-lines input)
       (map #(vector (directions (first %))
                     (puzzle/str->long (subs % 2))))))

(defn abs [x]
  (if (< x 0) (- x) x))

(defn sign [x]
  (cond (< x 0) -1
        (> x 0) 1
        :else 0))

(defn move-tail [[head-x head-y] [tail-x tail-y :as tailpos]]
  (let [diff-x (- head-x tail-x)
        diff-y (- head-y tail-y)]
    (cond (and (<= (abs diff-x) 1)
               (<= (abs diff-y) 1))
          tailpos

          :else
          [(+ tail-x (sign diff-x)) (+ tail-y (sign diff-y))]
          )))

;(move-tail [4 5] [3 3])

(defn print-rope [head tail-seq w h]
  (let [board (vec (repeat h (vec (repeat w \.))))
        board (reduce #(assoc-in %1 (reverse %2) \T)
                      board
                      tail-seq)
        board (assoc-in board (reverse head) \H)]
    (println)
    (doseq [r (rseq board)]
      (println (str/join r)))))

;(print-rope [1 0] [[0 0]] 6 5)

(def ^:dynamic *debug* false)

(defn move-around-1 [data]
  (let [move (puzzle/move-fn :sq-4 :infinite)]
    (loop [head-pos [0 0]
           tail-pos [0 0]
           [dir cnt] (first data)
           moves (rest data)
           collect-tail #{}]
    ;(println head-pos tail-pos)
      ;(if (empty? moves)
       ; (println cnt head-pos tail-pos))
      (cond (nil? dir)
            ;collect-tail
            (count collect-tail)

            (= cnt 0)
            (recur head-pos tail-pos (first moves) (rest moves) collect-tail)

            :else
            (let [head-pos (move head-pos dir)
                  tail-pos (move-tail head-pos tail-pos)]
              (recur head-pos
                     tail-pos
                     [dir (dec cnt)]
                     moves
                     (conj collect-tail tail-pos)))))))

(defn move-around [data tail-length]
  (let [move (puzzle/move-fn :sq-4 :infinite)]
    (loop [head-pos [0 0]
           tail (repeat tail-length head-pos)
           [dir cnt] (first data)
           moves (rest data)
           collect-tail #{}]
    ;(println head-pos tail-pos)
      (cond (nil? dir)
            ;collect-tail
            (count collect-tail)
            
            (= cnt 0)
            (do
              (when *debug*
                (println)
                (println "==" (first moves) "=="))
              (recur head-pos tail (first moves) (rest moves) collect-tail))
            
            :else
            (let [head-pos (move head-pos dir)
                  [tail last-tail] (reduce (fn [[tail prev] next]
                                             (let [next-moved (move-tail prev next)]
                                               [(conj tail next-moved)
                                                next-moved]))
                                           [[] head-pos]
                                           tail)]
              (when *debug*
                (print-rope head-pos tail 6 5))
              ;(if (empty? moves)
              ;  (println cnt head-pos tail last-tail))
              (recur head-pos
                     tail
                     [dir (dec cnt)]
                     moves
                     (conj collect-tail last-tail)))))))

(defn task-1 [input]
  (move-around (parse input) 1))

(defn task-2 [input]
  (move-around (parse input) 9))


(task-1 testdata)

(let [d (parse (puzzle/get-data 2022 9))
      set-1 (move-around-1 d)
      set-2 (move-around d 1)]
  (println (set/difference set-1 set-2))
  (contains? set-2 [-127 17]))

(task-1 (puzzle/get-data 2022 9))

(task-2 testdata)

(task-2 "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(task-2 (puzzle/get-data 2022 9))
