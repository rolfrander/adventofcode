(ns advent2020.day11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def testdata "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(defn parse [input]
  (->> input
       str/split-lines
       (map-indexed (fn [y line]
                      (map-indexed (fn [x char]
                                     (if (= char \L)
                                       [x y]
                                       nil))
                                   line)
                      ))
       (mapcat identity)
       (remove nil?)
       (into #{})))

(def neighbours [[-1 -1] [0 -1] [1 -1]
                 [-1  0]        [1  0]
                 [-1  1] [0  1] [1  1]])

(defn get-dim [function set]
  (inc (function (apply max-key function set))))

;;;; task 1
(defn find-neighbours [[x y]]
  (into #{} (map (fn [[xx yy]] [(+ x xx) (+ y yy)]) neighbours)))

(defn occupy? [taken-seats position]
  (empty? (set/intersection taken-seats (find-neighbours position))))

(defn leave? [taken-seats position]
  (<= 4 (count (set/intersection taken-seats (find-neighbours position)))))

(defn evolve
  "if there are no changes, return nil"
  [all-seats taken-seats]
  (let [leave-set (set/select (partial leave? taken-seats) taken-seats)
        occupy-set (set/select (partial occupy? taken-seats) (set/difference all-seats taken-seats))]
    (if (and (empty? leave-set)
             (empty? occupy-set))
      nil
      (-> taken-seats
          (set/difference leave-set)
          (set/union occupy-set)))))

;;;; task 2
(defn has-neighbour-direction? [taken-seats free-seats [mx my] [x y] [dx dy]]
  (loop [x (+ x dx)
         y (+ y dy)]
    (if (or (< x 0) (< y 0)
            (>= x mx) (>= y my)
            (contains? free-seats [x y]))
      false
      (if (contains? taken-seats [x y])
        true
        (recur (+ x dx)
               (+ y dy))))))

(defn count-neighbours-all-directions [taken-seats free-seats [x y]]
  (if (empty? taken-seats) 0
      (let [dim [(get-dim first taken-seats)
                 (get-dim second taken-seats)]]
        (->> neighbours
             (map (partial has-neighbour-direction? taken-seats free-seats dim [x y]))
             (remove false?)
             count))))

(defn more-taken-than [taken-seats free-seats taken-count-to position]
  (if (and (> taken-count-to 0)
           (empty? taken-seats))
    false
    (let [dim [(get-dim first taken-seats)
               (get-dim second taken-seats)]]
      (loop [i taken-count-to
             n neighbours]
        (cond (= i 0) true
              (empty? n) false
              
              (has-neighbour-direction? taken-seats free-seats dim position (first n))
              (recur (dec i) (rest n))
              
              :else (recur i (rest n)))))))

(defn occupy-2? [taken-seats free-seats position]
  (= 0 (count-neighbours-all-directions taken-seats free-seats position)))

(defn leave-2? [taken-seats free-seats position]
  (<= 5 (count-neighbours-all-directions taken-seats free-seats position)))

(defn occupy-2? [taken-seats free-seats position]
  (not (more-taken-than taken-seats free-seats 1 position)))

(defn leave-2? [taken-seats free-seats position]
  (more-taken-than taken-seats free-seats 5 position))

(defn evolve-2
  "if there are no changes, return nil"
  [all-seats taken-seats]
  (let [free-seats (set/difference all-seats taken-seats)
        leave-set (set/select (partial leave-2? taken-seats free-seats) taken-seats)
        occupy-set (set/select (partial occupy-2? taken-seats free-seats) free-seats)]
    (if (and (empty? leave-set)
             (empty? occupy-set))
      nil
      (-> taken-seats
          (set/difference leave-set)
          (set/union occupy-set)))))

;;;; common code

(defn display [all-seats taken-seats]
  (let [w (get-dim first all-seats)
        h (get-dim second all-seats)]
    (println "dimentions" [w h])
    (doseq [y (range h)]
      (->> (for [x (range w)]
             (cond (contains? taken-seats [x y]) \#
                   (contains? all-seats   [x y]) \L
                   :else \.))
           (str/join)
           (println)))))

(display (parse testdata) #{})

(time 
 (let [all-seats (parse testdata)]
   (->> #{}
        (evolve all-seats)
        (evolve all-seats)
        (evolve all-seats)
        (evolve all-seats)
        (evolve all-seats)
        (evolve all-seats)
        (evolve all-seats)
       ;(display all-seats)
        )))

(let [all-seats (parse (slurp "day11.txt"))]
     (->> #{}
          (evolve all-seats)
          (evolve all-seats)
          (display all-seats)))

(let [all-seats (parse (slurp "day11.txt")
                       )]
  (loop [taken-seats #{}
         t (System/currentTimeMillis)
         cnt 1]
    (if-let [next (evolve-2 all-seats taken-seats)]
      (recur next 
             (if (> (- (System/currentTimeMillis) t) 1000)
               (do (println (count taken-seats) cnt)
                   (System/currentTimeMillis))
               t)
             (inc cnt))
      (count taken-seats))))


