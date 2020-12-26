(ns advent2015.day18
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)

(def testdata ".#.#.#
...##.
#....#
..#...
#.#..#
####..")

(def ^:dynamic *dimensions* 6)

(defn parse [input]
  (->> (for [[y line] (map vector (range) (str/split-lines input))
             [x char] (map vector (range) line)
             :when (= char \#)]
         [x y])
       (into #{})))


(def neighbours [[-1 -1] [0 -1] [1 -1]
                 [-1  0]        [1  0]
                 [-1  1] [0  1] [1  1]])

;;;; task 1
(defn get-neighbours [[x y]]
  (->> (map (fn [[xx yy]] [(+ x xx) (+ y yy)]) neighbours)
       (filter (fn [[xx yy]]
                 (and (>= xx 0)
                      (>= yy 0)
                      (< xx *dimensions*)
                      (< yy *dimensions*))))))

(defn count-lit-neighbours 
  "we only care about 2, 3, more"
  [lit point]
  (->> (filter lit (get-neighbours point))
       (take 4)
       count))

(defn always-on [dimensions]
  (let [d (dec dimensions)]
    #{[0 0] [0 d] [d 0] [d d]}))

(defn evolve [lit]
  (let [turn-off (->> lit
                      (map #(vector % (count-lit-neighbours lit %)))
                      (filter #(#{0,1,4} (second %)))
                      (map first)
                      set)
        turn-on (->> (mapcat get-neighbours lit)
                     frequencies
                     (filter #(= 3 (second %)))
                     (map first)
                     (filter #(not (lit %)))
                     set)]
    (-> lit
        (set/difference turn-off)
        (set/union turn-on (always-on *dimensions*)))))

(defn get-dim [function set]
  (mapv #(function (apply % function set))
        [min-key max-key]))

(defn display [board]
  (let [y-range (range 0 *dimensions*)
        x-range (range 0 *dimensions*)]
    (doseq [y y-range]
      (->> x-range
           (map #(if (contains? board [% y]) \# \.))
           str/join
           println))))

(defn task-1 [data iterations]
  (let [initial (set/union (parse data) (always-on *dimensions*))
        game-sequence (iterate evolve initial)
        state-after-n (nth game-sequence iterations)]
    (when *debug* (display state-after-n))
    (count state-after-n)))

(binding [*debug* true]
  (task-1 testdata 5))

(binding [*dimensions* 100]
  (task-1 (slurp "resources/2015/day18.txt") 100))
;; => 821
