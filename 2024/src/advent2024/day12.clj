(ns advent2024.day12
  (:require
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.string :as str]))

(def testdata "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(def testdata2 "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO")

(def testdata3 "AAAA
BBCD
BBCC
EEEC")

(def neighbours (puzzle/neighbours-fn :sq-4 :infinite))

(defn paint-area [cell l visited]
  (let [n (into #{} (neighbours cell)) ; all possible neighbours
        n (->> (filter n l)
               (remove visited)) ; first neighbour not visited
        ]
    (reduce #(paint-area %2 l (conj %1 %2))
            (conj visited cell)
            n)))

(defn partition-area [l]
  (->> (iterate (fn [[l _p]] (if (empty? l) nil
                                 (let [v (paint-area (first l) l #{})
                                       m (remove v l)]
                                   [m v])))
                [l #{}])
       (drop 1)
       (map second)
       (take-while not-empty)))

(defn solve-1 [in]
  (let [m (puzzle/parse-map in)
        area (fn [l] (count l))
        count-adjacent-cells (fn [l cell] (-> (into #{} (neighbours cell))
                                              (filter l)
                                              count))
        price (fn [l] (let [a (area l)
                            adjacent (reduce + (map (partial count-adjacent-cells l) l))]
                        (* (- (* 4 a) adjacent)
                           a)))]

    (->> (mapcat partition-area (vals (m :markings)))
         (map price)
         (reduce +))))

;; corner-cases
;; XX   .X X. XX XX   .. X. XX .X .X X.   .. .. X. .X   ..
;; XX   XX XX X. .X   XX X. .. .X X. .X   .X X. .. ..   ..

(defn solve-2 [in]
  (let [m (puzzle/parse-map in)
        w (:width m)
        h (:height m)
        corner-matrix (for [x (range -1 (inc w))
                            y (range -1 (inc h))]
                        #{[x y] [x (inc y)] [(inc x) y] [(inc x) (inc y)]})
        count-corners (fn [c] (case (count c)
                                0 0
                                1 1
                                2 (if (= 2 (apply + (apply map (comp abs -) c))) 2 0)
                                3 1
                                4 0
                                0))
        count-all-corners (fn [x] (->> (map #(filter % x) corner-matrix)
                                       (remove empty?)
                                       (map count-corners)
                                       (reduce +)))
        area (fn [l] (count l))
        price (fn [l] (* (area l) (count-all-corners l)))]
    (->> (mapcat partition-area (vals (m :markings)))
         (map price)
         (reduce +))))



; probably not helpful...
(defn count-corners [in]
  (let [lines (str/split-lines in)
        pairs-of-lines (partition 2 1 lines)
        cc-internal (fn [l1 l2 y corners]
                       (loop [[[a b] & l1] (partition 2 1 l1)
                              [[c d] & l2] (partition 2 1 l2)
                              x 0
                              corner-set corners]
                         (if (nil? a)
                           corner-set
                           (let [uniq (conj #{} a b c d)]
                             (recur l1 l2 (inc x)
                                    (cond (> (count uniq) 2) (conj corner-set [x y])
                                          (= (count uniq) 1) corner-set
                                          (or (and (= a b) (= c d))
                                              (and (= a c) (= b d))) corner-set
                                          :else (conj corner-set [x y])
                                          ))))))]
    (loop [[[l1 l2] & lines] pairs-of-lines
           y 0
           corners #{}]
      (if (nil? l1)
        corners
        (recur lines (inc y) (cc-internal l1 l2 y corners))))
    ))


(solve-1 testdata3)

(solve-1 (puzzle/get-data 2024 12))
;;=> 1450422

(solve-2 testdata)
(solve-2 (puzzle/get-data 2024 12))
;;=> 906606
