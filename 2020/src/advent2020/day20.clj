(ns advent2020.day20
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def testdata "Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...")

(def seamonster 
"                  # 
#    ##    ##    ###
 #  #  #  #  #  #   ")

(defn get-coordinates [data border]
  (for [y (range border (- (count data) border))
        :let [li (nth data y)]
        x (range border (- (count li) border))
        :let [ch (nth li x)]
        :when (= ch \#)]
    [(- x border) (- y border)]))

(def seamonster-data
  (let [data (str/split-lines seamonster)]
    (set (get-coordinates data 0))))

(def ^:dynamic *width* 10)

(defn factorial [i]
  (case i
    1 1
    2 2
    3 6
    (* i (factorial (dec i)))))

(defn shift ^long [^long a ^long n]
  (bit-or (bit-shift-left a 1) n))

(defn bit-reverse ^long [^long bitmap]
  (loop [i 0
         from bitmap
         to 0]
    (if (= i *width*)
      to
      (recur (inc i) (bit-shift-right from 1) (shift to (bit-and from 0x01))))))

(defn to-string [{:keys [n e s w id]}]
  (format "<%d: %d,%d,%d,%d>" id n e s w))


(def identity-matrix [1 0 0
                      0 1 0
                      0 0 1])

(defn rotate-matrix [matrix]
  (let [[m1 m2 m3
         m4 m5 m6
         m7 m8 m9] matrix]
    [(- (* 7 m7) m4) (- (* 7 m8) m5) (- (* 7 m9) m6)
     m1              m2              m3
     m7              m8              m9]))

(defn flip-matrix [matrix]
  (let [[m1 m2 m3 
         m4 m5 m6 
         m7 m8 m9] matrix]
    [m4 m5 m6
     m1 m2 m3
     m7 m8 m9]))

(defn transform-coords [matrix coords]
  (let [[ m1  m2  m3 
          m4  m5  m6
         _m7 _m8 _m9] matrix
        [x y] coords]
    [(+ (* m1 x) (* m2 y) m3)
     (+ (* m4 x) (* m5 y) m6)]))

(defn shift-coords [coords offset-x offset-y]
  (let [[x y] coords]
    [(+ x offset-x) (+ y offset-y)]))

(defn min-dimension [dimension data]
  (dimension (apply min-key dimension data)))

(defn max-dimension [dimension data]
  (dimension (apply max-key dimension data)))

(defn get-bouding-box [coord-set]
 (let [min-x (min-dimension first coord-set)
       max-x (inc (max-dimension first coord-set))
       min-y (min-dimension second coord-set)
       max-y (inc (max-dimension second coord-set))]
   [min-x max-x min-y max-y]))

(defn transform-and-orient [matrix coord-set]
  (let [transformed (map #(transform-coords matrix %) coord-set)
        offset-x (- (min-dimension first transformed))
        offset-y (- (min-dimension second transformed))]
    (set (map #(shift-coords % offset-x offset-y)
              transformed))))

(defn get-coords [tile offset-x offset-y]
  (map #(shift-coords (transform-coords (:tx tile) %)
                      offset-x offset-y)
       (:coords tile)))

(defn rotate [{:keys [n e s w id coords tx]}]
  {:id id
   :n (bit-reverse w)
   :e n
   :s (bit-reverse e)
   :w s
   :coords coords
   :tx (rotate-matrix tx)
   })

(defn flip [{:keys [n e s w id coords tx]}]
  {:id id
   :n w
   :e s
   :s e
   :w n
   :coords coords
   :tx (flip-matrix tx)
   })

(defn permute [tile]
  (concat
   (take 4 (iterate rotate tile))
   (take 4 (iterate rotate (flip tile)))))

(defn permute-seamonster [coords]
  (->> (concat
        (take 4 (iterate rotate-matrix identity-matrix))
        (take 4 (iterate rotate-matrix (flip-matrix identity-matrix))))
       (map #(transform-and-orient % coords))
       ))

(comment binding [*width* 4]
  (print-table [:id :n :e :s :w :tx]
               (permute {:id 42 :n 1 :e 13 :s 7 :w 4 :tx identity-matrix :coords []})))

(comment binding [*width* 4]
  (-> {:id 42 :n 1 :e 13 :s 7 :w 4}
      flip 
      flip
      rotate
      rotate
      ))

(defn <<= [min cur max]
  (and (<= min cur)
       (<= cur max)))

(defn parse-tile [input]
  (let [parse-line (fn [line] (reduce (fn [r ch]
                                        (shift r (if (= ch \#) 1 0)))
                                      0
                                      line))
        [heading & data] (str/split-lines input)
        [_ id] (re-matches #"Tile ([0-9]+):" heading)

        tile-edges (loop [n 0
                          e 0
                          s 0
                          w 0
                          i 0
                          [line & lines] data]
                     (if (= i (count data))
                       {:id (Integer/parseInt id)
                        :n n
                        :e e
                        :s s
                        :w w}
                       (let [all-bits (parse-line line)
                             first-bit (bit-shift-right (bit-and all-bits 0x200) 9)
                             last-bit  (bit-and all-bits 1)]
                         (case i
                           0 (recur all-bits last-bit 0 first-bit (inc i) lines)
                           9 (recur n (shift e last-bit) all-bits (shift w first-bit) (inc i) lines)
                           (recur n (shift e last-bit) s (shift w first-bit) (inc i) lines)))))

        tile-contents (doall (get-coordinates data 1))]
    (-> tile-edges
        (assoc :coords tile-contents)
        (assoc :tx identity-matrix))))

(defn index-tiles [tiles]
  (into {} (map (fn [direction]
                  [direction (group-by direction tiles)])
                [:n :e :s :w]))
  )

(defn parse-tiles [input-string]
  (let [tilestrings (str/split input-string #"\n\n")]
    (->> tilestrings
         (map parse-tile))))

(defn place-tiles [input-string]
  (let [all-tiles (parse-tiles input-string)
        tiles (->> all-tiles (mapcat permute) index-tiles)
        dimensions (Math/sqrt (count all-tiles))]

    (letfn [(find-matching-tile [northtile westtile]
                                (cond
                                  (and northtile westtile)
                                  (filter #(= (:e westtile) (:w %))
                                          (get-in tiles [:n (:s northtile)]))

                                  northtile (get-in tiles [:n (:s northtile)])
                                  westtile  (get-in tiles [:w (:e westtile)])))

            (place-tiles-internal  [used board tileno]
                                   (let [x (int (mod tileno dimensions))
                                         y (int (quot tileno dimensions))]
                                     (if (>= y dimensions)
                                       board
                                       (->> (cond
                                              (= y 0) (find-matching-tile nil (get-in board [y (dec x)]))
                                              (= x 0) (find-matching-tile (get-in board [(dec y) x]) nil)
                                              :else   (find-matching-tile (get-in board [(dec y) x])
                                                                          (get-in board [y (dec x)])))
                                            (remove #(contains? used (:id %)))
                                            (some #(place-tiles-internal (conj used (:id %))
                                                                         (assoc-in board [y x] %)
                                                                         (inc tileno)))))))
            ]

      (let [startboard (vec (repeat dimensions (vec (repeat dimensions nil))))]
        
        ; we have noe tiles, just start at the top, picking the first      
        (loop [[starttile & othertiles] all-tiles]
          (if-let [result (place-tiles-internal #{starttile} (assoc-in startboard [0 0] starttile) 1)]
            result
            (if (empty? othertiles)
              nil
              (recur othertiles))))))))

(defn print-tiles [tiles]
  (doseq [row tiles]
    (println (mapv #(:id %) row))))

(defn display-coords [skip-each coord-set]
  (let [[min-x max-x min-y max-y] (get-bouding-box coord-set)]
    (println "bounding box" [min-x min-y] [max-x max-y])
    (doseq [row (range min-y max-y)]
      (if (= 0 (mod row skip-each)) (println))
      
      (->> (range min-x max-x) 
           (map #(if (contains? coord-set [% row]) \# \.))
           (partition-all skip-each) 
           (map str/join) 
           (str/join " ")
           (println)))))

(defn display-tile [tile]
  (let [c (into #{} (get-coords tile 0 0))]
    (println "Tile" (:id tile))
    (display-coords c)
    (println)))

(let [t (parse-tile testdata)
      u (rotate (rotate t))]
  (display-tile t)
  (display-tile u))

(place-tiles (slurp "day20-test.txt"))

(display-coords 100 seamonster-data)

(defn task-1 [data]
  (let [board (place-tiles data)
        dim (count board)
        max-coord (dec dim)]
    (print-tiles board)
    (println "task 1" (apply * (map #(get-in board (conj % :id))
                                    [[0 0] [0 max-coord] [max-coord 0] [max-coord max-coord]])))
    (->> (for [x (range 0 dim)
               y (range 0 dim)]
           (get-coords (get-in board [y x]) (* x 8) (* y 8)))
         (reduce into #{})
         )))

(defn count-seamonsters [map-coords max-x max-y sm-coords]
  (let [[_x sm-w _y sm-h] (get-bouding-box sm-coords) ; assume seamonster starts at (0,0)
        ]
    (->> (for [x (range 0 (inc (- max-x sm-w)))
               y (range 0 (inc (- max-y sm-h)))]
           [x y])
         (map (fn [[x y]] (map #(shift-coords % x y) sm-coords)))
         (filter (fn [sm] (every? #(contains? map-coords %) sm)))
         count
         )))


(defn task-2 [input]
  (let [map-coords (task-1 input)
        seamonsters (permute-seamonster seamonster-data)
        [x1 x2 y1 y2] (get-bouding-box map-coords)
        seamonster-search-result (map #(count-seamonsters map-coords x2 y2 %)
                                     seamonsters)]
    (println "result of seamonster search" seamonster-search-result)
    (println "number of waves in map" (count map-coords))
    (println "size of seamonster" (count seamonster-data))
    (println "waves that are not seamonster" (- (count map-coords) (* (apply max seamonster-search-result)
                                                                    (count seamonster-data))))))
  
(task-2 (slurp "day20-test.txt"))
(task-2 (slurp "day20.txt"))


(every? #(do (println "testing" %) (odd? %)) [1 2 3 5 7])