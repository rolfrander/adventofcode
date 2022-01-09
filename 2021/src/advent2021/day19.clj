(ns advent2021.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse [lines]
  (letfn [(parse-num [str] (Long/parseLong str))
          (parse-point [numbers-as-strings scanner point-number]
            (-> (map parse-num numbers-as-strings)
                (conj (vector scanner point-number))
                (conj :point)
                vec))]
    (->> (reduce (fn [[results scanner i] line]
                   (if-let [[_line scan-num] (re-matches #"--- scanner ([0-9]+) ---" line)]
                     [(assoc results scan-num []) scan-num 0]
                     (if-let [match-numbers (re-matches #"(-?[-0-9]+),(-?[-0-9]+),(-?[-0-9]+)" line)]
                       [(update results scanner conj (parse-point (rest match-numbers) scanner i)) scanner (inc i)]
                       [results scanner i])))
                 [{} "unknown" 0]
                 lines)
         first)))

;; all rotation vectors
;;     0  1  0   -1  0  0   -1  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0
;;     1  0  0    0  0  1    0  1  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0
;;     0  0 -1    0  1  0    0  0 -1    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0    0  0  0

;; sin cos
;;  0   1
;;  1   0
;;  0  -1
;; -1   0

(defn matrix-multiply [a b]
  (let [[a1 a2 a3
         a4 a5 a6
         a7 a8 a9] a
        [b1 b2 b3
         b4 b5 b6
         b7 b8 b9] b]
    [(+ (* a1 b1) (* a2 b4) (* a3 b7)) (+ (* a1 b2) (* a2 b5) (* a3 b8)) (+ (* a1 b3) (* a2 b6) (* a3 b9))
     (+ (* a4 b1) (* a5 b4) (* a6 b7)) (+ (* a4 b2) (* a5 b5) (* a6 b8)) (+ (* a4 b3) (* a5 b6) (* a6 b9))
     (+ (* a7 b1) (* a8 b4) (* a9 b7)) (+ (* a7 b2) (* a8 b5) (* a9 b8)) (+ (* a7 b3) (* a8 b6) (* a9 b9))]))

;(matrix-multiply [1 2 3
;                  4 5 6
;                  7 8 9]
;                 [10 11 12
;                  13 14 15
;                  16 17 18])

(def all-transforms
  (let [matr
        (for [[s c] [[0 1] [1 0] [0 -1] [-1 0]]
              [l m n] [[1 0 0] [0 1 0] [0 0 1]]]
          [(+ (* l (- 1 c)) c)         (- (* n s))         (* m s)
           (* n s) (+ (* m (- 1 c)) c)      (- (* l s))
           (- (* m s))        (* l s)     (+ (* n (- 1 c)) c)])
        mat-map
        (into {} (map vector [:rot0-x :rot0-y :rot0-z :rot1-x :rot1-y :rot1-z :rot2-x :rot2-y :rot2-z :rot3-x :rot3-y :rot3-z]
                      matr))
        mult (fn [a b] (matrix-multiply (mat-map a) (mat-map b)))]
    (->> (for [first-transform [:rot0-x :rot1-x :rot2-x :rot3-x]
               second-transform [:rot0-y :rot1-y :rot2-y :rot3-y :rot1-z :rot3-z]]
           [first-transform second-transform])
         (map #(apply mult %)))))

;(doseq [o [0 3 6]]
;  (doseq [m all-transforms]
;    (print (format "  %2d %2d %2d" (get m o) (get m (+ 1 o)) (get m (+ 2 o)))))
;  (newline))

(defn make-vector [point1 point2]
  (assert (and (= (first point1) :point)
               (= (first point2) :point)) "arguments has to be points")
  (letfn [(calc-length [v]
            (->> (map #(* % %) (nthrest v 3))
                 (apply +)
                 ;(Math/sqrt)
                 (conj v)))]

    (-> (map - (nthrest point2 2) (nthrest point1 2))
        (conj (second point2))
        (conj (second point1))
        (conj :vector)
        vec
        calc-length)))

(defn vector-length [v]
  (assert (= (first v) :vector) "argument has to be vector")
  (peek v))

(defn vector-dot [v1 v2]
  (assert (and (= (first v1) :vector)
               (= (first v2) :vector) "arguments has to be vectors"))
  (let [[_v1 _p11 _p12 x1 y1 z1 _l1] v1
        [_v2 _p21 _p22 x2 y2 z2 _l2] v2]
    (+ (* x1 x2)
       (* y1 y2)
       (* z1 z2))))

(defn vector-transform [v tx]
  (assert (= (first v) :vector) "argument has to be vector")
  (let [[_v from to x y z len] v
        [t1 t2 t3
         t4 t5 t6
         t7 t8 t9] tx]
    [:vector from to 
     (+ (* x t1) (* y t2) (* z t3))
     (+ (* x t4) (* y t5) (* z t6))
     (+ (* x t7) (* y t8) (* z t9))
     len])
  )

(defn point-sensor [p]
  (assert (= (first p) :point) "argument has to be point")
  (let [[_p [sensor _number] _x _y _z] p]
    sensor))
  

(defn point-transform [p tx new-sensor]
  (assert (= (first p) :point) "argument has to be point")
  (let [[_p [_sensor _number] x y z] p
        [t1 t2 t3
         t4 t5 t6
         t7 t8 t9] tx]
    [:point [new-sensor -1]
     (+ (* x t1) (* y t2) (* z t3))
     (+ (* x t4) (* y t5) (* z t6))
     (+ (* x t7) (* y t8) (* z t9))
     ]))


(defn vector-reverse [v]
  (assert (= (first v) :vector) "argument has to be vector")
  (let [[_v from to x y z len] v]
    [:vector to from (- x) (- y) (- z) len]))

(defn vector-congruent [v1 v2]
  (assert (and (= (first v1) :vector)
               (= (first v2) :vector) "arguments has to be vectors"))
  (let [[_v1 _p11 _p12 x1 y1 z1 _l1] v1
        [_v2 _p21 _p22 x2 y2 z2 _l2] v2
        =- (fn [a b] (= a (- b)))]
    (or (and (= x1 x2)(= y1 y2)(= z1 z2))
        (and (=- x1 x2)(=- y1 y2)(=- z1 z2)))))

(defn point-diff [p1 p2]
  (assert (and (= (first p1) :point)
               (= (first p2) :point) "arguments has to be points"))
  (let [[_v1 _n1 x1 y1 z1] p1
        [_v2 _n2 x2 y2 z2] p2]
  [(- x2 x1)(- y2 y1)(- z2 z1)]
    ))

(defn point-manhattan [p1 p2]
  (assert (and (= (first p1) :point)
               (= (first p2) :point) "arguments has to be points"))
  (let [[_v1 _n1 x1 y1 z1] p1
        [_v2 _n2 x2 y2 z2] p2]
    (+ (Math/abs (- x2 x1))
       (Math/abs (- y2 y1))
       (Math/abs (- z2 z1)))))

(defn point-translate [p tx]
  (assert (and (= (first p) :point) "arguments has to be a point"))
  (let [[_v1 n1 x y z] p
        [dx dy dz] tx]
    [:point n1 (+ x dx) (+ y dy) (+ z dz)]))

(defn vector-sensor [v]
  (assert (= (first v) :vector) "argument has to be vector")
  (let [[_v from _to _x _y _z _len] v]
    (first from)))

(defn vector-from-point [v]
  (assert (= (first v) :vector) "argument has to be vector")
  (nth v 1))

(defn vector-to-point [v]
  (assert (= (first v) :vector) "argument has to be vector")
  (nth v 2))

(defn get-point [data point-id]
  (get-in data point-id))

(defn vector-angle [v1 v2]
  (Math/acos (/ (vector-dot v1 v2)
                (* (vector-length v1) (vector-length v2)))))

; (vector-dot [:vector 3 4 7] [:vector -2 3 -5]) ; = -29
(* (/ 180 Math/PI) (vector-angle (make-vector [:point "p1" 0 0 0] [:point "p2" 3 4 7])
                                 (make-vector [:point "p1" 0 0 0] [:point "p2" -2 3 -5])))


(for [[a & x] (iterate rest [1 2 3 4 5 6])
      :while (not (nil? x))
      b x]
  [a b])

(defn calc-all-vectors [scanner-points]
  (for [[a & sp] (iterate rest scanner-points)
        :while (not (nil? sp))
        b sp]
    (make-vector a b)))

(def testdata1 (parse (line-seq (io/reader "resources/day19-test1.txt"))))

(def data (parse (line-seq (io/reader "resources/day19.txt"))))

;;; task-1

(let [data data
      vectors (reduce (fn [all-vectors scanner]
                        (concat all-vectors (calc-all-vectors (data scanner))))
                      []
                      (keys data))
      vectors-by-length (reduce (fn [vectors-by-length v]
                                  (let [l (vector-length v)]
                                    (update vectors-by-length l #(conj % v))))
                                {}
                                vectors)
      get-all-points-from-vectors (fn [vectors] (map #(vector (vector-from-point %)
                                                              (vector-to-point %))
                                                     vectors))
      points-seen-already (fn [seen point-pairs] (reduce (fn [max pair]
                                                           (let [[s1 s2] (map seen pair)]
                                                             (cond (and s1 s2) (reduced 2)
                                                                   (or s1 s2) 1
                                                                   :else max)))
                                                         0 point-pairs))
      save-points (fn [seen point-pairs] (reduce (partial apply conj)
                                                 seen point-pairs))



      construct-translate (fn [v1 v2 tx]
                            (let [[p1a p1b] (map #(get-point data (% v1)) [vector-from-point vector-to-point])
                                  [p2a p2b] (map #(get-point data (% v2)) [vector-from-point vector-to-point])
                                  p2at (point-transform p2a tx (point-sensor p1a))
                                  p2bt (point-transform p2b tx (point-sensor p1b))
                                  translate-cis1 (point-diff p2at p1a)
                                  translate-cis2 (point-diff p2bt p1b)
                                  translate-trans1 (point-diff p2bt p1a)
                                  translate-trans2 (point-diff p2at p1b)]
                              (if (= translate-cis1 translate-cis2)
                                [tx translate-cis1]
                                (if (= translate-trans1 translate-trans2)
                                  [tx translate-trans1]
                                  (throw (RuntimeException. (str "no translation found " v1 v2)))))))
      guess-transforms (fn [vectors]
                         (when (> (count vectors) 1)
                           (for [v1 vectors
                                 v2 vectors
                                 :when (not= v1 v2)]
                             (some #(when (vector-congruent v1 (vector-transform v2 %))
                                      [(vector-sensor v2) (vector-sensor v1) (construct-translate v1 v2 %)])
                                   all-transforms))))
      rot-transforms (let [tmpres (->> vectors-by-length
                                       (sort-by #(count (second %)) >)
                                       (mapcat #(guess-transforms (second %)))
                                       (filter identity)
                                       sort
                                       dedupe)
                           name (fn [tx] (str (first tx) "-" (second tx)))
                           from-to-pair-count (->> (map name tmpres)
                                                   (reduce (fn [res name]
                                                             (if (contains? res name)
                                                               (update res name inc)
                                                               (assoc res name 1)))
                                                           {}))]
                       (->> (filter #(= 1 (from-to-pair-count (name %))) tmpres)
                            (group-by second)))

      transform-all-points (fn [pointlist transforms]
                             (let [target-sensor (get-in transforms [0 1])
                                   tx (reduce (fn [acc [from _to transform]]
                                                (assoc acc from transform))
                                              {} transforms)]
                               (for [point pointlist]
                                 (let [sensor (get-in point [1 0])]
                                   (if (contains? tx sensor)
                                     (-> point
                                         (point-transform (first (tx sensor)) target-sensor)
                                         (point-translate (second (tx sensor))))
                                     point)))))]
     ;; task 1
  (comment ->> (reduce (fn [d tx] (transform-all-points d tx))
               (apply concat (vals data)) (vals rot-transforms))
          ;(as-> d (transform-all-points d (rot-transforms "0")))
       (map #(nthrest % 2))
       (into #{})
       count)
  
  ;; task 2
  ;; define origo in all scanners, transform everything to the same system
  (let [all-origo (for [i (range 38)]
                    [:point [(str i) -1] 0 0 0])
        translated-origo (reduce (fn [d tx] (transform-all-points d tx))
                                 all-origo (vals rot-transforms))]
    (reduce (fn [m cur] (max m cur)) 0
            (for [[p1 & points] (iterate rest translated-origo)
                  :while (not (nil? p1))
                  p2 points]
              (point-manhattan p1 p2))))
  
  )

(-> [:point "0" 686 422 578]
    (point-transform [-1 0 0 0 1 0 0 0 -1] "point 1-0 moved to 0")
    (point-translate [68 -1246 -43]))

(let [p1 [:point "2" 5 6 -4]
      v1 (make-vector [:point "1" 0 0 0]
                      p1)
      p2 [:point "2" -2 8 -1]
      v2 (make-vector [:point "1" 3 4 5]
                      p2)
      tx (some #(when (vector-congruent v1 (vector-transform v2 %)) %) all-transforms)
      tl (-> p2
             (point-transform  tx "foo")
             (point-diff p1))]
  (-> p2
      (point-transform tx "bar")
      (point-translate tl)
      ;(vector-transform v2 tx)
      )
  )

(comment
  (vector-length (apply make-vector (take 2 (get testdata1 "0"))))

  (group-by vector-from-point (calc-all-vectors (testdata1 "0")))
  (->> (calc-all-vectors (testdata1 "2"))
       (sort-by vector-length)
       (reduce (fn [[result seen] v]
                 (let [from (vector-from-point v)
                       to (vector-to-point v)]
                   (if-not (or (seen from)
                               (seen to))
                     [(conj result v) (conj seen from to)]
                     [result seen])))
               [[] #{}]))

  (vector-angle [:vector "4-2" "4-21" 366 -7 948 1016.2229086179863] [:vector "4-9" "4-21" 341 168 999 1068.880722999531])
  (vector-angle [:vector "2-17" "2-24" -604 -648 -498 1016.2302888617323] [:vector "3-11" "3-14" -647 -614 589 1068.8900785394164]))
