(ns advent2022.day17
    (:require [clojure.set :as set]
              [clojure.string :as str]
              [rolfrander.puzzle-lib :as puzzle]
              [cfft.core :as cfft]))

(def rocks-data ["####"
".#.
###
.#."

"..#
..#
###"

"#
#
#
#"

"##
##"])

(def data (str/replace (puzzle/get-data 2022 17)
                       #"[^<>]" ""))

(def testdata ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def ^:dynamic *debug* false)

(defn rock-x [^long rock] (bit-shift-right rock 32))
(defn rock-y [^long rock] (bit-and rock 0xffffffff))

(defn- bit-coords [^long x ^long y]
  (bit-or (bit-shift-left x 32)
          (bit-and y 0xffffffff)))

;((juxt rock-x rock-y) (bit-coords 1 -1))

(defn parse-rock [input]
  (let [lines (str/split-lines input)
        height (count lines)
        rock (->> (for [[y line] (map-indexed vector lines)
                        [x char] (map-indexed vector line)
                        :when (= char \#)]
                    ;[x (- 0 y)]
                    (bit-coords x (- height y 1))
                    )
                  ;(into #{})
                  )
        ;height (inc (- (apply min (map rock-y rock))))
        width (inc (apply max (map rock-x rock)))]
    [rock width height]))


; rock-bitmap
;    width    row 3    row 2    row 1    row 0
; 0x xxxxxxx0 00000000 00000000 00000000 00000000
; wall-bitmap
; 01 00000001 00000000 00000000 00000000 00000000

; the width-bitmap contains a string of 1 representing the total with of the rock
; the first two bits are 0, representing the starting position of the rock two positions
; from the left wall
;(doseq [b (map #(Long/toString % 2) [32 48 56 60 62])]
;  (println b))


(def wall-bitmap (bit-shift-left (Long/parseLong "100000001" 2)
                                 (* 4 8)))

(def rock-bitmap-mask 0xffffffff)

;(Long/toString wall-bitmap 2)

(defn parse-rock-bitmap [input]
  (let [lines (str/split-lines input)
        width-bitmap (bit-shift-left (get [0 32 48 56 60 62 63] (count (first lines)))
                                     (* 4 8))
        height (count lines)
        rock (for [[y line] (map-indexed vector lines)
                   [x char] (map-indexed vector line)
                   :when (= char \#)]
               [x (- height y 1)])
        bit-rock (reduce #(bit-set %1 (+ (- 4 (first %2))
                                         (* 8 (second %2))))
                         width-bitmap
                         rock)]
    bit-rock))

(defn print-rock-bitmap [bitmap]
  (printf "%10s\n" (Long/toString (bit-shift-right bitmap (inc (* 4 8))) 2))
  (doseq [sr [24 16 8 0]]
    (printf "%10s\n" (Long/toString (bit-and (bit-shift-right bitmap sr)
                                             0xff)
                                    2))))

(def rocks-bitmap (doall (map parse-rock-bitmap rocks-data)))
(def rocks (doall (map parse-rock rocks-data)))

;(doseq [r rocks]
;  (print-rock-bitmap r)
;  (println))

(defn move-rock [^long dx ^long dy rock]
  (map (fn [r]
         (bit-coords (+ (rock-x r) dx)
                     (+ (rock-y r) dy)))
       rock))

(defn max-y [coords]
  (if (empty? coords)
    0
    (apply max (map rock-y coords))))

(def ^:dynamic *print-height* nil)

(defn print-rocks [rocks rock]
  (let [max-y-rocks (max-y rocks)
        max-y-rock  (max-y rock)
        rock (into #{} rock)
        top (max max-y-rock max-y-rocks)]
    (doseq [y (range top (if *print-height*
                           (- top *print-height*)
                           0)
                     -1)]
      (print \|)
      (doseq [x (range 7)]
        (cond (contains? rocks (bit-coords x y)) (print \#)
              (contains? rock (bit-coords x y))  (print \@)
              :else (print \.)))
      (println \| y))
    (println "+-------+")))

(def rock-rows-bitmap
  (->> (iterate #(bit-shift-left %1 8) 0xff)
       (take 4)
       vec))

(defn rock-height-bitmap [rock]
  (loop [bm 0xff
         i 0]
    (if (or (= i 4)
            (= (bit-and rock bm) 0))
      i
      (recur (bit-shift-left bm 8)
             (inc i)))))

(defn print-rocks-bitmap [cave rock rock-offset]
  (let [rock-height (rock-height-bitmap rock)
        c (:cave cave)
        h (:height cave)]
    (loop [y (+ h
                rock-height
                rock-offset)
           i 0]
      (let [rock-byte (let [rock-byte-offset (- (+ h rock-offset rock-height) y 1)]
                        (if (<= 0 rock-byte-offset 3)
                          (bit-and 0xff (bit-shift-right rock (* 8 rock-byte-offset)))
                          0))
            placed-byte (if (>= y h) 0 (aget c (mod y (count c))))]
        (print \|)
        (doseq [x (range 6 -1 -1)]
          (cond (bit-test rock-byte x) (print \@)
                (bit-test placed-byte x) (print \#)
                :else (print \.)))
        (println \| y))
      (when (and (> y (- h (count c)))
                 (or (nil? *print-height*)
                     (< i *print-height*)))
        (recur (dec y) (inc i))))
    (when (<= h (count c))
      (println "+-------+"))))

;(print-rocks-bitmap [(first rocks)] (first rocks) 0)

(defn place-rock [[rock _rock-width rock-height] [placed height wind-seq]]
  (let [move { \< (partial move-rock -1 0)
               \> (partial move-rock 1 0)}
        down (partial move-rock 0 -1)
        valid-pos (fn [r]
                    (and (>= (apply min (map rock-x r)) 0)
                         (< (apply max (map rock-x r)) 7)
                         (> (apply min (map rock-y r)) 0)
                         (every? (comp not placed) r)))]
    (loop [r (move-rock 2
                        (+ height 4)
                        rock)
           [w & wind-seq] wind-seq]
      
      (when (= *debug* :every)
        (println "placing rock")
        (print-rocks placed r)
        )
      (let [; move sideways
            r2 ((move w) r)
            r3 (if (valid-pos r2) r2 r)
          ; move down
            r4 (down r3)]
        (when (= *debug* :every)
          (println)
          (print-rocks placed r3))
        (if (valid-pos r4)
          (recur r4 wind-seq)
          [(into placed r3)
           (apply max height (map rock-y r3))
           wind-seq])))))

(defn seq-to-bitmap [s]
  (loop [[f & s] s
         [bs & bs-rest] [0 8 16 24 32 40 48]
         v 0]
    (if (nil? f)
      v
      (recur s
             bs-rest
             (bit-or v
                     (bit-shift-left f bs))))))


;; => "1000 00000100 00000010 00000001"

(defn new-bitmap-cave [] 
  {:cave (byte-array 64)
   :height 0})

(set! *unchecked-math* :warn-on-boxed)
;(set! *warn-on-reflection* true)


(defn place-rock-bitmap [[cave wind-seq] rock]
  (let [height (rock-height-bitmap rock)

        move {\< #(bit-shift-left % 1)
              \> #(bit-shift-right % 1)}
        c (:cave cave)
        h (:height cave)
        valid-pos (fn [r p offset]
                    (let [ret
                          (and (= (bit-and r wall-bitmap) 0) ; does not intersect walls
                               (= (bit-and r p rock-bitmap-mask) 0)
                               (>= (+ h offset) 0))]
                      (when (= *debug* :valid-pos)
                        (println "compare rock:")
                        (print-rock-bitmap r)
                        (println "  to placed")
                        (print-rock-bitmap p)
                        (println "  ->" ret))
                      ret))
        fix (fn [r offset]
              (when (and (= *debug* :offset) (< offset -27))
                (println "offset" offset))
              (loop [[y & other-y] (range (+ h offset)
                                          (+ h offset height))
                     height h
                     r r]
                (if (nil? y)
                  (assoc cave
                         :height height
                         :cave c)
                  (let [pos (mod y (count c))]
                    (aset-byte c
                               pos
                               (if (>= y height)
                                 (bit-and r 0xff)
                                 (bit-or (aget c pos) (bit-and r 0xff))))
                    (recur other-y
                           (max height y)
                           (bit-shift-right r 8))))))]
    (loop [r rock
           p 0
           offset 3
           [w & wind-seq] wind-seq]

      (when (= *debug* :every)
        (println "placing rock")
        (print-rocks-bitmap cave r offset))
      (let [; move sideways
            r2 ((move w) r)
            r3 (if (valid-pos r2 p offset) r2 r)
          ; move down
            next-offset (dec offset)
            p (let [pos (+ h next-offset)]
                (if (> h pos -1)
                  (bit-or (bit-shift-left p 8)
                          (aget c (mod pos (count c))))
                  0))]
        (when (= *debug* :every)
          (println)
          (print-rocks-bitmap cave r3 next-offset))
        (if (valid-pos r3 p next-offset)
          (recur r3
                 p
                 next-offset
                 wind-seq)
          [(fix r3 offset)
           wind-seq])))))

;(binding [*debug* :every]
;  (print-rocks-bitmap (first (place-rock-bitmap [[60] (drop 4 (cycle testdata))] (second rocks)))
;                      0 0))

(defn task-1 [input iterations period]
  (let [start-time (System/currentTimeMillis)
        period (or period (* (count input) (count rocks)))
        heights
        (loop [[rock & rocks] (cycle rocks)
               state [#{} 0 (cycle (seq input))]
               i 0
               results []]
          (when (= *debug* :state)
            (println "cave")
            (print-rocks (first state) nil))
          (when (= 0 (mod i period))
            (let [t (- (System/currentTimeMillis) start-time)]
              (println i (second state) (quot t 1000) 
                       (when (> i 0) (/ (* 1.0 t (- iterations i))
                                        i)))))
          (if (= i iterations)
            (conj results [i (second state)])
            (recur rocks
                   (try (place-rock rock state)
                        (catch Exception e
                          (println "error in iteration" i)
                          (throw e)))
                   (inc i)
                   (if (= 0 (mod i period))
                     (conj results [i (second state)])
                     results))))]
    ;(println (take 25 wind-seq))
    ;(print-rocks placed nil)
    heights))

(defn task-1b [input iterations period]
  (let [start-time (System/currentTimeMillis)
        period (or period (* (count input) (count rocks-bitmap)))
        heights
        (loop [[rock & rocks] (cycle rocks-bitmap)
               state [(new-bitmap-cave) (cycle (seq input))]
               i 0
               results []]
          (when (= *debug* :state)
            (println "cave")
            (print-rocks-bitmap (first state) 0 0))
          (when (and (= *debug* :timing) (= 0 (mod i period)))
            (let [t (- (System/currentTimeMillis) start-time)]
              (println i (:height (first state)) (quot t 1000)
                       (when (> i 0) (/ (* 1.0 t (- iterations i))
                                        i)))))
          (if (= i iterations)
            (conj results [i (count (first state))])
            (recur rocks
                   (try (place-rock-bitmap state rock)
                        (catch Exception e
                          (println "error in iteration" i)
                          (throw e)))
                   (inc i)
                   (if (= 0 (mod i period))
                     (conj results [i (count (first state))])
                     results))))]
    ;(println (take 25 wind-seq))
    ;(print-rocks placed nil)
    heights
    ))

(defn task-2 [input start period inc-over-period iterations]
  (let [super-iter (quot (- iterations start) period)
        rest (mod (- iterations start) period)
        equiv-iter (+ start rest)] 
    (println "adding" super-iter "periods after" equiv-iter)
    (let [start-value (second (peek (task-1 input equiv-iter nil)))]
      (+ start-value (* super-iter inc-over-period)))))

(defn find-period [iter-heights]
  (let [heights (mapv second iter-heights)
        diffs (mapv - heights (rest heights))
        max-period (quot (- (count diffs) 4) 2)
        is-period (fn [i]
                    (every? #(= 0 (- (get diffs (+ % i))
                                     (get diffs %)))
                            (range 4 (- (count diffs) i))))
        period (first (filter is-period (range 2 max-period)))]
    (if (nil? period)
      nil
      [(first (get iter-heights 4))
       (first (get iter-heights period))
       (- (get heights (+ 4 period))
          (get heights 4))])))

(binding [*debug* nil]
  (->> (task-1b testdata 15000 nil)
       find-period))

(binding [*debug* :timing]
  (peek (task-1b testdata 2022 nil)))

(peek (task-1b data 2022 nil))

(binding [*debug* :timing]  ; 1000000000000
  (def iter-heights (task-1b data (* 50445 50) nil)))
(* 50445 2048)              ;; => 103311360

(count iter-heights)

(find-period (subvec iter-heights 0 4000))


(task-2 testdata        800 1400 2120 1000000000000)
(task-2 data 201820 17356520 27790614 1000000000000)

(quot (- 1000000000000 201820) 17558340)
(mod (- 1000000000000 201820) 17356520)
                         ;; => 3898380
(->> (take-while #(< (first %) 17218500) iter-heights)
     last)

(def freqs
  (let [heights (mapv second (subvec iter-heights 10 2059))
        diffs (mapv - (rest heights) heights)
        diffs (mapv (partial - 79843) diffs)
        length (fn [{:keys [real imag]}]
                 (Math/sqrt (+ (* real real)
                               (* imag imag))))
        ]
    
    (->>  ; (map #(Math/cos (/ (* Math/PI %) 8)) (range 512))
     diffs
     cfft/fft
     (cfft.matrix/matrix-apply length)
   ;(keep-indexed #(when (> %2 500.0) %1))
     )))

;(apply * (keep-indexed #(when (and (not= 0 %1) (> %2 400)) %1) freqs))

(doseq [x (map #(Math/sin (/ (* Math/PI %) 32)) (range 64))]
  (println (subs "-----------------------------" 0 (* 10 (+ 1.2 x)))))



(* (count rocks-bitmap)
   (count data))
(Long/MAX_VALUE)
(Integer/MAX_VALUE)