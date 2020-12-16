(ns advent2020.day14
  (:require [clojure.string :as str]))

(def testdata "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(defprotocol Executable (exec [instr mem mask]))
(defprotocol Mask (apply-mask [mask value]))

(defrecord mask [and-mask or-mask]
   Mask (apply-mask [mask value]
          (bit-or or-mask
                  (bit-and value and-mask)))
  Executable (exec [this mem _mask] [mem this])
  )

(defrecord memset [address value]
  Executable (exec [this mem mask] 
                   [(assoc mem address (apply-mask mask value))
                    mask]))

(defn parse-bitmask [mask]
  (let [shift (fn [a n] (bit-or (bit-shift-left a 1) n))
        [and-mask or-mask]
        (reduce (fn [[a o] char]
                  (case char
                    \0 [(shift a 0) (shift o 0)]
                    \1 [(shift a 1) (shift o 1)]
                    \X [(shift a 1) (shift o 0)]))
                [0 0]
                mask)]
    (mask. and-mask or-mask)))

(def default-mask (parse-bitmask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))

(defn parse-memset [address value]
  (memset. (Integer/parseInt address)
           (Integer/parseInt value)))

(defn parse [input]
  (->> input
       str/split-lines
       (map (fn [line]
              (or (if-let [m (re-matches #"^mask = ([X01]+)" line)] (parse-bitmask (second m)))
                  (if-let [m (re-matches #"^mem.([0-9]+). = ([0-9]+)" line)] (apply parse-memset (rest m))))))
       (remove nil?)))


;; **** Variant, oppgave 2 ****

(defrecord mask-2 [and-mask or-masks]
  Mask (apply-mask [mask value]
                   (for [or-mask or-masks] 
                     (bit-or or-mask
                             (bit-and value and-mask))))
  Executable (exec [this mem _mask] [mem this]))

(defrecord memset-2 [address value]
  Executable (exec [this mem mask]
               [(reduce #(assoc %1 %2 value) mem (apply-mask mask address))
                mask]))

(defn parse-bitmask-2 [mask]
  (let [shift (fn [a n] (bit-or (bit-shift-left a 1) n))
        [base-mask floating _i]
        (reduce (fn [[b f i] char]
                  (case char
                    \0 [(shift b 0) f (dec i)]
                    \1 [(shift b 1) f (dec i)]
                    \X [(shift b 1) (conj f i) (dec i)]))
                [0 [] 35]
                mask)]
    ;; create separate or-masks for each combination of floating-bits
    (let [cnt (count floating)
          var-cnt (bit-shift-left 1 cnt)
          or-masks (long-array var-cnt base-mask)]
      (dotimes [i var-cnt]
        (dotimes [b cnt]
          (aset-long or-masks i (if (bit-test i b)
                                  (bit-set (aget or-masks i) (get floating b))
                                  (bit-clear (aget or-masks i) (get floating b))))))
      (mask-2. (bit-not base-mask) or-masks))))

(defn parse-memset-2 [address value]
  (memset-2. (Integer/parseInt address)
           (Integer/parseInt value)))

(defn parse-2 [input]
  (->> input
       str/split-lines
       (map (fn [line]
              (or (if-let [m (re-matches #"^mask = ([X01]+)" line)] (parse-bitmask-2 (second m)))
                  (if-let [m (re-matches #"^mem.([0-9]+). = ([0-9]+)" line)] (apply parse-memset-2 (rest m))))))
       (remove nil?)))



;; **** Common ****

(defn execute [instr]
  (reduce (fn [[mem mask] i]
            ;(println i mem mask)
            (exec i mem mask))
          [{} nil]
          instr))

(->> (slurp "day14.txt")
     ;testdata
     parse
     execute
     first
     vals
     (apply +))
;; => 18630548206046

(def testdata-2 "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(->> (slurp "day14.txt")
     ;testdata-2
     parse-2
     execute
     first
     vals
     (apply +)
     )
;; => 4254673508445

