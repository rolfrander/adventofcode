(ns advent2024.day17
  (:require
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.string :as str]))

(def testdata "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0")

(def testdata2 "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0")


(def data (puzzle/get-data 2024 17))

(defn parse [in]
  (let [registers {"A" :a
                   "B" :b
                   "C" :c}]
    (loop [[m & matches] (re-seq #"(Register|Program)( [ABC]|): ([0-9,]+)" in)
           state {:pc 0 :output []}]
      (if (nil? m)
        state
        (case (get m 1)
          "Register" (recur matches (assoc state
                                           (registers (subs (m 2) 1))
                                           (puzzle/str->long (m 3))))
          "Program" (recur matches (assoc state
                                          :program
                                          (mapv puzzle/str->long (str/split (m 3) #", ?")))))))))

(def opcodes [:adv :bxl :bst :jnz :bxc :out :bdv :cdv])
(def arg-type {:adv :combo
               :bxl :lit
               :bst :combo
               :jnz :lit
               :bxc :ignore
               :out :combo
               :bdv :combo
               :cdv :combo})

(defn arg [v state]
  (cond (< v 4)
        v
        (< v 7)
        (state ([:a :b :c] (- v 4)))
        :else -1)) ; ignore
        

(defn cycle [s]
  (let [pc (s :pc)
        opcode (opcodes (get (:program s) pc))
        pc (inc pc)
        arg-combo (arg (get (:program s) pc) s)
        arg-lit (get (:program s) pc)
        pc (inc pc)
        s (assoc s :pc pc)]
    (case opcode
      :adv (update s :a bit-shift-right arg-combo)
      :bxl (update s :b bit-xor arg-lit)
      :bst (assoc  s :b (bit-and arg-combo 7))
      :jnz (if (= 0 (s :a))
             s
             (assoc  s :pc arg-lit))
      :bxc (update s :b bit-xor (s :c))
      :out (update s :output conj (bit-and arg-combo 7))
      :bdv (assoc  s :b (bit-shift-right (s :a) arg-combo))
      :cdv (assoc  s :c (bit-shift-right (s :a) arg-combo)))))

(defn disassembly [program]
  (let [decode-arg-combo (fn [v]
                           (cond (< v 4)
                                 v
                                 (< v 7)
                                 (['a 'b 'c] (- v 4))
                                 :else :ignore))] ; ignore
                                 
    (->> (partition 2 program)
         (map #(let [op (opcodes (first %))]
                 (vector op
                         (case (arg-type op)
                           :combo (decode-arg-combo (second %))
                           :lit (second %)
                           :ignore :ignore)))))))

(defn decompile [state]
  (let [p (disassembly (:program state))
        a (symbol "a")
        b (symbol "b")
        c (symbol "c")
        out (symbol "out")
        pc (symbol "pc")]
    (list 'loop ['a (:a state)
                 'b (:b state)
                 'c (:c state)
                 'out []
                 'pc 0]
          (concat '(case pc)
                  (apply concat
                         (map-indexed (fn [i [opcode arg]]
                                        [(* i 2) (case opcode
                                                   :adv `(recur (bit-shift-right ~a ~arg) ~b ~c ~out (+ ~pc 2))
                                                   :bxl `(recur ~a (bit-xor ~b ~arg) ~c ~out (+ ~pc 2))
                                                   :bst `(recur ~a (bit-and ~arg 7) ~c ~out (+ ~pc 2))
                                                   :jnz `(if (= 0 ~a)
                                                           (recur ~a ~b ~c ~out (+ ~pc 2))
                                                           (recur ~a ~b ~c ~out ~arg))
                                                   :bxc `(recur ~a (bit-xor ~b ~c) ~c ~out (+ ~pc 2))
                                                   :out `(recur ~a ~b ~c (conj ~out (bit-and ~arg 7)) (+ ~pc 2))
                                                   :bdv `(recur ~a (bit-shift-right ~a ~arg) ~c ~out (+ ~pc 2))
                                                   :cdv `(recur ~a ~b (bit-shift-right ~a ~arg) ~out (+ ~pc 2)))])
                                      p))
                  `(~out)))))

(defn interpret [state]
  (loop [s state]
    (if (>= (:pc s) (count (:program s)))
      s
      (recur (cycle s)))))

(defn interpret-2 [state]
  (loop [s state]
    (cond (>= (:pc s) (count (:program s)))
          (= (count (:output s))
             (count (:program s)))

          (let [output-cnt (count (:output s))]
            (and (> output-cnt 0)
                 (not= (peek (:output s))
                       (get (:program s) (dec output-cnt)))))
          false

          :else
          (recur (cycle s)))))

(defn solve-1 [in]
  (->> (parse in)
       interpret
       :output
       (str/join ",")))

(defn solve-2 [in]
  (let [state (parse in)
        interpret-with-a (fn [a]
                           (interpret-2 (assoc state :a a)))]
    (first (filter interpret-with-a (range)))))

(solve-1 testdata)
;;=> "4,6,3,5,6,3,5,2,1,0"
(solve-1 data)
;;=> "1,3,7,4,6,4,2,3,5"

(interpret (assoc (parse testdata2) :a 117440))
(solve-2 testdata2)
(solve-2 data)

(println (:program (parse data)))

(doseq [o (disassembly (:program (parse data)))]
  (println o))

(interpret (assoc (parse data) :a 054))

(decompile (parse data))

(defn decompiled-program [^long a orig-program]
  (loop
   [a a
    out []]
    (let [output-cnt (count out)]
      (if (and orig-program
               (> output-cnt 0)
               (not= (peek out)
                     (get orig-program (dec output-cnt))))
        false
        (let [b (bit-xor (bit-and a 7) 1)
              c (bit-shift-right a b)
              b (bit-xor (bit-xor b c) 4)
              a (bit-shift-right a 3)
              out (conj out (bit-and b 7))]
          (if (= 0 a)
            [a out]
            (recur a out)))))))

(let [p (:program (parse data))]
  (some #(decompiled-program % p) (range (bit-shift-left 1 44) (bit-shift-left 1 48))))

(decompiled-program (bit-shift-left 1 44) nil)



(let [state (parse data)]
  (doseq [a (range 200)]
    (println a (:output (interpret (assoc state :a a))))))

(def start-state (parse data))

(let [f (fn [a] (:output (interpret (assoc start-state :a a))))
      v1 (filter #(= (first (f %)) 2) (range 128))]
  (map f v1))

(let [f (fn [a] (:output (interpret (assoc start-state :a a))))
      program (:program start-state)]
  (loop [values (filter #(= (first (f %)) 2) (range 1024))
         shift 10
         i 1]
    (if (>= i (count program))
      (apply min values)
      (recur (for [a values
                   b (range 8)
                   :let [v2 (+ a (bit-shift-left b shift))
                         res (f v2)]
                   :when (and (> (count res) i)
                              (= (nth res i) (nth program i)))]
               v2)
             (+ shift 3)
             (inc i)))))

(interpret (assoc start-state :a 202367025818154))

