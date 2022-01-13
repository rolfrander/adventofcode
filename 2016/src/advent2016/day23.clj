(ns advent2016.day23
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]
            [clojure.string :as str]))

(def start-state (assoc (zipmap (map str "abcd") (repeat 0))
                        :toggle 0))

(defn interpreter
  "General interpreter logic.
   
   Input is assumed to be a string with lines formatted as `mnemonic parameters...`, with parameters separated by space.
   The CPU-function should have the following signature: `state mnemonic parameters` and return a new state. The state
   should not include instruction-pointer, that is covered by the interpreter. If the instruction is a jump, instead return
   `{:jmp relative-position}`."
  [input cpu-function start-state]
  (let [parse-line (fn [line] (map safe-parse-number (str/split line #" +")))
        program (cond (vector? input) input
                      (coll? input) (vec input)
                      :else (mapv parse-line (str/split-lines input)))
        last-ip (dec (count program))
        start (System/currentTimeMillis)]
    (loop [state start-state
           ip 0
           cnt 1]
      (comment when (= (mod cnt 10000000) 0)
        (printf "MIPS: %f, state=%s\n" (/ (/ cnt 1000.0) (- (System/currentTimeMillis) start)) state)
        (flush))
      (if (> ip last-ip)
        {:state state :ip ip :count cnt}
        (let [[mnemonic & params] (get program ip)]
          (let [next-state (cpu-function ip state mnemonic params)]
            (if (contains? next-state :jmp)
              (recur state (+ ip (:jmp next-state)) (inc cnt))
              (recur next-state (inc ip) (inc cnt)))))))))

(defn parse [input]
  (let [parse-line (fn [line] (map safe-parse-number (str/split line #" +")))
        program (cond (vector? input) input
                      (coll? input) (vec input)
                      :else (mapv parse-line (str/split-lines input)))]
    program))

(def toggle-map {"tgl" "inc"
                 "inc" "dec"
                 "dec" "inc"
                 "jnz" "cpy"
                 "cpy" "jnz"})

(defn cpu [ip state instr [a b]]
  (let [a-val (if (number? a) a (state a))
        b-val (if (number? b) b (state b))
        toggle (bit-test (:toggle state) ip)
        toggle-instr (+ ip a-val)
        instr-2 (if toggle
                  (toggle-map instr)
                  instr)]
    (case instr-2
      "tgl" (update state :toggle bit-flip toggle-instr)
      "cpy" (if (string? b)
              (assoc state b a-val)
              state) ; skip invalid
      "inc" (if (string? a) (update state a inc) state)
      "dec" (if (string? a) (update state a dec) state)
      "jnz" (if (zero? a-val)
              {:jmp 1}
              {:jmp b-val}))))

(def √ #(Math/sqrt %))

(def testdata "cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a")

(def subdata "cpy a b
dec b
cpy a d
cpy 0 a
cpy b c
inc a
dec c
jnz c -2
dec d
jnz d -5
dec b
cpy b c
cpy c d
dec d
inc c
jnz d -2")

(def toggled-end-of-program "cpy 90 c
cpy 81 d
inc a
dec d
jnz d -2
dec c
jnz c -5
")

(defn task-2 [input]
  (+ (fac (+ input 1)) (* 81 90)))

(task-2 7)

(defn print-linenum [input]
  (loop [l (str/split-lines input)
         i 0]
    (when-not (nil? l)
      (printf "%4d %s\n" i (first l))
      (recur (next l) (inc i)))))

(defn run-subprogram [program start end init]
  (interpreter (subvec program start end)
               cpu
               (merge start-state init)))

(interpreter testdata cpu start-state)

(def program (parse (get-data 2016 23)))

(defn compute-a [input]
  (get-in (interpreter program cpu (assoc start-state "a" input))
          [:state "a"]))

(defn fac [i]
  (reduce * (range 1 i)))

(compute-a 9)

(map #(+ (fac %) (* 81 90)) (range 7 10))

(- 12330 (fac 8))
(- 47610 (fac 9))
(- 370170 (fac 10))


; a >= 6, or endless loop

(doseq [i (range 4 13)]
  (let [s (:state (run-subprogram program 0 16 i))
        [a b c d] (map (comp s str) (seq "abcd"))]
    (printf "input = %2d, a=%7d b=%2d c=%2d d=%2d\n" i a b c d)))

(doseq [aa (range 7 10)]
  (let [s (:state (run-subprogram program 0 22 {"a" aa}))
        [a b c d] (map (comp s str) (seq "abcd"))]
    (printf "input: a=%d, a=%7d b=%2d c=%2d d=%2d toggle=%x\n" aa a b c d (:toggle s))
    (flush)))

(doseq [aa (range 1 15)]
  (let [s (:state (interpreter toggled-end-of-program cpu (assoc start-state "a" aa)))
        [a b c d] (map (comp s str) (seq "abcd"))]
    (printf "input: a=%d, a=%7d b=%2d c=%2d d=%2d toggle=%x\n" aa a b c d (:toggle s))
    (flush)))

; 

(print-linenum program)

;;    0 cpy a b   b = a
;;    1 dec b

;;    2 cpy a d   a = a * b
;;    3 cpy 0 a
;;    4 cpy b c   
;;    5 inc a    
;;    6 dec c
;;    7 jnz c -2
;;    8 dec d
;;    9 jnz d -5

;;   10 dec b     b--
;;   11 cpy b c   d=c=b
;;   12 cpy c d   
;;   13 dec d     c = d+c = 2b 
;;   14 inc c
;;   15 jnz d -2
;;   16 tgl c
;;   17 cpy -16 c
;; x 18 jnz 1 c
;;   19 cpy 90 c
;;                 a = fac(input+1)
;; x 20 jnz 81 d
;;   21 inc a
;; x 22 inc d
;;   23 jnz d -2  a=a+d
;; x 24 inc c
;;   25 jnz c -5  a=a+(d*c)