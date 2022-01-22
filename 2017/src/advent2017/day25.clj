(ns advent2017.day25
  (:require [rolfrander.puzzle-lib :refer [get-data safe-parse-number]]))

(defn tape [len]
  {:pos (quot len 2)
   :tape (int-array len 0)})

(defn left [tape]
  (update tape :pos dec))

(defn right [tape]
  (update tape :pos inc))

(defn tape-get [tape]
  (aget (:tape tape) (:pos tape)))

(defn tape-set [tape v]
  (aset-int (:tape tape) (:pos tape) v)
  tape)

(defn diagnostic-checksum [tape]
  (let [^ints t (:tape tape)]
    (areduce t i ret (int 0)
             (+ ret (aget t i)))))

(def test-program {"A" [[1 right "B"]
                        [0 left "B"]]
                   "B" [[1 left "A"]
                        [1 right "A"]]})

(def testdata "Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.")

(defn parse-operation [lines]
  [(nthrest lines 3)
   (->> (map (partial re-seq #"(?:\d|left|right|\p{Upper})(?=.$)")
             (take 3 lines))
        flatten
        (mapv #(case %
                "left" left
                "right" right
                (safe-parse-number %))))])

(defn parse-program [lines]
  (loop [lines lines
         program [nil nil]]
    (let [^String l (first lines)]
      (if (and l (.startsWith l "  If the"))
        (let [val (safe-parse-number (first (re-seq #"\d" l)))
              [rest-lines p] (parse-operation (rest lines))]
          (recur rest-lines (assoc program val p)))
        
        [lines program]))))

(defn parse [input]
  (loop [lines (re-seq #"[^\r\n]+" input)
         begin-state nil
         steps 0
         program {}]
    (let [^String l (first lines)]
      (cond (nil? l) [program begin-state steps]
            
            (.startsWith l "Begin in state")
            (recur (rest lines) (second (re-matches #"Begin in state (\w+)." l)) steps program)
            
            (.startsWith l "Perform a diagnostic")
            (let [steps (->> (re-matches #"Perform a diagnostic checksum after (\d+) steps." l)
                             second
                             safe-parse-number)]
              (recur (rest lines) begin-state steps program))
            
            (.startsWith l "In state")
            (let [state (second (re-matches #"In state (\w+):" l))
                  [rest-lines p] (parse-program (rest lines))]
              (recur rest-lines begin-state steps (assoc program state p)))
            
            :else
            (recur (rest lines) begin-state steps program)))))

(defn turing [program start-state iterations]
  (loop [state start-state
         tape (tape 10000)
         cnt 0]
    (if (>= cnt iterations)
      tape
      (let [[newval move next-state] ((program state) (tape-get tape))
            tape (-> tape
                     (tape-set newval)
                     move)]
        (recur next-state tape (inc cnt))))))

(defn task-1 [input]
  (diagnostic-checksum (apply turing (parse input))))

;(apply turing (parse testdata))
;(diagnostic-checksum (second (turing test-program "A" 6)))

(task-1 testdata)
(task-1 (get-data 2017 25))