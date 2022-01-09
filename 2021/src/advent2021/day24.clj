(ns advent2021.day24 
  [:require [clojure.string :as string]
            [advent2021.core :as core]])

(def testdata "inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2")

(def start-state {"x" 0
                  "y" 0
                  "z" 0
                  "w" 0})

(defn alu-inp 
  ([state target _ignore] (alu-inp state target))
  ([state target]
   (assoc state
          target (first (:input state))
          :input (rest (:input state)))))

(def registers #{"x" "y" "z" "w"})

(defn alu-cmd [f]
  (fn [state target src]
    (let [src (if (registers src)
                (state src)
                src)]
      (assoc state target (f (state target) src)))))

(def alu-mul (alu-cmd *))
(def alu-add (alu-cmd +))
(def alu-div (alu-cmd quot))
(def alu-mod (alu-cmd mod))
(def alu-eql (alu-cmd #(if (= %1 %2) 1 0)))

(def ^:dynamic *as-text* false)

(defn parse-line [line]
  (let [[mnemonic par1 par2] (string/split line #" ")
        par2 (cond (nil? par2) nil
                   (registers par2) par2
                   :else (Long/parseLong par2))]
    (if *as-text*
      [mnemonic par1 par2]
      
      (case mnemonic
        "inp" [:inp par1]
        "add" [alu-add par1 par2]
        "mul" [alu-mul par1 par2]
        "div" [alu-div par1 par2]
        "mod" [alu-mod par1 par2]
        "eql" [alu-eql par1 par2]))))

(defn gen-eval
  "returns a function taking two inputs: state and input, returning state. The supplied program must start with one :inp, and have no other inp-instructions."
  [program]
  (let [[[inp-instr inp-register] & program] program]
    (assert (= inp-instr :inp) "The supplied program must start with one :inp")
    (fn [state input]
      (reduce (fn [state [f par1 par2]] (f state par1 par2))
              (assoc state inp-register input)
              program))))

(defn parse [input]
  (->> (string/split-lines input)
       (map parse-line)
       (core/split-by (comp (partial not= :inp) first))
       (map gen-eval)))

(defn parse-as-text [input]
  (binding [*as-text* true]
    (->> (string/split-lines input)
         (map parse-line)
         (core/split-by (comp (partial not= "inp") first))
         doall)))

(defn alu-ast [program]
  (letfn [(over-9 [x] (or (and (number? x) (> x 9))
                          (and (vector? x)
                               (= (x 0) "add")
                               (or (over-9 (x 1))
                                   (over-9 (x 2))))))
          (optimize [f par1 par2]
            (cond
              (and (number? par1) (number? par2))
              (case f
                "mul" (* par1 par2)
                "add" (+ par1 par2)
                "div" (quot par1 par2)
                "mod" (mod par1 par2)
                "eql" (if (= par1 par2) 1 0))
              (and (= f "mul") (= par2 0)) 0
              (and (= f "mul") (= par2 1)) par1
              (and (= f "add") (= par2 0)) par1

              (and (= f "mul") (= par1 0)) 0
              (and (= f "mul") (= par1 1)) par2
              (and (= f "add") (= par1 0)) par2

              (and (= f "div") (= par2 1)) par1
              (and (= f "eql") (or (and (= par2 "inp") (over-9 par1))
                                   (and (= par1 "inp") (over-9 par2)))) 0
              (and (= f "eql") (= par1 par2)) 1
              :else [f par1 par2]))]

    (reduce (fn [state [f par1 par2]]
              (cond (= f "inp")
                    (assoc state par1 "inp")

                    (number? par2)
                    (assoc state par1 (optimize f (state par1) par2))

                    :else
                    (assoc state par1 (optimize f (state par1) (state par2)))))

            (assoc start-state 
                   "z" "z")
            program)))

(defn convert-to-clojure [ast]
  (condp apply [ast]
    vector? (map convert-to-clojure ast)
    string? (case ast
              "add" '+
              "mul" '*
              "div" '/
              "mod" 'mod
              "eql" '=
              ast)
    ast
    ))

(def data (core/load-data 24 session))

(let [program (parse-as-text data)]
  (letfn [(get-z [a] (get a "z"))]
    (map (comp convert-to-clojure get-z alu-ast) program)))

(defn execute-for-all-inputs 
  ([program states] (execute-for-all-inputs program states 1 10))
  ([program states from to]
   (let [all-inputs (for [s states
                          i (range from to)]
                      [s i])
         apply-and-reset-w (fn [[state input]]
                             (-> (program state input)
                                 (assoc "w" 0) ; w is irrelevant, always overwritten at start of program
                                 (assoc "x" 0) ; x is always set to 0 in start of every program
                                 (assoc "y" 0) ; y is also set to 0 before any use in every program
                                 ))]
     (group-by apply-and-reset-w all-inputs))))

(let [program (parse data)]
  ;(filter (comp (partial = 0) #(get % "z")))
  (count (reduce (fn [states p]
                   (keys (execute-for-all-inputs p states)))
                 [start-state]
                 (take 6 program))))

(defn execute-with-inputs [program state inputs]
  (reduce (fn [state [program input]]
            (program state input))
          state
          (map vector program inputs)))

(let [program (parse data)]
  ;            a b c c d d e e f g g f b a
  (let [input (map #(- (int %) (int \0)) (seq "48111514719111")) ;99995969919326
        programs (vec program)]

    (comment for [z (range 1 27)
          in (range 1 10)
          :let [out (-> start-state
                        (assoc "z" z)
                        ((programs 5) in)
                        (get "z"))]
          :when (= out 0)]
      [in (- z 6)])

    (-> (execute-with-inputs (take (count input) program) start-state input)
             (get "z")
             (Long/toString 26))))

(comment ;; obsolete
  (defn alu-eval [state program]
    (reduce (fn [state [f par1 par2]] (f state par1 par2))
            state
            program))

(defn print-subtree [tree level]
  (if (vector? tree)
    (if (< level 0)
      (str "[" (tree 0) " "
           (print-subtree (tree 1) -1) " "
           (print-subtree (tree 2) -1)
           "]")
      (print-subtree (tree 1) (dec level)))
    (str tree)))

  )