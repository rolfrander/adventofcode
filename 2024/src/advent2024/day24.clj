(ns advent2024.day24
  (:require
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.pprint :as pp]))

(def testdata1 "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02")

(def testdata2 "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj")

(def data (puzzle/get-data 2024 24))

(defn parse [in]
  (let [[input program] (str/split in #"\n\n")
        input (->> (re-seq #"([a-z0-9]+): ([01])" input)
                   (reduce (fn [res [_all var val]] (assoc res var (puzzle/str->long val)))
                           {}))
        program (->> (re-seq #"([a-z0-9]+) (AND|X?OR) ([a-z0-9]+) -> ([a-z0-9]+)" program)
                     (map (fn [[_all op1 cmd op2 res]]
                            (vector op1
                                    (case cmd
                                      "AND" :and
                                      "OR" :or
                                      "XOR" :xor)
                                    op2
                                    res))))
        ]
    {:input input
     :program program}))

(defn safe-inc [i]
  (if (nil? i) 1 (inc i)))

(defn compile [program]
  (let [assignments (reduce (fn [ret [op1 cmd op2 res]]
                              (assoc ret (symbol res) {:cmd (case cmd
                                                              :or 'clojure.core/or
                                                              :and 'clojure.core/and
                                                              :xor 'clojure.core/not=)
                                                       :op1 (symbol op1)
                                                       :op2 (symbol op2)}))
                            {} (:program program))

        all-rhs (reduce (fn [rhs {:keys [op1 op2]}] (-> rhs
                                                        (update op1 safe-inc)
                                                        (update op2 safe-inc)))
                        {} (vals assignments))

        inline (fn [o rhs]
                 (if (and (o rhs) (= 1 (all-rhs rhs)))
                   [(dissoc o rhs) (o rhs)]
                   [o rhs]))

        optimize (fn [o lhs]
                   (if (o lhs)
                     (let [{:keys [op1 op2]} (o lhs)
                           [o op1] (inline o op1)
                           [o op2] (inline o op2)]
                       (-> o
                           (assoc-in [lhs :op1] op1)
                           (assoc-in [lhs :op2] op2)))
                     o))

        optimized (reduce optimize assignments (keys assignments))
        ]
    (letfn [(getop [x]
                   (cond (nil? x) nil
                         (symbol? x) [x]
                         :else (concat (getop (:op1 x)) (getop (:op2 x)))))

            (dfs [[visited stack] vertex]
                 (if (or (nil? vertex) (visited vertex))
                   [visited stack]
                   (let [visited (conj visited vertex)
                         statement (optimized vertex)
                         [visited stack] (reduce dfs [visited stack] (getop statement))
                         stack (conj stack vertex)]
                     [visited stack])))

            (topo-sort [nodes]
                       (let [[_visited stack] (reduce dfs [#{} []] nodes)]
                         stack))

            (out-assignment [t i]
                            (if (> i 4)
                              (println t)
                              (if (or (nil? t) (symbol? t))
                                t
                                (list (:cmd t) (out-assignment (:op1 t) (inc i)) (out-assignment (:op2 t) (inc i))))))
            (output [out target]
                    (if (optimized target)
                      (conj out target (out-assignment (optimized target) 0))
                      (conj out target (list 'clojure.core/= 1 (list (symbol "input") (str target))))))]

      (list 'clojure.core/fn
            [(symbol "input")]
            (list 'clojure.core/let
                  (reduce (fn [out v]
                            (output out v))
                          [] (topo-sort (keys optimized)))

                  (vec (sort #(compare %2 %1) (remove all-rhs (keys assignments))))))
      )
    ))

(defn inline-all [program]
  
  (let [assignments (reduce (fn [ret [op1 cmd op2 res]]
                              (assoc ret res {:cmd cmd
                                              :op1 op1
                                              :op2 op2}))
                            {} (:program program))

        all-rhs (reduce (fn [rhs {:keys [op1 op2]}] (-> rhs
                                                        (update op1 safe-inc)
                                                        (update op2 safe-inc)))
                        {} (vals assignments))]
    (letfn [(inline [x]
              (if (contains? assignments x)
                (let [instr (get assignments x)]
                  (list (:cmd instr) (inline (:op1 instr)) (inline (:op2 instr))))
                x))]

      (map (juxt identity inline) (remove all-rhs (keys assignments))))))

(defn solve-1 [in]
  (let [program (parse in)
        f (eval (compile program))
        input (:input program)]
    (-> (map #(if % 1 0) (f input))
        (puzzle/digits->long :base 2))))

(compile (parse testdata1))
(solve-1 testdata1)
(solve-1 testdata2)
(solve-1 (puzzle/get-data 2024 24))
;;=> 58740594706150

(def program (parse data))

(let [replacements (reduce (fn [o [a b]]
                             (-> o
                                 (assoc a b)
                                 (assoc b a)))
                           {} [["hbk" "z14"]
                               ["kvn" "z18"]
                               ["dbb" "z23"]
                               ["tfn" "cvh"]])
      assignments (reduce (fn [ret [op1 cmd op2 res]]
                            (assoc ret (get replacements res res)
                                   (if (str/starts-with? op1 "x")
                                     [op1 cmd op2]
                                     [op2 cmd op1])))
                          {} (:program program))
      isinput (fn [x] (boolean (re-matches #"[xy]\d\d" x)))
      alias-a_xor_b (into {} (for [[res [op1 instr op2]] assignments
                                   :when (and (isinput op1)
                                              (isinput op2)
                                              (= instr :xor))
                                   :let [num (subs op1 1)]]
                               [res num]))
      alias-a_and_b (into {} (for [[res [op1 instr op2]] assignments
                                   :when (and (isinput op1)
                                              (isinput op2)
                                              (= instr :and))
                                   :let [num (subs op1 1)]]
                               [res num]))
      sum-alias (into {} (for [[res [op1 instr op2]] assignments
                               :let [num (or (alias-a_xor_b op1)
                                             (alias-a_xor_b op2))]
                               :when (and (= instr :xor)
                                          (some? num))]
                           [res num]))

      carry-out (into {} (for [[res [op1 instr op2]] assignments
                               :let [num (or (alias-a_and_b op1)
                                             (alias-a_and_b op2))]
                               :when (= instr :or)]
                           [res num]))

      carry-tmp (into {} (for [[res [op1 instr op2]] assignments
                               :let [num (or (alias-a_xor_b op1)
                                             (alias-a_xor_b op2))
                                     c-in (if (alias-a_xor_b op1) op2 op1)]
                               :when (and (some? num)
                                          (= instr :and))]
                           [res num]))
      
      res (reduce #(assoc %1 (format "%02d" %2) {}) {} (range 0 45))
      res (reduce-kv #(-> (assoc-in %1 [%3 :x-xor-y] %2)
                          (assoc-in [%3 :x] (get (assignments %2) 0))
                          (assoc-in [%3 :y] (get (assignments %2) 2)))
                     res alias-a_xor_b)
      res (reduce-kv #(assoc-in %1 [%3 :x-and-y] %2)
                     res alias-a_and_b)
      res (reduce-kv #(assoc-in %1 [%3 :sum] [%2 '= (assignments %2)])
                     res sum-alias)
      res (reduce-kv #(-> (assoc-in %1 [%3 :carry-tmp] %2)
                          (assoc-in [%3 :carry-in] (let [[op1 _and op2] (assignments %2)]
                                                     [:carry-out (keep carry-out [op1 op2])
                                                      :a-x-b (keep alias-a_xor_b [op1 op2])])))
                     res carry-tmp)
      res (reduce-kv #(assoc-in %1 [%3 :carry-out] %2)
                     res carry-out)
      ]
  (pp/print-table [:x :y :x-xor-y :x-and-y :sum :carry-tmp :carry-in :carry-out]
   (vals (sort res)))

  )

(->> [["hbk" "z14"]
     ["kvn" "z18"]
     ["dbb" "z23"]
     ["tfn" "cvh"]]
     flatten
     sort
     (str/join ","))
;;=> "cvh,dbb,hbk,kvn,tfn,z14,z18,z23"
