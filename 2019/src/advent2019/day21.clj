(ns advent2019.day21
  (:require
   [advent2019.intcode :as ic]
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]))

(def program (ic/parse (puzzle/get-data 2019 21)))

(def spring-cmd {:not "NOT"
                 :and "AND"
                 :or "OR"
                 :a "A"
                 :b "B"
                 :c "C"
                 :d "D"
                 :e "E"
                 :f "F"
                 :g "G"
                 :h "H"
                 :i "I"
                 :temp "T"
                 :jump "J"
                 :walk "WALK"
                 :run "RUN"})

(defn encode-springscript [script]
  (when-let [illegal (seq (remove #(contains? spring-cmd %) script))]
    (throw (IllegalArgumentException. (str "illegal instructions: " illegal))))
  (let [instruction-list (->> (map spring-cmd script)
                              (partition-all 3)
                              (map (partial str/join " "))
                              (#(concat % (list ""))))]
    (when (> (count instruction-list) 16)
      (throw (IllegalArgumentException. "to many instructions")))
    (str/join (char 10) instruction-list)))

;(print (encode-springscript [:not :a :temp 
;                             :and :temp :jump]))

(defn run-springscript [script]
  (let [run (fn [in] (->> (encode-springscript in)
                          (map int)
                          (ic/interpret-with-input program)))
        separate-output (fn [out] (let [sorted-output (group-by #(> % 127) out)]
                                    {:ascii (apply str (map char (sorted-output false)))
                                     :damage (first (sorted-output true))}))]
    (separate-output (run script))))

; part 1
; j = (and d (or (not c) (not a))))
(let [{:keys [ascii damage]} (run-springscript [:not :a :jump
                                                :not :c :temp
                                                :or :temp :jump
                                                :and :d :jump
                                                :walk])]
  (print ascii)
  damage)

(defn test-logic [hull logic jump-at-pos]
  (let [hull-vec (mapv #(= % \#) hull)
        ; assume sensors report "true" outside of hull-map
        hull-vec (apply conj hull-vec (repeat 11 true))
        do-eval (fn do-eval [logic sensors]
                  (if (seq? logic)
                    (let [[cmd op1 op2] logic]
                      (case cmd
                        :and (and (do-eval op1 sensors) (do-eval op2 sensors))
                        :or  (or (do-eval op1 sensors) (do-eval op2 sensors))
                        :not (not (do-eval op1 sensors))))
                    (sensors logic)))
        apply-sensors (fn [offset]
                        (zipmap [:a :b :c :d :e :f :g :h :i]
                                (subvec hull-vec offset)))]
    (for [i (range (count hull))
          :when (not= (do-eval logic (apply-sensors i))
                      (contains? jump-at-pos i))]
      i)))

(defn logic-tables [hull logic jump-at-pos undecided]
  (let [hull-vec (mapv #(= % \#) hull)
        ; assume sensors report "true" outside of hull-map
        hull-vec (apply conj hull-vec (repeat 11 true))
        
        do-eval (fn do-eval [logic sensors]
                  (if (seq? logic)
                    (let [[cmd op1 op2] logic]
                      (case cmd
                        :and (and (do-eval op1 sensors) (do-eval op2 sensors))
                        :or  (or (do-eval op1 sensors) (do-eval op2 sensors))
                        :not (not (do-eval op1 sensors))))
                    (sensors logic)))
        
        apply-sensors (fn [offset]
                        (zipmap [:a :b :c :d :e :f :g :h :i]
                                (subvec hull-vec (inc offset))))]
    (for [i (range 10) ; MAGIC CONSTANT
          :when (not (undecided i))
          :let [sensors (apply-sensors i)]]
      (assoc sensors
             :should-jump (contains? jump-at-pos i)
             :eval (do-eval logic sensors)
             :hull hull
             :jump jump-at-pos))
    ))

(defn bools->int [s]
  (loop [[b & s] s
         res 0]
    (if (nil? b)
        res
        (recur s (+ (* res 2) (if (not b) 0 1))))))

(defn digits
  "Returns a seq of each digit in the input as a list of long"
  [number & {:keys [base width]
             :or {base 10 width 1}}]
  (loop [t number
         s '()
         w width]
    (if (and (= 0 t) (<= w 0))
      s
      (recur (quot t base)
             (cons (mod t base) s)
             (dec w)))))

; part 2
;                      @ABC|EFGHI
(def testvectors [["#####.#.##.#.####" #{4 9} #{0 5 6 7 10 11 12 13 14 15 16}]
                  ["#####.###########" #{2}   #{3 4 5 6 7 8 9 10 11 12 13 14 15 16}]
                  ["#####.#..########" #{2 6} #{3 4 5 7 8 9 10 11 12 13 14 15 16}]
                  ["#####..#.#.##.###" #{3 7 11} #{4 5 6 8 9 10 12 13 14 15 16}] ; evt 12 i stedet for 11
                  ["#####...#########" #{4}   #{0 5 6 7 8 9 10 11 12 13 14 15 16}]
                  ["#####.##.#.#..###" #{3 7 11} #{4 5 6 8 9 10 12 13 14 15 16}]
;                              @ABC|EFGHI
;                   012345678901
                  ])

(defn runtest [testvectors logic]
  (let [table (->> (mapcat (fn [[hull jump undecided]] (logic-tables hull
                                                                     logic
                                                                     jump
                                                                     undecided))
                           testvectors)
                                 ;(filter :should-jump)
                   (map #(assoc % 
                                :abcd (bools->int (map % [:a :b :c :d]))
                                :efghi (bools->int (map % [:e :f :g :h :i]))
                                :abcdefghi (bools->int (map % [:a :b :c :d :e :f :g :h :i]))))
                   (sort-by :should-jump))
        k-table (vec (repeat 32 {:row 0
                                 :data (vec (take 16 (repeat nil)))}))
        k-table (reduce #(assoc-in %1 [%2 :row] %2)
                        k-table (range (count k-table)))
        k-table (->> (for [e table
                           :let [abcd (bools->int (map e [:a :b :c :d]))
                                 efghi (bools->int (map e [:e :f :g :h :i]))]]
                       [abcd efghi (:should-jump e)])
                     (reduce (fn [k [abcd efghi res]]
                               (assoc-in k [efghi :data abcd] res))
                             k-table))]

    (pp/print-table [:eval :should-jump :a :b :c :d :e :f :g :h :i :abcd :efghi :abcdefghi] table)
    (doseq [r k-table]
      (if (some boolean? (:data r))
        (println (map #(case %
                         true "1"
                         false "0"
                         "_") (:data r))
                 (digits (:row r) :base 2 :width 5))))
  ;k-table
    
    ))

(runtest testvectors
         '(:and :d
                (:or (:not :a)
                     (:or (:not :b)
                          (:and :h
                                (:not :c)))))
         )

;'(:and :d
;       (:or (:not :a)
;            (:or (:not :b)
;                 (:and :h
;                       (:not :c)))))
(let [{:keys [ascii damage]} (run-springscript [:not :c :temp
                                                :and :h :temp
                                                :not :b :jump
                                                :or :jump :temp
                                                :not :a :jump
                                                :or :temp :jump
                                                :and :d :jump
                                                :run])]
  (if (nil? damage)
    (->> (str/split-lines ascii)
         (filter #(str/starts-with? % "#"))
         first)
    damage))