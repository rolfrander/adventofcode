(ns advent2020.day08
  (:require [clojure.string :as str]))

(def testdata "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn parse-data
  "returns a vector containing instructions pairs of strings and parameters"
  [data]
  (->> data
       str/split-lines
       (map #(str/split % #" "))
       (into [])))

(defn interpret [instructions & {:keys [switch-instruction]
                                 :or {switch-instruction -1}}]
  (let [visited (boolean-array (count instructions) false)
        opposite (fn [instr]
                   (case instr
                     "acc" "acc"
                     "jmp" "nop"
                     "nop" "jmp"))]
    (loop [acc 0
           eip 0]
      (if (or (>= eip (count instructions))
              (aget visited (int eip)))
        [acc (>= eip (count instructions))]
        (let [[orig-instr param] (get instructions eip)
              instr (if (= eip switch-instruction)
                      (opposite orig-instr)
                      orig-instr)]
          (aset-boolean visited eip true)
          (case instr
            "acc" (recur (+ acc (Integer/parseInt param)) (inc eip))
            "jmp" (recur acc (+ eip (Integer/parseInt param)))
            "nop" (recur acc (inc eip))
            (print instr param)))))))

(defn switchable-instructions [data]
  (->> data
       (map-indexed (fn [i element]
                      (if (= (first element) "acc") 
                        nil
                        i)))
       (filter #(not (nil? %)))))

(interpret (parse-data testdata) :switch-instruction 7)

(interpret (parse-data (slurp "day08.txt")))

(let [data (parse-data (slurp "day08.txt"))]
  (some #(and (second %)
              %) (map #(interpret data :switch-instruction %)
                      (switchable-instructions data))))