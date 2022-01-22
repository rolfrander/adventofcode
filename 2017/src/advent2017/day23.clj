(ns advent2017.day23
  (:require [rolfrander.puzzle-lib :refer [get-data interpreter]]))

(def start-state {:mul-cnt 0})

(defn cpu [_instruction-pointer state mnemonic [a b]]
  (let [val-a (if (number? a) a (get state a 0))
        val-b (if (number? b) b (get state b 0))]
    (case mnemonic
      "set" (assoc state a val-b)
      "add" (update state a (fnil (partial + val-b) 0))
      "sub" (update state a (fnil #(- % val-b) 0))
      "mul" (-> state
                (update a (fnil (partial * val-b) 0))
                (update :mul-cnt inc))
      "mod" (update state a #(mod % val-b))
      "jnz" (if (not= val-a 0)
              {:jmp val-b}
              state))))

; (interpreter (get-data 2017 23) cpu start-state)