(ns advent2016.day12
  (:require [rolfrander.puzzle-lib :refer [interpreter get-data]]))

(def testdata "cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a")

(def start-state (zipmap (map str "abcd") (repeat 0)))

(defn cpu [state instr [a b]]
  (let [a-val (if (number? a) a (state a))]
    (case instr
      "cpy" (assoc state b a-val)
      "inc" (update state a inc)
      "dec" (update state a dec)
      "jnz" (if (zero? a-val)
              {:jmp 1}
              {:jmp b}))))

#_(binding [rolfrander.puzzle-lib/*debug* true])
(interpreter testdata cpu start-state)
(interpreter (get-data 2016 12) cpu start-state)
(interpreter (get-data 2016 12) cpu (assoc start-state "c" 1))

