(ns advent2016.day25
  (:require [rolfrander.puzzle-lib :refer [interpreter get-data]]
            [clojure.string :as str]))

(def start-state (-> (zipmap (map str "abcd") (repeat 0))
                     (assoc :out [])))

(defmacro when-mod
  "Evaluates test. If logical true, evaluates body in an implicit do."
  {:added "1.0"}
  [number modulus & body]
  (list 'if (list '= 0 (list 'mod number modulus)) (cons 'do body)))

(macroexpand '(when-mod (count (:out state)) 20
                        (println (str/join (:out state)))))

(defn cpu [_ip state instr [a b]]
  (let [a-val (if (number? a) a (state a))]
    (case instr
      "out" (let [state-cnt (count (:out state))]
              (if (or (> state-cnt 1000)
                      (and (> state-cnt 0)
                           (= a-val (peek (:out state)))))
                :hlt
                (update state :out conj a-val)))
      "cpy" (assoc state b a-val)
      "inc" (update state a inc)
      "dec" (update state a dec)
      "jnz" (if (zero? a-val)
              {:jmp 1}
              {:jmp b}))))

(def data (get-data 2016 25))

(doseq [i (range 157 159)]
  (let [c (->> (assoc start-state "a" i)
               (interpreter data cpu)
               :state
               :out
               count)]
    (when (> c 5) (println i c))))
