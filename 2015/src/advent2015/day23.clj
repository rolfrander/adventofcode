(ns advent2015.day23 
  [:require [clojure.string :as string]])

(def testdata "inc a
jio a, +2
tpl a
inc a")

(def state {:ip 0
            "a" 0
            "b" 0})


(defn safe-parse-number [s]
  (if (re-matches #"[+-][0-9]+" s)
    (Long/parseLong s)
    s))

(defn parse-line [line]
  (map safe-parse-number (string/split line #"[, ]+")))

(defn parse [input]
  (mapv parse-line (string/split-lines input)))

(defn interpret [state [instr par-1 par-2]]
  (case instr
    "hlf" (-> state
              (update par-1 #(quot % 2))
              (update :ip inc))
    "tpl" (-> state
              (update par-1 (partial * 3))
              (update :ip inc))
    "inc" (-> state
              (update par-1 inc)
              (update :ip inc))
    "jmp" (update state :ip (partial + par-1))
    "jie" (update state :ip #(if (even? (state par-1)) (+ par-2 %) (inc %)))
    "jio" (update state :ip #(if (= 1 (state par-1)) (+ par-2 %) (inc %)))))

(let [program (parse (slurp "resources/2015/day23.txt"))]
  (loop [state (assoc state "a" 1)]
    (if (>= (:ip state) (count program))
      state
      (recur (interpret state (program (:ip state)))))))