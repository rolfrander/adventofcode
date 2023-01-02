(ns advent2022.day10
      (:require [clojure.set :as set]
                [clojure.string :as str]
                [rolfrander.puzzle-lib :as puzzle]))

(def testdata-1 "noop
addx 3
addx -5")

(def testdata-2 "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(def ^:dynamic *task-2* false)

(defn cpu [_ip state cmd params]
  (let [check-signal-strength (fn [st]
                                (let [cycles (:cycles st)
                                      x (:x st)]
                                  (when *task-2*
                                    (let [pos (mod (dec cycles) 40)]
                                      (if (<= (puzzle/abs (- pos x)) 1)
                                        (print "#")
                                        (print "."))
                                      (when (= pos 39)
                                        (println))))
                                  (if (= (mod cycles (:signal-strength-repeat st))
                                         (:signal-strength-start st))
                                    (assoc-in st [:signal-strength cycles]
                                              x)
                                    st)))]
    (case cmd
      "noop" (-> state
                 (update :cycles inc)
                 (check-signal-strength))
      "addx" (-> state
                 (update :cycles inc)
                 (check-signal-strength)
                 (update :cycles inc)
                 (check-signal-strength)
                 (update :x + (first params)))))
  )

(defn task-1 [input]
  (let [state (puzzle/interpreter input
                                  cpu
                                  {:cycles 0 :x 1 
                                   :signal-strength-start 20 
                                   :signal-strength-repeat 40 
                                   :signal-strength {}})]
    ;(:state state)
    (->> (get-in state [:state :signal-strength])
         (map (partial apply *))
         (reduce +))
    ))

(defn task-2 [input]
  (binding [*task-2* true]
    (task-1 input)))

(task-1 testdata-1)
(task-1 testdata-2)
(task-1 (puzzle/get-data 2022 10))

(task-2 testdata-2)
(task-2 (puzzle/get-data 2022 10))
