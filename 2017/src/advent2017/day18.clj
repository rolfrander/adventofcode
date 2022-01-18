(ns advent2017.day18
  (:require [rolfrander.puzzle-lib :refer [get-data interpreter *debug*]]
            [clojure.core.async :as as :refer [>!! <!! >! <!]]))

(def ^:dynamic *local-debug* false)

(def testdata "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2")

(def testdata-2 "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d")

(def start-state {:sound -1
                  :send-ch nil
                  :recv-ch nil
                  :send-cnt 0
                  "p" 0})

(defn cpu [_instruction-pointer state mnemonic [a b]]
  (let [val-a (if (number? a) a (get state a 0))
        val-b (if (number? b) b (get state b 0))]
    (case mnemonic
      "snd" (assoc state :sound val-a)
      "set" (assoc state a val-b)
      "add" (update state a (fnil (partial + val-b) 0))
      "mul" (update state a (fnil (partial * val-b) 0))
      "mod" (update state a #(mod % val-b))
      "rcv" (if (zero? val-a)
              state
              {:jmp 99999})
      "jgz" (if (> val-a 0)
              {:jmp val-b}
              state))))

(defn cpu-2 [_instruction-pointer state mnemonic [a b]]
  (let [val-a (if (number? a) a (get state a 0))
        val-b (if (number? b) b (get state b 0))]
    (case mnemonic
      "snd" (do (when *local-debug* (println "send" (:id state) val-a "buffer:" (.count (.buf (:send-ch state)))) (flush))
                (>!! (:send-ch state) val-a)
                (when *local-debug* (println "   :" (:id state)) (flush))
                (update state :send-cnt inc))
      "set" (assoc state a val-b)
      "add" (update state a (fnil (partial + val-b) 0))
      "mul" (update state a (fnil (partial * val-b) 0))
      "mod" (update state a #(mod % val-b))
      "rcv" (do (when *local-debug* (println "recv" (:id state)) (flush))
                (let [ret (as/alt!! [(as/timeout 1000)] (do (println "timeout!") {:jmp 10000}) ; deadlock-prevention
                                    [(:recv-ch state)]  ([val] (assoc state a val)))]
                  (when *local-debug* (println "   :" (:id state)) (flush))
                  ret))
      "jgz" (if (> val-a 0)
              {:jmp val-b}
              state))))

(defn task-1 [data]
  (->> (interpreter data cpu start-state)
       :state
       :sound))

(defn task-2 [data]
  (let [ch-0 (as/chan 5000)
        ch-1 (as/chan 5000)
        th-0 (future (interpreter data cpu-2 (assoc start-state
                                                    "p" 0
                                                    :id 0
                                                    :send-ch ch-0
                                                    :recv-ch ch-1)))
        th-1 (future (interpreter data cpu-2 (assoc start-state
                                                    "p" 1
                                                    :id 1
                                                    :send-ch ch-1
                                                    :recv-ch ch-0)))
        ;monitor (future (dotimes [_i 5]
        ;                  (<!! (as/timeout 1000))
        ;                  (println "messages waiting:" (map #(.count (.buf %)) [ch-0 ch-1]))))
        ]
    (:send-cnt (:state @th-1))))

(task-1 testdata)
(task-1 (get-data 2017 18))

(task-2 testdata-2)

(task-2 (get-data 2017 18))
