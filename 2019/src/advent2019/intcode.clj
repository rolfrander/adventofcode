(ns advent2019.intcode
  (:require
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.pprint :as pp]
   [clojure.core.async :refer [>!! <!! chan thread pipe close!]]))


(def ^:dynamic *debug* false)

(defn parse [in]
  (mapv puzzle/str->long (re-seq #"-?\d+" in)))

; { :params :operation }
(def opcodes [{:operation :nop :params 0} ;0
              {:operation :add :params 3} ;1
              {:operation :mul :params 3} ;2
              {:operation :inp :params 1} ;3
              {:operation :out :params 1} ;4
              {:operation :jnz :params 2} ;5
              {:operation :jz  :params 2} ;6
              {:operation :lt  :params 3} ;7
              {:operation :eq  :params 3} ;8
              {:operation :rel :params 1} ;9
              ])

(defn paramtype [val cnt]
  (if (= cnt 0)
    nil
    (-> (paramtype (quot val 10) (dec cnt))
        (conj (mod val 10)))))

(defn interpret 
  "program must be a list of integers.
   input-fn should take the old state as input and return new state. The
   state shoud be a map containing atleast the key :value which
   should hold the input value.
   output-fn should take the the value to output and the old state as input
   and return the new state"
  [program & {:keys [input-fn output-fn]
                            :or {input-fn (fn [state] (assoc state :value 0))
                                 output-fn (fn [x state]
                                             (when *debug* (println "undefined output:" x))
                                             state)}}]
  (loop [state {:mem program
                :pc 0
                :rel-base 0
                :input-state {}
                :output-state nil}]
    (when *debug* (println state))
    
    (let [mem (fn [offset] (if (>= offset (count (state :mem)))
                             0
                             ((state :mem) offset)))
          opcode-number (mem (state :pc))]

      (if (= 99 opcode-number)
        state

        (let [opcode (opcodes (mod opcode-number 100))
              param (->> (paramtype (quot opcode-number 100) (:params opcode))
                         (map-indexed #(let [param-val (+ (state :pc) %1 1)]
                                         (case %2
                                           0 (mem param-val)
                                           1 param-val ; immediate mode?
                                           2 (+ (mem param-val) (state :rel-base)))))
                         vec)
              state (update state :pc + (:params opcode) 1)
              set-mem (fn [state pos val]
                        (let [mem (if (> pos (count (state :mem)))
                                    (into (state :mem) (repeat (- pos (count (state :mem))) 0))
                                    (state :mem))]
                          (assoc state :mem (assoc mem pos val))))]

          (when *debug* (println (:operation opcode) (map (comp mem param) (range (:params opcode)))))
          (recur (case (:operation opcode)

                   :add (set-mem state (param 2)
                                 (+' (mem (param 0))
                                     (mem (param 1))))

                   :mul (set-mem state (param 2)
                                 (*' (mem (param 0))
                                     (mem (param 1))))

                   :inp (let [in (input-fn (state :input-state))]
                          (-> (assoc state :input-state in)
                              (set-mem (param 0)
                                       (:value in))))

                   :out (update state :output-state output-fn (mem (param 0)))

                   :jnz (if (not= 0 (mem (param 0)))
                          (assoc state :pc (mem (param 1)))
                          state)

                   :jz  (if (= 0 (mem (param 0)))
                          (assoc state :pc (mem (param 1)))
                          state)

                   :lt  (set-mem state (param 2)
                                 (if (< (mem (param 0)) (mem (param 1)))
                                   1
                                   0))

                   :eq  (set-mem state (param 2)
                                 (if (= (mem (param 0)) (mem (param 1)))
                                   1
                                   0))

                   :rel (update state :rel-base + (mem (param 0)))
                   (assoc state :illegal-opcode opcode-number))))))))

(defn interpret-with-input [program input]
  (-> (interpret program
              :input-fn (fn [state]
                          (let [inputs (or (:inputs state) input)]
                            (-> state
                                (assoc :value (first inputs))
                                (assoc :inputs (rest inputs)))))
              :output-fn (fn [state x]
                           (let [state (if (nil? state) [] state)]
                             (when *debug* (println "output" x))
                             (conj state x))))
      ((juxt :mem :output-state))))


(defn robot 
  "starts the program in a thread. Returns a map with tree values:
   :input channel for reading input data
   :output channel for writing output data
   :done channel for writing internal state after the robot shuts down"
  [program]
  (let [in (chan 3)
        out (chan 3)
        done (thread (let [ret (interpret program
                                          :input-fn (fn [state] (assoc state :value (<!! in)))
                                          :output-fn (fn [state x] (>!! out x) state))]
                       (when *debug* (println "robot done"))
                       ;(close! in)
                       ;(close! out)
                       ret))]
    {:input in
     :output out
     :done done}
    ))