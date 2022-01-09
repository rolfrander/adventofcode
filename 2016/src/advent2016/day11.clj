(ns advent2016.day11
  (:require [advent2016.core :as advent2016]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.string :as string]))


(def floor (fd/domain 1 2 3 4))

(defn next-floor [current next]
  (l/all (fd/in current next floor)
         (l/conde [(fd/+ current 1 next)]
                  [(fd/+ next 1 current)])))

(defn valid-state [microchips generators]
  (l/project [generators]
             (l/everyg #(l/conda [(l/featurec microchips {% (generators %)})]
                                 [(l/fresh [f]
                                           (l/featurec microchips {% f})
                                           (l/membero f (vals generators)))
                                  l/fail]
                                 [l/succeed])
                       (keys generators))))

(comment
  (l/run* [m g]
          (l/== m {:hydrogen 1 :litium 2})
          (l/== g {:hydrogen 1 :litium 1})
          (valid-state m g)) ; ok

  (l/run* [m g]
          (l/== m {:hydrogen 1 :litium 1})
          (l/== g {:hydrogen 1 :litium 2})
          (valid-state m g)) ; fail

  (l/run* [m g]
          (l/== m {:hydrogen 2 :litium 3})
          (l/== g {:hydrogen 2 :litium 1})
          (valid-state m g)) ; ok
  )

(defn move-1-or-2 [c-floor n-floor input output]
  (l/fresh [_1 _2 _3 e1 e2]
           (l/project [input]
                      (l/rembero [e1 c-floor] (seq input) _1))
           (l/conde [(l/conso [e1 n-floor] _1 _3)]
                    [(l/rembero [e2 c-floor] _1 _2)
                     (l/appendo [[e1 n-floor] [e2 n-floor]] _2 _3)])
           (l/project [_3]
                      (l/== output (into {} _3)))))

(comment
  (l/run* [output]
          (move-1-or-2 1 2 {:a 1 :b 1 :c 2} output)))

(defn move-both [current next]
  (letfn [(move-1 [e c-floor n-floor input output]
            (l/fresh [_1 _2]
                     (l/project [input]
                                (l/rembero [e c-floor] (seq input) _1)
                                (l/conso [e n-floor] _1 _2))
                     (l/project [_2]
                                (l/== output (into {} _2)))))]

    (l/fresh [c-floor n-floor c-m n-m c-g n-g tmp-m tmp-g e]
             (l/== {:elevator c-floor :microchips c-m :generators c-g} current)
             (l/== {:elevator n-floor :microchips n-m :generators n-g} next)

             (move-1 e c-floor n-floor c-m n-m)
             (move-1 e c-floor n-floor c-g n-g))))

(comment
  (l/run* [output]
          (l/fresh [a b c]
                   (l/== {:elevator 2 :microchips b :generators c} output)
                   (move-both {:elevator 1
                               :microchips [[:hydrogen 1] [:litium 1]]
                               :generators [[:hydrogen 1] [:litium 3]]} output))))

(defn next-state [current next]
  (l/fresh [c-floor n-floor c-m n-m c-g n-g]
           (l/== {:elevator c-floor :microchips c-m :generators c-g} current)
           (l/== {:elevator n-floor :microchips n-m :generators n-g} next)
           (next-floor c-floor n-floor)
           (l/conde [(move-1-or-2 c-floor n-floor c-m n-m) (l/== n-g c-g)]
                    [(move-1-or-2 c-floor n-floor c-g n-g) (l/== n-m c-m)]
                    [(move-both   current next)])
           (valid-state n-m n-g)))

(comment
  (doseq [s (l/run* [res]
                    (l/fresh [a b l1 l2]
                             (next-state start-state a)
                             (l/== l1 (l/llist start-state a []))

                             (next-state a b)
                             (not-seen b l1)
                             (l/== l2 (l/llist b l1))

                             (next-state b res)
                             (not-seen res l2)))]
    (print-state s)
    (newline)))

; sjekker at state ikke er i state-list
(defn not-seen [state state-list]
  (l/conda [(l/membero state state-list) l/fail]
           [l/succeed]))

(defn step [state state-list]
  (l/fresh [next-states next]
           (l/conde [(l/== end-state state) (l/emptyo state-list)]
                    [(next-state state next)
                     ;(not-seen next state-list)
                     (step next next-states)
                     (l/conso next next-states state-list)])))

(defn print-state [{:keys [elevator generators microchips]}]
  (let [elements (sort (keys generators))
        print-item (fn [e floor check-list mnemonic]
                     (printf "%s " (if (= (check-list e) floor)
                                     (format "%c%c" (Character/toUpperCase (first (name e))) mnemonic)
                                     ". ")))]
    (doseq [floor [4 3 2 1]]
      (printf "F%d %s " floor (if (= floor elevator) "E" "."))
      (doseq [e elements]
        (print-item e floor generators \G)
        (print-item e floor microchips \M))
      (newline)))
  true)

(defn print-all [states]
  (doseq [s states]
    (print-state s)
    (newline)))

;(print-state start-state)

(print-all
 (l/run* [state]
         (next-state {:elevator 3
                      :microchips {:hydrogen 3 :litium 3}
                      :generators {:hydrogen 3 :litium 3}} state)))

(doseq [s (first
           (l/run 1 [states]
                  (step {:elevator 3
                         :microchips {:hydrogen 3 :litium 3}
                         :generators {:hydrogen 3 :litium 3}}
                        states)))]
  (print-state s)
  (newline))

;;;;

(def testdata "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.")

(defn parse-line [line]
  (let [tokens (re-seq #"The ([a-z]*) floor|a ([a-z]*)-compatible microchip|a ([a-z]*) generator" line)]
    (reduce (fn [[floor microchips generators] [all t-floor t-microchip t-generator]]
              (cond t-floor [(case t-floor
                               "first" 1
                               "second" 2
                               "third" 3
                               "fourth" 4)
                             microchips generators]
                    t-microchip [floor (cons (.intern t-microchip) microchips) generators]
                    t-generator [floor microchips (cons (.intern t-generator) generators)]
                    :else [floor microchips generators]))
            [0 '() '()]
            tokens)))

(defn parse [input]
  (reduce (fn [res [floor microchips generators]]
            (as-> res res
              (reduce #(update %1 :microchips assoc %2 floor) res microchips)
              (reduce #(update %1 :generators assoc %2 floor) res generators)))
          {:elevator 1 :microchips {} :generators {}}
          (map parse-line (string/split-lines input))))

;(parse testdata)

;;;;

(defn heuristic [state]
  (let [h (fn [l] (reduce + (map #(- 4 %) (vals l))))]
    (+ (h (:generators state))
       (h (:microchips state)))))

;(heuristic end-state)


(defn my-valid-state [{:keys [microchips generators]}]
  (let [generator-floors (vals generators)
        elements (keys generators)]
    (every? (fn [e]
              (let [microchip-floor (microchips e)]
                (or (= microchip-floor (generators e))
                    (not (some #(= microchip-floor %) generator-floors)))))
            elements)))


(comment
  (let [s {:elevator 1
           :microchips {:hydrogen 1 :litium 2}
           :generators {:hydrogen 1 :litium 1}}]
    (my-valid-state s)
    (bit-valid-state (state-to-bitmap s))) ; ok

  (let [s {:elevator 1
           :microchips {:hydrogen 1 :litium 1}
           :generators {:hydrogen 1 :litium 2}}]
    (my-valid-state s)
    (bit-valid-state (state-to-bitmap s))) ; fail

  (let [s {:elevator 1
           :microchips {:hydrogen 2 :litium 3}
           :generators {:hydrogen 2 :litium 1}}]
    (my-valid-state s)
    (bit-valid-state (state-to-bitmap s))) ; ok
  )

(set! *warn-on-reflection* true)

(defn my-next-state [{:keys [elevator microchips generators]}]
  (let [move-1 (fn [next-floor e-map]
                 (keep (fn [[k v]] (when (= v elevator) (assoc e-map k next-floor))) e-map))
        move-1-or-2 (fn [next-floor e-map]
                      (let [first-move (move-1 next-floor e-map)
                            second-move (into #{} (mapcat (partial move-1 next-floor) first-move))]
                        (concat first-move second-move)))
        move-both (fn [s]
                    (keep #(when (= elevator (microchips %) (generators %))
                             {:elevator s
                              :microchips (assoc microchips % s)
                              :generators (assoc generators % s)})
                          (keys generators)))
        next-floor (filter #(<= 1 % 4) (map #(% elevator) [inc dec]))]

    (->> (mapcat (fn [e] (concat
                          (map (fn [m] {:elevator e :microchips m :generators generators})
                               (move-1-or-2 e microchips))
                          (map (fn [g] {:elevator e :microchips microchips :generators g})
                               (move-1-or-2 e generators))
                          (move-both e)))
                 next-floor)
         flatten
         (filter my-valid-state))))

(defn bitmap-floor [d shift]
  (bit-and (bit-shift-right d shift) 0x3))

(defn state-to-bitmap
  "converts a map-encoded state to a bitmap where the least significant 8 bits are the number of elements. 
   Then two bits for the elevator and a list of microchips and generator with 2 bits per element. Valid floors are 0-3."
  [state]
  (let [elements (sort (keys (:generators state)))
        element-cnt (byte (count elements))
        shift-elevator 8
        shift-microchips (+ shift-elevator 2)
        shift-generators (+ shift-microchips (* 2 element-cnt))
        objects-to-bitmap (fn [element-map]
                            (reduce (fn [res element]
                                      (bit-or (bit-shift-left res 2)
                                              (dec (element-map element))))
                                    0
                                    elements))]
    (bit-or element-cnt
            (bit-shift-left (dec (:elevator state)) shift-elevator)
            (bit-shift-left (objects-to-bitmap (:microchips state)) shift-microchips)
            (bit-shift-left (objects-to-bitmap (:generators state)) shift-generators))))

(defn bitmap-to-state [bitmap elements]
  (let [elements (reverse (sort elements))
        element-cnt (bit-and bitmap 0xff)
        shift-elevator 8
        shift-microchips (+ shift-elevator 2)
        shift-generators (+ shift-microchips (* 2 element-cnt))
        bitmap-to-floor (fn [bitmap shift] (inc (bitmap-floor bitmap shift)))
        bitmap-to-objects (fn [bitmap shift]
                            (first (reduce (fn [[res bitmap] element]
                                             [(assoc res element (bitmap-to-floor bitmap 0))
                                              (bit-shift-right bitmap 2)])
                                           [{} (bit-shift-right bitmap shift)]
                                           elements)))]
    {:elevator (bitmap-to-floor bitmap shift-elevator)
     :microchips (bitmap-to-objects bitmap shift-microchips)
     :generators (bitmap-to-objects bitmap shift-generators)}))

; (printf "%x\n" (state-to-bitmap (parse testdata)) [:lit :hyd])

(defn bit-valid-state [state]
  (let [element-cnt (bit-and state 0xff)
        shift-elevator 8
        shift-microchips (+ shift-elevator 2)
        shift-generators (+ shift-microchips (* 2 element-cnt))
        microchips (bit-shift-right state shift-microchips)
        generators (bit-shift-right state shift-generators)
        ]
    (every? (fn [e]
              (let [bitmap (bit-shift-left 0x3 e)
                    microchip-floor (bitmap-floor microchips e)]
                (or (= (bit-and bitmap microchips) (bit-and bitmap generators))
                    (not (some #(= microchip-floor (bitmap-floor generators %)) (range 0 (* 2 element-cnt) 2))))))
            (range 0 (* 2 element-cnt) 2))))

(defn bit-next-state [state]
  (let [element-cnt (bit-and state 0xff)
        shift-elevator 8
        shift-microchips (+ shift-elevator 2)
        shift-generators (+ shift-microchips (* 2 element-cnt))
        microchips (bit-shift-right state shift-microchips) ; microchip bitmap also contains generators
        generators (bit-shift-right state shift-generators)
        elevator (bitmap-floor state shift-elevator)

        bit-assoc (fn [bitmap bit newval]
                    (-> bitmap
                        (bit-clear bit)
                        (bit-clear (inc bit))
                        (bit-or (bit-shift-left newval bit))))
        move-1 (fn [next-floor bitmap]
                 (keep (fn [e] (when (= (bitmap-floor bitmap e) elevator) (bit-assoc bitmap e next-floor)))
                       (range 0 (* 2 element-cnt) 2)))
        move-1-or-2 (fn [next-floor bitmap]
                      (let [first-move (move-1 next-floor bitmap)
                            second-move (into #{} (mapcat (partial move-1 next-floor) first-move))]
                        (concat first-move second-move)))
        move-both (fn [next-elevator]
                    (keep #(when (= elevator (bitmap-floor microchips %) (bitmap-floor generators %))
                             (bit-or element-cnt
                                     (bit-shift-left next-elevator shift-elevator)
                                     (-> microchips ; also contains generators
                                         (bit-shift-left shift-microchips)
                                         (bit-and-not (bit-shift-left           0x3 (+ % shift-microchips)))
                                         (bit-and-not (bit-shift-left           0x3 (+ % shift-generators)))
                                         (bit-or      (bit-shift-left next-elevator (+ % shift-microchips)))
                                         (bit-or      (bit-shift-left next-elevator (+ % shift-generators))))))
                          (range 0 (* 2 element-cnt) 2)))
        next-floor (filter #(<= 0 % 3) (map #(% elevator) [inc dec]))]
    (->> (mapcat (fn [e] (concat
                          (map (fn [m] (bit-or element-cnt
                                               (bit-shift-left e shift-elevator)
                                               (bit-shift-left m shift-microchips))) ; also contains generators
                               (move-1-or-2 e microchips))
                          (map (fn [g] (bit-or element-cnt
                                               (bit-shift-left e shift-elevator)
                                               (bit-and-not (bit-shift-left microchips shift-microchips)
                                                            (bit-shift-left -1 shift-generators))  ; must renove generators
                                               (bit-shift-left g shift-generators)))
                               (move-1-or-2 e generators))
                          (move-both e)))
                 next-floor)
         flatten
         (filter bit-valid-state))))

(comment (->> (parse testdata)
              state-to-bitmap
              bit-next-state
              (mapcat bit-next-state)
              (map #(bitmap-to-state % [:lit :hyd]))
              print-all)
         )

(defn end-state [elements]
  {:elevator 4
   :microchips (zipmap elements (repeat 4))
   :generators (zipmap elements (repeat 4))})

(defn task-1-logic [start-state]
  (let [elements (keys (:microchips start-state))
        e (end-state elements)]
    (advent2016/a-star
     start-state
     #(= e %)
     (constantly 1)
     heuristic
     (fn [_u path] path)
     #(l/run* [state]
              (next-state % state))
     :count)))

(defn task-1-plain [start-state]
  (let [elements (keys (:microchips start-state))
        e (end-state elements)]
    (advent2016/a-star
     start-state
     #(= e %)
     (constantly 1)
     heuristic
     (fn [_u path] path)
     my-next-state
     :count)))

(defn task-1-bitmap [start-state]
  (let [elements (keys (:microchips start-state))
        e (state-to-bitmap (end-state elements))]
    (advent2016/a-star
     (state-to-bitmap start-state)
     #(= e %)
     (constantly 1)
     heuristic
     (fn [_u path] path)
     bit-next-state
     :count)))

(binding [advent2016/*debug* false]
  (let [d (parse testdata)]
    (println (time (task-1-logic d)))
    (println (time (task-1-plain d)))
    (println (time (task-1-bitmap d)))))


(binding [advent2016/*debug* false]
  (let [d (parse (advent2016/get-data 2016 11))]
    (println (time (task-1-plain  d)))    ; Elapsed time: 8415.0189 msecs
    (println (time (task-1-bitmap d))))) ; Elapsed time: 8412.7953 msecs

(print-all (my-next-state (second (mapcat my-next-state (my-next-state (parse testdata))))))

(let [s (state-to-bitmap (second (mapcat my-next-state (my-next-state (parse testdata)))))]
  (time (dotimes [i 10000] (doall (bit-next-state s))))) ; 0.35 sekunder

(let [s (second (mapcat my-next-state (my-next-state (parse testdata))))]
  (time (dotimes [i 10000] (doall (my-next-state s))))) ; 0.4 sekunder

(let [s (second (mapcat my-next-state (my-next-state (parse testdata))))]
  (time (dotimes [i 10000] (doall (l/run* [state]
                                          (next-state s state)))))) ; 22 sekunder, run-nc: 19.6 sekunder

(let [data (parse (advent2016/get-data 2016 11))
      data (reduce #(assoc-in %1 %2 1)
                   data
                   (for [type [:microchips :generators]
                         element ["elerium" "dilithium"]]
                     [type element]))]
  (binding [advent2016/*debug* true]
    (task-1-plain data)))
