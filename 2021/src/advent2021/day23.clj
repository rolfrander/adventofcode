(ns advent2021.day23
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :as string]))

;;
;; #############
;; #...........#
;; ###B#C#B#D###
;;   #A#D#C#A#
;;   #########
;;
;; positions
;; #########################
;; #.0.1.2.3.4.5.6.7.8.9.10#
;; #####11##13##15##17######
;;     #12##14##16##18#
;;     #19##21##23##25#
;;     #20##22##24##26#
;;     ################

;; valid moves

(def neighbours
  ; for each from-position, a list of to-pos, temporary or end
  {0 [1]
   1 [0 2]
   2 [1 11 3]
   3 [2 4]
   4 [3 13 5]
   5 [4 6]
   6 [5 15 7]
   7 [6 8]
   8 [7 17 9]
   9 [8 10]
   10 [9]
   11 [2 12]
   12 [11 19]
   13 [4 14]
   14 [13 21]
   15 [6 16]
   16 [15 23]
   17 [8 18]
   18 [17 25]
   19 [12 20]
   20 [19]
   21 [14 22]
   22 [21]
   23 [16 24]
   24 [23]
   25 [18 26]
   26 [25]})

(def valid-end-states
  ; end pos => condition
  ; condition: [type pos?] (end pos is only valid of amphipod is of given type and pos is taken
  {11 [:A 12]
   12 [:A 19]
   13 [:B 14]
   14 [:B 21]
   15 [:C 16]
   16 [:C 23]
   17 [:D 18]
   18 [:D 25]
   19 [:A 20]
   20 [:A nil]
   21 [:B 22]
   22 [:B nil]
   23 [:C 24]
   24 [:C nil]
   25 [:D 26]
   26 [:D nil]})

(def energy {:A 1 :B 10 :C 100 :D 1000})

(def tmp-state #{0 1 3 5 7 9 10})

(def valid-positions (into tmp-state (keys valid-end-states)))

(defn dijkstra [positions source weight-fn neighbour-fn]
  (loop [dist (-> (into {} (map vector positions (repeat 999)))
                  (assoc source 0))
         prev {}
         Q (into (priority-map) dist)]
    (if (empty? Q)
      [dist prev]
      (let [u (first (peek Q))
            Q (pop Q)]
        (when (Q u) (throw (RuntimeException. (str u " is still in Q"))))
        (let [[new-dist new-prev new-q]
              (->> (filter Q (neighbour-fn u)) ; for each neighbour v of u, still in Q
                   (reduce (fn [[new-dist new-prev q] v]
                             (let [alt (+ (new-dist u) (weight-fn v))] ; alt <- dist[u] + length(u,v)
                               (if (< alt (new-dist v))                ; if alt < dist[v]
                                 [(assoc new-dist v alt)               ;   dist[v] <- alt
                                  (assoc new-prev v u)                 ;   prev[v] <- u
                                  (assoc q v alt)]                     ;   Q.decrease_priority(v, alt)
                                 [new-dist new-prev q])))
                           [dist prev Q]))]
          (recur new-dist new-prev new-q))))))

(defn find-all-distances [from-positions to-positions]
  (for [start from-positions]
    [start
     (let [[dist prev] (dijkstra (keys neighbours) start (constantly 1) neighbours)]
       (letfn [(get-path [dest]
                 (if-let [prev (prev dest)]
                   (conj (get-path prev) dest)
                   []))
               (prepare-passing-list [dest]
                 (let [path (get-path dest)]
                   (if (empty? path)
                     path
                     (filter valid-positions (pop path)))))]
         (for [d to-positions]
           {:to d
            :passing (prepare-passing-list d)
            :dist (dist d)})))]))

(def all-valid-moves
  (into {} (concat
            (find-all-distances (keys valid-end-states) tmp-state)
            (find-all-distances tmp-state (keys valid-end-states)))))

(def testdata {11 :B
               12 :D
               13 :C
               14 :C
               15 :B
               16 :B
               17 :D
               18 :A
               19 :D
               20 :A
               21 :B
               22 :D
               23 :A
               24 :C
               25 :C
               26 :A})

(defn correct-pos [[pos type]]
  (when-let [[required-type _checkpos] (valid-end-states pos)]
    (= required-type type)))

(defn find-valid-moves [state from]
  (let [type (state from)
        [required-type checkpos] (valid-end-states from)]
    (if (and (not (nil? required-type))
             (= type required-type)
             (or (nil? checkpos)
                 (= type (state checkpos))))
      '()
      (->> (all-valid-moves from)
           (remove #(state (:to %)))
           (remove #(some state (:passing %)))
           (remove #(when-let [[required-type checkpos] (valid-end-states (:to %))]
                      (or (not= required-type type)
                          (and (not (nil? checkpos))
                               (or (not (contains? state checkpos))
                                   (not= (state checkpos) required-type)))
                          false)))))))

(comment
  (find-valid-moves testdata 12) ; ()
  (find-valid-moves testdata 11) ; (0..10)
  (find-valid-moves {11 :B 12 :A 5 :C 14 :D 10 :B 16 :C 17 :D 18 :A} 5) ; 15
  (find-valid-moves {11 :B 12 :A 5 :C 14 :D 10 :B 13 :C 17 :D 18 :A} 5) ; 16
  (find-valid-moves {11 :B 12 :A 5 :C 14 :D 10 :B 16 :C 17 :D 18 :A} 17) ; 7, 9
  (find-valid-moves {11 :B 12 :A 5 :C 14 :A 10 :B 16 :C 17 :D 18 :D} 17) ; ()
  )

(defn is-endpos [state]
  (letfn []
    (every? correct-pos state)))

(comment
  (is-endpos testdata) ; false
  (is-endpos {11 :A
              12 :A
              13 :B
              14 :B
              15 :C
              16 :C
              17 :D
              18 :D}) ; true
  )

(defn move [state from to]
  (let [amph-type (state from)]
    (-> state
        (dissoc from)
        (assoc to amph-type))))

;(move testdata 13 9)

(defn print-state [state]
  (letfn [(s [i] (if-let [t (state i)]
                   (.charAt (str t) 1)
                   \.))]
    (printf "#############\n")
    (printf "#%s#\n" (string/join (map s (range 0 11))))
    (apply printf "###%c#%c#%c#%c###\n" (map s [11 13 15 17]))
    (apply printf "###%c#%c#%c#%c###\n" (map s [12 14 16 18]))
    (apply printf "###%c#%c#%c#%c###\n" (map s [19 21 23 25]))
    (apply printf "###%c#%c#%c#%c###\n" (map s [20 22 24 26]))
    (printf "  #########\n")))

(print-state testdata)

(def task-1 (memoize (fn [state]
                       (if (is-endpos state)
                         0
    ;; for hver pos p i state
    ;;   finn liste av gyldige nesteposisjoner next
    ;;   for hver nesteposisjon n i next
    ;;     e = flytteenergi til n + kall rekursivt (flytt til n)
    ;;   finn minimum e
                         (loop [[p & other-p] (keys state)
                                cur-min Long/MAX_VALUE]
                           (if (nil? p)
                             cur-min
                             (let [possible-moves (find-valid-moves state p)]
                               (if (empty? possible-moves)
                                 (recur other-p cur-min)
                                 (let [possible-min (->> (map (fn [nextpos]
                                                                (let [min-subtask (task-1 (move state p (:to nextpos)))]
                                                                  (if (= min-subtask Long/MAX_VALUE)
                                                                    Long/MAX_VALUE
                                                                    (+ min-subtask (* (energy (state p)) (:dist nextpos))))))
                                                              possible-moves)
                                                         (apply min))]
                                   (recur other-p (min cur-min possible-min)))))))))))

(task-1 testdata)



(defn parse [input]
  (->>
   (map keyword (re-seq #"[ABCD]" input))
   (map vector [11 13 15 17 12 14 16 18 19 21 23 25 20 22 24 26])
   (into {})))

(def data
  (parse
   "#############
#...........#
###D#D#C#C###
  #D#C#B#A#
  #D#B#A#C#
  #B#A#B#A#
  #########"))

(print-state data)
(task-1 data)