(ns advent2019.day15
  (:require
   [advent2019.intcode :as ic]
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.core.async :refer [>!! <!! chan thread pipe close!]]))

(def programtext (puzzle/get-data 2019 15))

(def dir [" " "n" "s" "w" "e"])
(def state [:wall :space :oxygen])
(def state-sym {:wall \#
                :space \.
                :oxygen \o
                :empty \space 
                :droid \D})
(def turn-left [0 3 4 2 1])
(def turn-right [0 4 3 1 2])

(defn format-report [chart pos]
  (let [[max-x max-y min-x min-y] (for [k [max-key min-key]
                                        d [first second]]
                                    (d (apply k d (keys chart))))]
    {:min-x min-x
     :max-x max-x
     :min-y min-y
     :max-y max-y
     :markings (if pos (assoc chart pos :droid) chart)}))

(defn draw-map [{:keys [min-x max-x min-y max-y markings]}]
  (doseq [y (range min-y (inc max-y))
          x (range min-x (inc max-x))]
    (when (and (= x min-x) (> y min-y)) (println))
    (print (state-sym (get markings [x y] :empty))))
  (println))

(def move (puzzle/move-fn :sq-4 :infinite))

(defn robot-controller [input output debug]
  (loop [pos [0 0]
         chart {pos :space}
         direction 1
         has-hit-wall false
         last-report-time (System/currentTimeMillis)]
    (>!! output direction)
    (let [state (state (<!! input))
          new-pos (move pos (dir direction))
          next-dir (fn [hit-wall?]
                     (if hit-wall?
                       (turn-right direction)
                       (turn-left direction)))
          time (System/currentTimeMillis)]
      (case state
        :wall   (let [newchart (assoc chart new-pos :wall)
                      do-report (> (- time last-report-time) 5000)]
                  (when (and debug do-report) (>!! debug (format-report newchart pos)))
                  (recur pos
                         newchart
                         (next-dir true)
                         true
                         (if do-report time last-report-time)))
        :space  (if (= new-pos [0 0]) ; stop when back at the beginning
                  (format-report chart new-pos)
                  (recur new-pos
                         (assoc chart new-pos :space)
                         (next-dir false)
                         has-hit-wall
                         last-report-time))
        :oxygen ;(format-report (assoc chart new-pos :oxygen) [0 0])
        (recur new-pos
               (assoc chart new-pos :oxygen)
               (next-dir false)
               has-hit-wall
               last-report-time)

        [:illegal-state state]))))

(defn solve []
  (let [{:keys [input output _done]} (ic/robot (ic/parse programtext))
        res (robot-controller output input nil)]
    (close! input)
    (close! output)
    res))

(defn solve-1 []
  (let [res (solve)]
    (draw-map res)
    (puzzle/dijkstra (reduce-kv #(if (#{:space :droid :oxygen} %3)
                                   (conj %1 %2)
                                   %1)
                                #{} (:markings res))
                     [0 0]
                     (puzzle/neighbours-fn :sq-4 :infinite)
                     :dest? (fn [pos] (= (get-in res [:markings pos]) :oxygen))
                     :result-type :dist)))

(defn solve-2 []
  (let [chart (:markings (solve))
        chart (reduce-kv #(update %1 %3 conj %2) {} chart)
        space (into #{} (:space chart))
        o2start (first (:oxygen chart))
        n (puzzle/neighbours-fn :sq-4 :infinite)]
    (loop [visited #{}
           current [o2start]
           t 0]
      (let [neighbours (->> (mapcat n current)
                            (remove visited)
                            (filter space)
                            (into #{}))]
        (if (seq neighbours)
          (recur (into visited neighbours) neighbours (inc t))
          t)))))

(solve-2)