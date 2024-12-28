(ns advent2019.day11
  (:require
   [advent2019.intcode :as ic]
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.core.async :refer [>!! <!! chan thread pipe close!]]))

(def testchannel (chan 2))

(defn robot-controller [start-color input output]
  (let [move (puzzle/move-fn :sq-4 :infinite)
        ;; inverted directions, "up" is south
        turn-right {"n" "w"
                   "e" "n"
                   "s" "e"
                   "w" "s"}
        turn-left {"n" "e"
                    "e" "s"
                    "s" "w"
                    "w" "n"}
        turn {0 turn-left
              1 turn-right}]
    (>!! output start-color) ; initial panel
    (loop [color (<!! input)
           direction (<!! input)
           pos [0 0]
           curdir "s"
           paint {}]
      (if (nil? color)
        paint
        (let [paint (assoc paint pos color)
              newdir ((turn direction) curdir)
              newpos (move pos newdir)]
          (>!! output (get paint newpos 0)) ; camera register color of new position, black for anything not painted
          (recur (<!! input) (<!! input) newpos newdir paint))))))

(defn solve-1 [program-text]
  (let [{:keys [input output _done]} (ic/robot (ic/parse program-text))]
    (count (robot-controller 0 output input))))

(defn test-log-collector [outputs]
  (let [robot-out (chan 3)
        robot-in (chan 3)
        _t1 (thread (doseq [o outputs]
                      (>!! robot-out o)) 
                    (close! robot-out))
        _t2 (thread (loop [i (<!! robot-in)]
                      (recur (<!! robot-in))))]
    (robot-controller 0 robot-out robot-in)))

;(for [k [max-key min-key]
;      d [first second]]
;  (d (apply k d (keys {[0 0] 0, '(-1 0) 0, '(-1 -1) 1, '(0 -1) 1, '(1 0) 1, '(1 1) 1}))))


(defn print-registration [paint]
  (let [[max-x max-y min-x min-y] (for [k [max-key min-key]
                                        d [first second]]
                                    (d (apply k d (keys paint))))
        idx (reduce-kv #(update %1 %3 conj (map - %2 [min-x min-y])) {} paint)]
    (puzzle/draw-map {:width (inc (- max-x min-x))
                      :height (inc (- max-y min-y))
                      :markings {\. (idx 0) \# (idx 1)}
                      })))

(defn solve-2 [program-text]
  (let [{:keys [input output _done]} (ic/robot (ic/parse program-text))]
    (print-registration (robot-controller 1 output input))))


(count (test-log-collector [1 0 0 0 1 0 1 0 0 1 1 0 1 0]))
;;=> 6

(print-registration (test-log-collector [1 0 0 0 1 0 1 0 0 1 1 0 1 0]))

(solve-1 (puzzle/get-data 2019 11))
;;=> 2441
(solve-2 (puzzle/get-data 2019 11))