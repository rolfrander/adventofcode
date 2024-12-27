(ns advent2019.day05
  (:require
   [advent2019.intcode :as ic]
   [rolfrander.puzzle-lib :as puzzle]))


;; *** day 5 ***
(binding [ic/*debug* true] (ic/interpret-with-input (ic/parse "3,0,4,0,99") [42]))
(ic/interpret (ic/parse "1002,4,3,4,33"))
(ic/interpret (ic/parse "1101,100,-1,4,0"))

(defn day-5 [in]
  (->> (ic/interpret-with-input (ic/parse (puzzle/get-data 2019 5)) [in])
       second))

(day-5 1)
;;=> [0 0 0 0 0 0 0 0 0 7265618]

(defn test-program [programtext input]
  (second (ic/interpret-with-input (ic/parse programtext) [input])))

(test-program "3,9,8,9,10,9,4,9,99,-1,8" 9)
(test-program "3,9,7,9,10,9,4,9,99,-1,8" 3)
(test-program "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 1)
(test-program "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" 9)
; 7 => 999, 8 => 1000, 9 => 1001

(day-5 5)
;;=> [7731427]
