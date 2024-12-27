(ns advent2019.day09
  (:require
   [advent2019.intcode :as ic]
   [rolfrander.puzzle-lib :as puzzle]))


;; *** day 9 ***
(binding [ic/*debug* false]
  (second (ic/interpret-with-input (ic/parse "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99") [])))
;;=>                                   [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99]

(defn test-program [programtext input]
  (second (ic/interpret-with-input (ic/parse programtext) [input])))

(test-program "1102,34915192,34915192,7,4,7,99,0" 0)
;;=> [1219070632396864]
(test-program "104,1125899906842624,99" 0)
;;=>              [1125899906842624]

(test-program (puzzle/get-data 2019 9) 1)
;;=> [2941952859]
;; day 9 part 2:
(test-program (puzzle/get-data 2019 9) 2)
;;=> [66113]
