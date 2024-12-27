(ns advent2019.day02
  (:require
   [advent2019.intcode :as ic]
   [rolfrander.puzzle-lib :as puzzle]))

;; *** day 2 ***

(def day2-testdata "1,9,10,3,2,3,11,0,99,30,40,50")
(ic/interpret (ic/parse day2-testdata))
;;=> [3500 9 10 70 2 3 11 0 99 30 40 50]

(defn day-2 [noun verb]
  (let [program (ic/parse (puzzle/get-data 2019 2))
        program (assoc program
                       1 noun
                       2 verb)]
    (-> (ic/interpret program)
        :mem
        (get 0))))

(day-2 12 2)
;;=> 3706713

(for [noun (range 0 100)
      verb (range 0 100)
      :when (= 19690720 (day-2 noun verb))]
  (+ (* 100 noun) verb))
;;=> (8609)