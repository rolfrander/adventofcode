(ns advent2019.day07
  (:require
   [advent2019.intcode :as ic]
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.core.async :refer [>!! <!! chan thread pipe close!]]))


;; *** day 7 ***

(defn day-7 [program-text]
  (let [program (ic/parse program-text)
        run (fn [phase-settings]
              (reduce #(get-in (ic/interpret-with-input program [%2 %1]) [1 0])
                      0 phase-settings))]
    (->> (map run (puzzle/permute [0 1 2 3 4]))
         (apply max))))


(binding [ic/*debug* false]
  (day-7 "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))

(day-7 "3,23,3,24,1002,24,10,24,1002,23,-1,23,
101,5,23,23,1,24,23,23,4,23,99,0,0") ; [0 1 2 3 4]

(day-7 "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"); [1 0 4 3 2]

(day-7 (puzzle/get-data 2019 7))
;;=> 47064

(defn day-7b [program-text]
  (let [program (ic/parse program-text)
        run (fn [phase-settings]
              (let [channels (vec (repeatedly 6 #(chan 3)))
                    ;; set up amplifiers interconnected by channels
                    amps (->> (map #(thread (ic/interpret program
                                                       :input-fn (fn [state] (assoc state :value (<!! (channels %))))
                                                       :output-fn (fn [state x] (>!! (channels (inc %)) x) state))
                                            (close! (channels (inc %))))
                                   (range (dec (count channels))))
                              (doall)
                              (vec))]
                ;; insert phase-settings into each channel
                (doseq [[phase channel] (map vector phase-settings channels)]
                  (>!! channel phase))
                (pipe (channels 5) (channels 0))
                (>!! (channels 0) 0)
                (doseq [a amps]
                  (<!! a))
                (<!! (channels 0))))]
    (->> (map run (puzzle/permute [5 6 7 8 9]))
         (apply max))))

(defn day-7b2 [program-text phase-settings]
  (let [program (ic/parse program-text)
        run (fn [phase-settings]
              (let [amps (->> (repeatedly 5 #(ic/robot program))
                              (doall)
                              (vec))]
                ;; insert phase-settings into each channel
                
                (doseq [i (range 4)]
                  (pipe (get-in amps [i :output])
                        (get-in amps [(inc i) :input])))

                (pipe (get-in amps [4 :output])
                      (get-in amps [0 :input]))

                (doseq [[phase amp] (map vector phase-settings amps)]
                  (>!! (amp :input) phase))

                (>!! ((amps 0) :input) 0)

                (doseq [a amps]
                  (<!! (a :done)))
                (<!! ((amps 0) :input))))]
    ;(->> (map run (puzzle/permute [5 6 7 8 9]))
    ;     (apply max))
    
    (run phase-settings )
    ))

(day-7b2 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" [9 8 7 6 5])
;;=> 139629729

(day-7b "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
;;=> 18216

(day-7b (puzzle/get-data 2019 7))
;;=> 4248984
