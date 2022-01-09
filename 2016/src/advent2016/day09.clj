(ns advent2016.day09
  (:require [advent2016.core :as core]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

;(def ^String data (core/get-data 2016 9))

(defn task-2 [data]
  (letfn [(decompress [start-pos end]
           (loop [total-length 0
                  start-pos start-pos]
             (cond (>= start-pos end)
                   total-length

                   (Character/isAlphabetic (int (.charAt data start-pos)))
                   (recur (inc total-length) (inc start-pos))

                   (= \( (.charAt data start-pos))
                   (let [[all cnt repeat] (map core/safe-parse-number
                                               (re-find #"\(([0-9]+)x([0-9]+)\)" (subs data start-pos
                                                                                       (min (+ start-pos 10)
                                                                                            (.length data)))))
                         after-marker (+ start-pos (count all))
                         new-start (+ after-marker cnt)
                         submessage-length (decompress after-marker new-start)
                         ]
                     (recur (long (+ total-length (* submessage-length repeat))) (long new-start)))

                   :else
                   (recur total-length (inc start-pos)))))]
    (decompress 0 (count data))))

(task-2 "(3x3)XYZ") ; 9
(task-2 "X(8x2)(3x3)ABCY") ; 20
(task-2 "(27x12)(20x12)(13x14)(7x10)(1x12)A") ; 241920
(task-2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") ; 445
(task-2 (core/get-data 2016 9)) ;; => 10755693147
