(ns advent2015.day17
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)

(def containers [43 3 4 10 21 44 4 6 47 41 34 17 17 44 36 31 46 9 27 38])

(defn fit-containers [next-containers rest-volume container-count]
  (cond (= rest-volume 0) (list container-count)

        (empty? next-containers) nil

        :else
        (let [[next-container & rest-containers] next-containers
              with-next-container (if (<= next-container rest-volume)
                                    (fit-containers rest-containers (- rest-volume next-container) (inc container-count))
                                    nil)
              without-next-container (fit-containers rest-containers rest-volume container-count)]
          (when *debug* (println with-next-container without-next-container))
          (concat with-next-container without-next-container))))

(binding [*debug* false]
  (frequencies (fit-containers [20 15 10 5 5] 25 0)))

(frequencies (fit-containers (sort containers) 150 0))
;; => {10 11, 9 110, 8 356, 7 552, 6 441, 5 151, 4 17}

;; => 1638
