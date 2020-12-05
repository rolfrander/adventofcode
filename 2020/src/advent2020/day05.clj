(ns advent2020.day05)

(def data (slurp "day05.txt"))

(defn find-missing [list]
  (get-in
   (reduce (fn [[acc prev] this]
             (if (= (- this prev) 2)
               [(conj acc (dec this)) this]
               [acc this]))
           [[] -1]
           list)
   [0 0]))

(->> data
     (re-seq #"[FBLR]+")
     (map
      (fn [line] (as-> line $
                   (map #(case %
                           \F \0
                           \B \1
                           \L \0
                           \R \1
                           %)
                        $)
                   (clojure.string/join $)
                   (Integer/parseInt $ 2))))
     (sort)
     (find-missing))

