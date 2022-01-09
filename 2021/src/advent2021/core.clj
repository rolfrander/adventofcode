(ns advent2021.core
  (:require [clj-http.client :as http]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn load-data [day session]
  (->> (http/get (format "https://adventofcode.com/2021/day/%d/input" day)
                 {:cookies {"session" {:value session}}})
       :body))

(defn split-by [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [[xs ys] (split-with pred s)]
       (if (seq xs)
         (cons xs (split-by pred ys))
         (let [!pred (complement pred)
               skip (take-while !pred s)
               others (drop-while !pred s)
               [xs ys] (split-with pred others)]
           (cons (concat skip xs)
                 (split-by pred ys))))))))
