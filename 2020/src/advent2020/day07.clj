(ns advent2020.day07
  (:require [clojure.string :as str]))


(def testdata "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(def testdata2 "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(defn parse-line
  "returns a tuple [bagcolor {map of bagcolor: number-of-contained-bags}]"
  [line]
  (let [[all bagcolor contents] (re-matches #"([a-z]+ [a-z]+) bags contain (.*)" line)]
    (if (re-matches #"no " contents)
      [bagcolor {}]
      [bagcolor (reduce (fn [acc [_ count color]]
                          (assoc acc color (Integer/parseInt count)))
                        {}
                        (re-seq #"([0-9]+) ([a-z]+ [a-z]+) bags?[,. *]" contents))])))

(defn build-structure [data]
  (->> data
       str/split-lines
       (map parse-line)
       (reduce #(assoc %1 (first %2) (second %2))
               {})))

; for hver veske v
;   for hver veske c i contains-listen
;     legg referanse c til v inn i resultatsett
(defn make-parent-structure [data]
  (reduce-kv (fn [result parent children]
               (reduce (fn [inner-result child]
                         (update inner-result child #(if (nil? %) 
                                                       #{parent}
                                                       (conj % parent))))
                       result
                       (keys children)))
             {}
             data))

(defn collect-parents [data child]
  (letfn [(collect [parentlist]
                   (reduce (fn [result p]
                             (into (conj result p)
                                   (collect (get data p))))
                           #{}
                           parentlist))]
    (collect (get data child)))
    )

; summer, for hvert barn
;   produktet av antall barn med rekursivt kall som teller
(defn count-children [data parent]
  (letfn [(collect [childlist]
                   (reduce-kv (fn [accum child count]
                                (+ accum (* count (collect (get data child)))))
                              1
                              childlist))]
    (dec (collect (get data parent)))))

(defn parse-and-count-parents [data child]
  (let [tree (build-structure data)
        reverse-tree (make-parent-structure tree)]
    (count (collect-parents reverse-tree child))
  ;(get reverse-tree "shiny gold")
    ))

(defn parse-and-count-children [data parent]
  (count-children (build-structure data)
                  parent))

(parse-and-count-parents testdata "shiny gold") ;; => 4
(parse-and-count-parents (slurp "day07.txt") "shiny gold") ;; => 126 

(parse-and-count-children testdata "shiny gold") ;; => 32
(parse-and-count-children testdata2 "shiny gold") ;; => 126 
(parse-and-count-children (slurp "day07.txt") "shiny gold") ;; => 220149

