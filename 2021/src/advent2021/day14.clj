(ns advent2021.day14
  (:require [clojure.string :as string]))

(def testdata "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")


(def data (advent2021.core/load-data 14 session))

(defn find-pairs [^String template]
  (let [pair-list (for [i (range (dec (count template)))]
                    (.substring template i (+ 2 i)))]
    (frequencies pair-list)))

(defn parse [input]
  (let [lines (string/split-lines input)
        template (first lines)]
    {:template-string template
     :pairs (find-pairs template)
     :template (seq template)
     :last (last template)
     :subst
     (->> (rest (rest lines))
          (map #(let [[_ from to] (re-matches #"([A-Z]+) -> ([A-Z]+)" %)]
                  [from (first to)]))
          (into {}))}))

(defn step [d]
  (loop [[template-letter & template-tail] (:template d)
         new-string []]
    (if (empty? template-tail)
      {:template (conj new-string template-letter)
       :subst (:subst d)}
      (let [letter-pair (str template-letter (first template-tail))
            insert-letter (get (:subst d) letter-pair)]
        (recur template-tail
               (if (nil? insert-letter)
                 (conj new-string template-letter)
                 (conj new-string template-letter insert-letter)))))))

(defn task-1 [data steps]
  (let [sorted-frequencies
        (->> (take (inc steps) (iterate step (parse data)))
             last
             :template
             seq
             frequencies
             (sort-by second >))
        top (first sorted-frequencies)
        bottom (last sorted-frequencies)]
    (- (second top)
       (second bottom))))

(defn my-inc [i j] (if (number? i) (+ i j) j))

(defn step-pairs [{:keys [pairs subst]}]
  {:pairs
   (reduce (fn [pairs [subst-pair cur-val]]
             (let [[a c] subst-pair
                   b (get subst subst-pair)]
               (-> pairs
                   (update subst-pair my-inc (- cur-val))
                   (update (str a b) my-inc cur-val)
                   (update (str b c) my-inc cur-val))))
           pairs
           (filter #(subst (first %)) pairs))
   :subst subst})

(let [{:keys [pairs subst]} (parse testdata)]
  (filter #(subst (first %)) pairs))

(defn task-2 [data steps]
  (let [d (parse data)
        sorted-frequencies (->> (loop [d d
                                       steps steps]
                                  (if (= steps 0)
                                    (:pairs d)
                                    (recur (step-pairs d) (dec steps))))
                                (reduce (fn [component-count [pair count]]
                                          (update component-count (first pair) my-inc count))
                                        {(:last d) 1})
                                (sort-by second >))
        top (first sorted-frequencies)
        bottom (last sorted-frequencies)
        ]
    (- (second top)
       (second bottom))))

(task-1 data 10)
(task-2 data 40)
