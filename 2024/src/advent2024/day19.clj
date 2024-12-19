(ns advent2024.day19
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(defn parse [in]
  (let [[towels _empty & patterns] (str/split-lines in)]
    {:towels (str/split towels #", ?")
     :patterns patterns}))

(def data (puzzle/get-data 2024 19))

(defn towels-to-regex [towels]
  (str "(" (str/join "|" towels) ")+"))

(defn strings-to-tree [s & {:keys [offset] :or {offset 0}}]
  (let [groups (group-by #(get % offset) (filter #(> (count %) offset) s))]
    (if (empty? groups)
      {:end (first s)}
      (let [ret (reduce (fn [groups key]
                          (update groups key #(strings-to-tree % :offset (inc offset))))
                        groups (keys groups))
            ending-now (filter #(= (count %) offset) s)]
        (if (empty? ending-now)
          ret
          (assoc ret :end (first ending-now)))))))

(defn debug [msg x]
  (println "***" msg x)
  x)

(defn match-towels [match-tree match-subtree pattern & {:keys [offset] :or {offset 0}}]
  (if (= offset (count pattern))
    ; end off pattern
    (if (contains? match-subtree :end)
      1
      0)
    
    ; not end of pattern
    (let [sum-subtree (if (contains? match-subtree :end)
                        (match-towels match-tree match-tree pattern :offset offset)
                        0)
          c (get pattern offset)
          c-tree (and match-subtree (match-subtree c))]
      (if (or (nil? c-tree) (instance? String c-tree))
        sum-subtree
        (->> (match-towels match-tree c-tree pattern :offset (inc offset))
             (+ sum-subtree))))))

(defn match-towels-re [re pattern]
  (boolean (re-matches re pattern)))

(defn solve-1 [in]
  (let [data (parse in)
        re (re-pattern (towels-to-regex (:towels data)))]
    (->> (:patterns data)
         (filter (partial match-towels-re re))
         count)
    ))

(strings-to-tree (:towels (parse data)))

(sort (filter #(str/starts-with? % "r") (:towels (parse data))))

(let [data (parse data)
      tree (strings-to-tree (:towels data))]
  (letfn [(print-tree [t indent]
            (if (instance? String t)
              (println (format "%s=> %s" (subs "                  " 0 (* 2 indent)) t))
              (doseq [[k v] t]
                (if (= k :end)
                  (println (format "%s=> %s" (subs "                  " 0 (* 2 indent)) v))
                  (do
                    (println (format "%s%c" (subs "                  " 0 (* 2 indent)) k))
                    (print-tree v (inc indent)))))))]
    ;(print-tree tree 0)
    (match-towels tree tree "rrwuwuurrrguggbrrrrgwwururguggwuwrwwurwuuwbw")))

(defn solve-2 [in]
  (let [data (parse in)
        tree (strings-to-tree (:towels data))]
    (->> (map (partial match-towels tree tree) (:patterns data))
         (apply +))))

(def towel-data (parse testdata))

(defn solve-2b [in]
  (let [towel-data (parse in)
        towel-by-length (group-by count (:towels towel-data))
        patterns (vec (:patterns towel-data))
        match-patterns (fn [pattern towels offset len]
                         (let [substring (.subSequence pattern offset (+ offset len))]
                           (count (filter #(.equals substring %) towels))))
        acc-all-matches-starting-at
        (fn [matches-by-length pattern offset]
          (reduce-kv (fn [res length towels]
                       (if (> (+ offset length) (count pattern))
                         res
                         (update res (+ length offset)
                                 + (* (match-patterns pattern towels offset length)
                                      (matches-by-length offset)))))
                     matches-by-length towel-by-length))

        find-matches-for-pattern
        (fn [pattern]
          (let [matches-by-length (assoc (vec (repeat (inc (count pattern)) 0)) 0 1)]
            (->> (reduce #(acc-all-matches-starting-at %1 pattern %2)
                         matches-by-length (range (count pattern)))
                 peek)))]
    
    (apply + (map find-matches-for-pattern patterns))))


(solve-1 testdata)
;;=> 6
(solve-1 data)
;;=> 308

(solve-2b testdata)
;;=> 16
(solve-2b data)
;;=> 662726441391898
;;=> 