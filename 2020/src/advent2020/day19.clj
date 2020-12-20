(ns advent2020.day19
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def testdata "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb")

(def testdata2 "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")

(defn parse-sequence [sequence]
  (let [s (str/split sequence #" \| ")
        subrules (map #(into [:sequence] (str/split % #" ")) s)]
    (if (> (count subrules) 1)
      (into [:or] subrules)
      (first subrules))))

(defn parse-rule [rule]
  (if-let [r (re-matches #"([0-9]+): ([0-9 |]+)" rule)]
    [(second r) (parse-sequence (nth r 2))]
    (let [[_ no terminal] (re-matches #"([0-9]+): \"([a-z])\"" rule)]
      [no [:terminal (first terminal)]])))

(defn parse [input]
  (let [[rules _ data] (->> input
                             str/split-lines
                             (partition-by #(= 0 (count %))))]
    {:rules (into {} (map parse-rule rules))
     :data data}
    ))


(defn compile-to-re [rules]
  (letfn [(do-compile [rule]
                      (case (first rule)
                        :terminal (second rule)
                        :sequence (str/join (map #(do-compile (get rules %)) (rest rule)))
                        :or (str "(" (str/join "|" (map do-compile (rest rule))) ")")))]
    (re-pattern (do-compile (get rules "0"))))
  )

(def ^:dynamic *debug* false)

(defn exec 
  ([rules input] (exec rules input "0"))
  ([rules ^String input startrule]
   (let [input-length (.length (str input))]
     (letfn [(do-exec [rule i]
               (let [res
                     (when (and i (< i input-length))
                       (when (and *debug* (string? rule))
                         (printf "%s%s TRY %s %n"
                                 (.substring "                                 " 0 i)
                                 (.substring input i)
                                 rule)
                         (flush))
                       (if (string? rule)
                         (case rule
                           "8"  (do (println "should not be here" i) (comment or (some->> i
                                                 (do-exec "42")
                                                 (do-exec "8"))
                                        (do-exec "42" i)))
                           "11" (or (some->> i
                                             (do-exec "42")
                                             (do-exec "11")
                                             (do-exec "31"))
                                    (some->> i
                                             (do-exec "42")
                                             (do-exec "31")))
                           (do-exec (get rules rule) i))
                         (case (first rule)
                           :terminal (if (= (char (second rule)) (.charAt input i)) (inc i) nil)
                           :sequence (reduce (fn [state rule]
                                               (if state (do-exec rule state)
                                                   nil))
                                             i
                                             (rest rule))
                           :or (some (fn [r] (do-exec r i))
                                     (rest rule))
                           (throw (IllegalStateException. (str "what?" rule))))))]

                 (when (and *debug* (string? rule) (< i input-length))
                   (printf "%s%s rule %s %s%n"
                           (.substring "                                 " 0 i)
                           (.substring input i (if res res input-length))
                           rule
                           (if res "match" "FAIL"))
                   (flush))

                 res))]
       (comment if *debug* 
                (do-exec startrule 0)
                
                (if-let [result (do-exec startrule 0)]
                  (= result (.length input))
                  false))
       
       (loop [i (do-exec "42" 0)]
         (if (nil? i)
           i
           (let [j (do-exec "11" i)]
             (if (or (nil? j) (not= j input-length))
               (recur (do-exec "42" i))
               i))))
       ))))

(defn construct [rules rule]
  (if (string? rule)
    (construct rules (get rules rule))
    (case (first rule)
      :terminal (second rule)
      :sequence (str/join (map #(construct rules %) (rest rule)))
      :or (construct rules (second rule)))))

(comment
(let [data (parse (slurp "day19.txt"))
      re (compile-to-re (:rules data))]
  (->> (:data data)
       (filter #(re-matches re %))
       count))

(let [data (parse (slurp "day19.txt"))]
  (->> (:data data)
       (filter #(exec (:rules data) %))
       count
       ;(map println)
       ))




(let [data (parse "0: 8 31
1: 11 42 | 5
2: 42 3
3: 42 4
4: 42 5 | 5
5: 8 6
6: 31 7
7: 31 42
42: \"a\"
31: \"b\"

aaaaaaaab")]
  (->> (:data data)
       first
       (exec (:rules data))))

(binding [*debug* false]
  (exec (:rules (parse testdata2)) "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa"))

(binding [*debug* true]
  (exec (:rules (parse testdata2)) "aaaaabbaabaaaaa" [:sequence "8"]))

(construct (:rules (parse testdata2)) "42")
;; => "babbbbabbbbbaba"

)