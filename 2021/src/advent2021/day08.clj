(ns advent2021.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-element-bitmap [element]
  (let [bits (->> element
                  (map #(- (int %) (int \a)))
                  (reduce bit-set 0))]
    [bits (count element)]))

(defn parse-line [line]
  (let [collect (fn [[in out mode] element]
                  (if (= element "|")
                    [in out :out]
                    (let [e (parse-element-bitmap element)]
                      (case mode
                        :in [(conj in e) out mode]
                        :out [in (conj out e) mode]))))
        result (reduce collect
                       [[] [] :in]
                       (re-seq #"[a-g]+|[|]" line))]
    {:in (first result)
     :out (second result)}))

(def testdata1 (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))

(def testdata2 (map parse-line (string/split-lines "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")))

(def data (map parse-line (line-seq (io/reader "resources/day08.txt"))))

(def segment-count {0 6
                    1 2
                    2 5
                    3 5
                    4 4
                    5 5
                    6 6
                    7 3
                    8 7
                    9 6})

(defn decode [data]
  (let [input (:in data)
        digits-by-cnt (group-by second input)
        obvious-digit (fn [i] (first (first (digits-by-cnt (segment-count i)))))
        one (obvious-digit 1)
        seven (obvious-digit 7)
        four (obvious-digit 4)
        eight (obvious-digit 8)

        [two x y] (let [[[x _] [y _] [z _]] (get digits-by-cnt 5)]
                    (cond (= (bit-or four x) eight) [x y z]
                          (= (bit-or four y) eight) [y x z]
                          (= (bit-or four z) eight) [z x y]
                          :else (throw (RuntimeException. (format "4=%x, 8=%x, two/three/five=(%x %x %x)" four eight x y z)))))

        nine (bit-or x y)

        [three five] (if (= (bit-or x one) x)
                       [x y]
                       [y x])

        six (let [[[x _] [y _] [z _]] (get digits-by-cnt 6)]
              (cond (= (bit-or seven x) eight) x
                    (= (bit-or seven y) eight) y
                    :else z))

        zero (let [[[x _] [y _] [z _]] (get digits-by-cnt 6)]
               (- (+ x y z) (+ six nine)))

        m {zero  0
           one   1
           two   2
           three 3
           four  4
           five  5
           six   6
           seven 7
           eight 8
           nine  9}]
    (reduce (fn [t digit]
              (+ (* 10 t) (get m (first digit))))
            0
            (:out data))))

(->> data
     (map decode)
     (apply +))
