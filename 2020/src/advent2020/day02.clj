(ns day02.core
  (:gen-class))

(re-find #"b{6,16}" "bbbbbbbbbbbbbbbpb")

(re-matches #"(\d+)-(\d+) (.): (.*)" "2-4 p: vpkpp")

(defn match-password [line]
  (let [[all from_ to_ letter_ password] (re-matches #"(\d+)-(\d+) (.): (.*)" line)
        from (Integer/parseInt from_)
        to (Integer/parseInt to_)
        letter (first letter_)
        count (reduce + (map #(if (= letter %) 1 0) password))
        ]
    (and (>= count from)
         (<= count to))
    ))

(defn match-password-2 [line]
  (let [[all from_ to_ letter_ password] (re-matches #"(\d+)-(\d+) (.): (.*)" line)
        from (Integer/parseInt from_)
        to (Integer/parseInt to_)
        letter (first letter_)
        l1 (nth password (dec from))
        l2 (nth password (dec to))]
    (and (or (= l1 letter)
             (= l2 letter))
         (not= l1 l2))
    ))


(with-open [input (clojure.java.io/reader "input.txt")]
  (->> input
       line-seq
       ;(take 10)
       (filter match-password-2)
       ;(group-by #(if (match-password %) "ja" "nei"))
       count
       ))
  
(match-password "6-7 z: zzfzzdzzzzz")

(let [x (re-find #"z{6,7}(z?)" "azzzzzzzzzz")]
     (cond
       (nil? x) "missing"
       (= "z" (second x)) "to long"
       :else "ok"))


{"ja" ["2-4 p: vpkpp" "6-16 b: bbbbbbbbbbbbbbbpb" "5-14 t: ttttnttttttdttttttt" "2-10 b: bfbbbbcbnpbbbbt"]
 "nei" ["6-7 z: zzfzzdz" "4-6 q: tfzqvqcpcmqqjqzd" "7-8 k: rkkkknkw" "3-4 h: hrht" "2-6 c: ccccccc" "5-7 g: pmtgqgg"]}

(match-password "2-6 c: ccccccc")
(first "a")
(reduce + (map #(if (= \z %) 1 0) "zsnsdfjgzdrgjenz"))