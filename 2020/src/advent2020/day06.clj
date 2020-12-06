(ns advent2020.day06)


(def testdata "abc

a
b
c

ab
ac

a
a
a
a

b")

(defn split-responses [str]
  (clojure.string/split str #"\r?\n\r?\n"))

(defn count-common-responses [resp]
  (let [cnt (count (clojure.string/split resp #"\r?\n"))
        accum (reduce (fn [a letter]
                        (if (Character/isAlphabetic (int letter))
                          (update a letter #(if % (inc %) 1))
                          a))
                      {}
                      resp)]
    (count (filter #(= cnt (second %))
                   accum))
    ;accum
    ))

(defn count-responses [resp]
  (->> resp
       seq
       (filter #(Character/isAlphabetic (int %)))
       (into #{})
       count))


(count-common-responses "abcx
abcy
abcz")

(->> (slurp "day06.txt")
     split-responses
     (map count-common-responses)
     (reduce +))

