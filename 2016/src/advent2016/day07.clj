(ns advent2016.day07 
  (:require [clojure.string :as string]
            [advent2016.core :as core]))

(def testdata "abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
ioxxoj[asdfgh]zxcvbn")

(def testdata-expect [true false false true])

(def testdata-2 "aba[bab]xyz
xyx[xyx]xyx
aaa[kek]eke
zazbz[bzb]cdb")

(def testdata-2-expect [true false true true])

(def data (core/get-data 2016 07))

(defn has-abba [str]
  (if-let [[_m a b] (re-find #"([a-z])([a-z])\2\1" str)]
    (not= a b)
    false))

(defn find-aba [s]
  (->> (map #(let [a (.charAt s %)
                   b (.charAt s (+ 1 %))
                   c (.charAt s (+ 2 %))]
               (if (and (= a c) (not= a b)) [a b] nil)) (range (- (.length s) 2)))
       (remove nil?)))

(defn parse-line [line]
  (->> (reduce (fn [[outside inside] [match-all match-ip]]
                 (if (string/starts-with? match-all "[")
                   [outside (conj inside match-ip)]
                   [(conj outside match-ip) inside]))
               [[] []] (re-seq #"\[?([a-z]+)\]?" line))
       (interleave [:outside :inside])
       (apply assoc {})))

(defn parse [input]
  (->> (string/split-lines input)
       (map parse-line)))

(defn supports-tls [ip]
  (or (and (some has-abba (:outside ip))
           (not (some has-abba (:inside ip))))
      false))

(defn supports-ssl [ip]
  (let [aba (mapcat find-aba (:outside ip))
        bab (mapcat find-aba (:inside ip))]
    (first (for [[a1 b1] aba
                 [b2 a2] bab
                 :when (and (= a1 a2)
                            (= b1 b2))]
             true))))

(defn task-1 [input]
  (->> (parse input)
       (filter supports-tls)
       count))

(defn task-2 [input]
  (->> (parse input)
       (filter supports-ssl)
       count))

(task-1 data) ; 115
(task-2 data) ; 231


(comment
  
  (parse-line (nth (string/split-lines data) 3))

  (= (map supports-tls (parse testdata) testdata-expect))

  (mapcat find-aba (:outside (parse-line "aba[bab]xyzyz")))

  (map supports-ssl (parse testdata-2))

  (->> (string/split-lines data)
       (map (partial re-find #"([a-z])([a-z])\1\2"))
       (remove nil?)
       (take 10))

  (let [^String s "abcdcdedf"]
    (->> (map #(let [a (.charAt s %)
                     b (.charAt s (+ 1 %))
                     c (.charAt s (+ 2 %))]
                 (if (and (= a c) (not= a b)) [a b] nil)) (range (- (.length s) 2)))
         (remove nil?))
    )

  (re-find #"(?<a>[a-z])(?<b>[a-z])\1" "aba")

  (->> (parse testdata)
       flatten
       (map (juxt identity has-abba)))
  )