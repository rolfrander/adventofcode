(ns advent2015.day11
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)

(def straights
  (conj (->> (iterate #(+ % 26r111) 26r123)
             (take 23)
             (map #(Long/toString % 26)))
        "012"))

(def letters
  (->> (range 26)
       (map (fn [i]
              (vector (char (+ (int \a) i))
                      (if (< i 10)
                        (char (+ (int \0) i))
                        (char (+ (int \a) i (- 10)))))))
       (into {})))

(def base26 (into {} (map (fn [[l b]] [b l]) letters)))

(defn base26-to-letters [password]
  (str/join (map base26 password)))

(defn letters-to-base26 [password]
  (str/join (map letters password)))

(defn base26-to-int [base26]
  (Long/parseLong base26 26))

(defn int-to-base26 [i]
  (let [c (Long/toString i 26)
        prefix (- 8 (count c))]
    (str/join (concat (repeat prefix \0) c))))

(defn has-increasing-string [password]
  (boolean (some #(str/includes? password %) straights)))

(defn count-doubles [password]
  (first (reduce (fn [[count prev] cur]
                   (if (= cur prev)
                     [(inc count) nil]
                     [count cur]))
                 [0 nil]
                 password)))

(def illegal-letters (set (map letters [\i \o \l])))

(defn no-illegal-letters [password]
  (not (some illegal-letters password)))

(defn has-2-doubles [password]
  (= 2 (count-doubles password)))

(let [p (letters-to-base26 "abcdeggg")]
  (map #(% p) [has-increasing-string no-illegal-letters has-2-doubles]))

(defn valid-password [b26]
  (every? #(% b26) [has-increasing-string no-illegal-letters has-2-doubles]))

(defn find-next-password [base26]
  (->> base26
       base26-to-int
       (iterate inc)
       (map int-to-base26)
       (filter valid-password)
       first))

(defn task-1 [password]
  (->> password
       letters-to-base26
       find-next-password
       base26-to-letters
       ))

(valid-password (letters-to-base26 "abcdeggg"))

(time (task-1 "ghijklmn"))

(time (task-1 "hxbxwxba"))
;; => "hxbxxyzz"


(->> (letters-to-base26 "abcdefgh")
     base26-to-int
     (iterate inc)
     (map int-to-base26)
     first
     )