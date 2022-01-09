(ns advent2021.day10
  (:require [clj-http.client :as http]
            [clojure.string :as string]))

(def testdata (string/split-lines "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"))

(def data (string/split-lines ))

(def data
  (->> (http/get "https://adventofcode.com/2021/day/10/input"
                 {:cookies {"session" {:value session}}})
       :body
       string/split-lines))

(def brackets {\[ \] \{ \} \( \) \< \> })
(def points {\) 3 \] 57 \} 1197 \> 25137})
(def points2 {\) 1 \] 2 \} 3 \> 4})

(defn parse-line-1 [line]
  (loop [[c & line] line
         q []]
    (if (nil? c)
      false
      (cond
        (brackets c) (recur line (conj q c))
        (= (brackets (peek q)) c) (recur line (pop q))
        :else [(brackets (peek q)) c]))))

(defn parse-line-2 [line]
  (loop [[c & line] line
         q []]
    (if (nil? c)
      (map brackets (reverse q))
      (cond
        (brackets c) (recur line (conj q c))
        (= (brackets (peek q)) c) (recur line (pop q))
        :else false))))

(parse-line-2 (get testdata 0))

(defn middle [elements]
  (let [middle (/ (dec (count elements)) 2)] ;; assume odd...
    (nth elements middle)))

; task-2
(->> (map parse-line-2 data)
     (filter identity)
     (map #(reduce (fn [sum next] (+ (* sum 5) (points2 next)))
                   0
                   %))
     sort
     middle)

