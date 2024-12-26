(ns advent2024.day21
  (:require
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]))

(def testdata "029A
980A
179A
456A
379A")

(def data (puzzle/get-data 2024 21))

;; +---+---+---+
;; | 7 | 8 | 9 |
;; +---+---+---+
;; | 4 | 5 | 6 |
;; +---+---+---+
;; | 1 | 2 | 3 |
;; +---+---+---+
;;     | 0 | A |
;;     +---+---+

(def numpad {\A [2 0]
             \0 [1 0]
             \1 [0 1]
             \2 [1 1]
             \3 [2 1]
             \4 [0 2]
             \5 [1 2]
             \6 [2 2]
             \7 [0 3]
             \8 [1 3]
             \9 [2 3]
             })

;;     +---+---+
;;     | ^ | A |
;; +---+---+---+
;; | < | v | > |
;; +---+---+---+
(def arrowpad {\^ [1 1]
               \A [2 1]
               \< [0 0]
               \v [1 0]
               \> [2 0]
               })

(def inv-arrowpad {[1 1] \^
                   [2 1] \A
                   [0 0] \<
                   [1 0] \v
                   [2 0] \>})

(defn move-numpad [from to]
  (let [[to-x to-y] (numpad to)]
    (loop [[x y] (numpad from)
           movements []]
      (cond (< y to-y) (recur [x (inc y)] (conj movements \^))
            (< x to-x) (recur [(inc x) y] (conj movements \>))
            (> x to-x) (recur [(dec x) y] (conj movements \<))
            (> y to-y) (recur [x (dec y)] (conj movements \v))
            :else movements))))

(defn move-arrowpad [from to]
  (let [[to-x to-y] (arrowpad to)]
    (loop [[x y] (arrowpad from)
           movements []]
      (cond (> y to-y) (recur [x (dec y)] (conj movements \v))
            (< x to-x) (recur [(inc x) y] (conj movements \>))
            (< y to-y) (recur [x (inc y)] (conj movements \^))
            (> x to-x) (recur [(dec x) y] (conj movements \<))
            :else movements))))

(defn inv-move [pad start movements]
  (let [move (fn [[x y] move]
               (case move
                 \< [(dec x) y]
                 \> [(inc x) y]
                 \^ [x (inc y)]
                 \v [x (dec y)]))]
    (->> (reduce (fn [[ret pos] m]
                   (if (= m \A)
                     [(conj ret (pad pos)) pos]
                     [ret (move pos m)]))
                 [[] start] movements)
         first
         str/join)))

(defn press [pad sequence]
  (->> (partition 2 1 (cons \A (seq sequence)))
       (mapcat #(conj (apply pad %)
                      \A))
       (str/join)))

(defn calc-movement [code]
  (->> (press move-numpad code)
       (press move-arrowpad)
       (press move-arrowpad)
       ))

(defn complexity [code]
  (let [num (puzzle/str->long (re-find #"\d+" code))]
    (* num (count (calc-movement code)))))

(->> (map calc-movement (str/split-lines testdata))
     (map count))

(->> (press move-numpad "379A")
     (press move-arrowpad)
     (press move-arrowpad)
     )
;;=> "v<<A>>^AvA^Av<<A>>^AAv<A<A>>^AAvAA^<A>Av<A>^AA<A>Av<A<A>>^AAAvA^<A>A"
;;   "<v<A>>^AvA^A<v A<  AA>>^AAvA<^A>AAv A^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
;;=> "<A>A<AAv<AA>>^AvAA^Av<AAA>^A"
;;   "<A>Av<<AA>^AA>AvAA^A<vAAA>^A"
;;=> "^A^^<<A>>AvvvA"

(count (calc-movement "029A"))

(->> "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
     (inv-move inv-arrowpad (arrowpad \A))
     (inv-move inv-arrowpad (arrowpad \A))
     )