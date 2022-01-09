(ns advent2016.day08
  (:require [advent2016.core :as core]
            [clojure.string :as string]))

(def data (core/get-data 2016 8))

(def testdata "rect 3x2
rotate column x=1 by 1
rotate row y=0 by 4
rotate column x=1 by 1")

(defn parse-line [line]
  (let [tokens (or (re-matches #"(rect) ([0-9]+)x([0-9]+)" line)
                   (re-matches #"rotate (row) y=([0-9]+) by ([0-9]+)" line)
                   (re-matches #"rotate (column) x=([0-9]+) by ([0-9]+)" line))]
    (map core/safe-parse-number (rest tokens))))

(defn parse [input]
  (map parse-line (string/split-lines input)))

(defn new-display [cols rows]
  (repeat rows (repeat cols \.)))

(defn print-display [display]
  (doseq [row display]
    (doseq [col row]
      (print col))
    (newline))
  display)

(defn count-display [display]
  (reduce (fn [sr row]
            (reduce (fn [sc col]
                      (if (= col \#) (inc sc) sc))
                    sr row))
          0 display))

(defn rect [display a b]
  (map-indexed (fn [row-no row]
                 (map-indexed (fn [col-no col]
                                (if (and (< row-no b) (< col-no a))
                                  \#
                                  col))
                              row))
               display))

(defn row [display a b]
  (let [b (- (count (first display)) b)]
    (map-indexed (fn [row-no row]
                   (if (= row-no a)
                     (concat (drop b row)
                             (take b row))
                     row))
                 display)))

(defn transpose [display]
  (apply mapv vector display))

(defn col [display a b]
  (-> display
      transpose
      (row a b)
      transpose))

(defn interpret [display data]
  (reduce (fn [display [cmd a b]]
            (case cmd
              "rect" (rect display a b)
              "row" (row display a b)
              "column" (col display a b)))
          display data))

(defn task-1 [data]
  (count-display (interpret (new-display 50 6) (parse data))))

(defn task-2 [data]
  (print-display (interpret (new-display 50 6) (parse data))))

(and (task-2 data) nil)


(comment
  (parse testdata)

  (-> (new-display 7 3)
      (rect 3 2)
      (col 1 1)
      (row 0 4)
      (col 1 1)
      (print-display)
      (count-display)
      )

  (print-display (transpose (new-display 7 3)))

  )