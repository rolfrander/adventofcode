(ns advent2024.day02
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(def data (puzzle/get-data 2024 02))

(defn parse-line [l]
  (map puzzle/str->long (re-seq #"[0-9]+" l)))

(defn parse [in]
  (->> (str/split-lines in)
       (map parse-line)))

(defn is-valid? [s]
  (let [diff (map #(apply - %) (partition 2 1 s))
        sign (map #(int (Math/signum (float %))) diff)
        diff (map abs diff)]
    (not (or (some #(or (< % 1)
                        (> % 3)) diff)
             (not (apply = sign))))))

(defn remove-idx [n s]
  (remove nil? (map-indexed #(if (= %1 n) nil %2) s)))

(defn is-valid-2? [s]
  (or (is-valid? s)
      (some #(is-valid? (remove-idx % s)) (range 0 (count s)))))

(defn solve [data]
  (count (filter is-valid? (parse data))))

(defn solve-2 [data]
  (count (filter is-valid-2? (parse data))))

(solve testdata)
;;=> 2
(solve data)
;;=> 502

(solve-2 testdata)
;;=> 4
(solve-2 data)
;;=> 544
