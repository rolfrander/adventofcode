(ns advent2024.day22
  (:require
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]))


(defn secret [num]
  (let [step (fn [n shift-left]
               (-> (if (neg? shift-left)
                     (bit-shift-right n (abs shift-left))
                     (bit-shift-left n shift-left))
                   (bit-xor n)
                   (bit-and 0xffffff)))]
    (-> num
        (step 6)
        (step -5)
        (step 11))))

(def testdata [1
               10
               100
               2024])

(def data (mapv puzzle/str->long (str/split-lines (puzzle/get-data 2024 22))))

(defn list->long [l]
  (reduce #(bit-or (bit-shift-left %1 5) (bit-and (+ 10 %2) 0x1f))
          0 l))

(defn long->list [l]
  (->> (take 4 (iterate #(bit-shift-right % 5) l))
       (map #(bit-and % 0x1f))
       (map #(- % 10))))


(defn solve-1 [in]
  (->> (map #(nth (iterate secret %) 2000) in)
       (apply +)))

(Math/pow 19 4)

(defn negotiation-input [start]
  (let [secret-seq (->> (iterate secret start)
                        (take 2001)
                        (map #(mod % 10)))]
    (->> secret-seq
         (partition 2 1)
         (map (partial apply -))
         (partition 4 1)
         (map list->long)
         (map #(vector %2 %1) (drop 4 secret-seq))
         (reduce (fn [r [s b]]
                   (if (contains? r s)
                     r
                     (assoc r s b)))
                 {}))))

(defn solve-2 [d]
  (->> (map negotiation-input d)
       (apply merge-with +)
       seq
       (apply max-key second)
       second
       ))


(solve-1 testdata)
;;=> 37327623
(solve-1 data)
;;=> 20401393616

(solve-2 [1 2 3 2024])
;;=> 23
(solve-2 data)
;;=> 2272

