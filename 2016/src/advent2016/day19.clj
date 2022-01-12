(ns advent2016.day19)

(defn create-ring [cnt]
  (assoc (vec (range 1 (inc cnt))) (dec cnt) 0))

(defn simulate [cnt]
  (loop [ring (create-ring cnt)
         cur 0]
    (if (= cur (ring cur))
      (inc cur)
      (let [next (ring (ring cur))]
        (recur (assoc ring cur next)
               next)))))

(defn simulate-count [n]
  (loop [i 1
         winner 1]
    (cond (= n i) winner
          (= i winner) (recur (inc i) 1)
          :else (recur (inc i) (+ winner 2)))))

(defn traverse [ring cur cnt]
  (nth (iterate ring cur) cnt))

(defn simulate-2 [cnt]
  (loop [ring (create-ring cnt)
         elves-left cnt
         cur 0]
    (if (= cur (ring cur))
      (inc cur)
      (let [before-oposite (traverse ring cur (dec (quot elves-left 2)))
            after-oposite (ring (ring before-oposite))
            next (ring cur)]
        (recur (assoc ring before-oposite after-oposite)
               (dec elves-left)
               (if (= before-oposite cur) after-oposite next))))))

(defn simulate-count-2 [n]
  (loop [i 1
         winner 1]
    (cond (= n i)            winner
          (= i winner)       (recur (inc i) 1)
          (< (* 2 winner) i) (recur (inc i) (inc winner))
          :else              (recur (inc i) (+ 2 winner)))))

(def data 3012210)
(map (juxt identity simulate) (range 1 40))
(simulate-count data)

(map (juxt identity simulate-2) (range 1 40))
(map simulate-count-2 (range 17 40))
(simulate-count-2 data)