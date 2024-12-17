(ns advent2024.day14
  (:require
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.string :as str]))

(def testdata "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(def data (puzzle/get-data 2024 14))

(defn parse-line [l]
  (let [[px py vx vy] (map puzzle/str->long (re-seq #"-?\d+" l))]
    {:px px
     :py py
     :vx vx
     :vy vy}))

(defn parse [in]
  (map parse-line (str/split-lines in)))

(defn print-robots [r w h]
  (let [robo-count (reduce #(update %1 [(:px %2) (:py %2)] puzzle/safe-inc)
                           {} r)]
    (doseq [y (range h)
            x (range w)]
      (when (and (= x 0) (> y 0))
        (println))
      (print (get robo-count [x y] \.)))
    (println)))

(defn safety-factor [r w h]
  (let [w-half (quot w 2)
        h-half (quot h 2)
        quadrant (fn [robot] (condp #(and ((first %1) (:px %2) w-half)
                                          ((second %1) (:py %2) h-half)) robot
                               [< <] 1
                               [< >] 2
                               [> <] 3
                               [> >] 4
                               0))]
    (->> (map quadrant r)
         (reduce #(update %1 %2 puzzle/safe-inc)
                 {})
         ( #(dissoc % 0))
         vals
         (reduce *)
         )))

(defn move [state w h]
  (let [move-single (fn [robot] (-> robot
                                    (assoc :px (mod (+ (:px robot) (:vx robot)) w))
                                    (assoc :py (mod (+ (:py robot) (:vy robot)) h))))]
    (map move-single state)))

(defn solve-1 [in w h iter]
  (let [robots (parse in)]
    (-> (iterate #(move % w h) robots)
        (nth iter)
        (safety-factor w h))))

(defn solve-2 [in w h iter]
  (let [robots (parse in)
        corner-size 15
        w-low corner-size
        w-high (- w corner-size)
        h-low corner-size
        h-high (- h corner-size)
        blank-corners (fn [r] (< (count (filter #(or (and (< (:px %) w-low) (< (:py %) h-low))
                                                     (and (< (:px %) w-low) (> (:py %) h-high))
                                                     (and (> (:px %) w-high) (< (:py %) h-low))
                                                     (and (> (:px %) w-high) (> (:py %) h-high)))
                                                r))
                                  10))]
    (loop [configs (iterate #(move % w h) robots)
           i 0]
      (if (> i iter)
        nil
        (do (when (blank-corners (first configs))
              (puzzle/print-png (map #(vector (:px %) (:py %)) (first configs)) w h (format "robots-%04d" i)))
            (recur (rest configs) (inc i)))))))

(defn varians [x]
  (let [sq (fn [a] (* a a))
        mean (/ (reduce + x)
                (count x))
        var (->> (map #(sq (- % mean)) x)
                 (reduce +))]
    (float var)))

(varians (map :px (parse data)))

(let [robots (parse data)
      w 101
      h 103]
  (->> (iterate #(move % w h) robots)
       (map #(map :py %))
       (map varians)
       (take 200)
       (map-indexed #(if (< %2 300000) [%1 %2] nil))
       (remove nil?)
       ))

(solve-1 testdata 11 7 100)
(solve-1 data 101 103 100)
;;=> 219512160

(solve-2 data 101 103 1000)
