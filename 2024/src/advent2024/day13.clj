(ns advent2024.day13
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]
            [clojure.core.logic :refer [run* fresh all]]
            [clojure.core.logic.fd :as fd]))

(def testdata "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(def data (puzzle/get-data 2024 13))

(defn parse [in]
  (map (fn [machine]
         (let [[ax ay bx by px py] machine]
           {:ax ax
            :ay ay
            :bx bx
            :by by
            :prize-x px
            :prize-y py}))
       (partition 6 (map puzzle/str->long (re-seq #"\d+" in)))))

(defn update-for-task-2 [m]
  (-> m
      (update :prize-x + 10000000000000)
      (update :prize-y + 10000000000000)))

(defn calc-for [m a b]
  (let [x (+ (* (:ax m) a) (* (:bx m) b))
        y (+ (* (:ay m) a) (* (:by m) b))]
    {:x x
     :y y
     :delta-x (- (:prize-x m) x)
     :delta-y (- (:prize-y m) x)}))

(defn eval [m]
  (let [max-a (long (max (/ (:prize-x m) (:ax m))
                         (/ (:prize-y m) (:ay m))))
        max-b (long (max (/ (:prize-x m) (:bx m))
                         (/ (:prize-y m) (:by m))))])
  (->> (run* [A B]
             (fresh [AX AY BX BY]
                    (fd/in A (fd/interval 0 100))
                    (fd/in B (fd/interval 0 100))
                    (fd/* A (:ax m) AX)
                    (fd/* B (:bx m) BX)
                    (fd/* A (:ay m) AY)
                    (fd/* B (:by m) BY)
                    (fd/+ AX BX (:prize-x m))
                    (fd/+ AY BY (:prize-y m))))
       (map (fn [x] (if (empty? x) 
                      x
                      (+ (* (first x) 3)
                         (second x)))))))

(defn eval-2 [m]
  (let [{:keys [ax bx ay by prize-x prize-y]} m
        a (/ (- (* prize-x by) (* prize-y bx))
             (- (* by ax) (* bx ay)))
        b (/ (- (* prize-x ay) (* prize-y ax))
             (- (* bx ay) (* by ax)))]
    (if (and (instance? Long a) (instance? Long b))
      (+ (* a 3) b)
      nil)))


(defn solve-1 [in]
  (->> (parse in)
       (map eval)
       (remove empty?)
       (map first)
       (reduce +)
       ))

(defn solve-1b [in]
  (->> (parse in)
       (map eval-2)
       (remove nil?)
       (reduce +)))

(defn solve-2 [in]
  (->> (parse in)
       (map update-for-task-2)
       (map eval-2)
       (remove nil?)
       (reduce +)))


(solve-1 testdata)
(solve-1b testdata)
(solve-1b data)
;;=> 33209

(solve-2 testdata)
(solve-2 data)
;;=> 83102355665474
