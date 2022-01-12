
(ns advent2016.day17
  (:require [rolfrander.puzzle-lib :as puzzle]
            [clojure.test :as test]
            [clojure.string :as str])
  (:import (java.security MessageDigest)))

(let [md (MessageDigest/getInstance "MD5")]
  (defn md5 [input]
    (.digest md (.getBytes input))))

(def direction-coords
  "result of the md5 controls the doors in this direction: up down left right. The coords in
   this array must be in that order"
  {"U" [0 -1]
   "D" [0 1]
   "L" [-1 0]
   "R" [1 0]})

(def directions ["U" "D" "L" "R"])

(defn get-open-doors [input]
  (let [split-byte (fn [byte] [(bit-and (bit-shift-right byte 4) 0xf)
                               (bit-and byte 0xf)])
        hash (subvec (vec (md5 input)) 0 2)]
    (->> hash
         (mapcat split-byte)
         (map-indexed #(when (>= %2 0xb) (directions %1)))
         (remove nil?))))

(test/deftest open-doors-test
  (test/is (= ["U" "D" "L"] (get-open-doors "hijkl")))) ; U D L

(def start-state {:pos [0 0] :path "" :passcode "hijkl"})

(defn neighbours [state]
  (let [{:keys [pos path passcode]} state
        next-state (fn [door] (assoc state
                                     :pos (map + (direction-coords door) pos)
                                     :path (str path door)))
        inside-board (fn [state]
                       (let [[x y] (:pos state)]
                         (and (<= 0 x 3)
                              (<= 0 y 3))))
        doors (get-open-doors (str passcode path))]
    (->> (map next-state doors)
         (filter inside-board))))

(test/deftest neighbours-test
  (test/is (let [n (neighbours start-state)]
             (and (= (count n) 1)
                  (= (:pos (first n)) [0 1]))))
  (test/is (let [n (neighbours (assoc start-state :path "DR"))]
             (= (count n) 0))))

(defn task-1 [passcode]
  (let [start (assoc start-state :passcode passcode)
        goal? (fn [state] (= (:pos state) [3 3]))]
    (:path (puzzle/a-star start goal? (constantly 1) neighbours (constantly 1) :last))))

(test/deftest task-1-test
  (test/are [pass path] (= path (task-1 pass))
            "ihgpwlah" "DDRRRD"
            "kglvqrro" "DDUDRLRRUDRD"
            "ulqzkmiv" "DRURDRUDDLLDLUURRDULRLDUUDDDRR"))

(def data (str/trim-newline (puzzle/get-data 2016 17)))
(task-1 data)
;; => "DUDRLRRDDR"

(defn max-depth [start goal? neighbours-fn]
  (letfn [(depth-first-search [start]
                              (if (goal? start)
                                0
                                (let [possible-paths (->> (neighbours-fn start)
                                                          (map depth-first-search)
                                                          (remove #(= -1 %)))]
                                  (if (= 0 (count possible-paths))
                                    -1
                                    (inc (apply max possible-paths))))))]
    (depth-first-search start)))

(defn task-2 [passcode]
  (let [start (assoc start-state :passcode passcode)
        goal? (fn [state] (= (:pos state) [3 3]))]
    (max-depth start goal? neighbours)))

(test/deftest max-depth-test
  (test/are [pass len] (= len (task-2 pass))
    "ihgpwlah" 370
    "kglvqrro" 492
    "ulqzkmiv" 830))

(task-2 data)
;; => 788


(test/run-all-tests #"advent2016.day17")