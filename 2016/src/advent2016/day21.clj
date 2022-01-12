(ns advent2016.day21
  (:require [rolfrander.puzzle-lib :refer [interpreter get-data safe-parse-number]]
            [clojure.string :as str]
            [clojure.test :as test]))

(def testdata "swap position 4 with position 0
swap letter d with letter b
reverse positions 0 through 4
rotate left 1 step
move position 1 to position 4
move position 3 to position 0
rotate based on position of letter b
rotate based on position of letter d")


(defn parse-line [line]
  (let [->num (fn [m] (map #(cond (= % "left") -1
                                  (= % "right") 1
                                  (re-matches #"[0-9]+" %) (Long/parseLong %)
                                  :else (first %)) (rest m)))]
    (condp (comp seq re-matches) line
      #"swap position ([0-9]+) with position ([0-9]+)" :>> #(cons :swap-pos (->num %))
      #"swap letter ([a-z]) with letter ([a-z])"       :>> #(cons :swap-let (->num %))
      #"rotate (left|right) ([0-9]+) steps?"           :>> #(list :rot-step (apply * (->num %)))
      #"rotate based on position of letter ([a-z])"    :>> #(cons :rot-lett (->num %))
      #"reverse positions ([0-9]+) through ([0-9]+)"   :>> #(cons :rev      (->num %))
      #"move position ([0-9]+) to position ([0-9]+)"   :>> #(cons :move     (->num %)))))

(defn parse [input]
  (map parse-line (str/split-lines input)))


(test/deftest parse-test
  (test/is (= '(:rot-step -2) (parse-line "rotate left 2 step")))
  (test/is (= '(:rot-step 2) (parse-line "rotate right 2 step")))
  (test/is (= '((:swap-pos 4 0)
                (:swap-let \d \b)
                (:rev 0 4)
                (:rot-step -1)
                (:move 1 4)
                (:move 3 0)
                (:rot-lett \b)
                (:rot-lett \d))
              (parse testdata))))

(def rev-r-l-offsets
  "only works for length 8"
  (into {}
        (let [l 8]
          (map
           #(let [rot-cnt (inc %)
                  rot-cnt (if (> rot-cnt 4) (inc rot-cnt) rot-cnt)
                  new-pos (mod (+ rot-cnt %) l)]
              [new-pos (- % new-pos)])
           (range 0 l)))))

(defn cpu [scramble instr [x y]]
  (let [l (count scramble)
        indexof (fn [letter] (loop [i 0] (cond (>= i l) 0
                                               (= (scramble i) letter) i
                                               :else (recur (inc i)))))
        rot-right (fn [steps] (vec (take l (drop (mod (- l steps) l)
                                                 (cycle scramble)))))
        shift-dir (fn [x y] (if (< y x) -1 1))]
    (case instr
      :swap-pos (let [[a b] (map scramble [x y])]
                  (assoc scramble x b y a))
      :swap-let (mapv #(cond (= % x) y
                             (= % y) x
                             :else %) scramble)
      :rot-step (rot-right x)
      :rot-lett (let [rot-cnt (inc (indexof x))
                      rot-cnt (if (> rot-cnt 4) (inc rot-cnt) rot-cnt)]
                  (rot-right rot-cnt))
      :rev-r-l (rot-right (rev-r-l-offsets (indexof x)))
      :rev      (mapv #(if (<= x % y)
                         (scramble (+ y x (- %)))
                         (scramble %))
                      (range 0 l))
      :move     (let [a (scramble x)]
                  (mapv #(cond (< % (min x y)) (scramble %)
                               (> % (max x y)) (scramble %)
                               (= % y) a
                               :else (scramble (+ % (shift-dir x y))))
                       (range 0 l)))
      :index (indexof x))))

(defn reverse-program [program]
  (letfn [(reverse-stmt [stmt]
            (let [[instr x y] stmt]
              (case instr
                :rot-step (list instr (- x) y)
                :rot-lett (list :rev-r-l x y)
                :move (list :move y x)
                stmt)))]
    (->> (map reverse-stmt program)
         (reverse))))

(test/deftest cpu-test
  (test/are [instr x y result] (= (vec result) (cpu (vec "abcdefg") instr [x y]))
    :swap-pos 2 3 "abdcefg"
    :swap-let \b \f "afcdebg"
    :swap-let \a \g "gbcdefa"
    :rot-step -2 0 "cdefgab"
    :rot-step  2 0 "fgabcde"
    :rot-step  0 0 "abcdefg"
    :rot-lett \a 0 "gabcdef"
    :rot-lett \b 0 "fgabcde"
    :rot-lett \d 0 "defgabc"
    :rot-lett \e 0 "bcdefga"
    :rev 0 2 "cbadefg"
    :rev 4 6 "abcdgfe"
    :rev 1 5 "afedcbg"
    :move 1 5 "acdefbg"
    :move 0 2 "bcadefg"
    :move 4 2 "abecdfg"))

(defn task-1 [initvec program]
  (->> (interpreter program cpu (vec initvec))
       (:state)
       (str/join)))

(test/deftest task-1-test
  (test/is (= "decab" (task-1 "abcde" testprogram))))

(def testprogram (parse testdata))

(test/deftest task-2-test
  (test/is (= "abcdefgh" (task-1 (task-1 "abcdefgh" testprogram) (reverse-program testprogram)))))

;(binding [rolfrander.puzzle-lib/*debug* true]
;  (task-1 "abcde" testdata))

(def program (parse (get-data 2016 21)))

(task-1 "abcdefgh" program)
(task-1 "fbgdceah" (reverse-program program))

(test/run-all-tests #"advent2016.day21")