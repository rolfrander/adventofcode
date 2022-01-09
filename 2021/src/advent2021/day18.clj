(ns advent2021.day18
  (:require [clojure.string :as string]))

(def test-split "[[[[0,7],4],[15,[0,13]]],[1,1]]")
(def test-explode-1 "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")
(def test-explode-2 "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")

(def left first)
(def right second)

(defn do-test [expression expected] (= expression expected))

(defn split [num]
  (letfn [(split-num [num] [(int (/ num 2)) (+ (quot num 2) (mod num 2))])]
    (if (number? num)
      (if (>= num 10)
        [(split-num num) true]
        [num false])
      (let [[left-tree has-split] (split (left num))
            right-tree (right num)]
        (if has-split
          [[left-tree right-tree] has-split]
          (let [[right-tree has-split] (split right-tree)]
            [[left-tree right-tree] has-split]))))))

(defn add-to-leftmost [tree i]
  (if (number? tree) 
    (+ tree i)
    [(add-to-leftmost (left tree) i) (right tree)]))

(defn add-to-rightmost [tree i]
  (if (number? tree)
    (+ tree i)
    [(left tree) (add-to-rightmost (right tree) i)]))

(do-test (add-to-leftmost (read-string test-explode-1) 1)  [[[[1 7] 4] [7 [[8 4] 9]]] [1 1]])
(do-test (add-to-rightmost (read-string test-explode-1) 1) [[[[0 7] 4] [7 [[8 4] 9]]] [1 2]])

(defn explode [num depth]
  (letfn [(explode- [num depth]
            ; returns [new-num add-to-left add-to-right do-explode]
            (cond (number? num) [num 0 0 false]
                  (= depth 0)   [0 (left num) (right num) true]
                  :else (let [[left-tree right-tree] num
                              [left-exploded add-left add-right do-explode] (explode- left-tree (dec depth))]
                          (if do-explode
                            [[left-exploded (add-to-leftmost right-tree add-right)] add-left 0 do-explode]
                            (let [[right-exploded add-left add-right do-explode] (explode- right-tree (dec depth))]
                              [[(add-to-rightmost left-tree add-left) right-exploded] 0 add-right do-explode])))))]

    (let [[tree _add-left _add-right has-exploded] (explode- num depth)]
      [tree has-exploded])
    ))

(do-test (split (read-string test-split))
      [[[[[0 7] 4] [[7 8] [0 13]]] [1 1]] true])

(do-test (explode (read-string test-explode-1) 4) [[[[[0 7] 4] [15 [0 13]]] [1 1]] true])

(defn reduce-step [num]
  (let [[num has-exploded] (explode num 4)]
    (if has-exploded [num true]
        (split num))))

(defn reduce-num [num]
  (loop [[num continue] (reduce-step num)]
    (if continue
      (recur (reduce-step num))
      num)))

(do-test (reduce-num (read-string "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")) [[[[0 7] 4] [[7 8] [6 0]]] [8 1]])

(defn snailfish-add [list]
  (reduce (fn [sum next]
            (reduce-num (vector sum next)))
          list))

(do-test (snailfish-add (read-string (str "[" "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]" "]"))) [[[[5 0] [7 4]] [5 5]] [6 6]])

(defn snailfish-magnitude [num]
  (if (number? num)
    num
    (+ (* 3 (snailfish-magnitude (left num)))
       (* 2 (snailfish-magnitude (right num)))))
  )

(do-test (snailfish-magnitude (read-string "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")) 3488)

(defn parse [input-string]
  (->> (string/split-lines input-string)
       (map read-string)))

(defn task-1 [input-string]
  (->> (parse input-string)
       snailfish-add
       snailfish-magnitude))

(defn task-2 [input-string]
  (let [numbers (parse input-string)]
    (apply max (for [a numbers
                     b numbers]
                 (snailfish-magnitude (reduce-num (vector a b))))))
  )

(do-test (task-1 "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]") 4140)

(task-1 (slurp "resources/day18.txt"))
(task-2 (slurp "resources/day18.txt"))
