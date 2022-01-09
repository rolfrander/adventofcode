(ns advent2015.day25)

(defn pos-to-cnt [row col]
  (let [r1 (dec (+ row col))
        prev-entries (/ (* r1 (dec r1)) 2)]
    (+ prev-entries col)))

(comment
(pos-to-cnt 2 3) ; 9
(pos-to-cnt 3 4) ; 19
(pos-to-cnt 6 1) ; 16
)

(defn mod-pow [base pow modulus]
  (loop [cur base
         ret 1
         cnt pow]
    (if (= 0 cnt)
      ret
      (let [ret (if (= 1 (bit-and cnt 1))
                  (mod (* cur ret) modulus)
                  ret)]
        (recur (mod (* cur cur) modulus) ret (bit-shift-right cnt 1))))))

;(mod-pow 5 4 100) ; 25

(defn calc
  "calculate init * base^pos (mod modulus), where pos is the number of cells between 1,1 and row,col (cantebury diagonal couting)"
  [init base modulus row col]
  (mod (* init (mod-pow base (dec (pos-to-cnt row col)) modulus)) modulus))

(defn task-1 [row col]
  (calc 20151125 252533 33554393 row col))

(task-1 2981 3075)