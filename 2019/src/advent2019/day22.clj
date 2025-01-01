(ns advent2019.day22
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]))

(def data (puzzle/get-data 2019 22))

(def testdata "deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1")

;cut 6
;deal with increment 7
;deal into new stack

(defn parse-line [l]
  (if (= l "deal into new stack")
    [:new-stack]
    (if-let [[_ num] (re-matches #"deal with increment (\d+)" l)]
      [:increment (puzzle/str->long num)]
      (if-let [[_ num] (re-matches #"cut (-?\d+)" l)]
        [:cut (puzzle/str->long num)]))))

(defn parse [in]
  (map parse-line (str/split-lines in)))

(defn mod-inverse [a n]
  (loop [t 0
         new-t 1
         r n
         new-r (mod a n)]
    (if (zero? new-r)
      (if (> r 1)
        nil
        (if (< t 0) (+' t n) t))
      (let [q (quot r new-r)]
        (recur new-t (mod (- t (*' q new-t)) n)
               new-r (mod (- r (*' q new-r)) n))))))

(defn mod-pow [p e n]
  (loop [res 1
         p (mod p n)
         e e]
    (if (zero? e)
      res
      (recur (if (= 1 (mod e 2))
               (mod (*' res p) n)
               res)
             (mod (*' p p) n)
             (bit-shift-right e 1)))))

(defn do-shuffle [stack shuf]
  (let [cnt (count stack)]
    
    (case (first shuf)
      :new-stack (vec (rseq stack))
    
      :increment (let [jump (second shuf)]
                   (loop [newstack (vec (repeat cnt nil))
                          from 0
                          to 0]
                     (if (= from cnt)
                       newstack
                       (recur (assoc newstack to (stack from))
                              (inc from)
                              (mod (+ to jump) cnt)))))
    
      :cut (let [cut (second shuf)
                 cut (if (neg? cut) (+ cnt cut) cut)
                 [stack-1 stack-2] (split-at cut stack)]
             (vec (concat stack-2 stack-1)))
      )))

(defn new-stack [cnt]
  (vec (range cnt)))

(defn new-mod-stack [cnt]
  {:offset 0 :increment 1 :cnt cnt})

(defn find-card-at-pos-mod [{:keys [offset increment cnt]} pos]
  (mod (+ offset (*' increment pos)) cnt))

(defn do-mod-shuffle [{:keys [offset increment cnt] :as stack} shuf]
  (let [mod*    (fn [a b] (mod (*' a b) cnt))
        mod-    (fn [a b] (mod (-' a b) cnt))
        mod+    (fn [a b] (mod (+' a b) cnt))
        mod-inv (fn [a] (mod-inverse a cnt))]
    (case (first shuf)
    ; new-stack ganger inkrement med -1 og øker offset med ny inkrement
      :new-stack (let [new-increment (mod- 0 increment)]
                   (assoc stack
                          :offset (mod+ offset new-increment)
                          :increment new-increment))
    ; increment ganger inkrement med inv(shuffle-parameter)
      :increment (let [i (second shuf)]
                   (assoc stack
                          :offset offset
                        ;  inv(i) er gjeldende posisjon for kortet som kommer til å havne i pos=1
                        ;  increment * inv(i) + offset er gjeldende verdi for dette kortet
                        ;  differansen fra offset (ny increment) er increment * inv(i)
                          :increment (mod* increment (mod-inv i))
                        ; (- (find-card-at-pos-mod stack (mod-inverse (second shuf) cnt)) offset)
                          ))
    ; cut øker offset med increment * cut-pos
      :cut (assoc stack :offset (find-card-at-pos-mod stack (second shuf))))))

(defn solve-1 [in find-card stack-size]
  (let [shuffle-spec (parse in)
        stack (new-stack stack-size)
        stack (reduce do-shuffle stack shuffle-spec)]
    (reduce #(when (= (stack %2) find-card)
               (reduced %2))
            -1 (range (count stack)))
    ))

(defn solve-1-mod [in find-card stack-size]
  (let [shuffle-spec (parse in)
        stack (new-mod-stack stack-size)
        stack (reduce do-mod-shuffle stack shuffle-spec)]
    ;(println stack)
    (loop [v (:offset stack)
           i 0]
      (if (= v find-card)
        i
        (recur (mod (+ v (:increment stack)) (:cnt stack))
               (inc i))))))

; https://www.reddit.com/r/adventofcode/comments/ee0rqi/2019_day_22_solutions/
(defn solve-2 [in card-at stack-size iter]
  (let [shuffle-spec (parse in)
        stack-0 (new-mod-stack stack-size)
        stack-1 (reduce do-mod-shuffle stack-0 shuffle-spec)

        mod* (fn [a b] (mod (*' a b) stack-size))
        mod- (fn [a b] (mod (-' a b) stack-size))
        mod-inv (fn [a] (mod-inverse a stack-size))
        mod-pow (fn [a b] (mod-pow a b stack-size))

        offset-diff (mod- (:offset stack-1) (:offset stack-0))
        increm-mult (mod* (mod-inv (:increment stack-0)) (:increment stack-1))

        stack-after-iter {:offset    (mod* offset-diff
                                           (mod* (mod- 1 (mod-pow increm-mult iter))
                                                 (mod-inv (mod- 1 increm-mult))))
                          :increment (mod-pow increm-mult iter)
                          :cnt       stack-size}]
    ;(println stack-after-iter)
    (find-card-at-pos-mod stack-after-iter card-at)
    )
  )

(solve-1 testdata 4 10)
(solve-1-mod testdata 4 10)
;;=> 5
(solve-1 data 2019 10007)
;;=> 4649

(let [stack-size 10007
      iterations 1
      look-for 2019
      position (solve-1-mod data look-for stack-size)]
  (solve-2 data position stack-size iterations))


(let [stack-size 119315717514047
      iterations 101741582076661
      position 2020]
  (solve-2 data position stack-size iterations))
;;=> 68849657493596N
