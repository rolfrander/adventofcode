(ns advent2020.day18
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def testdata [["2 * 3 + (4 * 5)"  26]
["5 + (8 * 3 + 9 + 3 * 4 * 3)" 437]
["5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"  12240]
["((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 13632]])

;;; oper = * | +
;;; num  = [0-9]+
;;; expr = ( expr ) |
;;;        oper num
;;;        oper expr
;;;        num
;;; start = expr

(defn eval-math [^String line]
  (let [token-matcher (re-matcher #"[0-9]+|[^ ]" line)
        tokens (map (fn [t]
                      (case t
                        "*" [:oper '*]
                        "+" [:oper '+]
                        "(" :lpar
                        ")" :rpar
                        [:num (Integer/parseInt t)]))
                    (take-while string? (repeatedly #(re-find token-matcher))))
        match (fn [expect [token-type token-value]]
                (if (= expect token-type)
                  token-value
                  nil))]
    (letfn
     [(expr [ts]
        (loop [token-tree nil
               oper nil
               [next-tok & ts] ts]
          (if next-tok
            (cond
              (and (= :lpar next-tok) (not (nil? oper)))
              (let [[sub-expr ts] (expr ts)]
                (recur (list oper token-tree sub-expr) nil ts))

              (and (= :lpar next-tok) (nil? token-tree))
              (let [[sub-expr ts] (expr ts)]
                (recur sub-expr nil ts))
              
              (= :rpar next-tok)
              [token-tree ts]

              :else
              (if-let [oper (match :oper next-tok)]
                (recur token-tree oper ts)
                (if-let [num (match :num next-tok)]
                  (if (nil? oper)
                    (recur num nil ts)
                    (recur (list oper token-tree num) nil ts))
                  (throw (IllegalStateException. (str/join "did not expect" next-tok))))))
            token-tree)))]
      (eval 
       (expr tokens)))))

;;; oper = * | +
;;; 
;;; num  = [0-9]+
;;; 
;;; par-expr = (mult-expr)
;;;          | num
;;;          
;;; plus-expr = par-expr
;;;           | par-expr + plus-expr
;;;           
;;; mult-expr = plus-expr
;;;           | plus-expr * mult-expr
;;;           
;;; start = mult-expr

(defn do-match [expect ts]
  (let [token (first ts)]
    (if (= expect (first token))
      (do ;(println "match" expect)
          [(second token) (rest ts)])
      [nil ts])))



(defn eval-math-2 [^String line]
  (let [token-matcher (re-matcher #"[0-9]+|[^ ]" line)
        tokens (map (fn [t]
                      (case t
                        "*" [:mult '*]
                        "+" [:plus '+]
                        "(" [:lpar true]
                        ")" [:rpar true]
                        [:num (Integer/parseInt t)]))
                    (take-while string? (repeatedly #(re-find token-matcher))))
        ]
    (letfn
     [(par-expr [ts]
                ;(println "> par-expr")
                (let [ret
                      (let [[token ts] (do-match :lpar ts)]
                        (if token
                          (let [[expr-tree ts] (mult-expr ts)
                                [_token ts] (do-match :rpar ts)]
                            [expr-tree ts])
                          (do-match :num ts)))]
                  ;(println "< par-expr")
                  ret))
      
      (plus-expr [ts]
                 ;(println "> plus-expr")
                 (let [ret
                       (let [[lhs ts] (par-expr ts)]
                         (if lhs
                           (let [[plus ts] (do-match :plus ts)]
                             (if plus
                               (let [[rhs ts] (plus-expr ts)]
                                 [(list plus lhs rhs) ts])
                               [lhs ts]))
                           (throw (IllegalStateException. "expected lpar or num, got" (first (first ts))))))]
                   ;(println "< plus-expr")
                   ret))
      
      (mult-expr [ts]
                 ;(println "> mult-expr")
                 (let [ret
                       (let [[lhs ts] (plus-expr ts)]
                         (if lhs
                           (let [[mult ts] (do-match :mult ts)]
                             (if mult
                               (let [[rhs ts] (mult-expr ts)]
                                 [(list mult lhs rhs) ts])
                               [lhs ts]))
                           (throw (IllegalStateException. "expected lpar or num, got" (first (first ts))))))]
                   ;(println "< mult-expr")
                   ret))
      
      ]
      (first (eval
              (mult-expr tokens))))))

(eval-math-2 (first (nth testdata 3)))

(defn test-eval [line expected]
  (let [res (eval-math line)]
    [(= expected res) expected res line]))

(map (partial apply test-eval) testdata)

(->> testdata
     (map first)
     (map eval-math)
     (reduce + 0))

(with-open [in (clojure.java.io/reader "day18.txt")]
  (->> in
       line-seq
       (map eval-math-2)
       (reduce + 0)
       ))
;; => part 2: 122438593522757


;; => part 1: 21993583522852
