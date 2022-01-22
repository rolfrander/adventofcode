(ns advent2017.day23
  (:require [rolfrander.puzzle-lib :refer [get-data interpreter is-prime?]]))

(def start-state {:mul-cnt 0})

(defn cpu [_instruction-pointer state mnemonic [a b]]
  (let [val-a (if (number? a) a (get state a 0))
        val-b (if (number? b) b (get state b 0))]
    (case mnemonic
      "set" (assoc state a val-b)
      "add" (update state a (fnil (partial + val-b) 0))
      "sub" (update state a (fnil #(- % val-b) 0))
      "mul" (-> state
                (update a (fnil (partial * val-b) 0))
                (update :mul-cnt inc))
      "mod" (update state a #(mod % val-b))
      "jnz" (if (not= val-a 0)
              {:jmp val-b}
              state))))




;; set b 57
;; set c b
;; jnz a 2
;; jnz 1 5
;; mul b 100
;; sub b -100000
;; set c b
;; sub c -17000

;; C:
;; set f 1
;; set d 2

;; B:
;; set e 2

;; A:
;;   set g d
;;   mul g e    g=d*e
  
;;   sub g b
;;   jnz g 2
;;   set f 0    if g==b: f=0
  
;;   sub e -1   e++
;;   set g e
;;   sub g b
;;   jnz g A    if e!=b: jmp A

;;   sub d -1   d++
;;   set g d
;;   sub g b
;;   jnz g B    if d!=b: jmp B

;; jnz f 2
;; sub h -1     if f == 0: h++

;; set g b
;; sub g c
;; jnz g 2 
;; jnz 1 3
;; sub b -17  
;; jnz 1 -23    if b!=c: b=b+17, jmp C


;; // pseudokode
;; if a!=0 {
;;   b=105700
;;   c=122700
;; } else {
;;   b = 57
;;   c = 57
;; }
;; for ; b <= c ; b+=17 {
;;     isprime = true // f
;;     for d=2; d<b; d++ {
;;         for e=2; e<b; e++ {
;;             if (d*e) == b {
;;                 isprime = false // b er ikke primtall
;;             }
;;         }
;;         d++
;;     }

;;     if !isprime {
;;         non_primes++ // h
;;     }
;; }

;; koden antall tall fra b til c i skritt av 17 som ikke er primtall
;; hvis a = 1 settes b=105700 og c=122700


(let [b 105700
      c 122700
      b 57 c 57]
  (count (filter (complement is-prime?) (range b (inc c) 17))))




; (interpreter (get-data 2017 23) cpu start-state)