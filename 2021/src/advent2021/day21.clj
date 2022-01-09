(ns advent2021.day21)
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn next-pos [prev diff]
  (inc (mod (+ prev diff -1) 10)))

(defn simulate [p1 p2 winning-score dietype]
  (letfn [(die-value [dt] (let [d (mod dt dietype)]
                            (if (= d 0) 10 d)))]
    (loop [pos1 p1
           pos2 p2
           score1 0
           score2 0
           die-total 1]
      (let [pos1 (next-pos pos1 (+ 3 (* 3 (die-value die-total))))
            score1 (+ score1 pos1)
            die-total (+ 3 die-total)]
        (if (>= score1 winning-score)
          [score2 (dec die-total)]
          (let [pos2 (next-pos pos2 (+ 3 (* 3 (die-value die-total))))
                score2 (+ score2 pos2)
                die-total (+ 3 die-total)]
            (if (>= score2 winning-score)
              [score1 (dec die-total)]
              (recur pos1 pos2 score1 score2 die-total))))))))

;(apply * (simulate 2 10 1000 100))
;(apply * (simulate 4 8 1000 100))

(def dice
  (->> (for [i [1 2 3]
             j [1 2 3]
             k [1 2 3]]
         (+ i j k))
       frequencies))

(def ^:dynamic *winning-score* 21)

(def next-player {:p1 :p2
                  :p2 :p1})

(defn simulate-2
  "Simulates different ways of reacing a score of 21, starting at pos with current score. 
   Returns a mapping of { winner => number of combinations leading to this }"
  ([first-player pos] 
   (let [score (apply assoc {} (interleave (keys pos) (repeat 0)))]
     (simulate-2 first-player pos score 1)))
  ([player player-pos player-score combinations]
   
   (if-let [winner (some #(when (>= (second %) *winning-score*) (first %)) player-score)]
     {winner combinations}
     (->> (keys dice) ; for all possible die-rolls
          (map #(let [pos (next-pos (player-pos player) %)] ; % is die value
                  (simulate-2 (next-player player)
                              (assoc player-pos player pos)          ; simulate from next pos
                              (update player-score player + pos)     ; increase score
                              (* combinations (dice %))))) ; multiply by number of combinations with this die value
          (apply merge-with +) ; merge scores together
          ))))

(defn simulate-2-optimize [player ^long p1 ^long p2 ^long combinations]
  (let [p1-score (bit-and p1 0xff)
        p1-pos (bit-shift-right p1 8)
        p2-score (bit-and p2 0xff)]
    (if (>= p2-score 21)
      (if player
        [combinations 0]
        [0 combinations])
      (->> [3 4 5 6 7 8 9] ; for all possible die-rolls
           (map #(let [pos (unchecked-long (unchecked-inc (mod (unchecked-dec (unchecked-add (unchecked-long p1-pos)
                                                                                             (unchecked-long %))) 10)))] ; % is die value
                   (simulate-2-optimize (not player)
                                        p2
                                        (bit-or (bit-shift-left pos 8); simulate from next pos
                                                (+ p1-score pos))     ; increase score
                                        (* combinations (unchecked-long (dice %)))))) ; multiply by number of combinations with this die value
           (reduce (fn [[^long s1 ^long s2] [^long p1 ^long p2]]
                     [(unchecked-add s1 p1)(unchecked-add s2 p2)])) ; merge scores together
           ))))


(time (simulate-2 :p1 {:p1 2 :p2 10})) ; {:p1 49975322685009, :p2 36271342990318}, 91 sekunder
(time (simulate-2-optimize true
                           (bit-shift-left 2 8)
                           (bit-shift-left 10 8)
                           1
                           )) ; [36271342990318 49975322685009], 21 sekunder

(time (simulate-2 :p1 {:p1 4 :p2 8} {:p1 17 :p2 17} 1))
(time (simulate-2-optimize :p1
                           (bit-or (bit-shift-left 4 8) 17)
                           (bit-or (bit-shift-left 8 8) 17) 1))

Long/MAX_VALUE
;; => 9 223 372 036 854 775 807

; min beregning
;{:p1 444 356 092 776 315, 
; :p2 341 960 390 180 808}

; facit
;p1: 444 356 092 776 315
;p2: 341 960 390 180 808