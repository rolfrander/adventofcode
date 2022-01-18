(ns advent2017.day17)

(defn task-1 [^long jmp ^long iterations]
  (loop [len 1
         pos 0
         buffer [0]]
    ;(println buffer)
    (if (> len iterations)
      (nth buffer (mod (inc pos) len))
      (let [newpos (inc (mod (+ pos jmp) len))]
        (recur (inc len)
               newpos
               (concat (take newpos buffer) [len] (nthrest buffer newpos)))))))

(defn task-2 [^long jmp ^long iterations]
  (loop [len 1
         pos 0
         value-after-0 -1]
    ;(println buffer)
    (if (> len iterations)
      value-after-0
      (let [newpos (inc (mod (+ pos jmp) len))]
        (recur (inc len)
               newpos
               (if (= newpos 1) len value-after-0))))))

;(task-1 3 2017)
(task-1 337 2017)

(map (partial task-2 3) (range 10))

(task-2 337 50000000)