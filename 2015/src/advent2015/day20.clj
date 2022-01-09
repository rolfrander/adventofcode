(ns advent2015.day20)

;; dette er sigma-funksjonen https://en.wikipedia.org/wiki/Divisor_function
;; faktoriserings-kode fra knowit kodekalender, 16. desember 2020


(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn ^booleans prime-sieve [cnt]
  (let [result (boolean-array cnt true)]
    (aset-boolean result 0 false)
    (aset-boolean result 1 false)
    (doseq [i (range 2 (Math/sqrt cnt))]
      (when (aget result i)
        (doseq [j (range (* i i) cnt i)]
          (aset-boolean result j false))))
    result))

(let [primes ^booleans (prime-sieve 1000000)]
  (defn is-prime? [look-for-number]
    (aget primes look-for-number)))

(defn gcd [^long a ^long b]
  (if (= b 0) a
      (recur b (long (mod a b)))))

;;;; sieve
(defn sigma-sieve [iterations]
  (let [sigma (int-array iterations -1)]
    (aset-int sigma 0 0)
    (aset-int sigma 1 1)
    (doseq [i (range 2 iterations)]
      (if (is-prime? i)
        ;; dersom primtall
        ; sigma(i^n) = sigma(i^(n-1))+i^n when i is prime
        (loop [i-to-n i
               sigma-i-to-n (inc i)]
          (aset-int ^ints sigma i-to-n sigma-i-to-n)
          (let [next (* i-to-n i)]
            (when (< next iterations) (recur next (+ sigma-i-to-n next))))))

      (let [sigma-i (aget ^ints sigma i)]
        ; sigma(i*j) = sigma(i) * sigma(j) when gcd(i,j)=1
        (doseq [j (range 2 (min i (Math/ceil (/ iterations i))))]
          (when (= 1 (gcd i j))
            (aset-int sigma (* j i) (* sigma-i
                                       (aget ^ints sigma j)))))))
    sigma))

(defn modified-sigma [start end limit]
  (let [packets (int-array (- end start) 0)]
    (loop [n (max 1 (quot start limit))]
      (loop [i 1]
        (when (< i limit)
          (let [house-no (* n i)
                packet-idx (- house-no start)]
            (if (and (>= packet-idx 0) (< packet-idx (alength packets)))
              (aset packets packet-idx (+ n (aget packets packet-idx))))
            (recur (inc i)))))
      (if (< n end)
        (recur (inc n))))
    packets))


(def data 34000000)

(def sigma-data (sigma-sieve 1000000))

(* 11 (get sigma-data 720720))

;; task-1
(def task-1
  (some identity (map-indexed (fn [i n] (when (> n (/ data 10)) i)) sigma-data)))

(def task-2 (modified-sigma 720720 1500000 50))
(+ 720720 (some identity (map-indexed (fn [i n] (when (> (* 11 n) data) i)) task-2)))

