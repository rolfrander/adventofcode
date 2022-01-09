(ns advent2021.day16
  (:require [clojure.string :as string]))

(defn unhexify [s]
  (letfn [(unhexify-2 [c1 c2]
            (unchecked-byte
             (+ (bit-shift-left (Character/digit c1 16) 4)
                (Character/digit c2 16))))]
    (map #(apply unhexify-2 %) (partition 2 s))))

(defn unhexify-str [s]
  (apply str (map char (unhexify s))))

(defn hex-input [^String input]
  (atom {:input input
         :bits-in-buffer 4
         :input-pos 1
         :buffer (Character/digit (.charAt input 0) 16)
         :value 0}))

(defn hex-get-bits [hex bitcount]
  (when (or (< bitcount 1) (> bitcount 58))
    (throw (RuntimeException. (str "bitcount must be between 0 and 59, was: " bitcount))))
  (letfn [(internal-hex-get-bits [hex bitcount]
            (let [{:keys [input bits-in-buffer input-pos buffer]} hex
                  keepmask 0xf
                  bitmask (bit-not keepmask)]
              (loop [i input-pos
                     b bits-in-buffer
                     buffer buffer
                     countdown bitcount]
                (if (= 0 countdown)
                  {:input input
                   :bits-in-buffer b
                   :input-pos i
                   :buffer (bit-and buffer keepmask)
                   :value (bit-shift-right (bit-and buffer bitmask) 4)}
                  (let [buffer (bit-shift-left buffer 1)
                        b (dec b)
                        next-nibble (if (< i (.length input))
                                      (Character/digit (.charAt input i) 16)
                                      0)]
                    (if (= b 0)
                      (recur (inc i) 4 (bit-or buffer next-nibble) (dec countdown))
                      (recur      i  b         buffer              (dec countdown))))))))]
    (:value (swap! hex internal-hex-get-bits bitcount))))

(defn hex-last-value [hex] (:value @hex))

(defn hex-get-bit-pos [hex]
  (let [{:keys [bits-in-buffer input-pos]} @hex]
    (- (* 4 input-pos) bits-in-buffer)))

(defn get-packet [hex]
  (letfn [(get-packet-version [] (hex-get-bits hex 3))
          (get-packet-type [] (hex-get-bits hex 3))
          (get-operator [] (let [length-type-id (hex-get-bits hex 1)]
                             (case length-type-id
                               0 (let [length (hex-get-bits hex 15)
                                       startpos (hex-get-bit-pos hex)]
                                   (loop [res []]
                                     (if (>= (- (hex-get-bit-pos hex) startpos) length)
                                       res
                                       (recur (conj res (get-packet hex))))))
                               1 (loop [pktcnt (hex-get-bits hex 11)
                                        res []]
                                   (if (= 0 pktcnt)
                                     res
                                     (recur (dec pktcnt) (conj res (get-packet hex))))))))
          (get-litteral [] (loop [value 0]
                             (let [cont (hex-get-bits hex 1)
                                   value (bit-or (bit-shift-left value 4)
                                                 (hex-get-bits hex 4))]
                               (if (= cont 1)
                                 (recur value)
                                 value))))]
    (let [version (get-packet-version)
          packet-type (get-packet-type)]
      [:version version
       (case packet-type
         4 [:litteral (get-litteral)]
         [:operator packet-type (get-operator)])])))

(defn add-versions [packet]
  (let [[version-litteral version content] packet]
    (cond (not= version-litteral :version)
          (throw (RuntimeException. (str "not a valid packet: " packet)))

          (= (first content) :litteral)
          version

          ;; operator
          :else
          (reduce (fn [sum packet]
                    (+ sum (add-versions packet)))
                  version
                  (get content 2)))))

(defn evaluate [packet]
  (let [gt (fn [a b] (if (> a b) 1 0))
        lt (fn [a b] (if (< a b) 1 0))
        eq (fn [a b] (if (= a b) 1 0))
        op {0 + 1 * 2 min 3 max 5 gt 6 lt 7 eq}
        [version-litteral _version content] packet]
    (cond (not= version-litteral :version)
          (throw (RuntimeException. (str "not a valid packet: " packet)))

          (= (first content) :litteral)
          (second content)

          ;; operator
          :else
          (let [[_ operator arglist] content]
            (apply (op operator) (map evaluate arglist))))))

(def testdata1 "D2FE28")
(def testdata2 "8A004A801A8002F478")
(def testdata3 "C0015000016115A2E0802F182340")
(def testdata4 "A0016C880162017C3686B18A3D4780")
(def data (advent2021.core/load-data 16 "53616c7465645f5f5a4604206c8513eae045f4ad5f28de5f922c27403a8b27dc3c61b859b662ac43879c8aaed36c456f"))

(get-packet (hex-input testdata1))
(get-packet (hex-input testdata2))
(get-packet (hex-input testdata3))

(defn task-1 [input]
  (->> input
       hex-input
       get-packet
       add-versions))

(defn task-2 [input]
  (->> input
       hex-input
       get-packet
       evaluate))

(task-2 data)

