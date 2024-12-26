(ns advent2023.day05 
  (:require
    [clojure.string :as str]
    [rolfrander.puzzle-lib :as puzzle]))

(def testdata "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn parse-map [m]
  (let [[_ign from to] (re-find #"(\w+)-to-(\w+) map:" (first m))]
    {:from from
     :to to
     :offsets (reduce (fn [r e]
                    (conj r
                          (let [[dest src length] (map puzzle/str->long (str/split e #" "))]
                            {:dst dest
                             :src src
                             :len length})))
                  []
                  (rest m))}))

(defn parse [in]
  (let [[seed & maps] (->> (str/split-lines in)
                           (puzzle/split-by #(not= % "")))
        maps (map (comp parse-map rest) maps)]
    {:seed (map puzzle/str->long (re-seq #"\d+" (first seed)))
     :maps maps}))

; dst < src
;         dst      dst+len     src       src+len
;      ----+----------+---------+-----------+------
; d=     s      s          s      s-src+dst    s

; dst > src
;         src     src+len      dst       dst+len
;      ----+----------+---------+-----------+------
; d=     s  s-src+dst      s          s        s


(defn offset-map-feil [src m]
  (cond (and (< src (:src m))
             (< src (:dst m))) src

        (and (>= src (+ (:src m) (:len m)))
             (>= src (+ (:dst m) (:len m)))) src

        (and (>= src (:src m))
             (< src (+ (:src m) (:len m)))) (+ (- src (:src m)) (:dst m))

        (and (>= src (+ (:src m) (:len m)))
             (< src (+ (:dst m) (:len m)))) (- src (:len m))

        :else
        (+ src (:len m))))

(defn offset-map [src m]
  (if (and (>= src (:src m))
           (< src (+ (:src m) (:len m))))
    (+ (- src (:src m)) (:dst m))
    nil))

(let [d (parse testdata)
      m (-> d :maps first :offsets)]
  (map (juxt identity #(or (some (partial offset-map %) m) %))
       (range 80 100))
  )

(offset-map 50 {:dst 50, :src 98, :len 2})

(defn- map-seeds [maps s]
  (loop [[{:keys [_from _to offsets]} & maps] maps
         seed s]
    (if (nil? offsets)
      seed
      (recur maps
             (or (some (partial offset-map seed) offsets) seed)))))

(defn solve-1 [in]
  (let [d (parse in)]
    (->> (map (partial map-seeds (:maps d))
              (:seed d))
         (apply min))))

(defn solve-2 [in]
  (let [d (parse in)
        s (partition 2 (:seed d))]
    (->> (mapcat #(map (partial map-seeds (:maps d))
                       (range (first %) (+ (first %) (second %))))
                 s)
         (apply min))))


(solve-1 testdata)
;;=> 35
(solve-1 (puzzle/get-data 2023 5))
;;=> 484023871

(solve-2 testdata)
;;=> 46
(solve-2 (puzzle/get-data 2023 5))