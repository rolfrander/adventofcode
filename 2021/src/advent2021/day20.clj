(ns advent2021.day20  
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def testinput "#.#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#...

#..#.
#....
##..#
..#..
..###")

(def testdata (string/split-lines testinput))

(defn parse-line [line padding]
  (let [l (count line)
        #^bytes a (byte-array (+ l (* 2 padding)))]
       (loop [[c & line] line
              i padding]
         (if (nil? c)
           a
           (do (aset-byte a (int i) (if (= c \.) (byte 0) (byte 1)))
               (recur line (inc i)))))))

(defn parse [lines padding]
  (let [enhancement (first lines)
        ret {:enhancement (parse-line enhancement 0)
             :image (loop [[l & lines] (nthrest lines 2)
                           first true
                           result []]
                      (let [linelen (count l)]
                        (if (empty? lines)
                          (reduce (fn [res _cnt] (conj res (byte-array (+ linelen (* 2 padding)))))
                                  (conj result (parse-line l padding))
                                  (range padding))
                          (if first
                            (recur lines
                                   false
                                   (-> (for [_i (range padding)]
                                         (byte-array (+ linelen (* 2 padding))))
                                       (vec)
                                       (conj (parse-line l padding))))
                            (recur lines false (conj result (parse-line l padding)))))))}]
    (assoc ret :new-image (vec (for [a (:image ret)]
                                 (byte-array (alength a) (byte 0)))))))

(defn convolute [{:keys [enhancement image new-image]}]
  (let [h (count new-image)
        w (alength (first new-image))
        get-value (fn [i j]
                    (->> (for [y [(inc j) j (dec j)]
                               x [(inc i) i (dec i)]]
                           (aget (get image y) x))
                         (map * (iterate (partial * 2) 1))
                         (reduce +)))]
    (doseq [j (range 1 (dec h))
            i (range 1 (dec w))]
      (aset-byte (get new-image j) i
                 (aget enhancement (get-value i j))))
    {:enhancement enhancement
     :image new-image
     :new-image image}))

(defn print-image [padding image]
  (let [w (alength (first image))
        h (count image)]
    (doseq [image-line (subvec image padding (- h padding))
            j (range (alength image-line))]
      (when (= j 0) (newline))
      (when (< padding j (- w padding))
        (print (if (= (aget image-line j) 0) \. \#))))))

  (let [image (:image (parse (string/split-lines testinput) 0))
        new-image (vec (for [a image]
                         (byte-array (alength a) (byte 0))))
        h (count new-image)
        w (alength (first new-image))
        get-value (fn [i j]
                    (->> (for [y [(inc j) j (dec j)]
                               x [(inc i) i (dec i)]]
                           (aget (get image y) x))
                         (map * (iterate (partial * 2) 1))
                         (reduce +)))]
    (get-value 3 1))

(defn count-pixels [padding image]
  (let [w (alength (first image))
        h (count image)]
    (->> (for [r (subvec image padding (- h padding))]
           (areduce r i ret 0
                    (if (< padding i (- w padding))
                      (+ ret (aget r i))
                      ret)))
         (reduce +))))

(def data
  (string/split-lines (advent2021.core/load-data 20 session)))


(let [img-seq (iterate convolute (parse testdata 80))
      img (nth img-seq 50)]
  ;(print-image 0 (:image img))
  (count-pixels 45 (:image img))
  )