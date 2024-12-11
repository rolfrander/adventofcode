(ns advent2024.day09 
  (:require
    [rolfrander.puzzle-lib :as puzzle]))

(def testdata "2333133121414131402")

(defn parse-1 [in]
  (loop [[f s & more] (map puzzle/char->digit in)
         id 0
         ret []]
    (if (nil? f)
      ret
      (recur more
             (inc id)
             (-> ret
                 (into (repeat f id))
                 (into (repeat (or s 0) nil)))))))

(defn checksum-1 [data]
  (->> (map-indexed #(if (nil? %2) 0 (* %1 %2)) data)
       (reduce +)))

(defn solve-1 [in]
    (loop [data (parse-1 in)
           i 0
           j (dec (count data))]
      (cond (>= i j) (checksum-1 data)

            (nil? (data j))
            (recur data i (dec j))

            (not (nil? (data i)))
            (recur data (inc i) j)

            :else
            (recur (-> data
                       (assoc i (get data j))
                       (assoc j nil))
                   (inc i)
                   (dec j)))))

(defn debug [x]
  (println "DEBUG" x)
  x)

(defn parse-2 [in]
  (loop [[f s & more] (map puzzle/char->digit in)
         id 0
         cur-pos 0
         files []
         free []
         free-offsets []]
    (if (nil? s)
      {:files (if (nil? f) files (conj files {:id id :len f}))
       :free free
       :free-offsets free-offsets}
      (recur more
             (inc id)
             (+ cur-pos f s)
             (-> files 
                 (conj {:id id :len f})
                 (into (repeat (+ f s -1) nil)))
             (conj free s)
             (conj free-offsets (+ cur-pos f))))))

(set! *warn-on-reflection* true)

(defn solve-2 [in]
  (let [move (fn [v ^long oldpos ^long newpos]
               (-> v
                   (assoc newpos (get v oldpos))
                   (assoc oldpos nil)))
        data (parse-2 in)
        files (loop [files (data :files)
                     free (data :free)
                     free-offsets (data :free-offsets)
                     i (count files)]
                (cond (= i 0) files

                      (nil? (get files i)) (recur files free free-offsets (dec i))

                      :else
                      (let [file (get files i)
                            [idx offset] (->> (map vector (range) free free-offsets)
                                              (some (fn [[idx free-cnt offset]]
                                                      (when (and (>= free-cnt (file :len))
                                                                 (< offset i))
                                                        [idx offset]))))]
                        ;(when-not (nil? idx) (println "*** move" (file :id) "len" (file :len) "to" offset "where space" idx "has" free-cnt "blocks"))
                        (if (nil? idx)
                          (recur files free free-offsets (dec i))
                          (recur (move files i offset)
                                 (update free idx #(- % (file :len)))
                                 (update free-offsets idx #(+ % (file :len)))
                                 (dec i))))))

        files-2 (first (reduce (fn [[ret fileno cnt] file]
                                 (cond (> cnt 0) [(conj ret fileno) fileno (dec cnt)]
                                       (nil? file) [(conj ret nil) fileno cnt]
                                       :else [(conj ret (file :id)) (file :id) (dec (file :len))]))

                               [[] 0 0] files))]
    (checksum-1 files-2)))

(assoc [] 0 \a)

(solve-1 testdata)
;;=> 1928
(solve-1 (puzzle/get-data 2024 9))
;;=> 6323641412437

(solve-2 testdata)
;;=> 2858
(time (solve-2 (puzzle/get-data 2024 9)))