(ns advent2022.day07
  (:require [rolfrander.puzzle-lib :as puzzle]
            [clojure.string :as str]))

(def testdata "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

; parsed data, list of
; [:cmd-ls]
; [:cmd-cd "directory"]
; [:output-dir "name"]
; [:output-file size "name"]

(defn token-line [l]
  (let [[a b c] (str/split l #" ")]
    (if (= a "$") ; command
      (if (= b "ls")
        [:cmd-ls]
        [:cmd-cd c])
      (if (= a "dir")
        [:output-dir b]
        [:output-file (puzzle/str->long a) b]))))

(defn tokenize [input]
  (map token-line (str/split-lines input)))

;(token testdata)

; datastructure
; {:name "asdf"
;  :type :dir/:file
;  :size 1234
;  :children [...]}

(defn parse-ls [input]
  (loop [[token & input] input
         data {}]
    ;(println token)
    (cond (= (first token) :output-dir)
          (let [[_ name] token]
            (recur input (assoc data name {:type :dir :size 0 :name name})))

          (= (first token) :output-file)
          (let [[_ size name] token]
            (recur input (assoc data name {:type :file :size size :name name})))

          :else
          [(cons token input) data])))

;(parse-ls (rest (token testdata)) {})

(defn parse-dir [input is-root]
  (loop [[token & input] input
         contents nil]
    ;(println "parse dir" token)
    (cond (or (nil? token)
              (and (= (first token) :cmd-cd) (= (second token) "..")))
          [input contents]

          (= (first token) :cmd-ls)
          (let [[input data] (parse-ls input)]
            (when (not (nil? contents))
              (throw (RuntimeException. (str "ls when data is known:" contents))))
            (recur input data))

          (= (second token) "/")
          (if is-root
            (recur input contents)
            ; if not at root, return token to calling parsing function
            [(cons token input) contents]
            )
          
          :else
          (do
            ;(println "going into" (second token))
            (let [[input data] (parse-dir input false)]
              ;(println "returning from" (second token) "with" data)
              (cond (not (contains? contents (second token)))
                    (throw (RuntimeException. (str "read unknown directory" (second token))))
                    
                    (contains? (get contents (second token)) :children)
                    (throw (RuntimeException. (str "overwriting directory" (second token))))
                    
                    :else
                    (recur input (assoc-in contents
                                           [(second token) :children]
                                           data))))))
    ))

(def ^:dynamic *indent* 0)

(defn print-dir [dir]
  (letfn [(print-subdir [dir]
                        (printf (str "%" *indent* "s - %s (%s%s)\n")
                                ""
                                (:name dir)
                                (:type dir)
                                (if (:size dir) (str ", size=" (:size dir)) ""))
                        (when (seq (:children dir))
                          (print-dir dir)))]
    (when (= *indent* 0)
      (printf "- / (dir size=%d)\n" (:size dir)))
    (binding [*indent* (+ *indent* 2)]
      (doseq [d (vals (:children dir))]
        (print-subdir d)))
    ))

(defn parse [input]
  {:name "/"
   :type :dir
   :size 0
   :children (second (parse-dir (tokenize input) true))})

(defn is-dir [e] (= :dir (:type e)))

(defn find-dir-sizes [dir]
  (letfn [(find-size [element]
                     (reduce (fn [dir child]
                               (if (is-dir child)
                                 (let [child (find-size child)]
                                   (-> dir
                                       (assoc-in [:children (:name child)] child)
                                       (update :size (partial + (:size child)))))
                                 (update dir :size (partial + (:size child)))))
                             element 
                             (vals (:children element))))]
    (find-size dir)))


(defn task-1 [input]
  (let [dir (parse input)]
    (letfn [(find-size [dir]
                       (let [grouped (group-by :type (vals (:children dir)))
                             file-size (reduce + (map :size (:file grouped)))
                             subdir-sizes (map find-size (:dir grouped))
                             subdir-total (reduce + (map first subdir-sizes))
                             all-dir-size (reduce + (map second subdir-sizes))
                             total-size (+ file-size all-dir-size)]
                         ;(printf (str "%" *indent* "s %s %d %d\n") "" (:name dir) dir-size total-size)
                         [(if (<= total-size 100000)
                            (+ total-size subdir-total)
                            subdir-total)
                          total-size]))]
      (first (find-size dir)))))

(defn task-1b [input]
  (let [dir (find-dir-sizes (parse input))]
    (letfn [(find-size [dir]
              (let [acc (->> (vals (:children dir))
                             (filter is-dir)
                             (map find-size)
                             (reduce +))]
                (if (<= (:size dir) 100000)
                  (+ (:size dir) acc)
                  acc)))]
      (find-size dir))))

(defn task-2 [input]
  (let [filesystem (find-dir-sizes (parse input))
        needed (- (:size filesystem) 40000000)]
    (letfn [(find-needed [dir cur-min]
                         (let [cur-min (->> (vals (:children dir))
                                            (filter is-dir)
                                            (map #(find-needed % cur-min))
                                            (reduce min cur-min))]
                           (if (>= (:size dir) needed)
                             (min cur-min (:size dir))
                             cur-min)))]
      (find-needed filesystem (:size filesystem)))))

(print-dir (find-dir-sizes (parse testdata)))
(task-1b testdata)
(print-dir (parse (puzzle/get-data 2022 7)))
(task-1b (puzzle/get-data 2022 7))

(task-2 testdata)
(task-2 (puzzle/get-data 2022 7))