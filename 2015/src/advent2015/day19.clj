(ns advent2015.day19
  (:require [clojure.string :as str]))

(def ^:dynamic *debug* false)

(defn debug [value]
  (when *debug* (prn value))
  value)

(defn parse-line [substitutions line]
  (if-let [m (re-matches #"([^ ]+) => (.*)" line)]
    [(update substitutions (nth m 1) conj (nth m 2)) nil]
    (if (empty? line)
      [substitutions nil]
      [substitutions line])))

(defn parse [input]
  (reduce (fn [[substitutions start] line]
            (parse-line substitutions line))
          [{} nil]
          input))

(defn parse-file [filename]
  (with-open [in (clojure.java.io/reader filename)]
    (doall (parse (line-seq in)))))

(def split-molecule (partial re-seq #"e|[A-Z][a-z]*"))

(defn reverse-mapping [subst]
  (->> (mapcat (fn [[key val]]
                 (map #(vector (split-molecule %) key) val))
               subst)
       (reduce (fn [mapping [to from]]
                 (assoc mapping to from))
               {})))

(def data (parse-file "resources/2015/day19.txt"))

; (reverse-mapping (first data))

(def testdata (parse (str/split-lines "e => H
e => O
H => HO
H => OH
O => HH

HOH")))

(defn generate-molecules [subst start] ;(parse (str/split-lines testdata))
  (let [tokens (split-molecule start)]
    (letfn [(generate-with-prefix [prefix token postfix]
              (map #(str/join (concat (conj prefix %) postfix)) (subst token)))]
      (loop [[t & tokens] tokens
             prefix []
             generated #{}]
        (if (nil? t)
          generated
          (recur tokens
                 (conj prefix t)
                 (into generated (generate-with-prefix prefix t tokens))))))))

;(generate-molecules (first testdata) (second testdata))

;(generate-molecules (first testdata) "e")

(defn molecule-startswith [molfragment prefix]
  (loop [mol molfragment
         [p & pre] prefix]
    (cond (nil? p) mol
          (not= (first mol) p) nil
          :else (recur (rest mol) pre))))

(defn molecule-merge [prefix postfix]
  ; this is really 'concat' but lets twiddle some lists for once...
  (if (empty? prefix)
    postfix
    (conj (molecule-merge (rest prefix) postfix) (first prefix))))


;(conj (molecule-startswith ["a" "b"] ["a" "b"]) "e")

(defn collect-all-single-subst [mol subst]
  (letfn [(collect-internal [mol]
            (if (empty? mol)
              ['() '()]
              (let [replace-molecules (->> (for [prefix (keys subst)
                                                 :let [postfix (molecule-startswith mol prefix)]
                                                 :when (not (nil? postfix))
                                                 new-prefix (subst prefix)
                                                 :when (or (not= new-prefix "e") (empty? postfix))]
                                             (conj postfix new-prefix))
                                           (into #{}))
                    cur-element (first mol)
                    [new-head collect-recursive] (collect-internal (rest mol))]
                [(conj new-head cur-element)
                 (concat (->> collect-recursive
                              (filter #(not= (first %) "e"))
                              (map #(conj % cur-element)))
                         replace-molecules)])))]
    (into #{} 
          (map #(apply str %))
          (second (collect-internal mol)))
    ;(collect-internal mol)
    ))

(defn overlap [x y]
  (letfn [(overlap-internal [a b]
            (some (partial molecule-startswith a) (take-while seq (iterate rest b))))]
    (or (overlap-internal x y)
        (overlap-internal y x))))

;(overlap '(1 2 3) '(2 3 4))

(defn get-unique-subst [available-keys]
  (let [subst (into #{} available-keys)]
    (first (reduce (fn [[safe-subst rest-subst] sub]
                     (if (not (contains? rest-subst sub))
                       ; already removed
                       [safe-subst rest-subst]
                       (let [o (filter (partial overlap sub) rest-subst)]
                       ;; (overlap x x) returns '()
                         (if (> (count o) 1)
                           [safe-subst (reduce disj rest-subst o)]
                           [(conj safe-subst sub) (disj rest-subst sub)]))))
                   [#{} subst]
                   subst))))



(defn task-2 [data]
  (let [subst (reverse-mapping (first data))
        original (split-molecule (second data))]
    (letfn [(do-replace [molecule slist]
              (if (empty? molecule)
                [0 molecule]
                (let [mol-head (first molecule)
                      mol-tail (rest molecule)
                      [cnt mol-new] (do-replace mol-tail slist)
                      mol-new (conj mol-new mol-head)]
                  (reduce (fn [[cnt mol-new] s]
                            (if-let [postfix (molecule-startswith mol-new s)]
                              [(inc cnt) (conj postfix (subst s))]
                              [cnt mol-new]))
                          [cnt mol-new] slist))))]
      (loop [molecule original
             total-cnt 0]
        (let [[cnt new-mol] (do-replace molecule (keys subst))]
          (if (= cnt 0)
            [total-cnt (str/join molecule)]
            (recur new-mol (+ total-cnt cnt)))))
      )))

(let [r (reverse-mapping (first data))
      s (get-unique-subst (keys r))]
  (map r (remove #(= (r %) "e") s)))

(time (count (collect-all-single-subst (split-molecule (second data))
                                       (reverse-mapping (first data)))))

(collect-all-single-subst (split-molecule "HOHOHO") (reverse-mapping (first testdata)))

(binding [*debug* false]
  (task-2 data))

(time (task-2 (reverse-mapping (first data)) (second data)))

(sort (map str/join (keys (reverse-mapping (first data)))))
