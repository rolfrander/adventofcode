(ns advent2024.day23
  (:require
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.set :as set]))

(def testdata "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")

(def data (puzzle/get-data 2024 23))

(defn parse [in]
  (let [connections (->> (str/split-lines in)
                         (map #(str/split % #"-")))
        update-or-new (fn [s n] (if (nil? s) #{n} (conj s n)))
        connections-map (reduce #(update %1 (first %2) update-or-new (second %2)) 
                                {} connections)
        connections-map (reduce #(update %1 (second %2) update-or-new (first %2))
                                connections-map connections)]
    connections-map))

(defn solve-1 [in]
  (->> (let [d (parse in)]
         (for [c1 (keys d)
               c2 (d c1)
               c3 (set/intersection (d c1) (d c2))]
           (str/join "," (sort [c1 c2 c3]))))
       (into #{})
       (filter #(re-find #"t[a-z]" %))
       count))

(map hash-set [1 2 3])

(defn solve-2 [in]
  (let [data (parse in)
        find-intersection-of-all (fn [computers]
                                   (apply set/intersection (map data computers)))]

    (->> (loop [levels (map hash-set (keys data))]
           (let [nextlevel (->> (for [x levels
                                      y (find-intersection-of-all x)]
                                  (conj x y))
                                (into #{}))]
             (if (empty? nextlevel)
               levels
               (recur nextlevel))))
         (map (fn [computerset]
                (str/join "," (sort computerset)))))))

(apply disj #{"a" "b" "c" "d" "e"} ["a" "d"])

(->> (let [d (parse data)]
       (for [c1 (keys d)
             c2 (d c1)
             c3 (disj (set/intersection (d c1) (d c2)) c1 c2)
             c4 (disj (set/intersection (d c1) (d c2) (d c3)) c1 c2 c3)
             :let [c5 (disj (set/intersection (d c1) (d c2) (d c3) (d c4)) c1 c2 c3 c4)]
             :when (> (count c5) 1)]
         [c1 c2 c3 c4 c5]))
     
     )

(solve-1 testdata)
;;=> 7
(solve-1 data)
;;=> 1064

(solve-2 testdata)
;;=> ("co,de,ka,ta")
(solve-2 data)
;;=> ("aq,cc,ea,gc,jo,od,pa,rg,rv,ub,ul,vr,yy")