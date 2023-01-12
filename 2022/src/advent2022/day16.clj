(ns advent2022.day16
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

(def data (puzzle/get-data 2022 16))

(defn parse [input]
  (let [parse-line (fn [[_line valve flow tunnels]]
                     {:valve valve :flow (puzzle/str->long flow) :tunnels (str/split tunnels #", ")})]
    (->> (re-seq #"Valve ([A-Z]+)[a-z ]*=([0-9]+);[a-z ]*([A-Z, ]+)" input)
         (map parse-line)
         (map (juxt :valve identity))
         (into {}))))

;; build network with only working valves

(defn network [data]
  (let [targets (into #{} (->> (filter #(> (:flow %) 0) (vals data))
                               (map :valve)))
        paths-to-targets (fn [start]
                           (-> (puzzle/dijkstra (keys data)
                                                start
                                                (comp :tunnels data)
                                                :result-type :dist)
                               (select-keys targets)))]
    (into {} (map #(vector % (dissoc (paths-to-targets %) %))
                  (conj targets "AA")))))

(defn index-keys [k first-key]
  (reduce #(if (= %2 first-key)
             %1
             (assoc %1 %2 (count %1)))
          {first-key 0}
          k))

;(index-keys ["b" "c" "d" "a"] "a")

(defn number-network [net index]
  (let [map-to-index (fn [f c] (reduce-kv #(assoc %1 (index %2) (f %3))
                                          (vec (repeat (count index) nil))
                                          c))]
    (map-to-index (partial map-to-index identity) net)))

(defn index-flow [valves index]
  (reduce-kv #(if (contains? index %2)
                (assoc %1 (index %2) (:flow %3))
                %1)
             (vec (repeat (count index) 0))
             valves))

(def calc-total-pressure
  (memoize (fn [path net flow total-time]
             (loop [time 0
                    current-pressure 0
                    total-pressure 0
                    [node & path] path]
    ;(println time current-pressure total-pressure node)
               (if (or (empty? path) (>= time total-time))
                 (+ total-pressure
                    (* current-pressure (- total-time time)))
                 (let [next-time (+ time (get-in net [node (first path)]) 1)
                       next-pressure (+ current-pressure (flow (first path)))
                       pressure-increase (* current-pressure (- next-time time))]
                   (recur next-time
                          next-pressure
                          (+ total-pressure pressure-increase)
                          path)))))))

(defn depth-first-path [net start time exclude]
  (letfn [(df [cur visited time-left]
              (if (< time-left 0)
                :nil
                (let [visited (conj visited cur)
                      neighbours (->> (keys (net cur))
                                      (remove visited)
                                      (remove #(<= time-left ((net cur) %))))]
                  (if (empty? neighbours)
                    (list (list cur))
                    (concat (list (list cur))
                            (for [r neighbours
                                  s (df r visited (- time-left ((net cur) r) 1))]
                              (cons cur s)))))))]
    (df start (or exclude #{}) time)))

(defn depth-first-flow [valves net start time exclude]
  ;; dette er i prinsippet samme algoritme som TSP under, bare uttrykt mer konsist med memoize
  (let [df (memoize (fn [recur-call cur visited time-left]
                      (if (<= time-left 0)
                        0
                        (let [pressure (* (:flow (valves cur))
                                          time-left)
                              visited (conj visited cur)]
                          (->> (keys (net cur))
                               (remove visited)
                               (map #(recur-call recur-call 
                                                 %
                                                 visited
                                                 (- time-left ((net cur) %) 1)))
                               (reduce max 0)
                               (+ pressure))))))]
    (df df start (or exclude #{}) time)))

(defn combine [cnt coll]
  ;; functional version of 
  ;; https://en.wikipedia.org/wiki/Heap%27s_algorithm
  (letfn [(generate [available chosen]
            (if (= cnt (count chosen))
              [chosen]
              (if (< (+ (count available) (count chosen)) cnt)
                nil
                (loop [[c & k] available
                       used '()
                       results '()]
                  (if (not c)
                    results
                    (let [r (generate k
                                      (conj chosen c))]
                      (recur k (conj used c) (concat results r))))))))]
    (generate coll #{})))

; https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm
; distance measured as [(* minutes (- max-pressure pressure)) minutes-after-opening-valve pressure-after-opening-valve]
; the current distance must be input to distance-function
; this add to previous distance
;function algorithm TSP (G, n) is
(defn tsp [valves net start timelimit]
;    for k := 2 to n do
;        g({k}, k) := d(1, k)
;    end for
  (let [max-pressure (reduce + (map :flow (vals valves)))
        targets (into #{} (remove #{start} (keys net)))
        d (fn [from to {:keys [dist time pressure]}]
            ;(println from to [dist time current-pressure])
            (if (nil? dist)
              {:dist (* timelimit max-pressure) :time timelimit :pressure 0 :prev nil}
              (let [delta-time (inc (get-in net [from to]))]
                {:dist (+ dist (* delta-time (- max-pressure pressure)))
                 :time (+ time delta-time)
                 :pressure (+ pressure (:flow (valves to)))
                 :prev from})))
        g (reduce #(assoc %1 [#{%2} %2] (d start %2 {:dist 0 
                                                     :time 0 
                                                     :pressure 0
                                                     :prev nil}))
                  {}
                  targets)

;    for s := 2 to n−1 do
;        for all S ⊆ {2, ..., n}, |S| = s do
;            for all k ∈ S do
;                g(S, k) := minm≠k,m∈S [g(S\{k}, m) + d(m, k)]
;            end for
;        end for
;    end for
        g (reduce (fn [g s]
                    (into g
                          (for [S (combine s targets)
                                k S
                                :let [S-k (disj S k)
                                      min-m (reduce (fn [tmp m]
                                                      (if  (= m k)
                                                        tmp
                                                        (min-key :dist tmp (d m k (g [S-k m])))))
                                                    {:dist (* timelimit max-pressure) 
                                                     :time timelimit
                                                     :pressure 0
                                                     :prev nil} 
                                                    S)]
                                :when (< (:time min-m) timelimit)]
                            [[S k] min-m])))
                  g (range 2 (inc (count targets))))]

    ;(reduce (partial min-key (comp first second)) g)
    g
;    opt := mink≠1 [g({2, 3, ..., n}, k) + d(k, 1)]
;    return (opt)
;end function
    ))

;; Genetisk algoritme
;; https://stackoverflow.com/questions/1544055/crossover-operation-in-genetic-algorithm-for-tsp
;; http://www.permutationcity.co.uk/projects/mutants/tsp.html
;; https://user.ceng.metu.edu.tr/~ucoluk/research/publications/tspnew.pdf
(defn inverse [perm]
  (let [N (count perm)]
    (vec (for [i (range N)]
           (->> (take-while (partial not= i) perm)
                (filter (partial < i))
                count)))))

(defn re-inv [inv] 
  (let [N (count inv)
        pos (->> (for [i (reverse (range N))
                       m (range (inc i) N)]
                   [i m])
                 (reduce (fn [pos [i m]]
                           (if (> (pos m) (inv i))
                             (update pos m inc)
                             pos))
                         (vec (map inc inv))
                         ))]
    (reduce #(assoc %1 (dec (pos %2)) %2) 
            (vec (repeat N 0))
            (range N))
    ;pos
    ))

(re-inv (inverse [3 5 1 6 2 0 4]))

(re-inv [4 2 2 1 0])

;; ************************************************************************************************

(let [valves (parse data)]
  (depth-first-flow valves (network valves) "AA" 30 nil))

(let [valves (parse data)
      tree (depth-first-path (network valves) "AA" 26 nil)]
  (count tree)
  )

(let [valves (parse testdata)
      net (network valves)]
  (->> ["AA" "JJ" "DD" "BB"]
       (partition 2 1)
       (map (partial get-in net))
       (map inc))
  )

(let [valves (parse testdata)
      net (network valves)
      index (index-keys (keys net) "AA")
      idx-net (number-network net index)
      idx-flow (index-flow valves index)]
  (get-in idx-net [1  6])
  ;index
  )

(let [valves (parse testdata)
      net (network valves)
      index (index-keys (keys net) "AA")
      idx-net (number-network net index)
      idx-flow (index-flow valves index)
      calc-pressure (fn [path] (calc-total-pressure path idx-net idx-flow 30))
      start-vectors (->> (iterate shuffle [1 2 3 4 5 6])
                         (take 100))]
  ;(map calc-pressure start-vectors)
  (map (juxt (constantly 0) inverse) start-vectors)
  ;(map (juxt (comp calc-pressure (partial cons 0))
  ;           identity)
  ;     start-vectors)
  ;(calc-pressure (map index ["AA" "DD" "BB" "JJ" "HH" "EE" "CC"]))
  )

;(shuffle [0 0 1 2 3 4 5 6])

(let [valves (parse testdata)]
  (reduce + (map :flow (vals valves))))

(defn valve-tsp-result-sets [valves timelimit]
  (let [max-pressure (reduce + (map :flow (vals valves)))]
    (->> (tsp valves (network valves) "AA" timelimit)
         (map #(let [{:keys [dist time pressure]} (second %)]
                 [(- (* max-pressure timelimit)
                     dist
                     (* (- max-pressure pressure) (- timelimit time)))
                  (first %) (second %)])))))

(defn valve-tsp [valves timelimit]
  (first (->> (valve-tsp-result-sets valves timelimit)
              (reduce #(max-key first %1 %2)))))

(defn task-1 [input]
  (let [valves (parse input)
        timelimit 30]
    (valve-tsp valves timelimit)))

(defn task-1b [input]
  (let [valves (parse input)
        timelimit 30]
    (depth-first-flow valves (network valves) "AA" timelimit nil)))

(defn task-1c [input]
  (let [valves (parse input)
        timelimit 30
        net (network valves)
        flow (reduce #(assoc %1 (:valve %2) (:flow %2)) {} (vals valves))]
    (->> (depth-first-path net "AA" timelimit nil)
         (map #(calc-total-pressure % net flow timelimit))
         (reduce max)
         )))

(defn task-2 [input]
  (let [valves (parse input)
        timelimit 26
        net (network valves)
        flow (reduce #(assoc %1 (:valve %2) (:flow %2)) {} (vals valves))
        calc-elephant (memoize (fn [path]
                                 (reduce (fn [cur-best2 path2]
                                           (max cur-best2
                                                (calc-total-pressure path2 net flow timelimit)))
                                         0 (depth-first-path net "AA" timelimit (set path)))))]
    (reduce (fn [cur-best [i path]]
              (when (= 0 (mod i 100)) (println i))
              (max cur-best
                   (+ (calc-total-pressure path net flow timelimit)
                      (calc-elephant path))))
            0 (map-indexed vector (depth-first-path net "AA" timelimit nil)))))

(defn task-2b [input]
  (let [valves (parse input)
        timelimit 26
        net (network valves)
        targets (remove #{"AA"} (keys net))
        tsm (fn [sub-targets]
              (let [exclude (->> (remove sub-targets targets)
                                 (into #{}))]
                (depth-first-flow valves net "AA" timelimit exclude)))
        subsets (->> (mapcat #(combine % targets) (range 1 (count targets)))
                     (map #(conj % "AA"))
                     (map (juxt identity tsm))
                     (into {}))]
    (->> (map-indexed (fn [idx [sub-targets pressure]]
                        (when (= 0 (mod idx 100)) (println idx))
                        (let [othertargets (conj (->> (remove sub-targets targets)
                                                      (into #{}))
                                                 "AA")]
                          (+ pressure (subsets othertargets))))
              subsets)
         (reduce max)
         )))

(defn task-2c [input]
  (let [valves (parse input)
        net (network valves)
        targets (remove #{"AA"} (keys net))
        subsets (->> (valve-tsp-result-sets valves 26)
                     (map #(vector  (first (second %))
                                    (first %)
                                    (second (second %))))
                     (group-by first)
                     vals
                     (map #(apply max-key second %))
                     (sort-by second >))
        non-overlapping (fn [s-a s-b] (empty? (set/intersection s-a s-b)))]
    (->> (for [[[my-targets my-pressure _my-last] & others] (take-while (comp not empty?)
                                                                        (iterate rest subsets))
               :let [[_e-targets e-pressure _e-last] (->> others
                                                          (filter #(non-overlapping my-targets
                                                                                    (first %)))
                                                          first)]
               :when e-pressure]
           (+ my-pressure e-pressure))
         (reduce max))))

(let [x [:a :b :c :d :e :f :g]]
  (for [[a & as] (take-while (comp not empty?) (iterate rest x))
         b as
        :while a]
    [a b]))

(task-1c testdata)
(task-1c data)

(task-2c testdata)
(task-2c data)
