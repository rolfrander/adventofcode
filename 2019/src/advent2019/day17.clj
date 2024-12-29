(ns advent2019.day17
  (:require
   [clojure.string :as str]
   [advent2019.intcode :as ic]
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.core.async :refer [>!! <!! chan thread pipe close!]]
   [flatland.useful.seq :refer [prefix-of?]]))

(def program (ic/parse (puzzle/get-data 2019 17)))


(def celltype {(int \#) :scaffold
               (int \.) :space
               (int \^) :startpos
               (int \<) :startpos
               (int \v) :startpos
               (int \>) :startpos
               10 :newline})

; turning is mirrored, our grid goes down, move-fn assumes up
(def start-dir {(int \^) "s"
                (int \<) "w"
                (int \v) "n"
                (int \>) "e"})

(def turn-left (zipmap ["w" "n" "e" "s"]
                       ["n" "e" "s" "w"]))

(def turn-right (zipmap ["n" "e" "s" "w"]
                        ["w" "n" "e" "s"]))

(def testdata "#######...#####
#.....#...#...#
#.....#...#...#
......#...#...#
......#...###.#
......#.....#.#
^########...#.#
......#.#...#.#
......#########
........#...#..
....#########..
....#...#......
....#...#......
....#...#......
....#####......")

(def test-map (let [d (:markings (puzzle/parse-map testdata))]
                {:scaffold (into (d \#) (d \^))
                 :startpos (first (d \^))
                 :start-dir (start-dir (int \^))}))

(defn read-output-from-program-thread []
  (let [{:keys [_input output _done]} (ic/robot program)]
    (loop [c (<!! output)
           x 0
           y 0
           chart {}]
      (if (nil? c)
        chart
        (let [cell (celltype c)]
          ;(print (char c))
          (cond (= cell :newline)
                (recur (<!! output) 0 (inc y) chart)

                (= cell :startpos)
                (recur (<!! output) (inc x) y (-> chart
                                                  (update :scaffold conj [x y])
                                                  (assoc :startpos [x y])
                                                  (assoc :start-dir (start-dir c))))

                :else
                (recur (<!! output) (inc x) y (update chart cell conj [x y]))))))))

(defn read-output-from-program []
  (let [m (->> (ic/interpret-with-input program [])
               (map char)
               str/join
               puzzle/parse-map
               :markings)]
    {:scaffold (into (m \#) (m \^))
     :startpos (first (m \^))
     :start-dir (start-dir (int \^))}))

(def move (puzzle/move-fn :sq-4 :infinite))

(defn full-movement-plan [{:keys [scaffold startpos start-dir]}]
  (let [scaffold (into #{} scaffold)]
    (loop [pos startpos
           dir start-dir
           movements []]
      (let [left (turn-left dir)
            right (turn-right dir)]
        (cond (scaffold (move pos dir))   (recur (move pos dir)   dir   (conj movements :forward))
              (scaffold (move pos left))  (recur (move pos left)  left  (conj movements :left :forward))
              (scaffold (move pos right)) (recur (move pos right) right (conj movements :right :forward))
              :else movements))
      )))

(defn reduce-plan [plan]
  (let [[plan final-moves] (reduce (fn [[p f-cnt] next]
                                     (if (= next :forward)
                                       [p (inc f-cnt)]
                                       [(cond-> p
                                          (> f-cnt 0) (conj f-cnt)
                                          true (conj next))
                                        0]))
                                   [[] 0] plan)]
    (if (> final-moves 0)
      (conj plan final-moves)
      plan)))

(defn est-chars [p]
  (reduce #(+ %1 (if (int? %2)
                   (if (< %2 10) 1 2)
                   1)
              1 ; this is the comma
              )
          -1 p) ; start with -1 to remove last comma
  )

(defn possible-movement-functions [plan]
  (->> (map #(let [sp (take % plan)]
               [(est-chars sp) sp])
            (range 1 (min (inc (count plan)) 12)))
       (filter #(<= (first %) 20))
       (sort-by first >)
       (map second)
       ))

(defn compress [rest-plan mf-vec]
  (if (empty? rest-plan)
    (list {:main '()
           :movement-fn mf-vec})

    (let [possible-prefix-nos (filter #(prefix-of? rest-plan (mf-vec %))
                                      (range (count mf-vec)))
          test-prefix (fn [prefix-no]
                        ; for each possible prefix number, return a list of compressed plans where this prefix is the next
                        (let [prefix (mf-vec prefix-no)
                              plans (compress (drop (count prefix) rest-plan)
                                              mf-vec)]
                          ;(println "plans from recursive call to compress" plans)
                          (map #(update % :main conj prefix-no) plans)))

          possible-plans (mapcat test-prefix possible-prefix-nos)]
      (->> (if (= 3 (count mf-vec))
             possible-plans
             (let [next-mf-list (possible-movement-functions rest-plan)]
               (concat possible-plans
                       (->> (mapcat #(compress rest-plan (conj mf-vec %))
                                    next-mf-list)
                            (remove nil?)))))
           (remove #(>= (est-chars (:main %)) 20))
           doall))))

(defn compile-plan [compressed-plan]
  (let [compile-single (fn [l]
                         (->> (map #(case %
                                      :left \L
                                      :right \R
                                      %) l)
                              (str/join ",")))
        comp {:main (compile-single (map [\A \B \C] (:main compressed-plan)))
              :movement-fn (mapv compile-single (:movement-fn compressed-plan))}]
    (assoc comp :total-length (apply + (count (:main comp)) (map count (:movement-fn comp)))))
  )

(defn find-optimized-plan [chart]
  (let [plans (-> (reduce-plan (full-movement-plan chart))
                  (compress []))]
    (apply min-key :total-length (map compile-plan plans))))

(defn encode-plan [plan with-video-feed]
  (let [encoded-plan (->> (str/join (char 10) (cons (:main plan) (seq (:movement-fn plan))))
                          (map int)
                          vec)]
    (conj encoded-plan 10 (if with-video-feed (int \y) (int \n)) 10)))

(defn solve-1 []
  (let [m (read-output-from-program)
        scaffold (into #{} (:scaffold m))
        n (puzzle/neighbours-fn :sq-4 :infinite)
        count-if (fn [pred coll] (count (filter pred coll)))]
    (->> (filter #(= 4 (count-if scaffold (n %)))
                 scaffold)
         (map (partial apply *))
         (apply +))))

(defn solve-2 []
  (let [plan (-> (read-output-from-program)
                 find-optimized-plan
                 (encode-plan false))
        program (assoc program 0 2)
        [video score] (split-with #(< % 128) (ic/interpret-with-input program plan))]
  ;(-> (map char video) str/join println)
    score
    ))

(solve-2)
