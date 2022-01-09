(ns advent2021.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def testdata0 (string/split-lines "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10"))

(def testdata (string/split-lines "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682"))

(defn tolong [str] (Long/parseLong str))

(defn parse-line [l]
  (let [[_all cmd & vals]
        (re-matches #"(on|off) x=(-?[0-9]+)..(-?[0-9]+),y=(-?[0-9]+)..(-?[0-9]+),z=(-?[0-9]+)..(-?[0-9]+)" l)
        [x1 x2 y1 y2 z1 z2] (map tolong vals)]
    (when (or (< x2 x1) (< y2 y1) (< z2 z1)) (throw (RuntimeException. "assumption about x1<x2 failed!")))
    {:cmd (case cmd
            "on" :on
            "off" :off
            "filter" :filter)
     :x [x1 (inc x2)]
     :y [y1 (inc y2)]
     :z [z1 (inc z2)]}))

(defn parse [input]
  (map parse-line input))

(defn clamp [x-min x x-max]
  (cond (< x x-min) x-min
        (> x x-max) x-max
        :else x))

(defn size
  ([area clamp-min clamp-max]
   (let [clamp-max (inc clamp-max)
         {:keys [x y z]} area
         dim (fn [[a b]] (- (clamp clamp-min b clamp-max)
                            (clamp clamp-min a clamp-max)))]
     (reduce * (map dim [x y z]))))

  ([area]
   (let [{:keys [x y z]} area
         dim (fn [[a b]] (- b a))]
     (reduce * (map dim [x y z])))))

;(size (first (parse ["on x=-1..1,y=2..3,z=4..5"]))) ; 12
;(size (first (parse ["on x=-1..1,y=2..3,z=101..102"])) -50 50) ; 0
(size (parse-line "on x=-1..1,y=2..3,z=49..50")) ; 12
(size (parse-line "on x=-1..1,y=2..3,z=49..50") -50 50) ; 12
(size (parse-line "on x=-1..1,y=2..3,z=49..53") -50 50) ; 12
(size (parse-line "on x=-1..1,y=2..3,z=59..63") -50 50) ; 0
(size (parse-line "on x=0..1,y=0..1,z=0..1")) ; 8

(defn intersect [foo bar]
  ; true if foo and bar overlap
  (let [intersect-dim (fn [[a1 a2] [b1 b2]]
                        (or (and (<= a1 b1) (< b1 a2)) ; a1 b1 a2 b2 | a1 b1 b2 a2
                            (and (<= b1 a1) (< a1 b2)) ; b1 a1 b2 a2 | b1 a1 a2 b2
                            ))]
    (every? #(intersect-dim (% foo) (% bar)) '(:x :y :z))))


(comment
  (apply intersect (parse ["on x=-1..1,y=2..3,z=4..5" "off x=0..1,y=2..3,z=4..5"])) ; true
  (apply intersect (parse ["on x=-1..2,y=2..3,z=4..5" "off x=0..1,y=2..3,z=4..5"])) ; true
  (apply intersect (parse ["on x=-1..2,y=2..3,z=4..5" "off x=-2..0,y=2..3,z=4..5"])); true
  (apply intersect (parse ["on x=-1..1,y=2..3,z=4..5" "off x=2..3,y=2..3,z=4..5"])) ; false
  (apply intersect (parse ["on x=0..1,y=0..1,z=0..1" "off x=2..3,y=2..3,z=2..3"])) ; false
  (apply intersect (parse ["on x=0..1,y=0..1,z=0..1" "off x=2..3,y=0..1,z=0..1"])) ; false
  (apply intersect (parse ["on x=0..1,y=0..1,z=0..1" "off x=1..2,y=1..2,z=1..2"])) ; true
  )

(defn contains [foo bar]
  ; true if bar is completely inside foo
  (let [contained-dim (fn [[a1 a2] [b1 b2]]
                        (<= a1 b1 b2 a2))]
    (every? #(contained-dim (% foo) (% bar)) '(:x :y :z))))

(comment
  (apply contains (parse ["on x=-1..1,y=2..3,z=4..5" "off x=2..3,y=2..3,z=4..5"])) ; false
  (apply contains (parse ["on x=-1..1,y=0..3,z=0..5" "off x=-1..1,y=2..3,z=4..5"])); true
  (apply contains (parse ["on x=-1..1,y=0..3,z=0..5" "off x=0..2,y=2..3,z=4..5"])) ; false
  (apply contains (parse ["on x=-1..1,y=0..3,z=0..5" "on x=-1..1,y=0..3,z=0..5"])); true
  )

(defn maybe-merge [a1 a2]
  (cond (and (= (:cmd a1) (:cmd a2))
             (= (:x a1) (:x a2))
             (= (:y a1) (:y a2))
             (= (:z a1) (:z a2)))
        a1

        (and (= (:cmd a1) (:cmd a2))
             (= (:x a1) (:x a2))
             (= (:y a1) (:y a2))
             (= (second (:z a1)) (first (:z a2))))
        (assoc a1 :z [(first (:z a1)) (second (:z a2))])

        (and (= (:cmd a1) (:cmd a2))
             (= (:x a1) (:x a2))
             (= (:z a1) (:z a2))
             (= (second (:y a1)) (first (:y a2))))
        (assoc a1 :y [(first (:y a1)) (second (:y a2))])

        (and (= (:cmd a1) (:cmd a2))
             (= (:y a1) (:y a2))
             (= (:z a1) (:z a2))
             (= (second (:x a1)) (first (:x a2))))
        (assoc a1 :x [(first (:x a1)) (second (:x a2))])

        :else nil))

(comment
  (maybe-merge {:cmd :on, :x '(-20 26), :y '(-36 -21), :z '(-47 -26)}
               {:cmd :on, :x [-20 26], :y '(-36 -21), :z '(-26 7)})

    (maybe-merge {:cmd :on, :x '(-20 26), :y '(-36 -21), :z '(-47 -26)}
                 {:cmd :on, :x [-20 26], :y '(-36 -21), :z '(-47 -26)})

  (intersect {:cmd :on, :x '(-20 26), :y '(-36 -21), :z '(-47 -26)}
               {:cmd :on, :x [-20 26], :y '(-36 -21), :z '(-47 -26)})
  
  (maybe-merge (parse-line "on x=1..2,y=1..2,z=1..2")
               (parse-line "on x=3..4,y=1..2,z=1..2"))
  )

(defn merge-list [l]
  (letfn [(merge-into-tail [res cmd]
            (if (empty? res)
              [cmd]
              (if-let [new (maybe-merge (peek res) cmd)]
                (recur (pop res) new)
                (conj res cmd))))]
    (reduce merge-into-tail [] l)))

(defn subdivide
  "returns two lists: areas representing the total areas covered by area1 with area2 added or removed, and parts of area2 left after mixing with area1"
  [area1 area2]
  (let [[x y z] (map #(->> (mapcat % [area1 area2])
                           sort
                           dedupe) [:x :y :z])
        all-subregions (for [xrange (partition 2 1 x)
                             yrange (partition 2 1 y)
                             zrange (partition 2 1 z)]
                         {:cmd :on
                          :x xrange
                          :y yrange
                          :z zrange})



        turn-off (fn [cmd] (assoc cmd :cmd :off))]
    (-> (case (:cmd area2)
          :on  (group-by #(cond (contains area1 %) :a1
                                (contains area2 %) :a2
                                :else nil)
                         all-subregions)
          :off (-> (group-by #(cond (and (contains area1 %)
                                         (not (contains area2 %))) :a1
                                    (and (not (contains area1 %))
                                         (contains area2 %)) :a2
                                    :else nil)
                             all-subregions)
                   (update :a2 #(map turn-off %))))
        (dissoc nil)
        (as-> a
              (map merge-list (map #(% a) [:a1 :a2])))
        ;(as-> a (map #(% a) [:a1 :a2]))
        )))

(comment
  (apply subdivide (parse ["on x=2..4,y=1..3,z=0..2" "on x=1..3,y=2..3,z=0..1"]))
  (apply subdivide (parse ["on x=2..4,y=1..3,z=0..1" "off x=5..6,y=7..8,z=0..1"]))
  (apply subdivide (parse ["on x=0..2,y=0..2,z=0..2" "off x=1..3,y=1..3,z=0..2"]))
  (->> (subdivide (parse-line "on x=0..2,y=0..2,z=0..2")
                  (parse-line "off x=2..4,y=-1..3,z=0..2"))
       (map #(map size %))); ((18) (3 3 30))
  (->> (apply subdivide (parse ["on x=0..1,y=0..1,z=0..1" "off x=2..3,y=2..3,z=0..2"]))
       (map #(map size %))) ; (8 12)
  (->> (apply subdivide (parse ["on x=0..2,y=0..2,z=0..0" "off x=1..3,y=1..3,z=0..0"]))
       (first)
       (map size)
       ) ; (3 2)
  )

(defn boot [commandlist]
  (let [myrange (fn [[from to]]
                  ;(range (if (< from -50) -50 from)
                  ;       (if (> to 51) 51 to))
                  (range (clamp -50 from 51) (clamp -50 to 51))
                  )
        coord (fn [{:keys [x y z]}]
                (for [x- (myrange x)
                      y- (myrange y)
                      z- (myrange z)]
                  (+ (* x- 128 128) (* y- 128) z-)))]
    (reduce (fn [state cmd]
              (case (:cmd cmd)
                :on (reduce conj state (coord cmd))
                :off (reduce disj state (coord cmd))))
            #{}
            commandlist)
    ;(coord {:x [1 2] :y [3 4] :z [5 6]})
    ))

(def ^:dynamic *do-merge* true)

(defn exec [state cmd]
  (loop [[s & state] (remove (partial contains cmd) state)
         cmdlist [cmd]
         res []]
    ;(println s cmdlist res)
    (if (nil? s)
      (cond->> cmdlist
        true (filter #(= (:cmd %) :on))
        true (concat res)
        true doall
        *do-merge* (merge-list)
        true doall)
      (let [[rest-state rest-cmd] (reduce (fn [[previous-states previous-cmd] cmd] ; for hver kommando i cmdlist
                                            (if (intersect s cmd) ; har den overlapp med s?, MEN HER SKAL VI BRUKE previous-states
                                              (let [rest-states-and-cmd (map #(if (intersects % cmd)
                                                                                (subdivide % cmd)
                                                                                [nil [cmd]]) previous-states)]
                                                [(concat previous-states (map first rest-states-and-cmd))
                                                 (concat previous-cmd (map second rest-states-and-cmd))]) ; del opp s+cmd, legg inn i state og rest-cmd
                                              [previous-states (conj previous-cmd cmd)])) ; hvis ikke, send cmd videre, s er ikke delt opp
                                          [[s] []] cmdlist)]
        (if (not= (reduce + (map size rest-state))
                  (reduce + (map size (merge-list rest-state))))
          (println "inconsistent data from applying" cmdlist))
        (recur state ; ferdig med s, videre med neste state
               rest-cmd ; cmd som ikke er brukt opp over
               (if (empty? rest-state) ; hvis denne er tom, betyr det at ingen cmd har truffet noen state
                 (conj res s)
                 (concat res rest-state)))))))

(defn inside? [c x y z]
  (let [[x1 x2] (:x c)
        [y1 y2] (:y c)
        [z1 z2] (:z c)]
    (and (<= x1 x) (< x x2)
         (<= y1 y) (< y y2)
         (<= z1 z) (< z z2))))

(defn task-1 [data]
  (count (boot (parse data))))

(defn task-1b [data]
  (->> (reduce exec [] (parse data))
       (map #(size % -50 50))
       (reduce +)))

(defn task-2 [data]
  (->> (reduce exec [] (parse data))
       (map size)
       (reduce +)))


(def data-boot (string/split-lines "on x=-16..31,y=1..46,z=-4..43
on x=-1..48,y=-8..36,z=-12..41
on x=-25..20,y=-25..28,z=-48..-3
on x=-32..21,y=-9..45,z=-26..18
on x=-13..37,y=-36..8,z=-24..21
on x=-14..38,y=-46..0,z=-33..15
on x=-20..29,y=-45..5,z=-42..11
on x=-36..14,y=-35..14,z=-24..28
on x=-43..11,y=-48..6,z=-12..39
on x=-3..43,y=-38..14,z=-10..36
off x=-13..1,y=9..26,z=18..33
on x=-5..43,y=-2..49,z=-41..9
off x=-15..-1,y=-46..-32,z=-43..-29
on x=-3..46,y=-10..34,z=-35..19
off x=-44..-25,y=10..27,z=-46..-29
on x=-33..20,y=-35..14,z=-32..18
off x=-10..4,y=-33..-21,z=16..35
on x=-6..44,y=-11..39,z=-9..43
off x=-40..-30,y=-35..-25,z=-42..-27
on x=-45..7,y=-12..41,z=-19..35"))

(for [[a & a-tail] (iterate rest [1 2 3])
      :while (seq a-tail)
      b a-tail]
  (+ a b))

(let [subdata testdata]
  (let [sort-key #(+ (* 2500 (first (:x %)))
                     (* 50 (first (:y %)))
                     (first (:z %)))
        res (doall (reduce exec [] (parse subdata)))
        culprit {:cmd :on, :x [-3 47], :y [-10 35], :z [-35 20]}
        res2 (binding [*do-merge* false] (doall (exec res culprit)))
        ;res2-merged (merge-list res2)
        ]
    (for [[a & a-tail] (iterate rest res2)
          :while (seq a-tail)
          b a-tail
          :when (intersect a b)]
      [a b (size a)])
    ;(map size res2-merged)
    ;[(reduce + (map size res2))
    ; (reduce + (map size res2-merged))]

    ;(comment loop [r (vec res2)
    ;       s nil]
    ;  (if (not= (reduce + (map size r))
    ;            (reduce + (map size (merge-list r))))
    ;    (recur (pop r) (peek r))
    ;    [r s]))

    ;[(sort-by sort-key res) (sort-by sort-key res2)]
    ;(filter (partial intersect culprit) res)
    )
  ;(binding [*do-merge* false]
  ;  [(task-1 subdata) (task-1b subdata)]) ; 500318 515060
  )

(let [a (reduce exec [] (parse (string/split-lines "on x=-25..20,y=-25..28,z=-48..-3
on x=-14..38,y=-46..0,z=-33..15
off x=-15..-1,y=-46..-32,z=-43..-29")))
      b (parse-line "on x=-3..46,y=-10..34,z=-35..19")
      find-overlap (fn [areas]
                     (for [[a & a-tail] (iterate rest areas)
                           :while (seq a-tail)
                           b a-tail
                           :when (intersect a b)]
                       a))
      ]
  (reduce (fn [[previous-states cmdlist] state]
            (if (empty? cmdlist)
              (reduced [previous-states cmdlist])
              (let [all-results (map (fn [cmd] (let [res (if (intersect state cmd)
                                                           (subdivide state cmd)
                                                           [nil [%]])]
                                                 (println "subdivide" state "by" cmd "gives" res)
                                                 res))
                                     cmdlist)
                    newstates (remove nil? (mapcat first all-results))
                    newstates (if (empty? newstates) [state] newstates)]
                [(concat previous-states newstates)
                 (mapcat second all-results)])))
          [[][b]]
          a)

  )

(flatten (concat [:a] (map first [[[1] [2]] [[3] [4]]])))

(size {:cmd :on, :x [21 39], :y [-38 1], :z [16 37]})

(task-1b testdata)
(task-1b subdata)
(task-1 (line-seq (io/reader "resources/day22-test2.txt")))
(task-1b (line-seq (io/reader "resources/day22-test2.txt")))
(task-2 (line-seq (io/reader "resources/day22-test2.txt")))
(task-2 (take 20 (line-seq (io/reader "resources/day22.txt"))))

(defn volume-list [[head & others]]
  (if (nil? head)
    0
    (let [[cmd coords] head
          intersection (fn [o]
                         (let [[fx1 fx2 fy1 fy2 fz1 fz2] coords
                               [ox1 ox2 oy1 oy2 oz1 oz2] (second o)
                               ix1 (max fx1 ox1) ix2 (min fx2 ox2)
                               iy1 (max fy1 oy1) iy2 (min fy2 oy2)
                               iz1 (max fz1 oz1) iz2 (min fz2 oz2)]
                           (when (and (<= ix1 ix2) (<= iy1 iy2) (<= iz1 iz2))
                             ["on" [ix1 ix2 iy1 iy2 iz1 iz2]])))
          volume (fn [[x1 x2 y1 y2 z1 z2]]
                   (* (inc (- x2 x1)) (inc (- y2 y1)) (inc (- z2 z1))))]
      ;(prn (map (juxt identity volume) (map intersection others)))
      ;(prn coords (volume coords))
      (if (= cmd "off")
        (volume-list others)
        (+ (volume coords)
           (volume-list others)
           (- (volume-list (remove nil? (map intersection others))))))))
  )


(defn task-2 [input]
  (letfn [(tolong [str] (Long/parseLong str))
          (parse-line [line]
            (let [[cmd & coords] (re-seq #"on|off|-?[0-9]+" line)]
              [cmd (map tolong coords)]))]
    (volume-list (map parse-line input))))

(map (juxt identity size) (parse (take 3 testdata)))

(task-2 (take 3 testdata))
(task-2 (line-seq (io/reader "resources/day22-test2.txt"))) ;; => 2758514936282235
(task-2 (line-seq (io/reader "resources/day22.txt"))) ;; => 1235484513229032
