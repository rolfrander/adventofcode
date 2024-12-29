(ns advent2019.day20
  (:require
   [clojure.string :as str]
   [rolfrander.puzzle-lib :as puzzle]
   [flatland.useful.map :refer [map-vals]]))

(def testdata1 "         A           
         A           
  #######.#########  
  #######.........#  
  #######.#######.#  
  #######.#######.#  
  #######.#######.#  
  #####  B    ###.#  
BC...##  C    ###.#  
  ##.##       ###.#  
  ##...DE  F  ###.#  
  #####    G  ###.#  
  #########.#####.#  
DE..#######...###.#  
  #.#########.###.#  
FG..#########.....#  
  ###########.#####  
             Z       
             Z       ")

(def testdata2 "                   A               
                   A               
  #################.#############  
  #.#...#...................#.#.#  
  #.#.#.###.###.###.#########.#.#  
  #.#.#.......#...#.....#.#.#...#  
  #.#########.###.#####.#.#.###.#  
  #.............#.#.....#.......#  
  ###.###########.###.#####.#.#.#  
  #.....#        A   C    #.#.#.#  
  #######        S   P    #####.#  
  #.#...#                 #......VT
  #.#.#.#                 #.#####  
  #...#.#               YN....#.#  
  #.###.#                 #####.#  
DI....#.#                 #.....#  
  #####.#                 #.###.#  
ZZ......#               QG....#..AS
  ###.###                 #######  
JO..#.#.#                 #.....#  
  #.#.#.#                 ###.#.#  
  #...#..DI             BU....#..LF
  #####.#                 #.#####  
YN......#               VT..#....QG
  #.###.#                 #.###.#  
  #.#...#                 #.....#  
  ###.###    J L     J    #.#.###  
  #.....#    O F     P    #.#...#  
  #.###.#####.#.#####.#####.###.#  
  #...#.#.#...#.....#.....#.#...#  
  #.#####.###.###.#.#.#########.#  
  #...#.#.....#...#.#.#.#.....#.#  
  #.###.#####.###.###.#.#.#######  
  #.#.........#...#.............#  
  #########.###.###.#############  
           B   J   C               
           U   P   P               ")

(def testdata3 "             Z L X W       C                 
             Z P Q B       K                 
  ###########.#.#.#.#######.###############  
  #...#.......#.#.......#.#.......#.#.#...#  
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  
  #.#...#.#.#...#.#.#...#...#...#.#.......#  
  #.###.#######.###.###.#.###.###.#.#######  
  #...#.......#.#...#...#.............#...#  
  #.#########.#######.#.#######.#######.###  
  #...#.#    F       R I       Z    #.#.#.#  
  #.###.#    D       E C       H    #.#.#.#  
  #.#...#                           #...#.#  
  #.###.#                           #.###.#  
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#  
CJ......#                           #.....#  
  #######                           #######  
  #.#....CK                         #......IC
  #.###.#                           #.###.#  
  #.....#                           #...#.#  
  ###.###                           #.#.#.#  
XF....#.#                         RF..#.#.#  
  #####.#                           #######  
  #......CJ                       NM..#...#  
  ###.#.#                           #.###.#  
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#  
  #.....#        F   Q       P      #.#.#.#  
  ###.###########.###.#######.#########.###  
  #.....#...#.....#.......#...#.....#.#...#  
  #####.#.###.#######.#######.###.###.#.#.#  
  #.......#.......#.#.#.#.#...#...#...#.#.#  
  #####.###.#####.#.#.#.#.###.###.#.###.###  
  #.......#.....#.#...#...............#...#  
  #############.#.#.###.###################  
               A O F   N                     
               A A D   M                     ")

(defn parse [in]
  (let [m (puzzle/parse-map in :type :by-coord)
        marks (:markings m)
        letter (->> (:markings m)
                    (vals)
                    (into #{})
                    (remove #{\space \. \#})
                    (into #{}))
        portals (->> (for [x (range (:width m))
                           y (range (:height m))
                           :when (and (letter (marks [x y]))
                                      (or (letter (marks [(inc x) y]))
                                          (letter (marks [x (inc y)]))))
                           :let [pos [x y]
                                 other-letter-pos (if (letter (marks [(inc x) y]))
                                                    [(inc x) y]
                                                    [x (inc y)])
                                 code (str (marks pos) (marks other-letter-pos))
                                 tile (let [diff (map - other-letter-pos pos)
                                            ext-1 (map - pos diff)
                                            ext-2 (map + other-letter-pos diff)]
                                        (if (= \. (marks ext-1))
                                          ext-1
                                          ext-2))]]
                       [tile code])
                     (group-by second))
        portals (map-vals portals #(map first %))
        start (first (portals "AA"))
        end (first (portals "ZZ"))
        portals (-> portals
                    (dissoc "AA")
                    (dissoc "ZZ")
                    vals)
        portals (reduce #(-> %1
                             (assoc (first %2) (second %2))
                             (assoc (second %2) (first %2)))
                        {} portals)
        map-by-type (puzzle/convert-map m :type :by-type)]
    {:startpos start
     :endpos end
     :portals portals
     :width (:width map-by-type)
     :height (:height map-by-type)
     :path (into #{} (get-in map-by-type [:markings \.]))}))

(parse testdata1)

(defn convert-for-part-2 [m]
  (let [portals (:portals m)
        outer-ring-x #{2 (- (:width m) 3)}
        outer-ring-y #{2 (- (:height m) 3)}
        portal-up (fn [[x y]] (boolean (or (outer-ring-x x)
                                           (outer-ring-y y))))
        
        grouped-portals (group-by portal-up (keys portals))]
    (assoc m 
           :portals-up (into #{} (grouped-portals true))
           :portals-down (into #{} (grouped-portals false)))))

(defn conj-if [coll x]
  (if x (conj coll x) coll))

(defn solve-1 [in]
  (let [m (parse in)
        {:keys [path portals startpos endpos]} m
        close-n (puzzle/neighbours-fn :sq-4 :infinite)
        n (fn [pos]
            (let [neighbours (filter path (close-n pos))
                  neighbours (conj-if neighbours (portals pos))]
              neighbours))
        dest? (fn [pos] (= pos endpos))]
    (puzzle/dijkstra path startpos n
                     :result-type :dist :dest? dest?)))

(defn solve-2 [in]
  (let [m (convert-for-part-2 (parse in))
        {:keys [startpos endpos portals portals-up portals-down]} m
        start {:pos startpos :level 0}
        end   {:pos endpos   :level 0}
        dest? (fn [{:keys [pos level]}] (and (= pos endpos)
                                             (= level 0)))
        distance (fn [m start]
                   (let [{:keys [path]} m
                         close-n (puzzle/neighbours-fn :sq-4 :infinite)
                         n (fn [pos]
                             (filter path (close-n pos)))]
                     (puzzle/dijkstra path start n
                                      :result-type :dist)))
        single-level-dist
        (->> (mapcat (fn [a] (let [all-dist (filter (fn [[_ dist]] (< dist 10000)) (distance m a))]
                               (map (fn [[b dist]]
                                      [a b dist])
                                    all-dist)))
                     (conj (keys portals) startpos endpos))
             (reduce (fn [res [a b dist]]
                       (if (> dist 1000)
                         res
                         (cond-> res
                           (not (or (= a endpos) (= b startpos))) (assoc-in [a b] dist)
                           (not (or (= a startpos) (= b endpos))) (assoc-in [b a] dist))))
                     {}))

        neighbours (fn [{:keys [pos level]}]
                     (cond-> (map #(do {:pos % :level level}) (keys (single-level-dist pos)))
                       (portals-up pos)   (conj {:pos (portals pos) :level (dec level)})
                       (portals-down pos) (conj {:pos (portals pos) :level (inc level)})))

        levels (inc (count (keys portals)))

        nodes (-> (for [p (keys portals)
                        l (range levels)]
                    {:pos p :level l})
                  (conj start)
                  (conj end))]
    (puzzle/dijkstra nodes start neighbours
                     :weight-fn (fn [{pos-u :pos level-u :level}
                                     {pos-v :pos level-v :level}]
                                  (if (= level-u level-v)
                                    (get-in single-level-dist [pos-u pos-v])
                                    1))
                     :result-type :dist
                     :dest? dest?)))


(solve-1 testdata1)
;;=> 23
(solve-1 testdata2)
;;=> 58
(def data (puzzle/get-data 2019 20))
(solve-1 data)
;;=> 620
(solve-2 testdata3)
;; 396
(solve-2 data)
;;=> 7366

(let [m (convert-for-part-2 (parse data))
      {:keys [startpos endpos portals]} m
      distance (fn [m start]
                 (let [{:keys [path]} m
                       close-n (puzzle/neighbours-fn :sq-4 :infinite)
                       n (fn [pos]
                           (filter path (close-n pos)))]
                   (puzzle/dijkstra path start n
                                    :result-type :dist)))]
  (mapcat (fn [a] (let [all-dist (filter (fn [[_ dist]] (< dist 10000)) (distance m a))]
                   (map (fn [[b dist]]
                          [a b dist])
                        all-dist)))
          (conj (keys portals) startpos endpos))

  )