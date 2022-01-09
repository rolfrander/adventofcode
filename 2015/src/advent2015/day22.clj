(ns advent2015.day22
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def ^:dynamic *debug* false)

(def instant-spells {"Magic Missile" {:damage 4 :heal 0 :cost 53}
                     "Drain"         {:damage 2 :heal 2 :cost 73}})

(def duration-spells {"Shield"        {:effect :armor  :amount   7 :duration 6 :cost 113}
                      "Poison"        {:effect :damage :amount   3 :duration 6 :cost 173}
                      "Recharge"      {:effect :mana   :amount 101 :duration 5 :cost 229}})

(concat instant-spells duration-spells)

(def all-spells (into {} (map #(vector (first %) (:cost (second %)))
                              (concat instant-spells duration-spells))))

(defn apply-spell [player spell-name cost]
  (-> player
      (update :mana - cost)
      (update :spent + cost)
      (assoc :last-spell spell-name)
      ))

(defn cast-spells [player spell-name]
  (let [s (duration-spells spell-name)
        cost (:cost s)]
    (if (and s (<= cost (:mana player)))
      (-> player
          (apply-spell spell-name cost)
          (update :recurring-spells conj [spell-name (:duration s) (:effect s) (:amount s)]))
      (assoc player :last-spell spell-name))))

(defn apply-instant-spell [player boss spell-name]
  (let [s (instant-spells spell-name)
        cost (:cost s)]
    (if (and s (<= cost (:mana player)))
      [(-> player
           (apply-spell spell-name cost)
           (update :hit-points + (:heal s)))
       (-> boss
           (update :hit-points - (:damage s)))]
      [(assoc player :last-spell spell-name) boss])))

(defn hard-addon [player]
  (update player :hit-points dec))

(defn update-timers [player]
  (let [new-mana (reduce (fn [sum [_name _duration effect amount]]
                           (if (= effect :mana)
                             (+ sum amount)
                             sum))
                         0 (:recurring-spells player))
        upd (fn [timer-list] (->> timer-list
                                  (map #(update % 1 dec))
                                  (remove #(<= (second %) 0))))]
    (-> player
        (update :mana + new-mana)
        (update :recurring-spells upd))))

(defn effective [attribute player]
  (reduce (fn [sum [_name _duration effect amount]]
            (if (= attribute effect)
              (+ sum amount)
              sum))
          (attribute player)
          (:recurring-spells player)))

(defn play-turn [[player boss] player-spell]
  (when *debug*
    (println "-- Player turn --")
    (println "- Player has" (:hit-points player) "hit points," (effective :armor player) "armor," (:mana player) "mana")
    (println "- Boss has" (:hit-points boss) "hit points"))
  (let [;; first turn
        damage (effective :damage player)
        player (update-timers player)
        player (hard-addon player)]
    (when *debug*
      (doseq [[name duration effect amount] (:recurring-spells player)]
        (println name "provides" amount effect "its timer is now" duration))
      (when player-spell
        (println "Player casts" player-spell))
      (newline))

    (if (<= (:hit-points player) 0)
      [player boss]
      (let [player (cast-spells player player-spell)
            [player boss] (apply-instant-spell player boss player-spell)
            boss (update boss :hit-points - damage)]

      ;; second turn, player might have won, calling function must check boss hit-points first
        (when *debug*
          (println "-- Boss turn --")
          (println "- Player has" (:hit-points player) "hit points," (effective :armor player) "armor," (:mana player) "mana")
          (println "- Boss has" (:hit-points boss) "hit points"))

        (let [armor (effective :armor player)
              damage (effective :damage player)
              player (update-timers player)
              player (hard-addon player)]
          (when *debug*
            (doseq [[name duration effect amount] (:recurring-spells player)]
              (println name "provides" amount effect "its timer is now" duration))
            (println "Boss attacks for" (:damage boss) "-" armor "=" (max 1 (- (:damage boss) armor)))
            (newline))
          
          (if (<= (:hit-points player) 0)
            [player boss]
            (let [boss (update boss :hit-points - damage)
                  player (update player :hit-points - (max 1 (- (:damage boss) armor)))]
              [player boss])))))))

(defn all-paths [[player _boss]]
  (if (<= (:hit-points player) 0)
    '()
    (let [mana (:mana player)
          current-effects (->> (:recurring-spells player)
                               (remove #(<= (second %) 1))
                               (map first)
                               (into #{}))]
      (when *debug* (prn current-effects))
      (->> (keys all-spells)
           (remove #(< mana (all-spells %)))
           (remove current-effects)))))

(defn all-next-states [state]
  (let [player-hitpoints (comp :hit-points first)]
    (->> (all-paths state)
         (map #(play-turn state %))
         (remove #(<= (player-hitpoints %) 0))))) ; 1 for hard-addon

(comment defn cost [this-state next-state]
         (let [[player-now _boss1] this-state
               [player-next _boss2] next-state]
           (- (:spent player-next)
              (:spent player-now))))

(defn cost [_this-state next-state]
  (all-spells (:last-spell next-state)))

(defn done [[_player boss]]
  (<= (:hit-points boss) 0)) ; 1 for hard-addon?

(defn guess-cost [[player boss]]
  ; assume some average cost per boss' hit-points
  (:hit-points boss))

(defn dijkstra
  "for all nodes, find the shortest path from source to destination. 
   weight-fn takes to paramenters, u and v, and returns the distance from u to v.
   neighbour-fn takes one parameter, v, and returns all neighbours of v.
   Both u and v are taken from nodes"
  [source destination-fn weight-fn neighbour-fn path-fn]
  (loop [dist (-> (assoc {} source 0))
         prev {}
         Q (into (priority-map) dist)]
    (if (empty? Q)
      nil
      (let [u (first (peek Q))
            Q (pop Q)]
        (when (Q u) (throw (RuntimeException. (str u " is still in Q"))))
        (if (destination-fn u)
          (dist u)
          (let [[new-dist new-prev new-q]
                (->> (path-fn u) ; for each path out of u, still in Q
                     (reduce (fn [[new-dist new-prev q] path]
                               (let [v (neighbour-fn u path)
                                     alt (+ (new-dist u) (weight-fn path))] ; alt <- dist[u] + length(u,v)
                                 (if (< alt (get new-dist v 99999999))                           ; if alt < dist[v]
                                   [(assoc new-dist v alt)                          ;   dist[v] <- alt
                                    (assoc new-prev v u)                            ;   prev[v] <- u
                                    (assoc q v alt)]                        ;   Q.decrease_priority(v, alt)
                                   [new-dist new-prev q])))
                             [dist prev Q]))]
            (recur new-dist new-prev new-q)))))))

(defn a-star [start fn-goal? fn-dist fn-heuristic fn-neighbour fn-paths result-type]
  (let [summarize (fn [came-from current start-i aggr-fn]
                    (loop [i start-i
                           current current]
                      (if (not (contains? came-from current))
                        i
                        (recur (aggr-fn i current) (came-from current)))))]
    (loop [;open-set (conj #{} d) ; openSet := {start}
           came-from {}          ; cameFrom := an empty map
           g-score (assoc {} start 0); gScore := map with default value of Infinity
           f-score (priority-map start (fn-heuristic start))] ; fScore := map with default value of Infinity
      (let [current (ffirst f-score)]                      ; current := the node in openSet having the lowest fScore value
        (cond (nil? current)      ; while openSet is not empty
              nil                    ; return failure

              (fn-goal? current)                              ; if current = goal
              (case result-type
                :path (summarize came-from current '() conj) ; return reconstruct_path(cameFrom, current)
                :count (summarize came-from current 0 #(fn [a _b] (inc a)))
                :last current)

              :else
              (let [f-score (pop f-score)             ; openSet.remove(current)
                    paths (fn-paths current) ; for each neighbour of current
                    [came-from g-score f-score]
                    (reduce (fn [[came-from g-score f-score] path]
                              (let [neighbour (fn-neighbour current path)
                                    tentative-g-score (+ (or (g-score current) 9999) (fn-dist path))] ; tentative_gScore := gScore[current] + d(current, neighbour)
                                (if (< tentative-g-score (or (g-score neighbour) 9999))          ; if tentative_gScore < gScore[neighbour]
                                  [(assoc came-from neighbour current)                           ; cameFrom[neighbour] := current
                                   (assoc g-score neighbour tentative-g-score)                   ; gScore[neighbour] := tentative_gScore
                                   (assoc f-score neighbour (+ tentative-g-score (fn-heuristic neighbour)))] ; fScore[neighbour] := tentative_gScore + h(neighbour)
                                  [came-from g-score f-score])))
                            [came-from g-score f-score]
                            paths)]
                (recur came-from g-score f-score)))))))

(defn brute-force [state current-min]
  (if (done state)
    (:spent (first state))
    (loop [current-min current-min
           next-state (all-next-states state)]
      (if (empty? next-state)
        current-min
        (if (< (:spent (first (first next-state))) current-min)
          (recur (min current-min (brute-force (first next-state) current-min))
                 (rest next-state))
          (recur current-min (rest next-state)))))))

(defn new-player [name basic-settings]
  (merge {:name name
          :recurring-spells '()
          :mana 0
          :spent 0}
         basic-settings))

(defn task-1 [player boss]
  (:spent (a-star [player boss] done all-spells guess-cost play-turn all-paths :last)))

(def player (new-player "player" {:hit-points 50
                                  :damage 0
                                  :armor 0
                                  :mana 500}))

(def boss   (new-player "boss" {:hit-points 58
                                :damage 9
                                :armor 0}))

(def boss-2 (new-player "boss-2" {:hit-points 71 :damage 10}))

(task-1 player boss)

(brute-force [player boss] 99999)

(brute-force [{:name "player"
               :recurring-spells '(["Recharge" 4 :mana 101] ["Poison" 3 :damage 3])
               :mana 199
               :spent 402
               :hit-points 32
               :damage 0
               :armor 0}
              {:name "boss", :recurring-spells (), :mana 0, :spent 0, :hit-points 49, :damage 9, :armor 0}]
             999999)

(dijkstra [player boss] done all-spells play-turn all-paths)

(->> (a-star [player boss-2] done all-spells guess-cost play-turn all-paths :path)
     (map (juxt (comp (juxt :hit-points) second)
                (comp (juxt :last-spell :hit-points :mana :spent :recurring-spells) first))))

(->> ["Poison" "Drain" "Recharge" "Shield" "Poison" "Recharge" "Drain" "Shield" "Poison" "Drain" "Drain"]
     ;["Poison" "Recharge" "Shield" "Poison" "Recharge" "Shield" "Poison" "Drain" "Shield" "Drain"]
     (map all-spells)
     (reduce +))

(binding [*debug* true]
  (let [player (new-player "player" {:hit-points 10 :damage 0 :mana 250 :armor 0})
        boss (new-player "boss" {:hit-points 13 :damage 8 :armor 0})]
    (as-> [player boss] players
      (play-turn players "Poison")
      (play-turn players "Magic Missile"))))

(binding [*debug* true]
  (let [player (new-player "player" {:hit-points 10 :damage 0 :mana 250 :armor 0})
        boss (new-player "boss" {:hit-points 14 :damage 8 :armor 0})]
    (as-> [player boss] players
      (play-turn players "Recharge")
      (play-turn players "Shield")
      (play-turn players "Drain")
      (play-turn players "Poison")
      (play-turn players "Magic Missile"))))


(let [player (new-player "testplayer" {:hit-points 30 :damage 0 :mana 250 :armor 0})
      boss (new-player "boss" {:hit-points 13 :damage 8 :armor 0})]
  (as-> [player boss] state
    (play-turn state "Poison")
    (play-turn state nil)
;    (play-turn state nil)
    (all-next-states state)
    (map first state)
    (map (juxt :hit-points :last-spell :recurring-spells) state)))

(let [player (new-player "testplayer" {:hit-points 30 :damage 0 :mana 250 :armor 0})
      boss (new-player "boss" {:hit-points 13 :damage 8 :armor 0})]
  (as-> [player boss] state
    (play-turn state "Poison")
    (all-paths state)
  ))

(binding [*debug* true]
  (all-next-states [{:name "player"
                     :recurring-spells '(["Recharge" 4 :mana 101] ["Poison" 3 :damage 3])
                     :mana 199
                     :spent 402
                     :hit-points 32
                     :damage 0
                     :armor 0
                     :last-spell nil}
                    {:name "boss", :recurring-spells (), :mana 0, :spent 0, :hit-points 49, :damage 9, :armor 0}]))