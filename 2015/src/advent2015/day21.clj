(ns advent2015.day21)

(defn hit [attacker victim]
  (update victim :hit-points #(- % (max 1 (- (:damage attacker)
                                             (:armor victim))))))

(defn play
  "each player and boss is a structure of {:hit-points :damage :armour}"
  [player boss]
  (cond (<= (:hit-points boss) 0) :player
        (<= (:hit-points player) 0) :boss
        :else (recur (hit boss player)
                     (hit player boss))
    )
  )

(play {:hit-points 8 :damage 5 :armor 5}
      {:hit-points 12 :damage 7 :armor 2})

(def player {:hit-points 100 :damage 0 :armor 0 :cost 0})
(def boss   {:hit-points 100 :damage 8 :armor 2 :cost 0})

(def weapon
  {"Dagger"      {:cost  8 :damage 4 :armor 0}
   "Shortsword"  {:cost 10 :damage 5 :armor 0}
   "Warhammer"   {:cost 25 :damage 6 :armor 0}
   "Longsword"   {:cost 40 :damage 7 :armor 0}
   "Greataxe"    {:cost 74 :damage 8 :armor 0}})
(def armor
  {"Leather"     {:cost  13 :damage 0 :armor 1}
   "Chainmail"   {:cost  31 :damage 0 :armor 2}
   "Splintmail"  {:cost  53 :damage 0 :armor 3}
   "Bandedmail"  {:cost  75 :damage 0 :armor 4}
   "Platemail"   {:cost 102 :damage 0 :armor 5}})
(def ring {"Damage +1"   {:cost  25 :damage 1 :armor 0}
           "Damage +2"   {:cost  50 :damage 2 :armor 0}
           "Damage +3"   {:cost 100 :damage 3 :armor 0}
           "Defense +1"  {:cost  20 :damage 0 :armor 1}
           "Defense +2"  {:cost  40 :damage 0 :armor 2}
           "Defense +3"  {:cost  80 :damage 0 :armor 3}})

(defn buy [player artefact]
  (merge-with + player artefact))

(-> player 
    (buy (weapon "Dagger"))
    (buy (armor "Leather"))
    (buy (ring "Damage +2"))
    (play boss))

(def all-games-lost
  (for [w (keys weapon)
        a (keys armor)
        [r1 & other-rings] (take-while seq (iterate rest (keys ring)))
        r2 other-rings
        player (let [p1 (-> player
                            (buy (weapon w))
                            ;(buy (armor a))
                            )
                     p2 (buy p1 (ring r1))
                     p3 (buy p2 (ring r2))]
                 [p1 p2 p3])
        :when (= (play player boss) :boss)]
    player))

(take 5 (sort-by :cost > all-games-lost))

(play {:hit-points 100, :damage 8, :armor 1, :cost 146} boss)