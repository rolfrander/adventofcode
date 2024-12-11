(ns advent2023.day02
  (:require [clojure.string :as str]
            [rolfrander.puzzle-lib :as puzzle]))

(def testdata "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(def colors ["red" "green" "blue"])

(defn parse-line [l]
  (let [newcube {"green" 0 "red" 0 "blue" 0}
        [_all game-id record] (re-find #"Game (\d+)(.*)" l)]
    (loop [[[spec type num color] & records] (re-seq #"([:,;]) (\d+) (\w+)" record)
           cubes nil
           result nil]
      (if (nil? spec)
        [(puzzle/str->long game-id) (conj result cubes)]
        (case type
          (":" ";") (recur records (assoc newcube color (puzzle/str->long num)) (if (nil? cubes) result (conj result cubes)))
          ","       (recur records (assoc cubes color (puzzle/str->long num)) result))))))

(defn parse [in]
  (map parse-line (str/split-lines in)))

(conj nil 42)

;(parse testdata)

(defn solve-1 [data]
  (let [games (parse data)
        sample-game {"red" 12 "green" 13 "blue" 14}
        check-single-pick (fn [g] (not-any? #(< (sample-game %) (g %)) colors))
        check-game-round (fn [[_id games]] (every? check-single-pick games))]
    (->> (filter check-game-round games)
         (map first)
         (reduce +)
         )))

(defn solve-2 [data]
  (let [games (map second (parse data))
        newcube {"green" 0 "red" 0 "blue" 0}
        maxcubes (fn [a b] (reduce #(assoc %1 %2 (max (a %2) (b %2)))
                                   {} colors))
        maxcubes-in-game (fn [g] (reduce maxcubes g))]
    (->> (map maxcubes-in-game games)
         (map #(apply * (vals %)))
         (reduce +))
    ))

(solve-1 testdata)
;;=> 8
(solve-1 (puzzle/get-data 2023 2))
;;=> 2505

(solve-2 testdata)
;;=> 2286
(solve-2 (puzzle/get-data 2023 2))
;;=> 70265


