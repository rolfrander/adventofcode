(ns advent2016.core
  (:require [clojure.java.io :as io]
            [clj-http.client :as http]
            [clojure.data.priority-map :refer [priority-map priority-map-by]]))

(def ^:dynamic *debug* false)

(defn get-config []
  (with-open [c (java.io.PushbackReader.
                 (io/reader "config"))]
    (read c)))

(defn get-data [year day]
  (let [url (format "https://adventofcode.com/%d/day/%d/input" year day)
        local-file (format "resources/advent%d/day%02d.txt" year day)]
    (if (.exists (io/file local-file))
      (do (when *debug* (println "loading input from" local-file))
          (slurp local-file))
      (let [response (http/get url {:cookies {"session" {:value (:session (get-config))}}})]
        (when *debug* (println "loading input from http"))
        (if (not= (:status response) 200)
          (throw (RuntimeException. (str "error getting data: " (:reason-phrase response))))
          (let [body (:body response)]
            (with-open [w (io/writer local-file)]
              (.write w body))
            body))))))

(defn safe-parse-number [s]
  (if (re-matches #"[+-]?[0-9]+" s)
    (Long/parseLong s)
    s))

(defn split-by
  "returns a lazy sequence of seq from coll, starting a new sequence everytime pred change from true to false"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [[xs ys] (split-with pred s)]
       (if (seq xs)
         (cons xs (split-by pred ys))
         (let [!pred (complement pred)
               skip (take-while !pred s)
               others (drop-while !pred s)
               [xs ys] (split-with pred others)]
           (cons (concat skip xs)
                 (split-by pred ys))))))))

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
           g-score (priority-map-by > start 0); gScore := map with default value of Infinity
           f-score (priority-map start (fn-heuristic start)) ; fScore := map with default value of Infinity
           current-longest 0]
      (let [[current _estimate] (peek f-score) ; current := the node in openSet having the lowest fScore value
            estimate (second (peek g-score))] 
        (when (and *debug* (> estimate current-longest)) (println "current longest path:" estimate))
        (cond (nil? current)      ; while openSet is not empty
              nil                    ; return failure

              (fn-goal? current)                              ; if current = goal
              (case result-type
                :path  (summarize came-from current '() conj) ; return reconstruct_path(cameFrom, current)
                :count (summarize came-from current   0 (fn [a _b] (inc a)))
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
                (recur came-from g-score f-score (max current-longest estimate))))))))