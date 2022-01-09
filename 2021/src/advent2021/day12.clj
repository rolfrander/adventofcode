(ns advent2021.day12
  (:require [clj-http.client :as http]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math*  :warn-on-boxed)

(def testdata1 "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def testdata2 "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(def testdata3 "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(defn parse [input]
  (loop [connection-map {}
         [connection & tokens] (re-seq #"([a-zA-Z]+)-([a-zA-Z]+)" input)]
    (if (nil? connection)
      connection-map
      (let [[_ from to] connection]
        (cond (= from "start") (recur (update connection-map from conj to) tokens)
              (= to "start")   (recur (update connection-map to conj from) tokens)
              (= to "end")     (recur (update connection-map from conj to) tokens)
              (= from "end")   (recur (update connection-map to conj from) tokens)
              :else            (recur (-> connection-map
                                          (update from conj to)
                                          (update to conj from))
                                      tokens))))))
              

(parse testdata1)

(defn task-1 [graph & {:keys [accept-one-minor-cave-twice]}]
  (letfn [(traverse [node visited repeat-minor]
                    (let [neighbours (graph node)]
                      (reduce (fn [path-count ^String next-step]
                                (+ path-count
                                   (cond (= next-step "end") 1

                                         (Character/isUpperCase (.charAt next-step 0))
                                         (traverse next-step visited repeat-minor)

                                         (not (visited next-step))
                                         (traverse next-step (conj visited next-step) repeat-minor)

                                         (true? repeat-minor)
                                         (traverse next-step visited nil)

                                         :else 0)))
                              0
                              neighbours)))]
    (traverse "start" #{} accept-one-minor-cave-twice)))

(defn task-2 [graph]
  (task-1 graph :accept-one-minor-cave-twice true))

(def data (load-data))

(task-1 (parse testdata1)) ; 10
(task-1 (parse testdata2)) ; 19
(task-1 (parse testdata3)) ; 226
(task-1 (parse data)); 3369

(task-2 (parse testdata1)) ; 36
(task-2 (parse testdata2)) ; 103
(task-2 (parse testdata3)) ; 3509
(time
 (task-2 (parse data))); 3369
