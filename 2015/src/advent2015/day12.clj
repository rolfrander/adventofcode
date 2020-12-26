(ns advent2015.day12
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]
            [clojure.data.json :as json]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

(def ^:dynamic *debug* false)

(def data 
  (with-open [in (clojure.java.io/reader "resources/2015/input12.txt")]
   (json/read in)))

(defn has-value [m value]
  (some #(= value (second %)) m))

(defmulti traverse class)

(defmethod traverse nil [data]
  0)

(defmethod traverse java.util.List [data]
  (->> data
       (map traverse)
       (reduce +)))

(defmethod traverse clojure.lang.IPersistentMap [data]
  (if (has-value data "red")
    0
    (->> data
         vals
         (map traverse)
         (reduce +))))

(defmethod traverse java.lang.String [data]
  0)

(defmethod traverse java.lang.Long [data]
  data)

(traverse data)
;; => 96852

;; => 156366
