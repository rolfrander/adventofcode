(ns advent2016.day05 
  (:require [clojure.string :as string]))

(def *warn-on-reflection* true)

(def md5 (java.security.MessageDigest/getInstance "MD5"))

(defn bytes-to-hex [input]
  (->> (map (partial format "%02x") input)
       string/join))

(defn md5-ascii [^String input]
  (bytes-to-hex (.digest md5 (.getBytes input "ASCII"))))

(def testdata "abc")
(def data "ugkcyxxp")

(time (->> (range)
           (map (partial format "%s%d" "abc"))
           (map md5-ascii)
           (filter #(.startsWith % "00000"))
           (take 2)
           (doall)))