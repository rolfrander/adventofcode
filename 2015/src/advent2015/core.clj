(ns advent2015.core
  (:require [clojure.java.io :as io]
            [clj-http.client :as http]))

(defn get-config []
  (with-open [c (java.io.PushbackReader.
                 (io/reader "config"))]
    (read c)))

(defn get-data [year day]
  (let [url (format "https://adventofcode.com/%d/day/%d/input" year day)
        local-file (format "resources/%d/day%02d.txt" year day)]
    (if (.exists (io/file local-file))
      (slurp local-file)
      (let [response (http/get url {:cookies {"session" {:value (:session (get-config))}}})]
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