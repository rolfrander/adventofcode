(ns advent2019.day13
  (:require
   [advent2019.intcode :as ic]
   [rolfrander.puzzle-lib :as puzzle]
   [clojure.core.async :refer [>!! <!! chan thread pipe close!]]))



(def tile-type [:empty :wall :block :horizontal-paddle :ball])
(def tile [\. \# \B \- \o])
(def tile-num (zipmap tile-type (range)))
(def tile-sym (reduce-kv #(assoc %1 %2 (tile %3)) {} tile-num))
(def ^:dynamic *terminal-output* false)

(defn screen-controller [input output]
  (let [esc (if *terminal-output* 
              (fn [s] (print (str (char 27) "[" s)) (flush))
              (fn [_s] nil))
        block (fn [x y ch] (esc (format "%d;%dH%c" (+ 3 y) (inc x) ch)))
        score (fn [score] (esc (format "1;1HScore: %d" score)))
        ]
    (esc "2J")
    (loop [x (<!! input)
           y (<!! input)
           tile-id (<!! input)
           paint {}]
      (cond (nil? x)
            paint

            (and (= x -1) (= y 0))
            (do (>!! output [:score tile-id 0])
                (score tile-id)
                (recur (<!! input) (<!! input) (<!! input)
                       paint))

            :else
            (let [cur-tile (paint [x y])]
              (block x y (tile tile-id))
              (case (tile-type tile-id)
                :empty (when (= cur-tile :block)
                         (>!! output [:remove-block x y]))
                :ball (do ;(Thread/sleep 500)
                          (>!! output [:ball x y]))
                :horizontal-paddle (>!! output [:horizontal-paddle x y])
                nil)
              (recur (<!! input) (<!! input) (<!! input)
                     (assoc paint [x y] (tile-type tile-id))))))))

(defn print-screen [paint]
  (let [[max-x max-y min-x min-y] (for [k [max-key min-key]
                                        d [first second]]
                                    (d (apply k d (keys paint))))
        idx (reduce-kv #(update %1 (tile-sym %3) conj (map - %2 [min-x min-y])) {} paint)]
    (puzzle/draw-map {:width (inc (- max-x min-x))
                      :height (inc (- max-y min-y))
                      :markings idx})))

(defn game-controller
  "starts the program in a thread. Returns a map with tree values:
   :input channel for reading input data
   :output channel for writing output data
   :done channel for writing internal state after the robot shuts down"
  [program joystick-ref player-channel]
  (let [out (chan)
        done (thread (let [ret (ic/interpret program
                                          :input-fn (fn [state]
                                                      (let [joystick @joystick-ref]
                                                        (>!! player-channel [:read-joystick joystick 0])
                                                        (assoc state :value joystick)))
                                          :output-fn (fn [state x] (>!! out x) state))]
                       (close! out)
                       ret))]
    {:output out
     :done done}))

(defn solve-1 [program-text]
  (let [joystick (atom 0)
        {:keys [output _done]} (game-controller (ic/parse program-text) joystick nil)
        scores (chan 3)
        _printthread (thread (loop [s (<!! scores)] (when s (println s) (recur (<!! scores)))))
        result (->> (screen-controller output scores)
                    vals
                    (filter #{:block})
                    count)]
    (close! scores)
    result))

(defn project-ball-pos [{[x y] :ball [px py] :ball-prev}]
  (when (and x y px py)
    (let [moving-down (and (not= y 20) (> y py))
          direction (- x px)
          new-x (if moving-down
                  (+ x (* direction (- 20 y)))
                  (+ x (* direction (- y 12))))]
      (if (<= new-x 30)
        new-x
        (- 60 new-x))
      
      )))

(defn compute-joystick-direction [{[x y] :ball [px py] :ball-prev paddle :paddle}]
  (if (and x y px py paddle)
    (let [direction (- x px)
          joystick (cond (= x paddle) direction
                         (= paddle (+ x direction)) 0
                         :else (compare x paddle))]
      ; overrides
      (cond (= paddle 2) (max joystick 0)
            (= paddle 40) (min joystick 0)
            (= paddle 1) 1
            (= paddle 41) -1
            :else joystick)
      )
    0))

(defn solve-2 [program-text]
  (let [state (atom {:ball nil
                     :ball-prev nil
                     :paddle nil
                     :score 0
                     :history nil})
        joystick (ref 0)
        program (ic/parse program-text)
        program (assoc program 0 2) ; insert coin...
        scores (chan)
        {:keys [output done]} (game-controller program joystick scores)
        update-joystick (fn [] (let [s @state
                                     where-will-ball-be (project-ball-pos s)]
                                 ;(if (some? where-will-ball-be)
                                 ;  (dosync (ref-set joystick (compare where-will-ball-be (:paddle s))))
                                 ;  0)
                                 (dosync (ref-set joystick (compute-joystick-direction s))))
                          )
        player (thread (loop [[type x y] (<!! scores)]
                         (if type
                           (do (swap! state update :history conj [type x y])
                               (case type
                                 :score (when (not= 0 x) (swap! state assoc :score x))
                                 :remove-block (do ;(println "remove block:" x y)
                                                   (when (not= x (first (:ball @state)))
                                                     ; hit a block in another x-column, possibly reflected back, alter direction immediately
                                                     (dosync (alter joystick -))))
                                 :ball (do (swap! state #(-> %
                                                             (assoc :ball-prev (:ball %))
                                                             (assoc :ball [x y])))
                                           (update-joystick))
                                 :horizontal-paddle (do (swap! state assoc :paddle x)
                                                        (update-joystick))
                                 ;:read-joystick (println "read joystick:" x "last known ball position:" (:ball @state) "bar position:" (:paddle @state))
                                 
                                 nil)
                               (recur (<!! scores)))
                           @state)))]
    ;; main loop
    (->> (screen-controller output scores)
         ;(print-screen)
         )
    ;(println "score" (:score @state))
    ; (println "done" (<!! done))
    (close! scores)
    (<!! player)
    (<!! done)
    (:score @state)))

(defn draw-last-state [s]
  (let [board (first (reduce (fn [[res ball-cnt] [type x y]]
                               (case type
                                 :ball              [(assoc res [x y] (if (< ball-cnt 10)
                                                                        ball-cnt
                                                                        \o))
                                                     (inc ball-cnt)]
                                 :horizontal-paddle [(assoc res [x y] \-) ball-cnt]
                                 :remove-block      [(assoc res [x y] \x) ball-cnt]
                                 [res ball-cnt]))
                             [{} 0] s
                             ))]
    (doseq [y (range 10 22)
            x (range 1 31)]
      (if-let [c (board [x y])]
        (print c)
        (print \.))
      (when (= x 30)
        (println)))))

(def data (puzzle/get-data 2019 13))

(draw-last-state (list 
                  [:score 0 0]
                  [:ball 19 22]
                  [:horizontal-paddle 22 21]
                  [:read-joystick -1 0]
                  [:ball 20 21]
                  [:horizontal-paddle 23 21]
                  [:read-joystick -1 0]
                  [:ball 21 20]
                  [:horizontal-paddle 24 21]
                  [:read-joystick -1 0]
                  [:ball 22 19]
                  [:horizontal-paddle 25 21]
                  [:ball 23 18]
                  [:read-joystick 1 0]
                  [:score 1228 0]
                  [:remove-block 25 16]
                  [:horizontal-paddle 24 21]
                  [:read-joystick 1 0]
                  [:ball 24 17]))

;(solve-1 (puzzle/get-data 2019 13))
;;=> 363

(solve-2 data) 

