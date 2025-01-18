(ns advent2019.client
  (:require [cljs.core.async :as a :refer [<! >! go go-loop]]
            [clojure.string :as str]
            [haslett.client :as ws]
            [haslett.format :as wsfmt]))

(def game-state (atom nil))

(defn map-vals
  "copied from flatland.useful/map"
  [m f & args]
  (when m
    (into {}
          (for [[k v] m]
            [k (apply f v args)]))))

(defn- query [query]
  (.querySelector js/document query))

(defn- append-html [element html]
  (.insertAdjacentHTML element "beforeend" html)
  element)

(defn- list-item [s]
  (str "<li>" s "</li>"))

(defn- clear-debug []
  (.replaceChildren (query "#debug")
                    ""))

(defn scroll-to-end [element]
  (set! (.-scrollTop element) (.-scrollHeight element))
  element)

(defn debug [s]
  (-> (query "#debug")
      (append-html (list-item s))
      scroll-to-end))

(defn- send-message [stream msg]
  (debug (str "send-message: " msg))
  (go (>! (:out stream) msg)))

(def commands {:north [0 -1]
               :south [0  1]
               :east  [1  0]
               :west  [-1  0]
               })

(defn- move [pos direction]
  (map + pos (get commands direction [0 0])))

;
; rooms are drawn within a 100x100 px tile
; inside this grid, there is 10 px margin on each side
; walls are 4 px thick
; doors are 20 px wide, on the middle of each wall (40 px from each corner of the room-tile)

(def room-graphical-size 100)

(def item-marks {"hypercube"  "X"
                 "mug"        "u"
                 "coin"       "o"
                 "easter egg" "e"
                 "candy cane" "I"
                 "manifold"   "m"
                 "molten lava" "$"
                 "astrolabe"   "A"
                 "giant electromagnet" "%"
                 "pointer"     "v"
                 "escape pod"  "/"
                 "infinite loop" "8"
                 "photons"       "."
                 })
;                          x  y  w  h
(def door-coords {:north [40  0 20 14]
                  :south [40 86 20 14]
                  :east  [86 40 14 20]
                  :west  [ 0 40 14 20]})

(defn draw-room [ctx room-name doors pos? items]
  (set! (.-fillStyle ctx) "#890000")
  (.fillRect ctx 10 10 80 80)
  (set! (.-fillStyle ctx) "#111111")
  (.fillRect ctx 14 14 72 72)
  ;(set! (.-lineWidth ctx) 2)
  (set! (.-fillStyle ctx) "#3d1b11")
  (doseq [door doors]
    (let [[x y w h] (door-coords door)]
      (.fillRect ctx x y w h)
      ))
  (set! (.-fillStyle ctx) "#dddddd")
  (set! (.-font ctx) "12px monospace")
  (.fillText ctx room-name 20 28 60)
  (when pos? (.fillText ctx "@" 20 52))
  (.fillText ctx (str/join (map item-marks items))
             20 66)
  )

(defn draw-tunnel-horiz [ctx y from-x to-x]
  (set! (.-fillStyle ctx) "#3d1b11")
  (.fillRect ctx
             (* room-graphical-size (inc from-x))
             (+ 40 (* room-graphical-size y))
             (* room-graphical-size (- to-x from-x 1))
             20))

(defn draw-tunnel-vert [ctx x from-y to-y]
  (set! (.-fillStyle ctx) "#3d1b11")
  (.fillRect ctx
             (+ 40 (* room-graphical-size x))
             (* room-graphical-size (inc from-y))
             20
             (* room-graphical-size (- to-y from-y 1))))

(defn- draw-map [canvas room-map pos]
  (let [[max-x max-y min-x min-y] (for [k [max-key min-key] d [first second]]
                                    (d (apply k d (vals room-map))))
        width (inc (- max-x min-x))
        height (inc (- max-y min-y))

        state @game-state
        rooms (:rooms state)]
    (set! (.-width canvas)  (* width room-graphical-size))
    (set! (.-height canvas) (* height room-graphical-size))
    (let [ctx (.getContext canvas "2d")]
      (.fillRect ctx 0 0 (* width  room-graphical-size) (* height room-graphical-size))
      (doseq [[name [x y]] room-map]
        (.save ctx)
        (.translate ctx (* (- x min-x) room-graphical-size)
                    (* (- y min-y) room-graphical-size))
        (draw-room ctx name (keys (rooms name)) (= name pos) 
                   ((:item-map state) name))
        (.restore ctx))
      (doseq [[room neighbour-map] rooms]
        (when-let [e (neighbour-map :east)]
          (draw-tunnel-horiz ctx (- (second (room-map room)) min-y)
                             (- (first (room-map room)) min-x)
                             (- (first (room-map e)) min-x)))
        (when-let [s (neighbour-map :south)]
          (draw-tunnel-vert ctx (- (first (room-map room)) min-x)
                             (- (second (room-map room)) min-y)
                             (- (second (room-map s)) min-y))))
      
      )
    )
  )

(let [bb (.getBoundingClientRect (query "#game"))]
  [(.-width bb) (.-height bb)])

(defn- place-rooms-on-grid [rooms]
  ;(prn rooms)
  (letfn [(sort-rooms [room-pos cur-pos cur-room last-direction-moved]
            (if (contains? room-pos cur-room)
              room-pos
              (let [place-current-room (fn [rp] (assoc rp cur-room cur-pos))
                    place-next-rooms   (fn [rp] (reduce (fn [r [dir name]]
                                                          (let [adjusted-cur-pos (r cur-room)]
                                                            (if (nil? name)
                                                              r
                                                              (sort-rooms r (move adjusted-cur-pos dir) name dir))))
                                                        rp (rooms cur-room)))
                    
                    shift-rooms-away-from-cur-pos (fn [rp]
                                                    (let [[cur-x cur-y] cur-pos]
                                                      (map-vals rp (fn [[x y]]
                                                                     (cond
                                                                       (and (= last-direction-moved :north)
                                                                            (<= y cur-y)) [x (dec y)]
                                                                       (and (= last-direction-moved :south)
                                                                            (>= y cur-y)) [x (inc y)]
                                                                       (and (= last-direction-moved :west)
                                                                            (<= x cur-x)) [(dec x) y]
                                                                       (and (= last-direction-moved :east)
                                                                            (>= x cur-x)) [(inc x) y]

                                                                       ;(>= y cur-y) [x (inc y)]
                                                                       ;(>= x cur-x) [(inc x) y]
                                                                       :else [x y])))))
                    ]
                (-> room-pos
                    shift-rooms-away-from-cur-pos
                    place-current-room
                    ;print-state
                    place-next-rooms))))]
    (let [cur-room (first (keys rooms))
          cur-pos [0 0]
          placed-rooms (sort-rooms {} cur-pos cur-room nil)]
      placed-rooms)))

(defn- update-view []
  (let [state @game-state
        room-name (query "#room-name")
        room-desc (query "#room-desc")
        room-items (query "#room-items")
        inventory (query "#inventory")
        autotake (query "#autotake-state")
        current-room (first (:pos state))]
    (debug (str "receive message with keys: " (keys state)))
    (debug (str "inventory: " (:inventory state)))
    (.replaceChildren room-name current-room)

    (set! (.-innerText autotake)
          (if (contains? (:flags state) :autotake)
            "on" "off"))
    (let [ul (.createElement js/document "ul")]
      (.replaceChildren room-items ul)
      (doseq [i (get-in state [:item-map current-room])]
        (let [li (.createElement js/document "li")]
          (.append li i)
          (.appendChild ul li))))

    (let [ul (.createElement js/document "ul")]
      (.replaceChildren inventory ul)
      (doseq [i (get state :inventory)]
        (let [li (.createElement js/document "li")]
          (.append li i)
          (.appendChild ul li))))

    (when (:desc state) (.replaceChildren room-desc (apply str (:desc state))))

    (draw-map (query "#game")
              (place-rooms-on-grid (:rooms state))
              (first (:pos state)))

    (doseq [d (->> (.-children (query "#buttons"))
                   (filter #(= "BUTTON" (.-tagName %))))]
      (if (contains? (:doors state)
                     (keyword (.-id d)))
        (set! (.-disabled d) false)
        (set! (.-disabled d) true)))))

(defn- handle-input-message [message]
  (when (contains? message :rooms)
    (when (:debug message) (debug (:debug message)))
    (reset! game-state message)
    (update-view)
    ))

(defn- start-listener [stream]
  (go-loop []
    (when-some [message (<! (:in stream))]
      (handle-input-message message)
      (recur))))

(defn- websocket-url [path]
  (let [loc   (.-location js/window)
        proto (if (= "https:" (.-protocol loc)) "wss" "ws")]
    (str proto "://" (.-host loc) path)))

(defn- websocket-connect [path]
  (ws/connect (websocket-url path) {:format wsfmt/transit}))

(defn- on-load [_]
  (.addEventListener (query "#clear-debug") "click"
                     (fn [_] (clear-debug)))
  (go (let [stream  (<! (websocket-connect "/chat"))]
        (start-listener stream)
        (.addEventListener (query "#room-items") "click"
                           (fn [e] (send-message stream {:cmd :take :params [(.-innerText (.-target e))]})))
        (.addEventListener (query "#inventory") "click"
                   (fn [e] (send-message stream {:cmd :drop :params [(.-innerText (.-target e))]})))
        (.addEventListener (query "#start-game") "click"
                           (fn [_]
                             (send-message stream {:cmd :start})))
        (.addEventListener (query "#toggle-autotake") "click"
                           (fn [_]
                             (send-message stream {:cmd :toggle :params [:autotake]})))
        (.addEventListener (query "#reload-state") "click"
                           (fn [_]
                             (send-message stream {:cmd :reload})))
        (.addEventListener (query "#game") "click"
                           (fn [e] (let [canvas (query "#game")
                                         bb (.getBoundingClientRect canvas)
                                         w (.-width bb)
                                         h (.-height bb)]
                                     (debug (str "click: x=" (.-offsetX e)
                                                 "y=" (int (* 11 (/ (.-offsetY e) h))))))))
        (doseq [b ["north" "east" "south" "west"]]
          (.addEventListener (query (str "#" b)) "click"
                             (fn [_] (send-message stream {:cmd (keyword b) :params []}))))))
  (debug "started!"))

(defn init []
  (.addEventListener js/window "load" on-load))

