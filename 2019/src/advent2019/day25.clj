(ns advent2019.day25
  (:require
   [advent2019.intcode :as ic]
   [clojure.core.async :refer [<!! >!! thread chan close!]]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [rolfrander.puzzle-lib :refer [get-data]]
   [flatland.useful.map :refer [into-map map-vals]]))

(def example-data "== Engineering ==
You see a whiteboard with plans for Springdroid v2.

Doors here lead:
- north
- east
- south

Items here:
- astrolabe

Command?")

(def commands {:north {:cmd "north" :params 0 :direction [0 -1] :oposite-dir :south}
               :south {:cmd "south" :params 0 :direction [0  1] :oposite-dir :north}
               :east  {:cmd "east"  :params 0 :direction [1  0] :oposite-dir :west}
               :west  {:cmd "west"  :params 0 :direction [-1  0] :oposite-dir :east}
               :take  {:cmd "take"  :params 1}
               :drop  {:cmd "drop"  :params 1}
               :inv   {:cmd "inv"   :params 0}
               ;:quit  {}
               })

(defn move [pos direction]
  (map + pos (get-in commands [direction :direction] [0 0])))

(defn oposite [direction]
  (get-in commands [direction :oposite-dir]))

(defn is-move? [cmd] (contains? (commands cmd) :direction))

(defn command-loop [input-ch]
  (let [output-ch (chan)]
    (thread (loop []
              (when-let [[command-kw parameter] (<!! input-ch)]
                (when-not (= command-kw :quit)
                  (log/debugf "command %s %s" command-kw parameter)
                  (if-let [{:keys [cmd params]} (commands command-kw)]
                    (let [cmdline (if (> params 0)
                                    (str cmd " " parameter)
                                    cmd)]
                      (log/debugf "send cmd: %s" cmdline)
                      (doseq [c (map int cmdline)] (>!! output-ch c))
                      (>!! output-ch 10) ; newline
                      )
                    (log/errorf "unknown command %s %s" command-kw parameter))
                  (recur))))
            (log/info "exit command-loop")
            (close! output-ch))
    output-ch))

(defn get-token [line]
  (let [token-rules [[:cmd   #"^Command."]
                     [:doors #"^Doors here lead:"]
                     [:items #"^Items here:"]
                     [:inv   #"^Items in your inventory:"]
                     [:illeg #"^You can't go that way."]
                     [:room  #"^== (.*) ==$"]
                     [:taken #"^You take the (.*)."]
                     [:drop  #"^You drop the (.*)."]
                     [:list  #"^- (.*)"]
                     [:empty #"^$"]
                     [:illeg #".*You can't move.*"]
                     [:too-heavy #".*Droids on this ship are lighter.*"]
                     [:too-light #".*Droids on this ship are heavier.*"]
                     [:text  #".*"]]]
    (some (fn [[type rule]]
            (when-let [m (re-matches rule line)]
              [type (if (vector? m) (peek m) m)]))
          token-rules)))

;(map get-token (str/split-lines example-data))

(defn parse-loop [input-ch]
  (let [vec-append (fn [v l] (if (nil? v) [l] (conj v l)))
        output-ch (chan)]
    (thread (loop [state :top
                   ret {}]
              (let [l (<!! input-ch)
                    ret (update ret :lines vec-append l)]
                (if (nil? l)
                  (>!! output-ch ret)

                  (let [[token value] (get-token l)]
                    (log/debugf "parsed '%s': %s" token value)
                    (case token
                      :cmd
                      (do (>!! output-ch ret)
                          (recur :top {}))

                      :doors
                      (recur :doors (assoc ret :doors []))

                      :items
                      (recur :items (assoc ret :items []))

                      :inv
                      (recur :inventory (assoc ret :inventory []))

                      :illeg
                      (recur :top (assoc ret :cant-move true))

                      :room
                      (recur :room
                             (if (:room ret)
                               ; several rooms in a row, replace the current with the new and collect all text
                               (assoc ret
                                      :desc (into [(:room ret)] (:desc ret))
                                      :room value)
                               (assoc ret :room
                                      value :desc [])))

                      :taken
                      (recur :top (assoc ret :confirm-taken value))

                      :drop
                      (recur :top (assoc ret :confirm-dropped value))

                      :list
                      (if (#{:items :inventory :doors} state)
                        (recur state (update ret state conj value))
                        (do (log/warnf "unknown input: %s (state: %s)" l state)
                            (recur state (update ret :illegal-lines vec-append [state l]))))

                      :empty
                      (if (= state :room)
                        (recur :top ret)
                        (recur state ret))

                      :too-heavy
                      (recur state (assoc ret :weight token))

                      :too-light
                      (recur state (assoc ret :weight token))

                      :text
                      (recur state (update ret :desc conj l))

                      (do (log/warnf "unknown input: %s (state: %s)" l state)
                          (recur state (update ret :illegal-lines vec-append [state l]))))))))
            (close! output-ch)
            (log/info "exit parse-loop"))
    output-ch))

;(let [in (chan)
;      out (parse-loop in)]
;  (doseq [l (str/split-lines example-data)]
;    (>!! in l))
;  (println (<!! out))
;  (close! in))

(defn read-loop
  "reads integers from input-channel, converts to char and assembles lines. Lines are print to stdout and sent to 
   output-channel. Returns output-channel."
  [input & {:keys [echo] :or {echo false}}]
  (let [output (chan)]
    (thread (loop [c (<!! input)
                   linebuf []]
              (when-not (nil? c)
                (if (= c 10)
                  (let [line (str/join (map char linebuf))]
                    (when echo (println line))
                    (>!! output line)
                    (recur (<!! input) []))
                  (recur (<!! input) (conj linebuf c)))))
            (log/info "exit read-loop")
            (close! output))
    output))

;(thread (loop []
;          (when-let [i (<!! out)]
;            (println i)
;            (recur))))

(defn print-rooms-by-pos [rooms rooms-by-pos pos]
  (let [room-str (fn [room x y part curpos]
                   (let [doors (into #{} (keys (rooms room)))]
                     (case part
                       0 (if (doors :north) "+---]  [---+" "+----------+")
                       1 (format "|%-10.10s|" room)
                       2 (format "%s %s        %s"
                                 (if (doors :west) " " "|")
                                 (if curpos        "@" " ")
                                   ;x y
                                 (if (doors :east) " " "|"))
                       3 (if (doors :south) "+---]  [---+" "+----------+"))))
        [max-x max-y min-x min-y] (for [k [max-key min-key] d [first second]]
                                    (d (apply k d (keys rooms-by-pos))))

        corridors (->> (for [y (range min-y (inc max-y))
                             x (range min-x (inc max-x))]
                         [x y])
                       (reduce (fn [c [x y]]
                                 (cond (rooms-by-pos [x y])
                                       c

                                       (or (= (c [(dec x) y]) :hor)
                                           (some? (get-in rooms [(rooms-by-pos [(dec x) y]) :east])))
                                       (assoc c [x y] :hor)

                                       (or (= (c [x (dec y)]) :vert)
                                           (some? (get-in rooms [(rooms-by-pos [x (dec y)]) :south])))
                                       (assoc c [x y] :vert)

                                       :else c))
                               {}))]

    (newline)
    (doseq [y (range min-y (inc max-y))]
      (dotimes [i 4]
        (doseq [x (range min-x (inc max-x))]
          (if-let [r (rooms-by-pos [x y])]
            (print (room-str r x y i (= pos r)))
            (if-let [c (corridors [x y])]
              (cond (and (= c :hor) (= i 2))
                    (print "============")
                    (= c :vert)
                    (print "     ||     ")
                    :else
                    (print "            "))
              (print "            "))))
        (newline)))
    (newline)))

(defn print-map [rooms pos]
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
                    room-exists-at-cur-pos (fn [] (boolean (some #(= cur-pos %)
                                                                 (vals room-pos))))
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
                    print-state (fn [rp]
                                  (println "==============================================================================================")
                                  (println "cur-pos" cur-pos "cur-room" cur-room)
                                  (print-rooms-by-pos rooms (group-by second rp) cur-room)
                                  rp)]
                (-> room-pos
                    shift-rooms-away-from-cur-pos
                    place-current-room
                    ;print-state
                    place-next-rooms))))]
    (let [cur-room (first (keys rooms))
          cur-pos [0 0]
          placed-rooms (sort-rooms {} cur-pos cur-room nil)
          rooms-by-pos (map-vals (group-by second placed-rooms)
                                 ffirst)]
      (print-rooms-by-pos rooms rooms-by-pos pos))))

(print-map {"Storage" {:south "Hot Chocolate Fountain"},
            "Security Checkpoint" {:south "Holodeck", :east nil, :west nil},
            "Engineering" {:south nil, :east "Science Lab", :north "Kitchen"},
            "Hull Breach" {:west "Gift Wrapping Center", :south "Hallway", :east "Passages"},
            "Gift Wrapping Center" {:west nil, :east "Hull Breach", :north nil},
            "Arcade" {:south "Corridor"},
            "Holodeck" {:west "Kitchen", :north "Security Checkpoint"},
            "Hallway" {:south "Stables", :east "Hot Chocolate Fountain", :north "Hull Breach"},
            "Stables" {:north "Hallway"},
            "Hot Chocolate Fountain" {:west "Hallway", :north "Storage"},
            "Passages" {:west "Hull Breach", :south "Science Lab", :north "Corridor"},
            "Corridor" {:south "Passages", :east nil, :north "Arcade"},
            "Kitchen" {:south "Engineering", :east "Holodeck"},
            "Sick Bay" {:west "Science Lab"},
            "Science Lab" {:west "Engineering", :east "Sick Bay", :north "Passages"}}

           "Sick Bay")

(defn robot
  "starts the program in a thread. Returns channel for writing output data"
  [program input-ch]
  (let [output-ch (chan)]
    (thread
      (try
        (let [ret (ic/interpret program
                                :input-fn (fn [state]
                                            (let [in (<!! input-ch)]
                                              (when (nil? in)
                                                (throw (RuntimeException. "quit")))
                                              (assoc state :value in)))
                                :output-fn (fn [state x] (>!! output-ch x) state))]
          ret)
        (catch RuntimeException e (log/warn "error from interpreter: " (.getMessage e)))
        (finally (close! output-ch))))
    (log/info "exit robot")
    output-ch))


(def dangerous-items #{"infinite loop"
                       "photons"
                       "escape pod"
                       "giant electromagned"
                       "molten lava"})

(defn pos [game-state] (peek (:pos game-state)))

(defn update-game-state [game-state game-output]
  (let [; map out possible moves from current pos
        available-move (into #{} (map keyword (game-output :doors)))

        update-item-map (fn [s] (assoc-in s [:item-map (pos s)] (game-output :items)))
        remove-item (fn [s] (update s :item-map dissoc (pos s)))
        update-room-map (fn [s]
                          (let [current-room (game-output :room)
                                previous-room (second (:pos s))
                                s (if (contains? (s :rooms) current-room)
                                    s
                                    (assoc-in s [:rooms current-room] {}))
                                current-doors (get-in s [:rooms current-room])
                                removed-doors (remove available-move
                                                      (keys (get-in s [:rooms current-room])))
                                added-doors (remove current-doors available-move)]
                            (cond-> s
                              (contains? (s :rooms)
                                         current-room) (assoc :removed-doors removed-doors)


                              true (update-in [:rooms current-room] into-map (interleave added-doors
                                                                                         (repeat nil)))

                              (s :last-move) (assoc-in [:rooms current-room (oposite (s :last-move))]
                                                       previous-room)
                              (s :last-move) (assoc-in [:rooms previous-room (s :last-move)]
                                                       current-room))))

        ; we should update the map etc if the last output describes a room different from the last one we visited
        after-move (and (game-output :room)
                        (not= (game-output :room)
                              (pos game-state)))]
    (log/debug "last move: " (game-state :last-move))
    (log/debug "pos-stack: " (pr-str (game-state :pos)))
    (cond-> game-state
      true                     (assoc :after-move after-move)
      after-move               (update :pos conj (game-output :room))
      ; doors may change between visits?
      after-move               update-room-map
      (game-output :room)      (assoc :doors available-move)
      (game-output :room)      update-item-map
      (game-output :confirm-taken) remove-item
      ; needs to recognize drop to track inventory
      ;(game-output :confirm-taken) (update :inventory conj (game-output :confirm-taken))
      )))

;(-> {:rooms {"bar" {:east nil}} :last-move :east :item-map {} :pos '("bar" "baz")}
;    (update-game-state {:room "foo" :doors ["west" "east" "north"]})
;    (assoc :last-move :north)
;    (update-game-state {:room "blah" :doors ["south"] :items ["this" "that"]}))

;{:rooms {"bar" {:east "foo"}, "foo" {:west "bar", :east nil, :north "blah"}, "blah" {:south "foo"}},
; :last-move :north,
; :item-map {"foo" nil, "blah" ["this" "that"]},
; :pos ("blah" "foo" "bar" "baz"),
; :removed-doors (),
; :doors #{:south}}

(defn print-game-output [game-output]
  (cond (:room game-output)
        (do (println "In room:" (:room game-output))
            (doseq [l (:desc game-output)]
              (println l)))

        (:confirm-taken game-output)
        (println "You take the" (:confirm-taken game-output))

        :else
        (doseq [l (game-output :lines)] (println l)))

  (when (:weight game-output)
    (println "You are too" (case (:weight game-output)
                             :too-heavy "too heavy"
                             :too-light "too light"
                             "something???"))))

(defn print-available-options [game-state]
  (let [print-opts (fn [s o]
                     (when (not-empty o)
                       (println s (str/join ", " o))))]
    (print-opts "Doors from here:" (:doors game-state))
    (print-opts "Items to take  :" (get-in game-state [:item-map (pos game-state)]))
    (doseq [i (get-in game-state [:item-map (pos game-state)])]
      (when (dangerous-items i)
        (println "warning: don't take the" i)))))

(defn adjust-cmd [state cmd param]
  (if (= cmd :take)
    (let [items (get-in state [:item-map (pos state)])
          item-to-take (or (first (filter #(str/starts-with? % param) items))
                           param)]
      [cmd item-to-take])
    [cmd param]))

; representation of rooms:
; rooms -> map from name to map of directions to adjoining room
; {
;    "room-name 1" -> { :east -> "room-name 2"
;                       :west -> "room-name 3" }
; }
; doors to unknown rooms point to nil

; connected with channels:


;     user      <------------------------------------------------------ std out
;      |                                                                  ^
;      v           +-----------+    +--------------+    +-----------+     |
; command-input -> | game-loop | -> | command-loop | -> |   robot   |     |
;                  +-----------+    +--------------+    +-----------+     |
;                       ^                                     |           |
;                       |                                     v           |
;                  +-----------+                        +-----------+     |
;                  |parse-loop | <--------------------- | read-loop | ----+
;                  +-----------+                        +-----------+ 

;{:room "Engineering",
; :desc ["You see a whiteboard with plans for Springdroid v2."],
; :doors ["north" "east" "south"],
; :items ["astrolabe"]}

; game loop
; - read output
; - update map
; - get command
; - if command is legal move: update position
; - execute command

(defn game-controller [program user-in-ch]
  (log/info "starting game")
  (let [get-command (fn [] (let [c (<!! user-in-ch)]
                             (cond (string? c) [(keyword c)]
                                   (nil? c) nil
                                   (and (seqable? c)
                                        (>= (count c) 1)) c
                                   :else nil)))

        command-in-ch (chan)
        state-ch (->> (command-loop command-in-ch)
                      (robot program)
                      (read-loop)
                      (parse-loop))]

    (loop [state {:rooms {}
                  :item-map {}
                  :pos (list)
                  :last-move nil
                  :inventory #{}
                  :flats #{}}]
      (log/info "waiting for game output")
      (when-let [game-output (<!! state-ch)]
        (let [{:keys [chart] :as state} (update-game-state state game-output)]
          (log/debug "state:" game-output)
          (try
            (print-game-output game-output)
            (print-available-options state)
            (when (:after-move state)
              (print-map (:rooms state) (pos state)))
            (catch Exception e (log/error e "error printing"))
            (finally (flush)))

          (let [safe-item (first (remove dangerous-items (:items game-output)))]
            (if (not (nil? safe-item))
              ; auto-take
              (do (>!! command-in-ch [:take safe-item])
                  (recur (assoc state :last-move nil)))
              ; wait for command
              (when-let [[cmd param] (get-command)]
                (let [[cmd param] (adjust-cmd state cmd param)]
                  (log/debug "send" [cmd param])
                  (>!! command-in-ch [cmd param])
                  (if (is-move? cmd)
                    (recur (assoc state :last-move cmd))
                    (recur (assoc state :last-move nil))))))))))

    (close! command-in-ch))
  "game over")


(def program (ic/parse (get-data 2019 25)))
(def game-input (chan))
(def game-thread (thread (game-controller program game-input)))

(do
  (>!! game-input [:west])
  (>!! game-input [:north])
  (>!! game-input [:south])
  (>!! game-input [:east])
  (>!! game-input [:south])
  (>!! game-input [:east])
  (>!! game-input [:north])
  (>!! game-input [:south])
  (>!! game-input [:west])
  (>!! game-input [:north])
  (>!! game-input [:east])
  (>!! game-input [:north])
  (>!! game-input [:north])
  (>!! game-input [:south])
  (>!! game-input [:south])
  (>!! game-input [:south])
  (>!! game-input [:east])
  (>!! game-input [:west])
  (>!! game-input [:west])
  (>!! game-input [:north])
  (>!! game-input [:east])
  (>!! game-input [:north]))


(>!! game-input [:south])
(>!! game-input [:north])
(>!! game-input [:east])
(>!! game-input [:west])
(>!! game-input [:inv])
(>!! game-input [:take "coin"])
(>!! game-input [:drop "hypercube"])

(>!! game-input [:east])
(>!! game-input [:drop "hypercube"])
(>!! game-input [:drop "astrolabe"])
(>!! game-input [:drop "pointer"])
(>!! game-input [:drop "coin"])
(>!! game-input [:drop "mug"])
(>!! game-input [:drop "easter egg"])
(>!! game-input [:drop "candy cane"])

(close! game-input)
(<!! game-thread)



