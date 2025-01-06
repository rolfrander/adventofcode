(ns advent2019.day25
  (:require
   [advent2019.intcode :as ic]
   [clojure.core.async :refer [<!! >!! thread chan close!]]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [rolfrander.puzzle-lib :refer [get-data digits]]
   [flatland.useful.map :refer [into-map map-vals]]))

(comment def example-data "== Engineering ==
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
                (when-not (or (nil? command-kw)
                              (= command-kw :quit))
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
                    ;(log/debugf "parsed '%s': %s" token value)
                    (case token
                      :cmd
                      (do (>!! output-ch (assoc ret 
                                                :expect-cmd true))
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

                      (:too-heavy,:too-light)
                      (do (>!! output-ch (assoc ret
                                                :expect-cmd false
                                                :weight token))
                          (recur :top {}))

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

(defn find-path [rooms current target]
  (let [node-list (loop [q (conj clojure.lang.PersistentQueue/EMPTY {:node current :parent nil :path nil})
                    visited #{current}]
               (if (empty? q)
                 nil
                 (if (= (:node (peek q)) target)
                   (peek q)
                   (let [v (peek q)
                         q (pop q)
                         [q visited] (reduce (fn [[q visited] [edge w]]
                                               (if (visited w)
                                                 [q visited]
                                                 [(conj q {:node w :parent v :path edge})
                                                  (conj visited w)]))
                                             [q visited] (rooms (:node v)))]
                     (recur q visited)))))]
    (loop [n node-list
           p '()]
      (if (nil? (:parent n))
        p
        (recur (:parent n)
               (conj p (:path n)))))))

(comment let [rooms {"Storage" {:south "Hot Chocolate Fountain"},
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
             "Science Lab" {:west "Engineering", :east "Sick Bay", :north "Passages"}}]

  ;((comp keys rooms) "Passages")
  (find-path rooms "Sick Bay" "Storage")
  )

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
                       "giant electromagnet"
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
  (cond (= cmd :take)
        (let [items (get-in state [:item-map (pos state)])
              item-to-take (or (first (filter #(str/starts-with? % param) items))
                               param)]
          [cmd item-to-take])

        (= cmd :goto)
        (let [rooms (keys (:rooms state))
              room (or (first (filter #(str/starts-with? % param) rooms))
                               param)]
          [cmd room])

        :else
        [cmd param]))

(defn- toggle-flag [state param]
  (let [state (cond-> state
                ((:flags state) param)       (update :flags disj param)
                (not ((:flags state) param)) (update :flags conj param)
                true                         (assoc :waiting-for-user true))]
    (println "current flags:" (:flags state))
    state))

(defn- print-game [input-message state]
  (try
    (print-game-output input-message)
    (print-available-options state)
    (when (state :after-move)
      (print-map (state :rooms) (pos state)))
    (catch Exception e (log/error e "error printing"))
    (finally (flush))))

(defn- goto [state target command-in-ch]
  (let [path (find-path (state :rooms) (pos state) target)]
    (log/debug "goto-path: " path)
    (>!! command-in-ch {:cmd :toggle :params [:noprint]})
    (doseq [p path]
      (>!! command-in-ch {:cmd p}))
    (>!! command-in-ch {:cmd :toggle :params [:noprint]}))
  state)

(defn construct-weight-plan [items]
  (let [items (vec items)
        gray-code (fn [i] (bit-xor i (bit-shift-right i 1)))]
    (->> (for [i (range (bit-shift-left 1 (count items)))] (gray-code i))
         next
         (reduce (fn [[r prev] cur]
                   (let [n (loop [d (bit-xor prev cur) i 0]
                             (if (<= d 1) i (recur (bit-shift-right d 1) (inc i))))]
                     [(conj r [(if (> prev cur) :take :drop) (get items n)]) cur]))
                 [[] 0])
         first)))

(defn- handle-weight-plan [state input-message command-in-ch]
  (if (or (empty? (:weight-plan state))
          (= (:room input-message) "Pressure-Sensitive Floor"))
    (-> state
        (dissoc :weight-plan)
        (assoc :waiting-for-user (:expect-cmd input-message)
               :last-move nil))
    (do (>!! command-in-ch (first (:weight-plan state)))
        (-> state
            (update :weight-plan next)
            (assoc :waiting-for-user false
                   :last-move nil)))))



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
  (let [annotate-message (fn [m from] (if (nil? m)
                                        nil
                                        (assoc m :from from)))

        command-in-ch (chan 30)
        state-ch (->> (command-loop command-in-ch)
                      (robot program)
                      (read-loop)
                      (parse-loop))]
    ;(pipe state-ch user-in-ch)
    (try
      (loop [state {:rooms {}
                    :item-map {}
                    :pos (list)
                    :last-move nil
                    :inventory #{}
                    :flags #{}
                    :waiting-for-user false}]
        (log/info "waiting for input from game or user")
        (when-let [input-message (if (:waiting-for-user state)
                                   (annotate-message (<!! user-in-ch) :user)
                                   (annotate-message (<!! state-ch)   :game))]
          (log/debug "input-message: " (pr-str input-message))
          (if (nil? input-message)

            nil ; game over

            (case (input-message :from)
              :user
              (let [{:keys [cmd params]} input-message
                    param (first params) ; assume only one for now...
                    [cmd param] (adjust-cmd state cmd param)]
                (case cmd
                  :toggle
                  (recur (toggle-flag state param))

                  :rooms
                  (do (doseq [r (keys (:rooms state))]
                        (println r))
                      (recur state))

                  :goto
                  (-> (goto state param user-in-ch)
                      (assoc :waiting-for-user true)
                      (recur))

                  :find-weight
                  (do (>!! command-in-ch [:inv nil])
                      (recur (assoc state
                                    :waiting-for-user false
                                    :weight-plan :start)))
                  ;; default
                  (do
                    (log/debug "send" [cmd param])
                    (>!! command-in-ch [cmd param])
                    (recur (assoc state
                                  :last-move (if (is-move? cmd) cmd nil)
                                  :waiting-for-user false)))))

              :game
              (let [state (update-game-state state input-message)
                    safe-items (remove dangerous-items ((:item-map state) (pos state)))]
                (when-not ((state :flags) :noprint)
                  (print-game input-message state))

                (cond (and ((:flags state) :autotake)
                           (seq safe-items))
                      (do (log/debug "autotake")
                          (>!! command-in-ch [:take (first safe-items)])
                          (recur (assoc state
                                        :last-move nil
                                        :waiting-for-user false)))

                      (and (:inventory input-message)
                           (= (:weight-plan state) :start))
                      (do (log/debug "start weight-plan")
                          (>!! command-in-ch [:east]) ; only east for now...
                          (-> state
                              (assoc :weight-plan (construct-weight-plan (:inventory input-message))
                                     :waiting-for-user false
                                     :last-move :east)
                              recur))

                      (and (:weight-plan state) (:room input-message) (:expect-cmd input-message))
                      (do (log/debug "next in weight-plan: " (first (:weight-plan state)))
                          (recur (handle-weight-plan state input-message command-in-ch)))

                      (and (:weight-plan state) (or (:confirm-dropped input-message)
                                                    (:confirm-taken input-message)))
                      (do (log/debug "in weight-plan, moving east")
                          (>!! command-in-ch [:east])
                          (-> state
                              (assoc :waiting-for-user false
                                     :last-move :east)
                              recur))
                      
                      :else
                      (do (log/debug "repeat")
                          (recur (assoc state
                                        :last-move nil
                                        :waiting-for-user (:expect-cmd input-message))))))

              (do (log/warn "shouldn't be here, input-message:" input-message)
                  (recur state))))))
      (catch Exception e
        (log/fatal e "Uncaught exception in game loop")))

    (close! command-in-ch))
  (log/info "game over")
  "game over")

(def program (ic/parse (get-data 2019 25)))
(def game-input (chan 30))
(def game-thread (thread (game-controller program game-input)))

(defn user-input [cmd & params]
  (>!! game-input {:cmd cmd
                   :params params}))

(user-input :toggle :autotake)
(do
  (user-input :west)
  (user-input :north)
  (user-input :south)
  (user-input :east)
  (user-input :south)
  (user-input :east)
  (user-input :north)
  (user-input :south)
  (user-input :west)
  (user-input :north)
  (user-input :east)
  (user-input :north)
  (user-input :north)
  (user-input :south)
  (user-input :south)
  (user-input :south)
  (user-input :east)
  (user-input :west)
  (user-input :west)
  (user-input :north)
  (user-input :east)
  (user-input :north))

(user-input :goto "Gift")
(user-input :find-weight)
(user-input :rooms)


(user-input :inv)
(user-input :drop "pointer")
(user-input :take "easter egg")
(user-input :east)

(close! game-input)
(<!! game-thread)
