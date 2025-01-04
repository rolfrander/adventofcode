(ns advent2019.day25
  (:require
   [advent2019.intcode :as ic]
   [clojure.core.async :refer [<!! >!! thread chan close!]]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [rolfrander.puzzle-lib :refer [get-data]]))

(def example-data "== Engineering ==
You see a whiteboard with plans for Springdroid v2.

Doors here lead:
- north
- east
- south

Items here:
- astrolabe

Command?")

(def commands {:north {:cmd "north" :params 0 :direction [ 0 -1] :oposite-dir :south}
               :south {:cmd "south" :params 0 :direction [ 0  1] :oposite-dir :north}
               :east  {:cmd "east"  :params 0 :direction [ 1  0] :oposite-dir :west}
               :west  {:cmd "west"  :params 0 :direction [-1  0] :oposite-dir :east}
               :take  {:cmd "take"  :params 1}
               :drop  {:cmd "drop"  :params 1}
               :inv   {:cmd "inv"   :params 0}
               ;:quit  {}
               })

(defn move [pos direction]
  (map + pos (get-in commands [direction :direction] [0 0])))

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
                     [:text  #".*"]]]
    (some (fn [[type rule]]
            (when-let [m (re-matches rule line)]
              [type (if (vector? m) (peek m) m)]))
          token-rules)))

;(map get-token (str/split-lines example-data))

(defn parse-loop [input-ch]
  (let [

        vec-append (fn [v l] (if (nil? v) [l] (conj v l)))
        output-ch (chan)]
    (thread (loop [state :top
                   ret {}]
              (let [l (<!! input-ch)
                    ret (update ret :lines vec-append l)]
                (if (nil? l)
                  (>!! output-ch ret)

                  (let [[token value] (get-token l)]
                    (log/debugf "parsed '%s': %s %s" l token value)
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
                      (recur :room (assoc ret :room value :desc []))

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

                      :text
                      (if (= state :room)
                        (recur :room (update ret :desc conj l))
                        ; getting here usually means game over, don't know how to handle this yet
                        (recur state (update ret :illegal-lines vec-append [state l])))
                      
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

(defn print-map [m]
  (let [[max-x max-y min-x min-y] (for [k [max-key min-key] d [first second]]
                                    (d (apply k d (keys m))))]
    (doseq [y (range min-y (inc max-y))
            x (range min-x (inc max-x))]
      (when (= x min-x) (newline))
      (print (get m [x y] \space)))
    (newline)))

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
                       "giant electromagned"})

(defn pos [game-state] (peek (:pos game-state)))

(defn update-game-state [game-state game-output]
  (let [; map out possible moves from current pos
        available-move (into #{} (map keyword (game-output :doors)))
        
        update-item-map (fn [s] (assoc-in s [:item-map (pos s)] (game-output :items)))
        remove-item (fn [s] (update s :item-map dissoc (pos s)))
        update-chart-with-current-pos (fn [s] (assoc-in s [:chart (pos s)] \.))
        update-chart-with-doors (fn [s]
                       (reduce (fn [st dir] (assoc-in st [:chart (move (pos st) dir)]
                                                      (if (available-move dir) \. \#)))
                               s [:north :south :east :west]))
        ]
    (cond-> game-state

      ; previous move was illegal
      (game-output :cant-move) (update :pos pop)
      true                     update-chart-with-current-pos

      (game-output :room)      update-chart-with-doors
      (game-output :room)      (assoc :doors available-move)
      (game-output :room)      update-item-map
      (game-output :confirm-taken) remove-item
      ; needs to recognize drop to track inventory
      ;(game-output :confirm-taken) (update :inventory conj (game-output :confirm-taken))
      )))

(defn print-game-output [game-output]
  (cond (:room game-output)
        (do (println "In room:" (:room game-output))
            (println (:desc game-output)))
        
        (:confirm-taken game-output)
        (println "You take the" (:confirm-taken game-output))

        :else
        (doseq [l (game-output :lines)] (println l))))

(defn print-available-optioins [game-state]
  (let [print-opts (fn [s o]
                     (when (not-empty o)
                       (println s (str/join ", " o))))]
    (print-opts "Doors from here:" (:doors game-state))
    (print-opts "Items to take  :" (get-in game-state [:item-map (pos game-state)]))
    (doseq [i (get-in game-state [:item-map (pos game-state)])]
      (when (dangerous-items i)
        (println "warning: don't take the" i)))))

; representation of rooms:
; rooms -> map from name to map of directions to adjoining room
; {
;    "room-name 1" -> { :east -> "room-name 2"
;                       :west -> "room-name 3" }
; }

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
                  :pos (list [0 0])
                  :inventory #{}}]
      (log/info "waiting for game output")
      (when-let [game-output (<!! state-ch)]
        (let [{:keys [chart] :as state} (update-game-state state game-output)]
          (log/debug "state:" game-output)
          (try
            (print-game-output game-output)
            (print-available-optioins state)
            (print-map (assoc chart (pos state) \@))
            (catch Exception e (log/error e "error printing"))
            (finally (flush)))

          ; wait for command
          (when-let [[cmd param] (get-command)]
            (log/debug "send" [cmd param])
            (>!! command-in-ch [cmd param])
            (if (is-move? cmd)
              (recur (update state :pos conj (move (pos state) cmd)))
              (recur state))))))

    (close! command-in-ch))
  "game over")


(def program (ic/parse (get-data 2019 25)))
(def game-input (chan))
(def game-thread (thread (game-controller program game-input)))

(>!! game-input [:south])
(>!! game-input [:north])
(>!! game-input [:east])
(>!! game-input [:west])
(>!! game-input [:inv])
(>!! game-input [:take "candy cane"])
(>!! game-input [:drop "easter egg"])

(close! game-input)
(<!! game-thread)


(let [game-output (->> (command-loop game-input)
                       ;(robot program)
                       ;(read-loop)
                       )]
  (thread (loop []
            (when-let [l (<!! game-output)]
              (println l)
              (recur)))))

