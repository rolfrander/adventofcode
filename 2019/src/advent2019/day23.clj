(ns advent2019.day23
  (:require
   [advent2019.intcode :as ic]
   [clojure.core.async :refer [<!! >!! poll! chan thread close! sliding-buffer]]
   [rolfrander.puzzle-lib :as puzzle]))

(def program (ic/parse (puzzle/get-data 2019 23)))

(def debug-channel (chan (sliding-buffer 20)))

(let [msgno (atom 0)]
  (defn debug [src & msg]
    (>!! debug-channel {:id (swap! msgno inc) :src src :msg (apply str msg)})))

; network-packet: 
; {:type [:init, :payload, :exit, :idle]
;  :src 
;  :dst :x :y (type=:payload)
;  :exitvalue (type=:exit)
; }
; command-packet to router:
; {:type :cmd
;  :cmd (payload, currently only [:connect (compupter-channel)] which runs the connection-routine)
; }
; each computer: thread running ic/interpret
;  - connected to two channels for input and output
;  - all output goes to router (computer-output-channel is router-input-channel)
;  - all input comes from router (router connected to all input-channels)
; router: 1 input, n output
; no separate abstraction for network?
; connection-routine:
; - start computer thread with input set to a new input-channel and output set to the router input-channel
; - connect input-channel to router
; - router assigns network number and send bootstrap package, :type set to :init
; - computer recieves bootstrap-package and sets it's own network number

(defn computer
  [program output]
  (let [id (atom -1)
        input (chan 50)

        input-fn (fn [state]
                   (merge state
                          (if (not (contains? state :id)) ; uninitialized, wait for first message
                            (let [{:keys [dst]} (<!! input)]
                              (reset! id dst)
                              {:id dst :unread-y nil :value dst :idle false})

                            (if-let [y (:unread-y state)]
                              {:unread-y nil :value y :idle false} ; y still waiting in buffer

                              (if-let [{:keys [dst x y type] :as packet} (poll! input)]
                                (do ;(debug dst "recv" packet ", input x=" x ", state:" state)
                                  (case type

                                    :payload
                                    {:unread-y y :value x :idle false} ; message received, save y for later, return x
                                    
                                    :exit
                                    (throw (RuntimeException. (str "forced exit from " dst)))

                                    nil))

                                     ; no message recieved, return -1
                                (if (:idle state)
                                  (let [{:keys [idle-cnt]} state]
                                    (when (zero? idle-cnt)
                                      ; idle for 5 rounds, notify router
                                      (>!! output {:src @id :type :idle}))
                                    (Thread/sleep 20)
                                    {:value -1 :idle true :idle-cnt (max (dec idle-cnt) -1)})

                                  ; switch from non-idle to idle, starting counter
                                  {:value -1 :idle true :idle-cnt 5}
                                  )
                                
                                ;(do (when-not (:idle state)
                                ;           ; notify router that we just changed to idle-state
                                ;      (>!! output {:src @id :type :idle}))
                                ;    (Thread/sleep 20)
                                ;    {:value -1 :idle true})
                                )))))

        output-fn (fn [state val]
                    (let [state (or state {})
                          buffer (get state :buffer [])
                          src (get state :src @id)
                          [dst x y :as buffer] (conj buffer val)]
                      
                      (if (= 3 (count buffer))
                        (let [packet {:src @id
                                      :dst dst
                                      :type :payload
                                      :x x
                                      :y y}]
                          ;(debug src "send" packet)
                          (>!! output packet)
                          {:buffer []})

                        (assoc state :buffer buffer))))]

    (thread (let [ret (try (ic/interpret program
                                         :input-fn input-fn
                                         :output-fn output-fn)
                           (catch RuntimeException _e
                             "forced exit"))]
              (>!! output {:src @id :type :exit :exitvalue ret})
              ret))
    input))

; network-packet: 
; {:type [:init, :payload, :exit]
;  :src (don't really need this, but it might make debugging easier)
;  :dst
;  :x :y (type=:payload)
;  :exitvalue (type=:exit)
; }
; command-packet to router:
; {:type :cmd
;  :cmd payload, one of 
;        - [:connect (compupter-channel)] initializes computer comms
;        - [:start] which runs the connection-routine for all initialized computers
;        - [:exit] stops the router thread
; }
; router also stops automatically when all outputs have exited

(defn router
  [with-nat]
  (let [input (chan 50)
        outputs (atom [])

        send-to-all (fn [msg] (let [outputs @outputs]
                                (dotimes [id (count outputs)]
                                  (let [packet (merge {:dst id} msg)]
                                    (>!! (get outputs id) packet)))))

        start-time (System/currentTimeMillis)
        running (thread (loop [{:keys [idle-set exit-value] :as router-state} {:idle-set #{}
                                                                               :exit-value nil
                                                                               :nat-packet nil
                                                                               :nat-y-buffer nil
                                                                               :nat-packet-cnt 0
                                                                               :nat-packet-sent 0}
                               counter 0
                               last-report-time (System/currentTimeMillis)]
                          (if-not (nil? exit-value)
                            (do 
                              (send-to-all {:type :exit})
                              exit-value)
                            (let [state (merge router-state
                                               (let [{:keys [type src dst cmd] :as packet} (<!! input)]
                                                 (case type
                                                   :payload
                                                   (if (= dst 255)
                                                     (if with-nat
                                                       {:nat-packet packet
                                                        :nat-packet-cnt (inc (:nat-packet-cnt router-state))}
                                                       {:exit-value (:x packet)})

                                                     (if-let [recipient (get @outputs dst)]
                                                       (do (>!! recipient packet)
                                                           {:idle-set (disj idle-set recipient)})

                                                       (do (debug "RO" "unknown recipient: " dst)
                                                           nil)))

                                                   :idle ; get this message when computer idle-state goes from false to true
                                                   (let [idle (conj idle-set src)]
                                                     (if (and (= (count idle) (count @outputs))
                                                              (:nat-packet router-state)) ; power-saving mode
                                                       (let [{:keys [nat-packet nat-y-buffer]} router-state
                                                             {:keys [y]} nat-packet]
                                                              ; (debug "RO" "cnt: " counter ", NAT-packet: " nat-packet ", nat-y-buffer: " nat-y-buffer)
                                                         (>!! (get @outputs 0) (assoc nat-packet :dst 0))
                                                         (when (= y nat-y-buffer)
                                                           (debug "RO" "cnt: " counter ", y repeated: " nat-y-buffer))
                                                                ;{:exit-value y}
                                                         
                                                         {:idle-set (disj idle 0)
                                                          :nat-y-buffer y
                                                          :nat-packet nil
                                                          :nat-packet-sent (inc (:nat-packet-sent router-state))})

                                                       {:idle-set idle}))

                                                   :cmd
                                                   (let [[op param] cmd]
                                                     (case op
                                                       :exit
                                                       {:type :forced-router-exit}

                                                       :connect
                                                       (let [_outputs (swap! outputs conj param)]
                                                         nil)

                                                       :start
                                                       (do (send-to-all {:type :init})
                                                           nil)

                                                       :wakeup ;noop
                                                       nil

                                                       (do (debug "RO" "unknown cmd" cmd)
                                                           nil)))

                                                   :exit
                                                   (do (debug "RO" "computer exited:" src)
                                                       nil)

                                                   (do (debug "RO" "unknown packet-type" type)
                                                       nil))))
                                  
                                  t             (System/currentTimeMillis)
                                  delta-t       (- t last-report-time)
                                  running-total (- t start-time)]
                              (recur
                               (if (> running-total 30000)
                                   (assoc state :exit-value :timeout)
                                   state)
                               (inc counter)
                               ; statistics timer
                               (if (> delta-t 2000)
                                 (do (println (format "running-time: %.2f" (/ running-total 1000.0))
                                              "pkg-count:" counter
                                              "idle count:" (count idle-set)
                                              "nat-packets recieved:" (:nat-packet-cnt state)
                                              "nat-packets sent:" (:nat-packet-sent state)
                                              "msg/sec:" (int (* 1000.0 (/ (float counter) (float running-total)))))
                                     t)
                                 last-report-time))))))]

    {:input input :running running}))

(defn solve [with-nat]
  (let [{router-input :input router-running :running} (router with-nat)]
    (dotimes [_i 50]
      (let [computer-input (computer program router-input)]
        (>!! router-input {:type :cmd :cmd [:connect computer-input]})))
    
    (>!! router-input {:type :cmd :cmd [:start]})

    (loop [router-output (poll! router-running)
           cnt 1]
      (if (nil? router-output)
        (do (Thread/sleep 200)
            (when (= cnt 0) (>!! router-input {:type :cmd :cmd [:wakeup]}))
            (recur (poll! router-running)
                   (mod (inc cnt) 25)))
        router-output))))

(def debug-thread (thread (loop [continue true]
                            (when continue
                              (Thread/sleep 200)
                              (when-let [{:keys [id src msg]} (<!! debug-channel)]
                                (println id src msg)
                                (recur true))))))



(debug "TS" "foo")
;(<!! debug-channel)
;(close! debug-channel)
(solve false)
;;=> 22171
(solve true)
; different answers: 13324 12966 13609 13414 13354 13358
; feil: 13352
; feil: 13332 (for høy, så vidt jeg husker)
