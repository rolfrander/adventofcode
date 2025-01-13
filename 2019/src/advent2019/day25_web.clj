(ns advent2019.day25-web
  (:require 
   [clojure.core.async :refer [chan mult tap pipe thread <!! >!! close!]]
   [clojure.tools.logging :as log]
   [advent2019.day25 :as game]
   [ring.adapter.jetty :refer [run-jetty]]
   [reitit.ring :as r]
   [ring.websocket.async :as wsa]
   [ring.websocket.transit :as wst]
   [ring.websocket.keepalive :as wska]
   ))

(defn ws-game-controller [client-in-ch client-out-ch]
  (log/info "starting game")
  (let [annotate-message (fn [m from] (if (nil? m)
                                        nil
                                        (assoc m :from from)))

        command-in-ch (chan 30)
        state-ch (->> (game/command-loop command-in-ch)
                      (game/robot (game/get-program))
                      (game/read-loop)
                      (game/parse-loop))]
    ;(pipe state-ch user-in-ch)
    (try
      (log/debug "wait for start message")
      (let [start-msg (<!! client-in-ch)]
        (if (= :start (:cmd start-msg))
          (log/info "game started by client")
          (log/warn "unexpected start-message, starting anyway: " start-msg)))

      (>!! client-out-ch {:debug "game started"})

      (loop [state {:rooms {}
                    :item-map {}
                    :pos (list)
                    :last-move nil
                    :inventory #{}
                    :flags #{}
                    :waiting-for-user false}]
        (log/info (str "waiting for input from "
                       (if (:waiting-for-user state)
                         "user" "game")))
        ; read data from game, unless the game is waiting for user input
        (when-let [input-message (if (:waiting-for-user state)
                                   (annotate-message (<!! client-in-ch) :user)
                                   (annotate-message (<!! state-ch)     :game))]
          (log/debug "input-message: " (pr-str input-message))
          (if (nil? input-message)

            nil ; game over

            (case (input-message :from)
              :user
              (let [{:keys [cmd params]} input-message
                    param (first params)]
                (case cmd
                  :toggle
                  (recur (game/toggle-flag state param))

                  :rooms
                  (do (doseq [r (keys (:rooms state))]
                        (println r))
                      (recur state))

                  :reload
                  (do (>!! client-out-ch state)
                      (recur state))

                  :goto
                  (-> (game/goto state param client-in-ch)
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
                                  :last-move (if (game/is-move? cmd) cmd nil)
                                  :waiting-for-user false)))))

              :game
              (let [state (game/update-game-state state input-message)
                    safe-items (remove game/dangerous-items ((:item-map state) (game/pos state)))]
                
                (>!! client-out-ch state)

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
                              (assoc :weight-plan (game/construct-weight-plan (:inventory input-message))
                                     :waiting-for-user false
                                     :last-move :east)
                              recur))

                      (and (:weight-plan state) (:room input-message) (:expect-cmd input-message))
                      (do (log/debug "next in weight-plan: " (first (:weight-plan state)))
                          (recur (game/handle-weight-plan state input-message command-in-ch)))

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

(defn make-game-handler [to-game from-game]
  (let [readers (mult from-game)]
    (fn handler [_request]
      (wsa/go-websocket [in out]
                        (tap readers out)
                        (pipe in to-game false)))))

(defn make-app-handler [to-game-ch from-game-ch]
  (r/ring-handler
   (r/router ["/chat" (make-game-handler to-game-ch from-game-ch)])
   (r/routes
    ; https://cljdoc.org/d/metosin/reitit/0.7.2/api/reitit.ring?q=create-default-handler#create-resource-handler
    (r/create-resource-handler {:path "/"})
    (r/create-default-handler))
   {:middleware [[wst/wrap-websocket-transit]
                 [wska/wrap-websocket-keepalive]]}))

(defonce server (atom nil))


(defn start-game-server []
  (when (nil? @server)
    (let [to-game-ch (chan 3)
          from-game-ch (chan)
          game-thread (thread (ws-game-controller to-game-ch from-game-ch))
          jetty (run-jetty (make-app-handler to-game-ch from-game-ch)
                           {:port 4000 :join? false})]
      (reset! server {:jetty jetty
                      :game-thread game-thread
                      :command-ch to-game-ch}))))

(defn stop-server []
  (when-not (nil? @server)
    (let [{:keys [jetty game-thread command-ch]} @server]
      (.stop jetty)
      (close! command-ch)
      (log/debug "waiting for game-thread")
      (log/info (<!! game-thread))
      (reset! server nil))))

(start-game-server)
(stop-server)

(>!! (:command-ch @server) {:cmd :inv})

(<!! (:command-ch @server))

;(.reconfigure (org.apache.logging.log4j.LogManager/getContext false))
;(log/debug "test")
