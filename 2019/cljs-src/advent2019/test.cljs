(ns advent2019.test)

(defn- query [query]
  (.querySelector js/document query))

(let [ctx (.getContext (query "#game") "2d")]
  (set! (.-fillStyle ctx) "#0a0")
  (.fillRect ctx 10 10 280 130))

(.-width (.getBoundingClientRect (query "#game-wrapper")))

(defn- append-html [element html]
  (.insertAdjacentHTML element "beforeend" html))

(defn- message-html [message]
  (str "<li>" message "</li>"))




(.addEventListener js/window
                   "resize"
                   (fn [e] (debug (.-type e))))

(advent2019.client/debug "test")
