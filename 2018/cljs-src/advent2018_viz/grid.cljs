(ns ^:figwheel-hooks advent2018-viz.grid
  (:require [cljsjs.d3]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [clojure.string :as str])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def app-state (atom {:info nil
                      :data nil}))

(defn clear-debug []
  (let [info-panel (:info @app-state)]
    (-> info-panel
        (.select "#debug")
        (.remove))
    (-> info-panel
        (.select "#error")
        (.remove))))

(defn debug [predata]
  (let [info-panel (:info @app-state)]
    (-> info-panel
        (.select "#debug")
        (.remove))
    (-> info-panel
        (.append "pre")
        (.attr "id" "debug")
        (.text predata)))
  predata)

(defn show-error [msg]
  (let [info-panel (:info @app-state)]
    (-> info-panel
        (.select "#error")
        (.remove))
    (-> info-panel
        (.append "p")
        (.attr "id" "error")
        (.text msg))))

(defn mouseover-handler [event]
  (let [info-panel (:info @app-state)
        text (-> (.-target event)
                 (.getAttribute "info")
                 (str/split #" *, *"))]
    (-> info-panel
        (.select "#mouseover")
        (.remove))
    (let [div (-> info-panel
                  (.append "div")
                  (.attr "id" "mouseover"))]
      (doseq [t text]
        (-> div
            (.append "p")
            (.text t))))))

(defn max-by [key coll]
  (apply max (map key coll)))

(defn grid
  "Draws a map of squares.
   Input is assumed to have the following format:
   {:xrange [min max]
    :yrange [min max]
    :data [elements...]}
   where each element is:
   {:x :y :class (css-class-names as a string) :style (optional, additional css styling) :info (string to show user)}"
  [data]

  (-> js/d3
      (.selectAll "#app svg *")
      (.remove))

  (let [svg    (.select js/d3 "#app svg")
        width  (.-value (.-baseVal (.-width (.node svg))))
        height (.-value (.-baseVal (.-height (.node svg))))
        [min-x max-x] (:xrange data)
        [min-y max-y] (:yrange data)
        square-dim (min (/ (float height) (inc (float (- max-y min-y))))
                        (/ (float width)  (inc (float (- max-x min-x)))))]
    (debug (str "max-x: " max-x "\r\n"
                "max-y: " max-y "\r\n"
                "square-dim: " square-dim "\r\n"
                (:debug data)))
    (doseq [{:keys [x y class style info]} (:data data)]
      (cond-> svg
        true  (.append "rect")
        true  (.attr "x" (str (* (- x min-x) square-dim)))
        true  (.attr "y" (str (* (- y min-y) square-dim)))
        true  (.attr "width" (str (* square-dim .7)))
        true  (.attr "height" (str (* square-dim .7)))
        class (.attr "class" class)
        info  (.attr "info" info)
        style (.attr "style" style)
        true  (.on "mouseover" #'mouseover-handler)))))


(defn set-simulate-fn [function]
  (swap! app-state assoc :simulate function))

(defn set-prepare-fn [function]
  (swap! app-state assoc :prepare function))

(defn set-data-fn [function]
  (swap! app-state assoc :get-data function))

(defn simulate-step [_event]
  (let [f (:simulate @app-state)
        state (swap! app-state update :data f)]
    (grid (:data state))))

(defn simulate-start [event]
  (if (:run @app-state)
    (do (js/clearInterval (:run @app-state))
        (swap! app-state dissoc :run)
        (.attr (:simulate-button @app-state) "value" "start simulering"))
    (do (swap! app-state assoc :run (js/setInterval #(simulate-step nil) 250))
        (.attr (:simulate-button @app-state) "value" "stopp simulering"))))

(defn get-json [d]
  (js->clj (.parse js/JSON (:body d)) :keywordize-keys true))

(defn load-data [url]
  (go (let [data-ch (http/get url) 
            unparsed-data (<! data-ch)]
        (if (not= 200 (:status unparsed-data))
          (show-error (:error-text unparsed-data))
          (let [data ((:prepare (debug @app-state)) (get-json unparsed-data))]
            (swap! app-state assoc :data data)
            (grid data))))))


(defn url-submit [_event]
  (let [url-input (-> js/d3
                      (.select "#app input#url")

                      (.property "value"))
        url-input (str "/api/" url-input)]
    (clear-debug)
    (comment debug (->> url-input
                        js-keys
                        js->clj
                        (str/join "\r\n")))
    (load-data url-input)))


(defn remove-contents []
  (-> js/d3
      (.selectAll "#app *")
      (.remove)))

(defn append-svg []
  (-> js/d3
      (.select "#app")
      (.append "svg")
      (.attr "class" "viz")))

(defn button [container label callback]
  (-> (.append container "input")
      (.attr "type" "submit")
      (.attr "value" label)
      (.on "click" callback)))

(defn append-apiform []
  (let [form (-> js/d3
                 (.select "#app")
                 (.append "div")
                 (.attr "class" "apiform")
                 (.append "form")
                 (.attr "action" "#"))]
    (-> (.append form "label")
        (.text "URL"))
    (-> (.append form "span")
        (.text "/api/")
        (.attr "class" "code")
        (.style "margin-left" "3em"))
    (-> (.append form "input")
        (.attr "type" "text")
        (.attr "size" 40)
        (.attr "id" "url"))
    (button form "hent data" #'url-submit)
    (button form "simuleringssteg" #'simulate-step)
    (swap! app-state 
           assoc :simulate-button (button form "start simulering" #'simulate-start))))


(defn append-div []
  (-> js/d3
      (.select "#app")
      (.append "div")
      (.attr "class" "info")))

(defn ^:export main []
  (println "main")
  (let [api-url (append-apiform)
        svg (append-svg)
        info (append-div)]
    (swap! app-state assoc :info info)))

(defn ^:after-load setup []
  (println "js reload")
  (remove-contents)
  (main))

(defonce start-up (setup))