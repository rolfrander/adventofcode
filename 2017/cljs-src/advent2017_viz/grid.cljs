(ns ^:figwheel-hooks advent2016-viz.day24
  (:require [cljsjs.d3]
            [cljs.core.async :refer [<!]] 
            [cljs-http.client :as http]
            [clojure.string :as str])
    (:require-macros [cljs.core.async.macros :refer [go]]))

(def app-state (atom {:info nil}))

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
        (.text predata))))

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
                "square-dim: " square-dim))
    (doseq [{:keys [x y class style info]} (:data data)]
      (cond-> svg
        true  (.append "rect")
        true  (.attr "x" (str (* (- x min-x) square-dim)))
        true  (.attr "y" (str (* (- y min-y) square-dim)))
        true  (.attr "width" (str square-dim))
        true  (.attr "height" (str square-dim))
        class (.attr "class" class)
        info  (.attr "info" info)
        style (.attr "style" style)
        true  (.on "mouseover" #'mouseover-handler))))
  )

(defn load-data [url]
  (go (let [get-json #(js->clj (.parse js/JSON (:body %)) :keywordize-keys true)
            data-ch (http/get url)]
        (let [unparsed-data (<! data-ch)]
          (if (not= 200 (:status unparsed-data))
            (show-error (:error-text unparsed-data))
            (grid (get-json unparsed-data)))))))

(defn url-submit [_event]
  (let [info (:info @app-state)
        url-input (-> js/d3
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
      (.append "svg")))

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
    (-> (.append form "input")
        (.attr "type" "submit")
        (.on "click" #'url-submit))))

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
    (swap! app-state assoc :info info)
    ))


(defn ^:after-load setup []
  (println "js reload")
  (remove-contents)
  (main))

