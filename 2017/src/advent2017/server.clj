(ns advent2017.server
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.util.response :refer [resource-response content-type]]
            [clojure.data.json :as json]
            [compojure.core :refer [defroutes GET]]
            [advent2017.day22 :refer [dispatch] :rename {dispatch day22}]))

(defn minimum-handler [req]
  (println "minimum-handler" (:uri req))
  (or
   (when (= "/" (:uri req))
     (some-> (resource-response "index.html" {:root "public"})
             (content-type "text/html; charset=utf-8")))
   {:status 404
    :headers {"Content-Type" "text/html"}
    :body "Not found"}))


(defroutes advent
  (GET "/api/day22/:function" [function :as {:keys [params]}] 
    {:body (json/write-str (day22 function params))})
  (GET "/" [] (-> (resource-response "index.html" {:root "public"})
                  (content-type "text/html; charset=utf-8"))))


(defonce server (atom nil))

(reset! server
  (run-jetty
   (wrap-defaults #'advent site-defaults)
   {:port 4000
    :join? false}))

;(.stop @server)

;(re-matches #"/api/([a-z0-9-.]+)/([a-z0-9-.]+)\?(.*)")