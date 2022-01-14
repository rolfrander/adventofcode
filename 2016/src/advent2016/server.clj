(ns advent2016.server
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.util.response :refer [resource-response content-type]]
            [clojure.data.json :as json]
            [ring.util.codec :refer [form-decode]]
            [clojure.walk :refer [keywordize-keys]]
            [compojure.core :refer [defroutes GET]]
            [compojure.response :refer [render]]))

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
  (GET "/api/day24/:function" [function :as {:keys [params]}] {:body (json/write-str (advent2016.day24/dispatch function params))})
  (GET "/api/test/:function" [function :as {:keys [params]}] (str function "\n"
                                                                  params))
;  (GET "/api/hello-world" [] "hello world")
;  (GET "/" req (minimum-handler req))
  ;(route/resources "/" {:root "public"})
  (GET "/" [] (-> (resource-response "index.html" {:root "public"})
                  (content-type "text/html; charset=utf-8"))))


;(println (:body (advent {:uri "/api/test/map" :params {:a "b"} :request-method :get})))

(def server (run-jetty
             (wrap-defaults #'advent site-defaults)
             {:port 4000
              :join? false}))

;(.stop server)

;(re-matches #"/api/([a-z0-9-.]+)/([a-z0-9-.]+)\?(.*)")