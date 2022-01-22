(ns advent2017.server
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.util.response :refer [resource-response content-type]]
            [clojure.data.json :as json]
            [compojure.core :refer [defroutes GET]]
            ;[advent2017.day22 :refer [dispatch] :rename {dispatch day22}]
            [rolfrander.puzzle-lib :refer [get-data str->long]]))

(defn minimum-handler [req]
  (println "minimum-handler" (:uri req))
  (or
   (when (= "/" (:uri req))
     (some-> (resource-response "index.html" {:root "public"})
             (content-type "text/html; charset=utf-8")))
   {:status 404
    :headers {"Content-Type" "text/html"}
    :body "Not found"}))

(defn invoke-dispatch [module function params]
  (let [f (ns-resolve (find-ns (symbol (str "advent2017." module)))
                      (symbol "dispatch"))]
    (when f (f function params))))

(defroutes advent
  (GET "/api/:module/:function" [module function :as {:keys [params]}] 
    {:body (json/write-str (invoke-dispatch module function params))})
  (GET "/api/data/:year/:date" [year date]
    {:body (get-data (str->long year) (str->long date))})
  ;(GET "/" [] (-> (resource-response "index.html" {:root "public"})
  ;                (content-type "text/html; charset=utf-8")))
  )

(defonce server (atom nil))

(defn start-server []
  (reset! server
          (run-jetty
           (wrap-defaults #'advent site-defaults)
           {:port 4000
            :join? false})))

(defn stop-server []
  (.stop @server)
  (reset! server nil))
