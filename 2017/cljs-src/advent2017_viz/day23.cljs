(ns ^:figwheel-hooks advent2017-viz.day23
  (:require [clojure.string :as str]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [cljs.pprint :refer [pprint]])
  (:require-macros [cljs.core.async.macros :refer [go]]))



(def start-state {:mul-cnt 0})

(defn cpu [_instruction-pointer state mnemonic [a b]]
  (let [val-a (if (number? a) a (get state a 0))
        val-b (if (number? b) b (get state b 0))]
    (case mnemonic
      "set" (assoc state a val-b)
      "add" (update state a (fnil (partial + val-b) 0))
      "sub" (update state a (fnil #(- % val-b) 0))
      "mul" (-> state
                (update a (fnil (partial * val-b) 0))
                (update :mul-cnt inc))
      "mod" (update state a #(mod % val-b))
      "jnz" (if (not= val-a 0)
              {:jmp val-b}
              state))))

; **************************************************

(defn str->long
  "Safe parsing of string to long.
   If s looks like a base-10 number, return a long, otherwise return original string"
  [s]
  (if (and (not (nil? s)) (re-matches #"[+-]?[0-9]+" s))
    (js/parseInt s 10)
    s))

(def url "/api/data/2017/23")

(def app-state (r/atom {:data-url url
                        :error ""
                        :debug ""
                        :jumps #{}}))

(defn debug [d]
  (println d)
  (swap! app-state update :debug str d "\r\n"))

(def cpu-state (r/atom {:ip 0
                        :cpu-fn nil
                        :program []
                        :state nil
                        :is-halted false
                        :breakpoints #{}
                        :cnt 0}))

(def set-register (r/atom {:name ""
                           :value ""}))

(defn update-new-register [key val]
  (swap! set-register assoc key val))

(defn store-new-register []
  (let [{:keys [name value]} @set-register]
    (debug (str "set register '" name "'=" value))
    (swap! cpu-state assoc-in [:state name] (str->long value))))


(defn parse-program [input]
  (let [parse-line (fn [line] (map str->long (str/split line #" +")))]
    (cond (vector? input) input
          (coll? input) (vec input)
          :else (mapv parse-line (str/split-lines input)))))

(defn interpreter-step [cpu-in step]
  (loop [{:keys [ip cpu-fn program state is-halted] :as cpu} cpu-in
         first true]
    (if (and (not first)
             (or (= step :step)
                 (and (= step :break)
                      (contains? (:breakpoints @cpu-state) ip))))
      cpu

      (if (or is-halted (>= ip (count program)))
        (assoc cpu :is-halted true)

        (let [[mnemonic & params] (get program ip)
              next-state (cpu-fn ip state mnemonic params)]
          (cond (= next-state :hlt)
                (assoc cpu :is-halted true)

                (contains? next-state :jmp)
                (do (swap! app-state update :jumps conj [ip (+ ip (:jmp next-state))])
                    (recur (-> cpu
                               (update :ip (partial + (:jmp next-state)))
                               (update :cnt inc))
                           false))

                :else
                (recur (-> cpu
                           (assoc :state next-state)
                           (update :ip inc)
                           (update :cnt inc))
                       false)))))))

(defn setup-interpreter [input cpu-function start-state]
  (swap! cpu-state assoc
         :ip 0
         :cpu-fn cpu-function
         :program (parse-program input)
         :state start-state))

(defn reset-cpu []
  (swap! cpu-state assoc
         :ip 0
         :state start-state
         :is-halted false)
  (swap! app-state assoc :jumps #{}))

(defn step-interpreter []
  (swap! cpu-state interpreter-step :step))

(defn run-interpreter []
  (swap! cpu-state interpreter-step :break))

(defn toggle-breakpoint [ip]
  (swap! cpu-state update :breakpoints #(if (% ip)
                                          (disj % ip)
                                          (conj % ip))))

(defn load-data []
  (go (let [data-ch (http/get url)
            http-return (<! data-ch)]
        (if (not= 200 (:status http-return))
          (swap! app-state assoc :error (:error-text http-return))
          (let [data (:body http-return)]
            (setup-interpreter data cpu start-state))))))

; **************************************************

(defn error-boundary [comp]
  (let [error (r/atom nil)]
    (r/create-class
     {:component-did-catch (fn [this e info]
                             (reset! error e))
      :get-derived-state-from-error-test (fn [error] #js {})
      :reagent-render (fn [comp]
                        (if @error
                          [:div
                           "Something went wrong."
                           [:button {:on-click #(reset! error nil)} "Try again"]]
                          comp))})))

(defn button [title callback]
  [:input {:type "button" :value title :on-click callback}])

(defn api-form []
  [:div {:class "apiform"}
   (button "Load data" load-data)
   (button "Step CPU" step-interpreter)
   (button "Run to breakpoint" run-interpreter)
   (button "Restart" reset-cpu)])

(defn info-pane []
  [:div {:class "info"}
   [:h1 "general info"]
   [:p "program length " (count (:program @cpu-state))]
   [:p "ip " (:ip @cpu-state)]
   [:p "registers " (get @cpu-state :state "")]
   [:h1 "last error"]
   [:pre (:error @app-state)]
   [:h1 "debug output"]
   [:pre (:debug @app-state)]
   [:pre (:jumps @app-state)]])

(def line-offsets (r/atom []))

(defn overlay-drawing []
     [:svg {:class "ignoremouse"}
      [:marker#arrowhead {:markerWidth "6" :markerHeight "4"
                          :refX "0" :refY "2" :orient "auto"}
       [:polygon {:points "0 0, 6 2, 0 4" :fill "greenyellow"}]]
      (doall (keep-indexed (fn [idx [from to]]
                             (let [from-y (get @line-offsets from)
                                   to-y (get @line-offsets to)
                                   x (+ 170 (* idx 10))]
                               (when (and from-y to-y)
                                 ^{:key from-y}
                                 [:polyline {:points (str "140," from-y " " x "," from-y " "
                                                          x "," to-y " 150," to-y)
                                             :fill "none"
                                             :stroke "greenyellow" :stroke-width "2" :marker-end "url(#arrowhead)"}])))
                           (:jumps @app-state)))]
  )

(defn visualization []
  (let [{:keys [ip breakpoints]} @cpu-state
        breakpoints (if-not breakpoints #{} breakpoints)]
    [:div.viz



     [:div.codelisting
      (doall (map-indexed (fn [idx [mnemonic par-a par-b]]
                            (swap! line-offsets conj nil)
                            ^{:key idx}
                            [:div {:class (cond->                  "foo"
                                            (= idx ip)        (str " ip")
                                            (breakpoints idx) (str " breakpoint"))
                                   :on-click #(toggle-breakpoint idx)
                                   :ref (fn [el] (when el (let [rect (.getBoundingClientRect el)
                                                                top (goog.object/getValueByKeys rect #js["top"])
                                                                bottom (goog.object/getValueByKeys rect #js["bottom"])
                                                                y-val (quot (+ bottom top) 2)]

                                                            (swap! line-offsets assoc idx y-val))))}
                             [:code idx]
                             [:code mnemonic]
                             [:code par-a]
                             [:code par-b]])
                          (:program @cpu-state)))]
     [:div.registers
      [:div [:code "ip:"] [:code ip]]
      (doall (map (fn [[r v]] ^{:key r} [:div [:code r] [:code v]]) (:state @cpu-state)))
      [:p "Set register value"]
      [:input {:type "text" :placeholder "name" :on-change #(update-new-register :name (-> % .-target .-value))}]
      [:input {:type "text" :placeholder "value" :on-change #(update-new-register :value (-> % .-target .-value))}]
      (button "set register" store-new-register)]]))

(defn top-frame []
  [:div
   [api-form]
   [info-pane]
   [visualization]
   [overlay-drawing]
   ])

(defn ^:export run []
  (rdom/render [top-frame] (js/document.getElementById "app")))

(defn ^:after-load setup []
  (println "js reload")
  (run))

(defonce start-up (run))
