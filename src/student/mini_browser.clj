(ns student.mini-browser
  (:require [hiccup.core :as hiccup])
  (:require [hiccup.page-helpers :as hiccup.helpers])
  (:require [labrepl.layout :as layout])
  (:use compojure.core)
  (:require [compojure.route :as route])
  (:require [ring.adapter.jetty :as jetty]))

(defn mockup-1
  []
  (hiccup/html
 [:body {:title "Mini-Browser" :id "browser"}
  [:p {:class "awesome"} "Hello, World!"]]))

(defn mockup-2 []
  (hiccup/html
   [:head
    [:title "Mini-Browser"]]
   [:body {:id "browser"}
    [:div {:id "header"}
     [:h2 "Mini-Browser"]]
    [:div {:id "content"}
     "Body"]
    [:div {:id "footer"}
     "Clojure Mini-Browser"]]))

(defn mockup-3 []
  (hiccup/html
   [:head
    [:title "Mini-Browser"]
    (apply hiccup.helpers/include-css layout/default-stylesheets)
    (apply hiccup.helpers/include-js layout/default-javascripts)]
   [:body {:id "browser"}
    [:div {:id "header"}
     [:h2 "Mini-Browser"]]
    [:div {:id "content"}
     "Body TBD"]
    [:div {:id "footer"}
     "Clojure Mini-Browser"]]))

(defn mockup-server []
  (jetty/run-jetty (var mockup-routes) {:port 8999
                                  :join? false}))

(defroutes mockup-routes
  (GET "/m1" [] (mockup-1))
  (GET "/m2" [] (mockup-2))
  (GET "/m3" [] (mockup-3))
 (route/files "/"))
