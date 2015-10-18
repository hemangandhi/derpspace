(ns server-test.core
  (:require [ring.adapter.jetty :as jetty]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.core :refer [GET POST defroutes]]
            [clojure.java.io :as io]
            [cemerick.friend :as friend]
            [cemerick.friend.credentials :as creds]
            [cemerick.friend.workflows :as wrkflws]))

(def users {"friend" {:userName "Neil" :password (creds/hash-bcrypt "Gay")}})

(defroutes app-routes
  (GET "/nm" {params :params} (str (if (= (:login-failed params) "Y")
                             "<p>Logged in" "<p>Not logged in")
                           ", go back: <a href=\"index.html\"> log out</a></p>"))
  (GET "/" [] (slurp "public/index.html"))
  (POST "*" [req] )
  (route/files "/")
  (route/not-found (slurp "public/index.html")))  (route/not-found (slurp "public/index.html"))


(def app (handler/site
           (friend/authenticate
             app-routes
             {:login-uri "/nm"
              :default-landing-uri "/"
              :credential-fn #(creds/bcrypt-credential-fn users %)
              :workflows [(wrkflws/interactive-form)]
              }
             )))

(defn- main []
  (jetty/run-jetty app {:port 8080}))

(println (.exists (io/as-file "web/lib/angular.min.js")))
(main)
