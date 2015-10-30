(ns server-test.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.util.response :as resp]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.core :refer [GET POST defroutes]]
            [cemerick.friend :as friend]
            [cemerick.friend.credentials :as creds]
            [cemerick.friend.workflows :as wrkflws]))

(def users (ref {0 {:username "asdf" 
                    :password (creds/hash-bcrypt "password") 
                    :roles #{::user}}}))

(defn make-user [{:keys [username password confirm] :as user-data}]
  (if (= password confirm)
    (dosync
      (alter users assoc (inc (first (last @users))) {:username username 
                                                      :password (creds/hash-bcrypt password) 
                                                      :roles #{::user}})
      (assoc {} :username username :password (creds/hash-bcrypt password) :roles #{::user}))
    nil))

(defroutes app-routes
  (GET "/login" [] (str "<p>Fail</p>" (slurp "public/index.html")))
  (GET "/authorized" req 
       (friend/authorize #{::user} (slurp "public/userpage.html")))
  (GET "/" [] (slurp "public/index.html"))
  (POST "/signup" {{:keys [username password confirm] :as params} :params :as req}
        (let [u (make-user (select-keys params [:username :password :confirm]))]
          (if (nil? u)
            (slurp "public/signup.html")
            (friend/merge-authentication (resp/redirect "/authorized") u))))
  (friend/logout (POST "/logout" req (slurp "public/index.html")))
  (route/files "/")
  (route/not-found (slurp "public/index.html")))


(def app (handler/site
           (friend/authenticate
             app-routes
             {:credential-fn (partial creds/bcrypt-credential-fn @users)
              :workflows [(wrkflws/interactive-form)]})))

(defn- main []
  (jetty/run-jetty app {:port 8080}))

(main)
