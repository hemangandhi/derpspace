(defproject server-test "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [ring/ring-core "1.3.2"]
                 [ring/ring-defaults "0.1.2"]
                 [ring/ring-jetty-adapter "1.2.1"]
                 [clj-template "1.0.1"]
                 [compojure "1.4.0"]
                 [com.cemerick/friend "0.2.1"]]
  :plugins [[lein-ring "0.9.1"]]
  :ring {:handler server-test.core/handler})

