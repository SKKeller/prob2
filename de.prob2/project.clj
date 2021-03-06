(defproject de.prob2 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :repositories [["cobra" "http://cobra.cs.uni-duesseldorf.de/artifactory/repo"]]

  :source-paths ["src/clj" "src/cljs"]

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [cljsjs/react "0.12.2-5"]
                 [reagent "0.5.0-alpha3"]
                 [reagent-forms "0.4.3"]
                 [reagent-utils "0.1.2"]
                 [secretary "1.2.1"]
                 [org.clojure/clojurescript "0.0-2814" :scope "provided"]
                 [com.stuartsierra/component "0.2.2"]
                 [ring/ring-core "1.3.2"]
                 [ring/ring-servlet "1.3.2"]
                 [ring/ring-defaults "0.1.3"]
                 [com.taoensso/sente "1.3.0"]
                 [http-kit "2.1.19"]
                 [prone "0.8.0"]
                 [compojure "1.3.1"]
                 [selmer "0.8.0"]
                 [environ "1.0.0"]
                 [com.cognitect/transit-clj "0.8.259"]
                 [com.cognitect/transit-cljs "0.8.205"]
                 [cljs-ajax "0.3.10"]
                 [dorothy "0.0.6"]]

  :plugins [
            [lein-cljsbuild "1.0.4"]
            [lein-environ "1.0.0"]
          ;  [lein-ring "0.9.1"]
            [lein-asset-minifier "0.2.2"]]

;;  :ring {:handler de.prob2.handler/app
;;         :uberwar-name "de.prob2.war"}

  :min-lein-version "2.5.0"

  :uberjar-name "de.prob2.jar"

  :main de.prob2.server

  :clean-targets ^{:protect false} ["resources/public/js"]

  :minify-assets
  {:assets
    {"resources/public/css/site.min.css" "resources/public/css/site.css"}}

  :cljsbuild {:builds {:app {:source-paths ["src/cljs"]
                             :compiler {:output-to     "resources/public/js/app.js"
                                        :output-dir    "resources/public/js/out"
                                        ;;:externs       ["react/externs/react.js"]
                                        :asset-path   "js/out"
                                        :optimizations :none
                                        :pretty-print  true}}}}

  :profiles {:dev {:repl-options {:init-ns user
                                  :timeout 120000
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

                   :dependencies [[ring-mock "0.1.5"]
                                  [ring/ring-devel "1.3.2"]
                                  [leiningen "2.5.1"]
                                  [figwheel "0.2.3-SNAPSHOT"]
                                  [weasel "0.6.0-SNAPSHOT"]
                                  [org.clojure/test.check "0.7.0"]
                                  [com.cemerick/piggieback "0.1.6-SNAPSHOT"]
                                  [pjstadig/humane-test-output "0.6.0"]]
                   :resource-paths ["kernel/build/libs/*.jar"]
                   :source-paths ["env/dev/clj"]
                   :plugins [[lein-figwheel "0.2.3-SNAPSHOT"]
                             [lein-expand-resource-paths "0.0.1"]]

                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]

                   :figwheel {:http-server-root "public"
                              :server-port 3449
                              :css-dirs ["resources/public/css"]
                              ;:ring-handler de.prob2.handler/app
                            }

                   :env {:dev? true}

                   :cljsbuild {:builds {:app {:source-paths ["env/dev/cljs"]
                                              :compiler {   :main "de.prob2.dev"
                                                         :source-map true}}
}
}}

             :uberjar {:hooks [leiningen.cljsbuild minify-assets.plugin/hooks]
                       :env {:production true}
                       :aot :all
                       :omit-source true
                       :cljsbuild {:jar true
                                   :builds {:app
                                             {:source-paths ["env/prod/cljs"]
                                              :compiler
                                              {:optimizations :advanced
                                               :pretty-print false}}}}}

             :production {:ring {:open-browser? false
                                 :stacktraces?  false
                                 :auto-reload?  false}
                          :cljsbuild {:builds {:app {:compiler {:main "de.prob2.prod"}}}}
                          }})
