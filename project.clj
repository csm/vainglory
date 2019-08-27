(defproject com.github.csm/vainglory "0.1.0-SNAPSHOT"
  :description "Data-driven clients for Swagger APIs"
  :url "https://github.com/csm/vainglory"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "0.4.500"]
                 [org.clojure/core.cache "0.7.2"]
                 [org.clojure/core.memoize "0.7.2"]
                 [com.cognitect/http-client "0.1.99"]
                 [com.cognitect/anomalies "0.1.12"]
                 [metosin/jsonista "0.2.4"]
                 [io.forward/yaml "1.0.9"]
                 [org.flatland/ordered "1.5.7"]
                 [com.arohner/uri "0.1.2"]
                 [aleph "0.4.6"]]
  :profiles {:test {:resource-paths ["test-resources"]}}
  :plugins [[lein-codox "0.10.3"]]
  :codox {:output-path "docs"
          :namespaces [vainglory.core vainglory.async]}
  :repl-options {:init-ns vainglory.repl})
