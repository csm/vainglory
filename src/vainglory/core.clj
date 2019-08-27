(ns vainglory.core
  (:require [clojure.java.io :as io]
            [jsonista.core :as json]
            [vainglory.async :as va]
            [vainglory.client :as vc]
            [yaml.reader :as yaml]
            [manifold.stream :as s]
            [clojure.string :as string])
  (:import [java.io StringWriter]))

(defn- safe-keyword
  [s]
  (or (keyword (re-matches #"^\p{L}[^/:\p{Blank}]*$" s)) s))

(defn load-yaml
  "Load a Swagger specification from a YAML source.

  Argument may be anything that can be coerced to a Reader."
  [readable]
  (with-open [r (io/reader readable)
              w (StringWriter.)]
    (io/copy r w)
    (yaml/parse-string (.toString w) :keywords safe-keyword)))

(defn load-json
  "Load a Swagger specification from a JSON source.

  Argument may be anything that can be coerced to a Reader."
  [readable]
  (with-open [r (io/reader readable)]
    (json/read-value r (json/object-mapper {:decode-key-fn safe-keyword}))))

(defn client
  "Create a new REST client from the supplied Swagger/OpenAPI spec.

  spec argument is a Swagger/OpenAPI specification (e.g. from [[load-yaml]] or [[load-json]]).

  Optional option map may contain the following keys:

  * `:conn-pool` An aleph.http connection pool, e.g. via [[aleph.http/connection-pool]]."
  ([spec] (client spec {}))
  ([spec options]
   (vc/create spec options)))

(defn invoke
  "Invoke a REST call, blocking for a response.

  Client argument is a client as produced by [[client]].

  Argument map has the following keys:

  * `:op` A keyword naming the operation to invoke. Required.
  * `:request` A map containing the request to pass; required based on operation -- see [[ops]] for
    a way to discover what arguments are required.
  * `:scheme` -- :https or :http; required if no schemes are specified in the swagger spec.
  * `:host` -- hostname to connect to; required if no hostname is specified in the swagger spec.
  * `:port` -- port to connect to; optional, based on scheme used.
  * `:headers` -- map of header name to value. Optional.
  * `:decode-key-fn` -- function to use to decode JSON keys; default leaves keys as is; can be
    `true` to keywordize keys, or any arbitrary 1-arg function.

  Return value will either be a response map, possibly with a `:body`, on success.

  On failure, an anomaly map will be returned."
  [client arg-map]
  (deref (s/take! (va/invoke client arg-map))))

(defn- get-schema
  [api schema-ref]
  (let [path (->> (string/split schema-ref #"[#/]")
                  (remove empty?)
                  (map keyword))]
    (get-in api path)))

(declare describe-parameter)

(defn- describe-schema
  [api schema]
  (when schema
    (if-let [ref (get schema "$ref")]
      (recur api (get-schema api ref))
      (->> (:properties schema)
           (map (fn [[prop-name prop]]
                  [prop-name (describe-parameter api prop)]))
           (into {})))))

(defn- describe-parameter
  [api parameter]
  (if-let [type (:type parameter)]
    (or (some-> parameter :format keyword)
        (some-> parameter :enum set)
        (some->> parameter :items (describe-parameter api))
        (keyword type))
    (when-let [schema (:schema parameter)]
      (describe-schema api schema))))

(defn ops
  "Describe operations client supports.

  Returns a map of operation ID keywords to operation spec maps. Each
  operation spec will contain keys:

  * `:request` A description of the request object to pass in.
  * `:response` A description of the response body the operation returns, if any.
  * `:required` A sequence of keys that are required in the `:request` map.
  * `:summary` The summary from the swagger spec.
  * `:description` The description from the swagger spec."
  [client]
  (into {}
    (mapcat (fn [[_ verbs]]
              (map (fn [[_ op]]
                     [(keyword (:operationId op))
                      {:request (->> (:parameters op)
                                     (map (fn [param]
                                            [(keyword (:name param))
                                             (describe-parameter (:api client) param)]))
                                     (into {}))
                       :response (->> (:responses op)
                                      (filter #(some? (:schema %)))
                                      (first)
                                      (describe-parameter (:api client)))
                       :required (->> (:parameters op)
                                      (filter :required)
                                      (map :name)
                                      (map keyword)
                                      (vec))
                       :summary (:summary op)
                       :description (:description op)}])
                   verbs))
            (-> client :api :paths))))