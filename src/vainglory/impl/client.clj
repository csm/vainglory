(ns ^:no-doc vainglory.impl.client
  "Implementation namespace, use vainglory.core in your code."
  (:require [aleph.http :as http]
            [cemerick.uri :as uri]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [cognitect.anomalies :as anom]
            [jsonista.core :as json]
            [manifold.deferred :as d]
            [vainglory.impl.topo :as topo])
  (:import [java.util UUID]
           [com.google.common.io ByteStreams]
           [java.io InputStream]))

(defrecord Client [conn-pool api ops api-id])

(defn find-path-to
  "Recursively scan m for a value that matches pred,
  returning all paths in m that matched, or nil if
  no match is found."
  [pred m]
  (->> m
       (mapcat (fn [[k v]]
                 (if (pred v)
                   [[k]]
                   (when (map? v)
                     (when-let [sub (find-path-to pred v)]
                       (map #(cons k %) sub))))))
       (filter some?)))

(defn map-exception
  [e]
  (let [cat (or (some-> e ex-data :status
                  (case 400 ::anom/incorrect
                        401 ::anom/forbidden
                        402 ::anom/incorrect
                        403 ::anom/forbidden
                        404 ::anom/not-found
                        405 ::anom/unsupported
                        406 ::anom/incorrect
                        408 ::anom/busy
                        409 ::anom/conflict
                        429 ::anom/busy
                        501 ::anom/unsupported
                        503 ::anom/unavailable
                        504 ::anom/busy
                        ::anom/fault))
                ::anom/fault)]
    {::anom/category cat
     ::anom/message (or (some-> e ex-data :body
                                (as-> body
                                      (cond (some-> (get-in (ex-data e) [:headers "content-type"]) (.contains "application/json"))
                                            (json/read-value body)

                                            :else (slurp body))))
                        (.getMessage e)
                        (str e))
     ::exception e}))

(defn strip-slash
  [s]
  (if (= \/ (first s))
    (recur (.substring s 1))
    s))

(defn invoke
  [client {:keys [op request scheme host port headers decode-key-fn]
           :or {decode-key-fn identity}}]
  (if-let [operation (get-in client [:ops op])]
    (if-let [spec (s/get-spec (keyword (str "vainglory.spec.api-" (:api-id client) \. (name op)) "request"))]
      (if-let [error (s/explain-data spec request)]
        (d/success-deferred {::anom/category ::anom/incorrect
                             ::anom/message  (with-out-str (s/explain-out error))
                             ::failure       error})
        (if-let [scheme (or scheme
                            (some-> client :api :schemes set (get "https") keyword)
                            (some-> client :api :schemes set (get "http") keyword))]
          (if-let [host (or host (get-in client [:api :host]))]
            (let [headers (or headers {})
                  base-path (get-in client [:api :basePath] "/")
                  port (or port (case scheme :https 443 :http 80))
                  path-params  (filter #(= "path" (:in %)) (:request operation))
                  query-params (filter #(= "query" (:in %)) (:request operation))
                  form-params  (filter #(= "formData" (:in %)) (:request operation))
                  body-params  (filter #(= "body" (:in %)) (:request operation))
                  query (->> query-params
                             (reduce (fn [params param]
                                       (if-let [value (get request (keyword (:name param)))]
                                         (if (sequential? value)
                                           (into params (map #(vector (:name param) %) value))
                                           (conj params [(:name param) value]))
                                         params))
                                     [])
                             (not-empty))
                  form-data (->> form-params
                                 (reduce (fn [params param]
                                           (if-let [value (get request (keyword (:name param)))]
                                             (assoc params (keyword (:name param)) value)
                                             params))
                                         {})
                                 (not-empty))
                  body-data (if (and (= 1 (count body-params))
                                     (= :bytes (-> body-params first :param)))
                              (if (or (bytes? request) (instance? InputStream request))
                                request
                                (get request (-> body-params first :name)))
                              (not-empty (select-keys request (map :name body-params))))
                  path (reduce (fn [p param]
                                 (let [value (get request (:name param))
                                       replace-string (str "\\{" (name (:name param)) "\\}")]
                                   (.replaceAll p replace-string (str value))))
                               (:path operation)
                               path-params)
                  content-type (if (not-empty form-params)
                                 "multipart/form-data"
                                 (cond
                                   (and (= 1 (count body-params))
                                        (= :bytes (-> body-params first :param)))
                                   (or (get headers "content-type") "application/octet-stream")

                                   (not-empty body-params)
                                   "application/json"

                                   :else nil))
                  url (as-> (uri/map->URI {:protocol (name scheme)
                                           :host host
                                           :port port})
                            uri
                            (uri/uri uri base-path (strip-slash path))
                            (if (some? query)
                              (assoc uri :query query)
                              uri)
                            (str uri))
                  request-map {:url url
                               :pool (:conn-pool client)
                               :request-method (:verb operation)
                               :headers (if (some? content-type)
                                          (assoc headers "content-type" content-type)
                                          headers)}
                  request-map (if (some? form-data)
                                (assoc request-map :form-params form-params)
                                request-map)
                  request-map (cond
                                (and (not-empty body-data)
                                     (or (bytes? body-data)
                                         (string? body-data)))
                                (assoc request-map :body body-data)

                                (not-empty body-data)
                                (assoc request-map :body (json/write-value-as-bytes body-data))

                                :else
                                request-map)]
              (-> (http/request request-map)
                  (d/catch' map-exception)
                  (d/chain' (fn [result]
                              (cond (some-> (get-in result [:headers "content-type"]) (.contains "application/json"))
                                    (update result :body json/read-value (json/object-mapper {:decode-key-fn decode-key-fn}))

                                    (some? (:body result))
                                    (update result :body #(some-> % (ByteStreams/toByteArray)))

                                    :else result)))))
            (d/success-deferred {::anom/category ::anom/incorrect
                                 ::anom/message "can't automatically determine host to use; pass :host key to invoke"}))
          (d/success-deferred {::anom/category ::anom/incorrect
                               ::anom/message "can't automatically determine scheme to use; pass :scheme key as :http or :https to invoke"})))
      (d/success-deferred {::anom/category ::anom/fault
                           ::anom/message (str "client " client " appears incorrect; no request spec for operationId " op)}))
    (d/success-deferred {::anom/category ::anom/incorrect
                         ::anom/message (str "operation " op " not valid")})))

(defn int32?
  [v]
  (s/int-in-range? Integer/MIN_VALUE (inc Integer/MAX_VALUE) v))

(defn int64?
  [v]
  (s/int-in-range? Long/MIN_VALUE (inc (bigint Long/MAX_VALUE)) v))

(def type->pred
  {["string" "date"] inst?
   ["string" "dateTime"] inst?
   ["string" "byte"] bytes?
   ["string" "password"] string?
   ["string" nil] string?
   ["string" "binary"] bytes?
   ["integer" "int32"] int32?
   ["integer" "int64"] int64?
   ["number" "float"] float?
   ["number" "double"] double?
   ["boolean" nil] boolean?})

(defn param-name
  [prop]
  (if (and (empty? (:name prop)) (= "body" (:in prop)))
    :body
    (keyword (:name prop))))

(defn generate-specs*
  [api api-id]
  ;(println "api:" api "api-id:" api-id)
  (let [api-ns (str "vainglory.spec.api-" api-id)
        make-spec (fn [id [prop-name prop]]
                    (let [spec-id (keyword (str api-ns \. id) (name prop-name))]
                      (if (some? (get prop "$ref"))
                        `(s/def ~spec-id
                           ~(let [path (rest (string/split (get prop "$ref") #"/"))]
                              (keyword (str api-ns \. (string/join \. (butlast path))) (last path))))
                        (if (= "array" (:type prop))
                          `(s/def ~spec-id
                             (s/coll-of ~(if (some? (-> prop :items (get "$ref")))
                                           (let [path (rest (string/split (-> prop :items (get "$ref")) #"/"))]
                                             (keyword (str api-ns \. (string/join \. (butlast path))) (last path)))
                                           (if-let [enums (-> prop :items :enum)]
                                             (set enums)
                                             (type->pred [(-> prop :items :type) (-> prop :items :format)])))))
                          (if-let [enums (:enum prop)]
                            `(s/def ~spec-id #{~@enums})
                            `(s/def ~spec-id ~(type->pred [(:type prop) (:format prop)])))))))
        definition-specs (mapcat
                           (fn [[id definition]]
                             ;(println "id:" id "definition:" definition)
                             (let [required (set (map keyword (:required definition)))
                                   req (select-keys (:properties definition) required)
                                   opt (apply dissoc (:properties definition) required)
                                   prop-specs (map #(make-spec (str "definitions." (name id)) %)
                                                   (:properties definition))]
                               (cons
                                 `(s/def ~(keyword (str api-ns ".definitions") (name id))
                                    ~(if (and (empty? req) (empty? opt))
                                       map?
                                       `(s/keys :req-un [~@(map (fn [[prop-id]]
                                                                  (keyword (str api-ns ".definitions." (name id)) (name prop-id)))
                                                                req)]
                                                :opt-un [~@(map (fn [[prop-id]]
                                                                  (keyword (str api-ns ".definitions." (name id)) (name prop-id)))
                                                                opt)])))
                                 prop-specs)))
                           (:definitions api))
        parameter-specs (mapcat
                          (fn [[_path methods]]
                            (mapcat (fn [[_method mspec]]
                                      (let [mspec-id (keyword (str api-ns \. (name (:operationId mspec))) "request")]
                                        (if (and (= 1 (count (:parameters mspec)))
                                                 (= "body" (-> mspec :parameters first :in))
                                                 (some? (-> mspec :parameters first :schema)))
                                          (let [spec (if-let [path (some-> mspec :parameters first :schema (get "$ref") (string/split #"/") rest)]
                                                       `(s/def ~mspec-id ~(keyword (str api-ns \. (string/join \. (butlast path))) (last path)))
                                                       (if (= "array" (-> mspec :parameters first :schema :type))
                                                         (if-let [path (some-> mspec :parameters first :schema :items (get "$ref") (string/split #"/") rest)]
                                                           `(s/def ~mspec-id (s/coll-of ~(keyword (str api-ns \. (string/join \. (butlast path))) (last path))))
                                                           (if-let [enums (some-> mspec :parameters first :schema :items :enum)]
                                                             `(s/def ~mspec-id #{~@enums})
                                                             `(s/def ~mspec-id ~(type->pred [(-> mspec :parameters first :schema :items :type) (-> mspec :parameters first :schema :item :format)]))))
                                                         (if-let [enums (some-> mspec :parameters first :schema :enum)]
                                                           `(s/def ~mspec-id #{~@enums})
                                                           `(s/def ~mspec-id ~(type->pred [(-> mspec :parameters first :schema :type) (-> mspec :parameters first :schema :format)])))))]
                                            (if (-> mspec :parameters first :required)
                                              [spec]
                                              [`(s/nilable ~spec)]))
                                          (if (empty? (:parameters mspec))
                                            [`(s/def ~mspec-id nil?)]
                                            (cons
                                              (if (empty? (:parameters mspec))
                                                `(s/def ~mspec-id map?)
                                                `(s/def ~mspec-id (s/keys :req-un [~@(->> (:parameters mspec)
                                                                                          (filter :required)
                                                                                          (map (fn [param]
                                                                                                 (keyword (str api-ns \. (name (:operationId mspec)) ".request") (name (param-name param))))))]
                                                                          :opt-un [~@(->> (:parameters mspec)
                                                                                          (remove :required)
                                                                                          (map (fn [param]
                                                                                                 (keyword (str api-ns \. (name (:operationId mspec)) ".request") (name (param-name param))))))])))
                                              (map (fn [parameter]
                                                     (make-spec (str (name (:operationId mspec)) ".request") [(param-name parameter) parameter]))
                                                   (:parameters mspec)))))))
                                    methods))
                          (:paths api))]
    (concat definition-specs parameter-specs)))

(def route-prefix-pattern #"^/(?:apiv?[0-9]*|v[0-9]+)(/.*)$")

(defn generate-operation-id
  "Generate a operationId based off a route definition, for APIs that
  don't include an operationId."
  [path verb]
  (let [path (or (last (re-matches route-prefix-pattern path)) path)]
    (case verb
      (:get :delete) (keyword (str (name verb) (->> (string/split path #"/+")
                                                    (remove empty?)
                                                    (remove #(re-matches #"^\{.+\}$" %))
                                                    (mapcat #(string/split % #"[-_]"))
                                                    (map string/capitalize)
                                                    (string/join))))
      :put (keyword (str "set" (->> (string/split path #"/+")
                                    (remove empty?)
                                    (remove #(re-matches #"^\{.+\}$" %))
                                    (mapcat #(string/split % #"[-_]"))
                                    (map string/capitalize)
                                    (string/join))))
      :post (let [[part & parts] (->> (string/split path #"/+")
                                      (remove empty?)
                                      (remove #(re-matches #"^\{.+\}$" %))
                                      (mapcat #(string/split % #"[-_]")))]
              (keyword (str part (->> parts
                                      (map string/capitalize)
                                      (string/join))))))))

(defn get-schema
  [api schema-ref]
  (let [path (->> (string/split schema-ref #"[#/]")
                  (remove empty?)
                  (map keyword))]
    (get-in api path)))

(declare describe-parameter)

(defn describe-type
  [{:keys [type format]}]
  (case [type format]
    ["integer" "int32"]   :int32
    ["integer" "int64"]   :int64
    ["number" "float"]    :float
    ["number" "double"]   :double
    ["string" nil]        :string
    ["string" "byte"]     :bytes
    ["string" "binary"]   :bytes
    ["boolean" nil]       :boolean
    ["string" "date"]     :instant
    ["string" "dateTime"] :instant
    ["string" "password"] :string
    nil))

(defn describe-schema
  [api schema]
  (when schema
    (cond
      (some? (get schema "$ref"))
      (recur api (get-schema api (get schema "$ref")))

      (not-empty (:properties schema))
      (->> (:properties schema)
           (map (fn [[prop-name prop]]
                  [prop-name (describe-parameter api prop)]))
           (into {}))

      :else
      (describe-type schema))))

(defn describe-parameter
  [api parameter]
  (if-let [type (:type parameter)]
    (or (some-> parameter :format keyword)
        (some-> parameter :enum set)
        (some->> parameter :items (describe-parameter api) vector)
        (keyword type))
    (when-let [schema (:schema parameter)]
      (describe-schema api schema))))

(defn operation-id
  [path verb op]
  (or (some-> (:operationId op) keyword)
      (generate-operation-id path verb)))

(defn build-ops
  [api api-id]
  (into {}
    (mapcat (fn [[path verbs]]
              (map (fn [[verb op]]
                     (let [operation-id (operation-id path verb op)]
                       ; (println "build-op" path verb operation-id op)
                       [operation-id
                        {:request (->> (:parameters op)
                                       (map (fn [param]
                                              (when-let [p (describe-parameter api param)]
                                                {:param p
                                                 :name (param-name param)
                                                 :in (:in param)})))
                                       (remove nil?))
                         :response (->> (:responses op)
                                        (filter #(some? (:schema %)))
                                        (first)
                                        (describe-parameter api))
                         :required (->> (:parameters op)
                                        (filter :required)
                                        (map param-name)
                                        (map keyword)
                                        (vec))
                         :summary (:summary op)
                         :description (:description op)
                         :path path
                         :verb verb
                         :spec {:request (keyword (str "vainglory.spec.api-" api-id \. (name operation-id)) "request")}}]))
                   verbs))
            (-> api :paths))))

(defn sort-specs
  [specs]
  (let [g (into {} (map (fn [spec]
                          (let [[_op _spec-id spec-def] spec]
                            (if (keyword? spec-def)
                              [spec #{(first (filter #(= spec-def (second %)) specs))}]
                              [spec #{}])))
                        specs))]
    (reverse (topo/kahn-sort g))))

(defn create
  [api {:keys [conn-pool api-id]}]
  (let [paths (->> (:paths api)
                   (reduce-kv (fn [m path ops]
                                (assoc m path (reduce-kv (fn [m verb op]
                                                           (if (nil? (:operationId op))
                                                             (assoc m verb (assoc op :operationId (generate-operation-id path verb)))
                                                             (assoc m verb op)))
                                                         {} ops)))
                              {}))
        api (assoc api :paths paths)
        api-id (or api-id (UUID/nameUUIDFromBytes (json/write-value-as-bytes api)))]
    ; todo is there a better way to generate dynamic specs?
    (doseq [spec (sort-specs (generate-specs* api api-id))]
      ;(println "gen spec:" spec)
      (eval spec))
    (->Client (or conn-pool (http/connection-pool {})) api (build-ops api api-id) api-id)))

