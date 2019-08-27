(ns vainglory.client
  "Implementation namespace, use vainglory.core in your code."
  (:require [aleph.http :as http]
            [cemerick.uri :as uri]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [cognitect.anomalies :as anom]
            [jsonista.core :as json]
            [manifold.stream :as stream]
            [manifold.deferred :as d]
            [clojure.java.io :as io])
  (:import [java.util UUID]))

(defrecord Client [conn-pool api api-id])

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
  [client {:keys [op request scheme host port headers decode-key-fn] :or {decode-key-fn identity}}]
  (if-let [operation (find-path-to #(= op (some-> % :operationId keyword)) (:paths (:api client)))]
    (let [[path verb] (first operation)
          op-data (get-in client [:api :paths path verb])]
      (if-let [spec (s/get-spec (keyword (str "vainglory.spec.api-" (:api-id client) \. (name (:operationId op-data))) "request"))]
        (if-let [error (s/explain-data spec request)]
          (stream/->source [{::anom/category ::anom/incorrect
                             ::anom/message  (with-out-str (s/explain-out error))
                             ::failure       error}])
          (if-let [scheme (or scheme
                              (some-> client :api :schemes set (get "https") keyword)
                              (some-> client :api :schemes set (get "http") keyword))]
            (if-let [host (or host (get-in client [:api :host]))]
              (let [headers (or headers {})
                    base-path (get-in client [:api :basePath] "/")
                    port (or port (case scheme :https 443 :http 80))
                    path-params (filter #(= "path" (:in %)) (:parameters op-data))
                    query-params (filter #(= "query" (:in %)) (:parameters op-data))
                    form-params (filter #(= "formData" (:in %)) (:parameters op-data))
                    body-params (apply dissoc request (concat (map :name path-params)
                                                              (map :name query-params)
                                                              (map :name form-params)))
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
                    body-data (not-empty (select-keys request (map (comp keyword :name) body-params)))
                    path (reduce (fn [p param]
                                   (let [value (get request (keyword (:name param)))
                                         replace-string (str "\\{" (:name param) "\\}")]
                                     (.replaceAll p replace-string (str value))))
                                 (name path) path-params)
                    content-type (if (not-empty form-params)
                                   "multipart/form-data"
                                   (when (not-empty body-params)
                                     "application/json")) ; todo, do better content-negotiation
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
                                 :request-method verb
                                 :headers (if (some? content-type)
                                            (assoc headers "content-type" content-type)
                                            headers)}
                    request-map (if (some? form-data)
                                  (assoc request-map :form-params form-params)
                                  request-map)
                    request-map (if (some? body-data)
                                  (assoc request-map :body (json/write-value-as-bytes body-data))
                                  request-map)]
                (-> (http/request request-map)
                    (d/catch' map-exception)
                    (d/chain' (fn [result]
                                (cond (some-> (get-in result [:headers "content-type"]) (.contains "application/json"))
                                      (update result :body json/read-value (json/object-mapper {:decode-key-fn decode-key-fn}))

                                      (some? (:body result))
                                      (update result :body slurp)

                                      :else result)))
                    (stream/->source)))
              (stream/->source [{::anom/category ::anom/incorrect
                                 ::anom/message "can't automatically determine host to use; pass :host key to invoke"}]))
            (stream/->source [{::anom/category ::anom/incorrect
                               ::anom/message "can't automatically determine scheme to use; pass :scheme key as :http or :https to invoke"}])))
        (stream/->source [{::anom/category ::anom/fault
                           ::anom/message (str "client " client " appears incorrect; no request spec for operationId " op)}])))
    {::anom/category ::anom/incorrect
     ::anom/message (str "operation " op " not valid")}))

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
   ["integer" "int32"] int32?
   ["integer" "int64"] int64?
   ["number" "float"] float?
   ["number" "double"] double?
   ["boolean" nil] boolean?})

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
                                    (s/keys :req-un [~@(map (fn [[prop-id]]
                                                              (keyword (str api-ns ".definitions." (name id)) (name prop-id)))
                                                            req)]
                                            :opt-un [~@(map (fn [[prop-id]]
                                                              (keyword (str api-ns ".definitions." (name id)) (name prop-id)))
                                                            opt)]))
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
                                                           (do
                                                             (prn {:path _path :method _method :mspec mspec})
                                                             (throw (UnsupportedOperationException. "not implemented (explicit array)"))))
                                                         (do
                                                           (prn {:path _path :method _method :mspec mspec})
                                                           (throw (UnsupportedOperationException. "not implemented (explicit schema)")))))]
                                            (if (-> mspec :parameters first :required)
                                              [spec]
                                              [`(s/nilable ~spec)]))
                                          (if (empty? (:parameters mspec))
                                            [`(s/def ~mspec-id nil?)]
                                            (cons
                                              `(s/def ~mspec-id (s/keys :req-un [~@(->> (:parameters mspec)
                                                                                        (filter :required)
                                                                                        (map (fn [param]
                                                                                               (keyword (str api-ns \. (name (:operationId mspec)) ".request") (:name param)))))]
                                                                        :opt-un [~@(->> (:parameters mspec)
                                                                                        (remove :required)
                                                                                        (map (fn [param]
                                                                                               (keyword (str api-ns \. (name (:operationId mspec)) ".request") (:name param)))))]))
                                              (map (fn [parameter]
                                                     (make-spec (str (name (:operationId mspec)) ".request") [(:name parameter) parameter]))
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

(defn create
  [api {:keys [conn-pool]}]
  (let [paths (->> (:paths api)
                   (reduce-kv (fn [m path ops]
                                (assoc m path (reduce-kv (fn [m verb op]
                                                           (if (nil? (:operationId op))
                                                             (assoc m verb (assoc op :operationId (generate-operation-id path verb)))
                                                             (assoc m verb op)))
                                                         {} ops)))
                              {}))
        api (assoc api :paths paths)
        api-id (UUID/nameUUIDFromBytes (json/write-value-as-bytes api))]
    (doseq [spec (generate-specs* api api-id)]
      ;(println "gen spec:" spec)
      (eval spec))
    (->Client (or conn-pool (http/connection-pool {})) api api-id)))

