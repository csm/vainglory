# vainglory

[![cljdoc](https://cljdoc.org/badge/com.github.csm/vainglory)](https://cljdoc.org/d/com.github.csm/vainglory/CURRENT)
[![Clojars Project](https://img.shields.io/clojars/v/com.github.csm/vainglory.svg)](https://clojars.org/com.github.csm/vainglory)

Data-driven Swagger clients.

Alpha, subject to change.

```clojure
(require '[vainglory.client :as v])

; load a swagger spec from anything that can turn into a reader: files, URLs, etc.
(def petstore (v/load-json "https://petstore.swagger.io/v2/swagger.json"))
(def petstore-client (v/client petstore))

; all available ops returned as a map from v/ops
(v/ops petstore-client)

; get a data description of an operation
(:findPetsByStatus (v/ops petstore-client))

; invoke an operation
(v/invoke petstore-client {:op :findPetsByStatus :request {:status ["available"]}})

; async invoke is also available
(require '[vainglory.async :as va])
(require '[manifold.deferred :as d])

; invoke returns a manifold deferred that will yield the result
(d/chain
  (va/invoke petstore-client {:op :findPetsByStatus :request {:status ["sold"]}})
  (fn [result] ...))
```

Only supports Swagger 2.0 at the moment, and won't tell the difference
if you feed it something else.

Mad props: inspired by [aws-api](https://github.com/cognitect-labs/aws-api).