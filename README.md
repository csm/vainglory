# vainglory

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
(require 'manifold.stream)

; invoke returns a manifold stream that will yield the result from a take!
@(manifold.stream/take! (va/invoke petstore-client {:op :findPetsByStatus :request {:status ["sold"]}}))
```