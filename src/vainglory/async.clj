(ns vainglory.async
  "Asynchronous REST client built from swagger specs."
  (:require [vainglory.impl.client :as vc]))

(defn invoke
  "Invoke an asynchronous API call to a server.

  Client should be produced by [[vainglory.core/client]].

  Request map is the same as [[vainglory.core/invoke]]. Returns a manifold
  deferred that will yield the response. The value yielded
  by the deferred is the same as that from [[vainglory.core/invoke]]."
  [client request]
  (vc/invoke client request))