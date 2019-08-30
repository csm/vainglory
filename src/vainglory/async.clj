(ns vainglory.async
  (:require [vainglory.impl.client :as vc]))

(defn invoke
  "Invoke an asynchronous API call to a server.

  Client should be a constructed client produced by [[vainglory.core/client]].

  Request map is the same as [[vainglory.core/invoke]]. Returns a manifold
  stream that you can `take!` a single response value from. The value yielded
  by this `take!` is the same as that from [[vainglory.core/invoke]]."
  [client request]
  (vc/invoke client request))