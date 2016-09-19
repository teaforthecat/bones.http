(ns bones.http.service
  (:require [com.stuartsierra.component :as component]
            [aleph.http :as http]
            [bidi.ring :refer [make-handler]]))

(defrecord Server [conf routes]
  component/Lifecycle
  (start [cmp]
    (let [config (get-in cmp [:conf :http/service])
          app (get-in cmp [:routes])]
      (assoc cmp :server (http/start-server (make-handler (:routes app))
                                            {:port (or (:port config) 3000)
                                             :raw-stream? true}))))
  (stop [cmp]
    (when-let [server (:server cmp)]
      (.close server))
    ;; dissoc looses type
    (assoc cmp :server nil)))
