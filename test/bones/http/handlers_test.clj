(ns bones.http.handlers-test
  (:require [bones.http.handlers :as handlers]
            [bones.http.auth :as auth]
            [clojure.test :refer [deftest testing is]]
            [clojure.edn :as edn]
            [io.pedestal.test :refer [response-for]]
            [io.pedestal.http :as bootstrap]
            [io.pedestal.http.route.definition :refer [defroutes]]
            [bones.http.service :as service]
            [ring.middleware.session.store :as store]
            [ring.util.codec :as codec]
            [clojure.core.async :as a]
            [schema.core :as s]
            ))

(defn test-events [event-channel ctx]
  (a/put! event-channel {:name "greetings"
                         :data "hello"
                         :id 1})
  (a/close! event-channel))

;; pretend query handler
(defn graph [params req]
  (let [g (-> params :q edn/read-string)]
    {:graph g}))

(def graph-spec {:q s/Str})

(def conf {:http/auth {:secret  (apply str (map char (range 32)))
                       :cookie-name "pizza"}
           :http/handlers {:event-stream-handler test-events
                           :query-handler graph
                           :query-schema graph-spec}})
(def shield (.start (auth/map->Shield {:conf conf})))
(def cqrs (.start (handlers/map->CQRS {:shield shield
                                       :conf conf})))
(def routes (:routes cqrs))
(def service
  (::bootstrap/service-fn (bootstrap/create-servlet (service/service routes {}))))

(defn edn-post
  ([body-params]
   (edn-post body-params {}))
  ([body-params headers]
   (edn-post body-params headers "/api/command"))
  ([body-params headers path]
   (response-for service
                 :post path
                 :body (pr-str body-params)
                 :headers (merge {"Content-Type" "application/edn"
                                  "Accept" "application/edn"}
                                 headers))))

(defn login-post [body-params]
  (edn-post body-params {} "/api/login"))

(defn http-get
  ([path]
   (http-get path {}))
  ([path headers]
   (response-for service
                 :get path
                 :headers headers)))

(defn edn-get
  ([path]
   (edn-get path {}))
  ([path headers]
   (http-get path (merge {"Content-Type" "application/edn"
                          "Accept" "application/edn"}
                         headers))))

(defn edn-body [{:keys [body]}]
  (-> body edn/read-string))

(def valid-token
  {"authorization" (str "Token " (auth/token shield {:identity {:user-d 123}}))})

(def invalid-token
  {"authorization" "Token nuthin"})

(deftest register-command-test
  ;; this affects the tests below - maybe use fixtures somehow
  (testing "optional handler argument"
    (is (handlers/register-command :test {} ::hello)))
  (testing "explicit handler with a namespace"
    (is (handlers/register-command :test {} ::handlers/echo)))
  (testing "non existing function throws error"
    (is (thrown? clojure.lang.ExceptionInfo
                 (handlers/register-command :nope {}))))
  (testing "all at once with register-commands"
    (is (handlers/register-commands [[:test {} ::hello]
                                     [:test {} ::handlers/echo]]))))

;; pretend command handler
(defn hello [args req]
  (let [who (get-in args [:who])]
    {:greeting (str "hello " who)}))

(handlers/register-command :hello {:who s/Str})

;; pretend command handler
(defn thrower [args req]
  (throw (ex-info "something fake" {:other "stuff" :status 555})))

(handlers/register-command :thrower {:what s/Keyword})

(deftest command-resource-test
  ;; given the registered commands above
  (testing "invalid token"
    (let [body-params {:command :hello :args {:who "mr teapot"}}
          response (edn-post body-params invalid-token)]
      (is (= "{:message \"Not Authenticated\"}" (:body response)))
      (is (= "application/edn"  (get (:headers response)  "Content-Type")))
      (is (= 401 (:status response)))))

  (testing "non-existant args"
    (let [response (edn-post {:command :echo #_no-args} valid-token)]
      (is (= "{:message \"args not valid\", :data {:args missing-required-key}}" (:body response)))
      (is (= "application/edn" (get (:headers response) "Content-Type")))
      (is (= 400 (:status response)))))

  (testing "non-existant command"
    (let [response (edn-post {:command :nuthin} valid-token)]
      (is (= {:message "command not found"
              :data {:message "command not found: :nuthin"
                     :available-commands '(:echo :hello :thrower :login)}}
             (edn-body response)))
      (is (= "application/edn" (get (:headers response) "Content-Type")))
      (is (= 400 (:status response)))))

  (testing "args that are something other than a map"
    (let [response (edn-post {:command :echo :args [:not :a-map]} valid-token)]
      (is (= "{:message \"args not valid\", :data {:args (not (map? [:not :a-map]))}}" (:body response)))
      (is (= "application/edn" (get (:headers response) "Content-Type")))
      (is (= 400 (:status response)))))

  (testing "built-in echo command with valid args"
    (let [response (edn-post {:command :echo :args {:yes :allowed}} valid-token)]
      (is (= "{:yes :allowed}" (:body response)))
      (is (= "application/edn" (get (:headers response) "Content-Type")))
      (is (= 200 (:status response)))))

  (testing "registered command with valid args"
    (let [body-params {:command :hello :args {:who "mr teapot"}}
          response (edn-post body-params valid-token)]
      (is (= "{:greeting \"hello mr teapot\"}" (:body response)))
      (is (= "application/edn" (get (:headers response) "Content-Type")))
      (is (= 200 (:status response)))))

  (testing "an exception's ex-data gets rendered and status gets used"
    (let [body-params {:command :thrower :args {:what :blowup}}
          response (edn-post body-params valid-token)]
      (is (= "application/edn" (get (:headers response) "Content-Type")))
      (is (= "{:message \"something fake\", :data {:other \"stuff\"}}" (:body response)))
      (is (= 555 (:status response))))))

;; pretend command handler
(defn login [args req]
  (if (= "shortandstout" (:password args))
    {:user-id 123 :role "appliance"}))

(handlers/register-command :login {:username s/Str :password s/Str})

(def valid-login {:command :login :args {:username "mr teapot" :password "shortandstout"}})

(def invalid-login {:command :login :args {:username "mr teapot" :password "tallandfuzzy"}})

(deftest login-resource-test
  ;; given the registered login handler above
  (testing "Set-Cookie is set on response if login command returns data(non-nil)"
    (let [response (login-post valid-login)]
      (is (= 200 (:status response)))
      (is (= (get (:headers response) "Content-Type") "application/edn"))
      (is (contains? (:headers response) "Set-Cookie"))
      (is (contains? (edn-body response) :token))))
  (testing "no session is set if login command returns nil"
    (let [response (login-post invalid-login)]
      (is (= 400 (:status response)))
      (is (not (contains? (:headers response) "Set-Cookie")))
      (is (= {:message "Invalid Credentials"} (edn-body response)))
      (is (= (get (:headers response) "Content-Type") "application/edn")))))

(deftest command-list-resource-test
  ;; this is order/time sensitive because of the registered commands
  (testing "get"
    (let [response (edn-get "/api/command")]
      (is (= '{:available-commands ({:args {Any Any}
                                    :command (enum :echo)}
                                   {:args {:who java.lang.String}
                                    :command (enum :hello)}
                                   {:args {:what Keyword}
                                    :command (enum :thrower)}
                                    ;; login should probably be filtered out?
                                   {:args {:username java.lang.String
                                           :password java.lang.String}
                                    :command (enum :login)})}
             (edn-body response)))
      (is (= "application/edn" (get (:headers response) "Content-Type")))
      (is (= 200 (:status response))))))

(deftest query-resource-test
  ;; given the registered query handler above
  (testing "a valid token or session is required"
    (let [response (edn-get "/api/query?q=abc" invalid-token)]
      (is (= 401 (:status response)))))
  (testing "the query handler can be overwritten by `register-query-handler'"
    (let [response (edn-get "/api/query?q=abc" valid-token)]
      (is (= "{:graph abc}" (:body response)))
      (is (= 200 (:status response)))))
  (testing "the query params gets checked against the schema"
    (let [response (edn-get "/api/query?something=else" valid-token)]
      (is (= "{:message \"query params not valid\", :data {:q missing-required-key, :something disallowed-key}}" (:body response)))
      (is (= 400 (:status response))))))

(defn parse-cookie [cookie]
  (-> cookie
      first
      (clojure.string/split #";")
      first))

(deftest session-requests
  (let [login-response (login-post valid-login)
        cookie (get-in login-response [:headers "Set-Cookie"])]
    (testing "command with session - reuse the cookie"
      (let [body-params {:command :echo :args {:who "mr teapot"}}
            response (edn-post body-params {"cookie" (parse-cookie cookie)})]
        (is (= "{:who \"mr teapot\"}" (:body response)))
        (is (= 200 (:status response)))))))

(deftest event-stream-test
  (testing "sends data from a core/async put!"
    ;; doesn't require a Content-Type header
    (let [response (http-get "/api/events" valid-token)
          rn "\r\n"]
      (is (= (str
              "event: greetings" rn
              "data: hello" rn
              "id: 1" rn rn) (:body response)))
      (is (= 200 (:status response))))))
