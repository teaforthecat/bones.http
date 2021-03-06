(ns bones.http-test
  (:require [bones.http :as http]
            [clojure.test :refer [deftest testing is]]
            [com.stuartsierra.component :as component]
            [clojure.spec.alpha :as s]))

(s/def ::any-q map?)
(s/def ::abc string?)
(s/def ::abc-map (s/keys :req-un [::abc]))

(def sys (atom {}))
(def conf {::http/auth
           {:secret "a 16 byte stringa 32 byte string"
            :cookie-name "pizza"}})

(deftest start-system
  (testing "shield gets created and used by routes"
    (http/build-system sys conf)
    (http/start sys)
    (is (= "a 16 byte stringa 32 byte string" (get-in @sys [:routes :shield :secret])))
    (is (= "pizza" (get-in @sys [:routes :shield :cookie-opts :cookie-name])))
    (http/stop sys)))

(deftest validate-configuration
  (testing "missing a schema"
    (let [commands [[:a]]
          config (assoc-in conf [::http/handlers :commands] commands)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (http/build-system sys config)))))
  (testing "a command with an explicit handler"
    (let [commands [[:a ::abc-map 'a]
                    [:a ::abc-map 'a]]
          config (assoc-in conf [::http/handlers :commands] commands)]
      (is (satisfies? component/Lifecycle
                      (http/build-system sys config)))))
  (testing "missing a query schema"
    (let [query [(fn [req] "hi")]
          config (assoc-in conf [::http/handlers :query] query)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (http/build-system sys config)))))
  (testing "a valid query schema"
    (let [query [::any-q (fn [req] "hi")]
          config (assoc-in conf [::http/handlers :query] query)]
      (is (satisfies? component/Lifecycle
                      (http/build-system sys config)))))
  (testing "missing a login schema"
    (let [login [(fn [req] "hi")]
          config (assoc-in conf [::http/handlers :login] login)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (http/build-system sys config)))))
  (testing "a valid login schema"
    (let [login [::any-q (fn [req] "hi")]
          config (assoc-in conf [::http/handlers :login] login)]
      (is (satisfies? component/Lifecycle
                      (http/build-system sys config))))))
