(ns recognitum.walkthrough-test
  "Each walkthrough under `dev` runs to the end and prints what its sections show."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [recognitum.authz :as authz]
   [recognitum.demo :as demo]
   [recognitum.orders :as orders]))

(defn- printed [main] (with-out-str (main)))

(deftest the-heist-demo-runs
  (is (str/includes? (printed demo/-main) "only-crew-get-cuts")))

(deftest the-refund-walkthrough-runs
  (let [out (printed authz/-main)]
    (is (str/includes? out "refunded 1500 cents on order 42 for dana"))
    (is (str/includes? out "[:allowed [[\"dana\" :refund 42] [\"eli\" :refund 42] [\"eli\" :refund 41]]]"))
    (is (str/includes? out "after a grant is revoked"))))

(deftest the-order-walkthrough-runs
  (let [out (printed orders/-main)]
    (is (str/includes? out "=> 3300"))
    (is (str/includes? out "billing-country-served"))))
