(ns deburger.core-test
  (:require [clojure.test :refer :all]
            [deburger.core :as deburger]))


(defn test-sum-function [num-a num-b]
  (+ num-a num-b))

;; just to skip compiler errors
(declare num-a num-b)


(deftest tracing-functions-test
  (testing "Traces should be empty"
    (let [traced-fns (deburger/show-traced)]
      (is (empty? traced-fns))))

  (testing "Should create a trace entry for function"
    (deburger/trace-fn 'test-sum-function)

    (let [traced-fns (deburger/show-traced)]
      (is (= traced-fns
             [:deburger.core-test/test-sum-function]))))

  (testing "Original function call should add a trace-call entry"
    (test-sum-function 5 8)

    (let [result (deburger/show-result 'test-sum-function)
          inputs (deburger/show-inputs 'test-sum-function)]
      (is (= result 13))
      (is (= inputs [5 8]))))

  (testing "Should define a global variables from function inputs"
    (test-sum-function 53 48)
    (deburger/define-inputs 'test-sum-function)

    (is (= num-a 53))
    (is (= num-b 48)))

  (deburger/untrace-all))
