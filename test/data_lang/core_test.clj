(ns data-lang.core-test
  (:require [clojure.test :refer :all]
            [data-lang.core :refer :all]))

(deftest normalize-test
  (testing "Normalize function"
    (is (= (denormalize-all (normalize-all
                             '((define (adder x y)
                                 (+ x y))
                               (adder 1 2))))
           '((define (adder x y)
               (+ x y))
             (adder 1 2))))))

