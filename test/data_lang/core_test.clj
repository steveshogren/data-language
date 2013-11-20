(ns data-lang.core-test
  (:require [clojure.test :refer :all]
            [data-lang.core :refer :all]))

(deftest normalize-test
  (testing "Normalize function"
    (is (=
         (with-redefs [gensym (fn [x] 'test)]
           (normalize-all
            '((define (adder x y)
                (+ x y))
              (adder 1 2))))
         '[{:id test, :function adder, :args ([test x] [test y]), :body {:expr +, :args (test test)}} {:expr test, :args (1 2)}]
           ))
    (is (=
         (denormalize-all '({:id 1 :function adder, :args (x y), :body ({:expr +, :args (x y)})}
                            {:expr 1, :args (1 2)}))
         '[(define (adder x y) (+ x y)) (adder 1 2)]
           ))))



