(ns data-lang.core-test
  (:require [clojure.test :refer :all]
            [data-lang.core :refer :all]))

(deftest normalize-test
  (testing "Normalize function"
    (is (=
         (with-redefs [gensym (fn [x] (symbol (str x "X")))]
           (normalize-all
            '((define (adder x y)
                (+ x y))
              (adder 1 2))))
         '[{:id adderX, :function adder, :args ([xX x] [yX y]), :body ({:expr +, :args (xX yX)})} {:expr adderX, :args (1 2)}]
           ))
    (is (=
         (denormalize-all
          '[{:id adderX, :function adder, :args ([xX x] [yX y]), :body ({:expr +, :args (xX yX)})} {:expr adderX, :args (1 2)}])
         '[(define (adder x y) (+ x y)) (adder 1 2)]
         ))
    (is (=
         (denormalize-all
          (normalize-all
           '[(define (adder x y) (+ x y)) (adder 1 2)]))
         '[(define (adder x y) (+ x y)) (adder 1 2)]
         ))

    ))



