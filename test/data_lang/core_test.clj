(ns data-lang.core-test
  (:require [clojure.test :refer :all]
            [data-lang.normalizer :as n]
            [data-lang.denormalizer :as d]
            [data-lang.core :refer :all]))

data-lang.core/language-mappings

(defn round [code]
  (let [[normed _] (n/normalize-all code data-lang.core/language-mappings)
        [denormed _] (d/denormalize-all normed data-lang.core/language-mappings)]
    denormed))
(defn denorm [edn]
  (let [[denormed _] (d/denormalize-all edn data-lang.core/language-mappings)]
    denormed))
(defn norm [code]
  (let [[normed _] (n/normalize-all code data-lang.core/language-mappings)]
    normed))

(def sample-edn
  '[{:id adderX,
     :function adder,
     :params ({:id adder.xX, :name x} {:id adder.yX, :name y}),
     :body ({:expr clojure.+, :args (adder.xX adder.yX)})}
    {:expr adderX, :args (1 2)}])

(deftest normalize-test
  (testing "Normalize function"
    (is (= (with-redefs [gensym (fn [x] (symbol (str x "X")))]
             (norm '((defn adder [x y] (+ x y)) (adder 1 2))))
           sample-edn))
    (is (= (denorm sample-edn)
           '[(defn adder [x y] (+ x y)) (adder 1 2)]))
    (is (= (round '[(defn adder [x y] (+ x y)) (adder 1 2)])
           '[(defn adder [x y] (+ x y)) (adder 1 2)]))
    ))




