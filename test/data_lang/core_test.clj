(ns data-lang.core-test
  (:require [clojure.test :refer :all]
            [data-lang.normalizer :as n]
            [data-lang.denormalizer :as d]
            [data-lang.core :refer :all]))


(def new-language-mappings
  '([clojure.+ + 2 test]
      [clojure.print print 1 test]
      [clojure.if if 3 test]
      [clojure.equal = 2 test]
      [clojure.- - 2 test]))

(defn round [code]
  (let [[normed _] (n/normalize-all code new-language-mappings)
        [denormed _] (d/denormalize-all normed new-language-mappings)]
    denormed))
(defn denorm [edn]
  (let [[denormed _] (d/denormalize-all edn new-language-mappings)]
    denormed))
(defn norm [code]
  (let [[normed _] (n/normalize-all code new-language-mappings)]
    normed))

(def sample-edn
  '[{:id adderX,
     :function adder,
     :params ({:id adder.xX, :name x} {:id adder.yX, :name y}),
     :body ({:expr clojure.+, :args (adder.xX adder.yX)})
     :ns test}
    {:expr adderX, :args (1 2)}])

(deftest normalize-test
  (testing "Denormalizing"
    (is (= (denorm sample-edn)
           '[(defn adder [x y] (+ x y)) (adder 1 2)]))
    (is (= (denorm (rename-in-edn sample-edn 'adder 'adder2))
           '[(defn adder2 [x y] (+ x y)) (adder2 1 2)])))
  (testing "Round tripping"
    (is (= (round '[(defn adder [x y] (+ x y)) (adder 1 2)])
           '[(defn adder [x y] (+ x y)) (adder 1 2)]))
    (is (= (round '[(ns test.testns (:require [blah]))
                    (defn adder2 [x y] (+ x y)) (adder2 1 2)] )
       '[(ns test.testns (:require [blah]))
         (defn adder2 [x y] (+ x y)) (adder2 1 2)])))
  (testing "Renaming"
    (is (= (denorm (rename-in-edn sample-edn 'adder 'adder2))
           '[(defn adder2 [x y] (+ x y)) (adder2 1 2)])))
  (testing "Normalizing"
    (is (= (with-redefs [gensym (fn [x] (symbol (str x "X")))]
             (norm '((ns test) (defn adder [x y] (+ x y)) (adder 1 2))))
           sample-edn))))




