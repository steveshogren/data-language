(ns data-lang.core
  (:use clojure.tools.trace)
  (:require [data-lang.denormalizer :as d]
            [data-lang.normalizer :as n]
            [data-lang.io :as io])
  (:gen-class))

(def language-mappings
  '([scheme.+ + 2]
      [scheme.- - 2]))

(def store-name "test.edn")
(def filename "test.rkt")

(defn round-trip []
  (let [denorm-input  (io/read-data filename)
        normalized (n/normalize-all denorm-input language-mappings)]
    (do 
      (io/write-edn normalized store-name)
      (let [norm-from-store (io/read-edn store-name)
            denormalized (d/denormalize-all norm-from-store language-mappings)]
        (io/write-data denormalized filename)))))

(defn renam [data name new-name]
  (map
   #(if (and (contains? % :function)
             (= name (:function %)))
      (assoc % :function new-name)
      %)
   data))

(defn rename-global-symbol [name new-name]
  (let [data (io/read-edn store-name)
        data (renam data name new-name)
        denormalized (d/denormalize-all data language-mappings)]
    (do
      (io/write-edn data store-name)
      (io/write-data denormalized filename))))

(defn -main
  [filename & args]
  (round-trip filename))

