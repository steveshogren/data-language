(ns data-lang.core
  (:use clojure.tools.trace)
  (:require [data-lang.denormalizer :as d]
            [data-lang.normalizer :as n]
            [data-lang.io :as io])
  (:gen-class))

(def language-mappings
  '([scheme.+ + 2]
      [scheme.- - 2]))

(defn round-trip [filename]
  (let [denorm-input  (io/read-data filename)
        normalized (n/normalize-all denorm-input language-mappings)]
    (do 
      (io/write-edn normalized "test.edn")
      (let [norm-from-store (io/read-edn "test.edn")
            denormalized (d/denormalize-all norm-from-store language-mappings)]
        (io/write-data denormalized filename)))))

(round-trip "test.rkt")

(defn -main
  [filename & args]
  (round-trip filename))

