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
  (let [denorm-input (trace "data" (read-string (io/read-data filename)))
        normalized (n/normalize-all denorm-input language-mappings)
        denormalized (d/denormalize-all normalized language-mappings)]
    (do 
      (io/write-edn normalized "test.edn")
      (io/write-data denormalized filename))))

#_(read-string  "(define (test x y) (+ x y))")

(round-trip "test.rkt")

(defn -main
  [filename & args]
  (round-trip filename))

