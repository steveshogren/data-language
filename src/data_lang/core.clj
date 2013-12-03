(ns data-lang.core
  (:use clojure.tools.trace)
  (:require [data-lang.denormalizer :as d]
            [data-lang.python-denormalizer :as pythond]
            [data-lang.normalizer :as n]
            [data-lang.io :as io])
  (:gen-class))

(def language-mappings
  '([scheme.+ + 2]
      [scheme.print print 1]
      [scheme.- - 2]))

(def store-name "test.edn")
(def filename "test.rkt")

(defn display  
  ([] (display d/denormalize-all)) 
  ([denormer] 
     (let [norm-from-store (io/read-edn store-name)
           [denormalized _] (denormer norm-from-store language-mappings)]
       (io/write-code denormalized filename))))

#_(defn display-p [] (display pythond/denormalize-all))
#_(display-p)

(defn save []
  (let [denorm-input (io/read-code filename)
        [normalized _] (n/normalize-all denorm-input language-mappings)]
    (io/write-edn normalized store-name)))

(defn round-trip []
  (do (save)
      (display)))

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
        [denormalized _] (d/denormalize-all data language-mappings)]
    (do
      (io/write-edn data store-name)
      (io/write-code denormalized filename))))

(defn -main
  [& args]
  #_(round-trip)
  (loop [x 1]
    (let [inp (read-line)]
      (if (= "y" inp)
        (do (round-trip)
            (recur 1))
        nil))))

