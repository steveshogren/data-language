(ns data-lang.core
  (:use clojure.tools.trace)
  (:require [data-lang.denormalizer :as d]
            [data-lang.python-denormalizer :as pythond]
            [data-lang.normalizer :as n]
            [data-lang.io :as io])
  (:gen-class))

(def language-mappings
  '([clojure.+ + 2]
      [clojure.print print 1]
      [clojure.if if 3]
      [clojure.equal = 2]
      [clojure.- - 2]))

(def store-name "test.edn")
(def filename "test.clj")

(defn display  
  ([] (display d/denormalize-all)) 
  ([denormer] 
     (let [norm-from-store (io/read-edn store-name)
           [denormalized _] (denormer norm-from-store language-mappings)]
       (io/write-code denormalized filename))))

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
  (do (round-trip)
      (println "Written..")
      (loop [x 1]
        (let [cmd (read-line)
              [cmd & args] (clojure.string/split cmd #" ")]
          (cond
           (or  (= "s" cmd)
                (= "save" cmd))
           (do (round-trip)
               (println "Written..")
               (recur 1))
           (or (= "r" cmd)
               (= "rename" cmd))
           (do (rename-global-symbol (symbol (first args))
                                     (symbol (second args)))
               (println "Written..")
               (recur 1))
           (= "q" cmd) nil
           :else (do (println "Invalid command: try 's' or 'r'")
                     (recur 1)))))))
               

