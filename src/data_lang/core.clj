(ns data-lang.core
  (:use clojure.tools.trace)
  (:require [data-lang.denormalizer :as d]
            [data-lang.python-denormalizer :as pythond]
            [data-lang.normalizer :as n]
            [data-lang.io :as io])
  (:gen-class))

;; Trying to come up with a storage format that allows
;; multiple signatures
(def language-mappings
  '([clojure.+ + [[&]]]
      [clojure.print print [[&]]]
      [clojure.if if [[2] [3]]]
      [clojure.equal = [[&]]]
      [clojure.- - [[&]]]))

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
           (or  (= "h" cmd) (= "help" cmd))
           (do (println "s causes a save-rewrite, r old new")
               (recur 1))
           (or  (= "s" cmd) (= "save" cmd))
           (do (round-trip)
               (println "Written..")
               (recur 1))
           (or (= "r" cmd) (= "rename" cmd))
           (do (rename-global-symbol (symbol (first args))
                                     (symbol (second args)))
               (println "Written..")
               (recur 1))
           (= "q" cmd) nil
           :else (do (println "Invalid command: try 's' or 'r old new'")
                     (recur 1)))))))
               

