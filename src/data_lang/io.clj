(ns data-lang.io
  (:require [clojure.edn :as r]))

(defn read-edn [filename]
  (with-open [infile (java.io.PushbackReader. (clojure.java.io/reader filename))]
    (binding [*in* infile]
      (let [edn-seq (repeatedly r/read)]
        (first edn-seq)))))

(defn write-edn [d filename]
  (with-open [w (clojure.java.io/writer filename :append false)]
    (.write w (pr-str d))))

(defn read-data [filename]
  (slurp filename))

(defn write-data [d filename]
  (with-open [w (clojure.java.io/writer filename :append false)]
    (.write w (str d))))

