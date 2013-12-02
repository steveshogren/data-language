(ns data-lang.io
  (:use clojure.pprint)
  (:require [clojure.edn :as r]
            clojure.pprint
            [clojure.java.io :as jio]))

(defn- safe-read [s]
  (binding [*read-eval* false]
    (read-string s)))

(defn read-edn [filename]
  (with-open [infile (java.io.PushbackReader. (clojure.java.io/reader filename))]
    (binding [*in* infile]
      (let [edn-seq (repeatedly r/read)]
        (first edn-seq)))))

(defn write-edn [d filename]
  (do
    (jio/delete-file filename)
    (with-open [w (clojure.java.io/writer filename :append false)]
      (.write w (pr-str d)))))

(defn read-code [filename]
  (safe-read (str "[" (slurp filename) "]")))

(defn write-code [d filename]
  (do
    (jio/delete-file filename)
    (with-open [w (jio/writer filename :append true)]
      (loop [cur (first d)
             res (rest d)]
          (if (nil? cur) nil
              (do (with-pprint-dispatch code-dispatch (pprint cur w))
                  (recur (first res) (rest res))))))))

