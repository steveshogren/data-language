(ns data-lang.normalizer
  (:require [data-lang.helpers :as h]))

(def missing :MISSING-ARGS)
(def too-many :TOO-MANY-ARGS)

(declare normalize)

(defn normalize-body [bodies env new-bindings]
  (map #(first (normalize % (h/add-params-to-env env new-bindings)))
       bodies))

(defn normalize [denorms env]
  (if (h/expr? denorms) 
    (let [func-name (first denorms)
          args (rest denorms)]
      (cond
       (= 'defn func-name)
       (let [[name params & body] args
             id (gensym name)
             params (map #(h/make-param name %) params)
             env (h/add-fn-to-env env id name params)
             body (normalize-body body env params)]
         [(h/make-fn id name params body) env])
       #_(= 'let func-name)
       #_(let [bindings (first args)
             body (rest args)
             bindings (map (fn [binding val]
                             [(h/make-param "" binding)
                              (normalize val)])
                           bindings)
             body (normalize-body (rest body) env (map first bindings))]
         [{:expr (h/lookup-by-name env func-name)
           :args (concat bindings body)} env])
       ;; do let*
       :else
       (let [normed-args (map #(first (normalize % env))
                              (filter #(and (not= too-many %)
                                           (not= missing %))
                                      args))
             actual-arg-count (count normed-args)
             expected-arg-counts (h/lookup-param-count env func-name)
             arg-error (detect-arg-errors expected-arg-counts actual-arg-count)]
         [{:expr (h/lookup-by-name env func-name) :args (concat normed-args arg-error)}
          env])))
    ;; if primitive...
    [(h/lookup-by-name env denorms) env]))

(defn detect-arg-errors [expected-arg-counts actual-arg-count]
  (cond 
   ;; When looking up library calls, temporary
   (nil? expected-arg-count) []
   (< actual-arg-count expected-arg-counts) [missing]
   (> actual-arg-count expected-arg-counts) [too-many]
   :else []))

(defn normalize-all [denorm-list language-mappings]
  (loop [cur (first denorm-list)
         next (rest denorm-list)
         env language-mappings
         ret []]
    (let [[normed env] (normalize cur env)
          ret (conj ret normed)]
      (if (empty? next)
        [ret env]
        (recur (first next)
               (rest next)
               env
               ret)))))
