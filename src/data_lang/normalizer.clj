(ns data-lang.normalizer
  (:require [data-lang.helpers :as h]))

(def missing :MISSING-ARGS)
(def too-many :TOO-MANY-ARGS)

(declare normalize)

(defn normalize-body [bodies env new-bindings ns name]
  (map #(first (normalize % (h/add-params-to-env env new-bindings ns) ns))
       bodies))

(defn normalize [denorms env ns]
  (if (h/expr? denorms) 
    (let [func-name (first denorms)
          args (rest denorms)]
      (cond
       (= 'defn func-name)
       (let [[name params & body] args
             id (gensym name)
             params (map #(h/make-param name %) params)
             env (h/add-fn-to-env env id name params ns)
             body (normalize-body body env params ns name)]
         [(h/make-fn id name params body ns) env])
       :else
       (let [normed-args (map #(first (normalize % env ns))
                              (filter #(and (not= too-many %)
                                           (not= missing %))
                                      args))
             actual-arg-count (count normed-args)
             expected-arg-count (h/lookup-param-count env func-name ns)
             arg-error (cond 
                        ;; When looking up library calls, temporary
                        (nil? expected-arg-count) []
                        (< actual-arg-count expected-arg-count) [missing]
                        (> actual-arg-count expected-arg-count) [too-many]
                        :else [])]
         [{:expr (h/lookup-by-name env func-name ns) :args (concat normed-args arg-error)}
          env])))
    ;; if primitive...
    [(h/lookup-by-name env denorms ns) env]))

(defn norm-ns [denorm-list]
  (if (= 'ns (first (first denorm-list)))
    [(second (first denorm-list)) (rest denorm-list)]
    ['user denorm-list]))

(defn normalize-all [denorm-list language-mappings]
  (let [[ns denorm-list] (norm-ns denorm-list)]
    (loop [cur (first denorm-list)
           next (rest denorm-list)
           env language-mappings
           ret []]
      (let [[normed env] (normalize cur env ns)
            ret (conj ret normed)]
        (if (empty? next)
          [ret env]
          (recur (first next)
                 (rest next)
                 env
                 ret))))))
