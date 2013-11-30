(ns data-lang.normalizer
  (:require [data-lang.helpers :as h]))

(defn normalize [denorms env]
  (if (h/expr? denorms) 
    (let [func-name (first denorms)
          args (rest denorms)]
      (cond
       (= 'define func-name)
       (let [[name & params] (first args)
             id (gensym name)
             params (map #(h/make-param name %) params)
             env (h/add-fn-to-env env id name params)
             body (map #(first (normalize % (h/add-params-to-env env params)))
                       (rest args))]
         [(h/make-fn id name params body) env])
       :else
       (let [normed-args (map #(first (normalize % env)) args)
             actual-arg-count (count normed-args)
             expected-arg-count (h/lookup-param-count env func-name)
             arg-error (cond 
                        ;; When looking up library calls, temporary
                        (nil? expected-arg-count) []
                        (< actual-arg-count expected-arg-count) [:MISSING-ARGS]
                        (> actual-arg-count expected-arg-count) [:TOO-MANY-ARGS]
                        :else [])]
         [{:expr (h/lookup-by-name env func-name) :args (concat normed-args arg-error)}
          env])))
    ;; if primitive...
    [(h/lookup-by-name env denorms) env]))

#_(denormalize-all
 (normalize-all
    '(
      (define (adder x y)
        (+ x y))
      (adder 1 2)
    )))

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
