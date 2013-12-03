(ns data-lang.python-denormalizer
  (:require [data-lang.helpers :as h]))

(defn denormalize [norms env]
  (if (map? norms) 
    ;; is Expr
    (cond
     ;; function define
     (contains? norms :function)
     (let [{:keys [function params id body]} norms
           env (h/add-fn-to-env env id function)
           arg-names (map :name params)
           ;; function (symbol (str function (list* arg-names) ":")) 
           [denorm-body inner-env] (map #(first (denormalize % (h/add-params-to-env env params))) body)]
       [(format "def %s (%s):" function (reduce #(str %1 ", " %2) arg-names))
        env])
     (contains? norms :expr)
     (let [expr (h/lookup-func env (:expr norms))
           args (map #(first (denormalize % env)) (:args norms))]
       [(format "%s(%s)" expr (reduce #(str %1 ", " %2) args))
        env]))
    ;; if primitive...
    [(h/lookup-func env norms) env]))

(defn denormalize-all [norm-list language-mappings]
  (loop [cur (first norm-list)
         next (rest norm-list)
         env language-mappings
         ret []]
    (let [[denormed env] (denormalize cur env)
          ret (conj ret denormed)]
      (if (empty? next)
        [ret env]
        (recur (first next)
               (rest next)
               env
               ret)))))

