(ns data-lang.core
  (:use [clojure.tools.trace]))

(defn lookup-func-name [env name-to-find]
  (if (some #(= (second %) name-to-find) env)
        (let [[id func-name] (first
                              (filter (fn [[id name]] (= name name-to-find))
                                      env))]
          id)
        name-to-find))
#_(lookup-func-name [[1 :a] [2 :b]] :c)

(deftrace normalize [denorms env]
  (if (list? denorms) 
    ;; is Expr
    (let [func-name (first denorms)
          args (rest denorms)]
      (cond
       (= 'define func-name)
       (let [[name & params] (first args)
             id (gensym name)
             params (map (fn [x] [(gensym x) x]) params)
             body (rest args)
             env (conj env [id name])
             body (map #(first (normalize % (concat env params))) body)]
         [{:id id :function name :args params :body body}
          env])
       :else
       (let [normed-args (map #(first (normalize % env)) args)]
         [{:expr (lookup-func-name env func-name) :args normed-args}
          env])))
    ;; if primitive...
    [(lookup-func-name env denorms) env]))

(defn normalize-all [denorm-list]
  (loop [cur (first denorm-list)
         next (rest denorm-list)
         env []
         ret []]
    (let [[normed env] (normalize cur env)
          ret (conj ret normed)]
      (if (empty? next) ret
          (recur (first next)
                 (rest next)
                 env
                 ret)))))

(normalize-all
    '(
      (define (adder x y)
        (+ x y))
      (adder 1 2)
      ))

(deftrace lookup-func [env id-to-find]
  (if (some #(= (first %) id-to-find) env)
    (let [[id func-name] (first
                     (filter (fn [[id func]] (= id id-to-find))
                             env))]
      func-name)
    id-to-find))

(deftrace denormalize [norms env]
  (if (map? norms) 
    ;; is Expr
    (cond
     ;; function define
     (contains? norms :function)
     (let [{:keys [function args id body]} norms
           env (conj env [id function])
           arg-names (map second args)
           [denorm-body inner-env] (map #(first (denormalize % (concat env args))) body)]
       [`(~'define (~function ~@arg-names) ~denorm-body)
        env])
     (contains? norms :expr)
     (let [expr (lookup-func env (:expr norms))
           args (map #(lookup-func env %) (:args norms))]
       [`(~expr ~@args)
        env]))
    ;; if primitive...
    [(lookup-func env norms) env]))


(defn denormalize-all [norm-list]
  (loop [cur (first norm-list)
         next (rest norm-list)
         env []
         ret []]
    (let [[denormed env] (denormalize cur env)
          ret (conj ret denormed)]
      (if (empty? next) ret
          (recur (first next)
                 (rest next)
                 env
                 ret)))))

#_(denormalize-all
 '[{:id 11, :function adder, :args ([22 x] [33 y]),
    :body ({:expr +, :args (22 33)})}
   {:expr 11, :args (1 2)}])




