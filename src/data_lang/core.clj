(ns data-lang.core
  (:use [clojure.tools.trace]))

(defn lookup-by-name [env name-to-find]
  (cond (some #(= (second %) name-to-find) env)
        (let [[id func-name] (first
                              (filter (fn [[id name]] (= name name name-to-find))
                                      env))]
          id)
        (number? name-to-find) name-to-find
        :else (symbol (str name-to-find "UNBOUND"))))
;;(lookup-by-name '([adder1631 adder] [adder.x1632 x] [adder.y1633 y]) 'y)

(defn lookup-param-count [env name-to-find]
  (let [[_ _ param-count]
        (first (filter (fn [[_ name]] (= name name name-to-find)) env))]
    param-count))

(defn add-fn-to-env [env id name params]
  (conj env [id name (count params)]))

(defn add-params-to-env [env params]
  (let [params (map (fn [{:keys [id name]}] [id name 0]) params)]
    (concat env params)))

(defn make-fn [id name params body]
  {:id id :function name :params params :body body})

(defn make-param [fn-name param-name]
  {:id (gensym (str fn-name "." param-name)) :name param-name})

(defn expr? [x] (list? x))

(defn normalize [denorms env]
  (if (expr? denorms) 
    (let [func-name (first denorms)
          args (rest denorms)]
      (cond
       (= 'define func-name)
       (let [[name & params] (first args)
             id (gensym name)
             params (map #(make-param name %) params)
             env (add-fn-to-env env id name params)
             body (map #(first (normalize % (add-params-to-env env params)))
                       (rest args))]
         [(make-fn id name params body) env])
       :else
       (let [normed-args (map #(first (normalize % env)) args)
             actual-arg-count (count normed-args)
             expected-arg-count (lookup-param-count env func-name)
             arg-error (cond 
                        ;; When looking up library calls, temporary
                        (nil? expected-arg-count) []
                        (< actual-arg-count expected-arg-count) [:MISSING-ARGS]
                        (> actual-arg-count expected-arg-count) [:TOO-MANY-ARGS]
                        :else [])]
         [{:expr (lookup-by-name env func-name) :args (concat normed-args arg-error)}
          env])))
    ;; if primitive...
    [(lookup-by-name env denorms) env]))

#_(denormalize-all)
(normalize-all
   '(
     (define (adder x y)
       (+ x y))
     (define (adder y)
       (+ y))
     (adder 1)
    ))
#_(normalize-all
 '(
   (+ 1 1)
     ))

(defn normalize-all [denorm-list]
  (loop [cur (first denorm-list)
         next (rest denorm-list)
         env '([lang.+ + 2]
               [lang.- - 2])
         ret []]
    (let [[normed env] #_(trace "pout") (normalize cur env)
          ret (conj ret normed)]
      (if (empty? next) ret
          (recur (first next)
                 (rest next)
                 env
                 ret)))))


(defn lookup-func [env id-to-find]
  (if (some #(= (first %) id-to-find) env)
    (let [[id func-name] (first
                     (filter (fn [e] (= (:id e) id-to-find))
                             env))]
      func-name)
    id-to-find))

(defn denormalize [norms env]
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
           args (map #(first (denormalize % env)) (:args norms))]
       [`(~expr ~@args)
        env]))
    ;; if primitive...
    [(lookup-func env norms) env]))


(defn denormalize-all [norm-list]
  (loop [cur (first norm-list)
         next (rest norm-list)
         env '() 
         ret []]
    (let [[denormed env] (denormalize cur env)
          ret (conj ret denormed)]
      (if (empty? next) ret
          (recur (first next)
                 (rest next)
                 env
                 ret)))))

(denormalize-all
   '[{:id adder2838, :function adder, :args ([x2839 x] [y2840 y]), :body ({:expr +, :args (x2839 y2840)})}
     {:expr adder2838, :args (1 {:expr +, :args (1 1)})}])





