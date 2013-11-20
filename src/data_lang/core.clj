(ns data-lang.core)

(defn- lookup-func-name [env name-to-find]
  (if (= '+ name-to-find) name-to-find
      (let [[id func-name] (first
                            (filter (fn [[id func]] (= func name-to-find))
                                    env))]
        id)))

(defn normalize [denorms env]
  (if (list? denorms) 
    ;; is Expr
    (let [func-name (first denorms)
          args (rest denorms)]
      (cond
       (= 'define func-name)
       (let [[name & fargs] (first args)
             id (gensym name)
             body (rest args)
             env (conj env [id name])
             [body i-env] (map #(first (normalize % env)) body)]
         [{:id id :function name :args fargs :body body}
          env])
       :else
       (let [[normed env] (map #(first (normalize % env)) args)]
         [{:expr func-name #_(lookup-func-name env func-name) :args normed}
          env])))
    ;; if primitive...
    [denorms env]))

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


(defn- lookup-func [env id-to-find]
  (if (number? id-to-find)
    (let [[id func-name] (first
                     (filter (fn [[id func]] (= id id-to-find))
                             env))]
      func-name)
    id-to-find))

(defn- denormalize [norms env]
  (if (map? norms) 
    ;; is Expr
    (cond
     ;; function define
     (contains? norms :function)
     (let [func (:function norms)
           args (:args norms)
           id (:id norms)
           body (:body norms)
           env (conj env [id func])
           [denorm-body inner-env] (map #(first (denormalize % env)) body)]
       [`(~'define (~func ~@args) ~denorm-body)
        env])
     (contains? norms :expr)
     (let [expr (lookup-func env (:expr norms))
           args (:args norms)]
       [`(~expr ~@args)
        env]))
    ;; if primitive...
    norms))


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

(denormalize-all '({:id 1 :function adder, :args (x y), :body ({:expr +, :args (x y)})}
                   {:expr 1, :args (1 2)}))




