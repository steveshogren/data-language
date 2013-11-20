(ns data-lang.core)

(defn make-stmts []
  [{:fn-id 2 :arg1 2 :arg2 2}
   {:fn-id 1 :arg1 1 :arg2 1}])

(defn make-fns []
  [{:fn-id 1 :map 'map}
   {:fn-id 1 :map '+}])

(defn lookup-fn [id fns]
  (:map (first (filter #(= id (:fn-id %)) fns))))

(defn foo [stmts fns]
  (map
   (fn [x]
     (list (lookup-fn (:fn-id x) fns)
           (:arg1 x)
           (:arg2 x)))
   stmts))

(foo (make-stmts) (make-fns))


(defn mapper [fnk x]
  (defn inner [vals]
    (if (empty? vals) '()
        (conj (inner (rest vals))
              (fnk (first vals)))))
  (inner x))

(mapper (fn [z] (+ z 1)) [1 2 3 4 5]) 

(defn normalize [denorms]
  (if (list? denorms) 
    ;; is Expr
    (let [func (first denorms)
          args (rest denorms)]
      (cond
       (= 'define func)
       (let [[name & fargs] (first args)
             body (rest args)]
         {:function name :args fargs :body (map normalize body)})
       (= '+ func) {:expr '+ :args (map normalize args)}
       :else {:expr func :args (map normalize args)}))
    ;; if primitive...
    denorms))

(defn normalize-all [denorm-list]
  (map normalize denorm-list))

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




