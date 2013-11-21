(ns data-lang.core
  (:use [clojure.tools.trace]))

(defn lookup-func-name [env name-to-find]
  (cond (some #(= (second %) name-to-find) env)
        (let [[id func-name] (first
                              (filter (fn [[id name]] (= name name-to-find))
                                      env))]
          id)
        (or (number? name-to-find)
            (some #(= name-to-find %) ['+ '-]))
        name-to-find
        :else (symbol (str name-to-find "UNBOUND" ))))


(defn normalize [denorms env]
  (if (list? denorms) 
    ;; is Expr
    (let [func-name (first denorms)
          args (rest denorms)]
      (cond
       (= 'define func-name)
       (let [[name & params] (first args)
             id (gensym name)
             params (map (fn [x] [(gensym (str name "." x)) x]) params)
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


#_(denormalize-all
  (normalize-all
    '(
      (define (adder x y)
        (+ x y))
      (adder 1 (+ 1 1))
      (define (subtracter x y)
        (- x y))
      (adder 1 (subtracter 3 1))
      )))

(defn lookup-func [env id-to-find]
  (if (some #(= (first %) id-to-find) env)
    (let [[id func-name] (first
                     (filter (fn [[id func]] (= id id-to-find))
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
         env []
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





