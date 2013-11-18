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
       (= '+ func) {:expr `+ :args (map normalize args)}
       :else :fail))
    ;; if primitive...
    denorms))

(defn normalize-all [denorm-list]
  (map normalize denorm-list))

(normalize-all
 '((define (adder x y)
     (+ x y))
   (adder 1 2)))



