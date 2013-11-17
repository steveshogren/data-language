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
