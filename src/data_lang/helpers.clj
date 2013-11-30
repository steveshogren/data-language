(ns data-lang.helpers)

(defn lookup-by-name [env name-to-find]
  (cond (some #(= (second %) name-to-find) env)
        (let [[id func-name] (first
                              (filter (fn [[id name]] (= name name name-to-find))
                                      env))]
          id)
        (number? name-to-find) name-to-find
        :else (symbol (str name-to-find "-UNBOUND"))))

(defn lookup-param-count [env name-to-find]
  (let [[_ _ param-count]
        (first (filter (fn [[_ name]] (= name name name-to-find)) env))]
    param-count))

(defn add-fn-to-env
  ([env id name] (add-fn-to-env env id name []))
  ([env id name params] (conj env [id name (count params)])))

(defn add-params-to-env [env params]
  (let [params (map (fn [{:keys [id name]}] [id name 0]) params)]
    (concat env params)))

(defn make-fn [id name params body]
  {:id id :function name :params params :body body})

(defn make-param [fn-name param-name]
  {:id (gensym (str fn-name "." param-name)) :name param-name})

(defn expr? [x] (list? x))


;; Denormalize

(defn lookup-func [env id-to-find]
  (if (some #(= (first %) id-to-find) env)
    (let [[id func-name] (first
                          (filter (fn [[id name]] (= id id-to-find))
                                  env))]
      func-name)
    id-to-find))
