(ns data-lang.helpers)

;; Find the id by name/ns
(defn lookup-by-name [env name-to-find ns-to-find]
  (let [unbound "-UNBOUND"
        name-to-find (if (number? name-to-find)
                       name-to-find
                       (symbol (clojure.string/replace name-to-find unbound "")))]
    (if (number? name-to-find) name-to-find
        (let [match? (filter (fn [[id name _ ns]]
                               (and (= ns ns-to-find)
                                    (= name name-to-find)))
                             env)]
          (if (seq match?)
            (first (first match?))
            ;; none found, mark "unbound"
            (symbol (str name-to-find unbound)))))))

(defn lookup-param-count [env name-to-find ns-to-find]
  (let [[_ _ param-count _]
        (first (filter (fn [[_ name _ ns]]
                         (and (= name name-to-find)
                              (= ns ns-to-find)))
                       env))]
    param-count))

(defn add-fn-to-env
  ([env id name ns] (add-fn-to-env env id name [] ns))
  ([env id name params ns] (conj env [id name (count params) ns])))

(defn add-params-to-env [env params ns]
  (let [params (map (fn [{:keys [id name]}] [id name 0 ns]) params)]
    (concat env params)))

(defn make-fn [id name params body ns]
  {:id id :function name :params params :body body :ns ns})

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
