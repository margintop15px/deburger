(ns deburger.core
  (:import [clojure.lang MultiFn]))


(defonce calls
  (atom {}))

(defn tracer [function-symbol function]
  (fn [& args]
    (let [result       (try
                         (apply function args)
                         (catch Throwable t
                           (println "Exception in traced function" function-symbol)
                           (println (.getMessage t))
                           (throw t)))
          function-var (resolve function-symbol)
          trace-key    (symbol function-var)
          trace        {:inputs (vec args)
                        :result result}]
      (swap! calls assoc trace-key trace)
      result)))


(defonce traces
  (atom {}))

(defn trace-fn [function-symbol]
  (let [function-var      (resolve function-symbol)
        function-meta     (meta function-var)
        original-function @function-var
        trace-key         (symbol function-var)
        traced            {:name     (keyword (str (:ns function-meta))
                                              (str (:name function-meta)))
                           :meta     function-meta
                           :original original-function}
        tracer-fn         (partial tracer function-symbol)]
    (swap! traces assoc trace-key traced)
    (alter-var-root function-var tracer-fn)))


(defn untrace-fn [function-symbol]
  (let [function-var (resolve function-symbol)
        trace-key    (symbol function-var)
        {:keys [original]} (get @traces trace-key)]
    (swap! traces dissoc trace-key)
    (swap! calls dissoc trace-key)
    (alter-var-root function-var (constantly original))))


(defn function? [maybe-fn]
  (or (fn? maybe-fn)
      (= (type maybe-fn) MultiFn)))


(defn trace-ns [ns-symbol]
  (let [ns     (the-ns ns-symbol)
        ns-fns (->> ns ns-interns vals (filter (comp function? var-get)))]
    (doseq [function ns-fns]
      (trace-fn (symbol function)))))


(defn trace-current-ns []
  (trace-ns (symbol *ns*)))


(defn untrace-ns [ns-symbol]
  (let [ns     (the-ns ns-symbol)
        ns-fns (->> ns
                    ns-interns
                    vals
                    (filter (comp function? var-get)))]
    (doseq [function ns-fns]
      (untrace-fn (symbol function)))))


(defn untrace-current-ns []
  (untrace-ns (symbol *ns*)))


(defn untrace-all []
  (let [all-traced (keys @traces)]
    (doseq [trace-key all-traced]
      (untrace-fn trace-key))))


(defn show-traced []
  (->> @traces
       vals
       (mapv :name)))


(defn show-result [function-symbol]
  (let [function-var (resolve function-symbol)
        trace-key    (symbol function-var)]
    (get-in @calls [trace-key :result])))


(defn show-inputs [function-symbol]
  (let [function-var (resolve function-symbol)
        trace-key    (symbol function-var)]
    (get-in @calls [trace-key :inputs])))


(defmacro define-inputs [function-symbol]
  (let [fs           (eval function-symbol)
        function-var (resolve fs)
        trace-key    (symbol function-var)
        inputs       (get-in @calls [trace-key :inputs])
        args-names   (first (get-in @traces [trace-key :meta :arglists]))]
    (mapv (fn [arg-name input]
            `(def ~arg-name ~input))
          args-names
          inputs)))


(defn define-locals [])
(defn define-all [])
