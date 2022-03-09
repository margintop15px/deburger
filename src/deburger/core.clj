(ns deburger.core
  (:import [clojure.lang MultiFn]))


(def calls
  (atom {}))

(defn tracer [function-symbol function]
  (fn [& args]
    (let [result (try
                   (apply function args)
                   (catch Throwable t
                     (println "Exception in traced function" function-symbol)
                     (println (.getMessage t))
                     (throw t)))
          trace  {:inputs (vec args)
                  :result result}]
      (swap! calls assoc function-symbol trace)
      result)))


(def traces
  (atom {}))

(defn trace-fn [function-symbol]
  (let [function-var      (resolve function-symbol)
        function-meta     (meta function-var)
        original-function @function-var
        traced            {:name     (keyword (str (:ns function-meta))
                                              (str (:name function-meta)))
                           :meta     function-meta
                           :original original-function}
        tracer-fn         (partial tracer function-symbol)]
    (swap! traces assoc function-symbol traced)
    (alter-var-root function-var tracer-fn)))


(defn untrace-fn [function-symbol]
  (let [{:keys [original]} (get @traces function-symbol)
        function-var (resolve function-symbol)]
    (swap! traces dissoc function-symbol)
    (swap! calls dissoc function-symbol)
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


(defn show-traced []
  (->> @traces
       vals
       (mapv :name)))


(defn show-result [function-symbol]
  (-> @calls
      (get-in [function-symbol :result])))


(defmacro define-inputs [function-symbol]
  (let [fs         (eval function-symbol)
        inputs     (get-in @calls [fs :inputs])
        args-names (first (get-in @traces [fs :meta :arglists]))]
    (mapv (fn [arg-name input]
            `(def ~arg-name ~input))
          args-names
          inputs)))


(defn define-locals [])
(defn define-all [])


(defn test-fn [a b]
  (+ a b))

(comment

 @traces
 @calls

 (trace-fn 'test-fn)
 (test-fn 1 3)
 (show-traced)
 (define-inputs 'test-fn)
 (show-result 'test-fn)

 nil)
