(ns re-frame-macros.core)

(defmacro ...
  "A macro that turns a list of variables into a map"
  [& vars]
  (let [vars (if (-> vars first vector?) (first vars) vars)]
    (let [m (apply merge
                   (for [v vars]
                     {(keyword (str v)) v}))]
      m)))

(defmacro ->time
  "Own time implementation - output information about the function and the time of its execution"
  [f]
  `(let [start# (-> (js/Date.) .getTime)
         ret#   (~f)]
     (prn (-> '~f str remove-fn-text)
          (str "Elapsed time: "
               (/ (double (- (-> (js/Date.) .getTime) start#)) 1000000.0) " msecs"))
     ret#))

(defmacro ->time->
  "->time with arguments (for -> macro)"
  [arg f]
  `(let [start# (-> (js/Date.) .getTime)
         ret#   (~f ~arg)]
     (prn '~f (str "Elapsed time: "
                   (/ (double (- (-> (js/Date.) .getTime) start#)) 1000000.0) " msecs"))
     ret#))

(defmacro ->time->>
  "->time with arguments (for ->> macro)"
  [f arg]
  `(let [start# (-> (js/Date.) .getTime)
         ret# (~f ~arg)]
     (prn '~f (str "Elapsed time: " (/ (double (- (-> (js/Date.) .getTime) start#)) 1000000.0) " msecs"))
     ret#))

(defmacro reg-sub-event [name default-value]
  `(do
     (re-frame.core/reg-event-db
      ~name
      (fn [db# [_ value#]]
        (assoc db# ~name value#)))
     (re-frame.core/reg-sub
      ~name
      (fn [db# [_]]
        (get db# ~name ~default-value)))))

(defmacro reg-sub [name default-value]
  `(re-frame.core/reg-sub
    ~name
    (fn [db# [_]]
      (get db# ~name ~default-value))))

(defmacro reg-sub-in [name path default-value]
  `(re-frame.core/reg-sub
    ~name
    (fn [db# [_]]
      (get-in db# ~path ~default-value))))

(defmacro reg-sub-in-map [name path map-fn]
  `(re-frame.core/reg-sub
    ~name
    (fn [db# [_]]
      (map
       ~map-fn
       (get-in db# ~path)))))

(defmacro reg-sub-in-find [name path find-fn]
  `(re-frame.core/reg-sub
    ~name
    (fn [db# [_ param#]]
      (->> (get-in db# ~path)
           (filter (partial ~find-fn param#))
           first))))

(defmacro reg-sub= [name test-value]
  `(re-frame.core/reg-sub
    ~name
    (fn [db# [_]]
      (= (get db# ~name)
         ~test-value))))

(defmacro reg-sub-in= [name path test-value]
  `(re-frame.core/reg-sub
    ~name
    (fn [db# [_]]
      (= (get-in db# ~path)
         ~test-value))))

(defmacro reg-event [name]
  `(re-frame.core/reg-event-db
    ~name
    (fn [db# [_ value#]]
      (assoc db# ~name value#))))

(defmacro reg-event-update [name value func]
  `(re-frame.core/reg-event-db
    ~name
    (fn [db# [_]]
      (if (contains? db# ~name)
        (update db# ~name ~func)
        (assoc db# ~name ~value)))))

(defmacro reg-event-in [name path]
  `(re-frame.core/reg-event-db
    ~name
    (fn [db# [_ value#]]
      (assoc-in db# ~path value#))))

(defmacro reg-event-in-reset [name path default-value]
  `(re-frame.core/reg-event-db
    ~name
    (fn [db# [_]]
      (assoc-in db# ~path ~default-value))))

(defmacro reg-event-debug [name message]
  `(re-frame.core/reg-event-fx
    ~name
    (fn [db# [_ result#]]
      (taoensso.timbre/debug ~name ~message result#)
      nil)))

(defmacro reg-event-info [name message]
  `(re-frame.core/reg-event-fx
    ~name
    (fn [db# [_ result#]]
      (taoensso.timbre/info ~name ~message result#)
      nil)))

(defmacro let-sub [bindings & body]
  (cond
    (empty? bindings)                  `(do ~@body)
    (and (symbol? (first bindings))
         (keyword? (second bindings))) `(let [~(first bindings) (re-frame.core/subscribe [~(second bindings)])]
                                          (let-sub ~(vec (drop 2 bindings)) ~@body))
    (symbol? (first bindings))         `(let [~(first bindings) ~(second bindings)]
                                          (let-sub ~(vec (drop 2 bindings)) ~@body))
    :else                              `(let [~(symbol (name (first bindings))) (re-frame.core/subscribe [~(first bindings)])]
                                          (let-sub ~(vec (rest bindings)) ~@body))))

