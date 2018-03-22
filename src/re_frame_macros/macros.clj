(ns re-frame-macros.macros)

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

(defmacro reg-event-fx-debug [name]
  `(re-frame.core/reg-event-fx
    ~name
    (fn [db# [_ result#]]
      (taoensso.timbre/debug ~name result#))))

(defmacro reg-event-fx-info [name]
  `(re-frame.core/reg-event-fx
    ~name
    (fn [db# [_ result#]]
      (taoensso.timbre/info ~name result#))))

