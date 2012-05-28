(ns fonduta.operations)


(defn- dispatch-fn [o & args]
  (let [t (:type o)]
    (if (nil? t) ; if there's no :type field
      (if (and (= (count o) 2)
               (every? number? o))
        :vector
        :outline)
      (:type o))))

(defmulti translate dispatch-fn)
(defmulti rotate dispatch-fn)
(defmulti scale dispatch-fn)
(defmulti skew-x dispatch-fn)
(defmulti reverse-all dispatch-fn)

(defmulti draw dispatch-fn)

