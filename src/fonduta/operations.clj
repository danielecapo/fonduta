(ns fonduta.operations
  (:use fonduta.utils))

(defmulti translate :type)
(defmulti rotate :type)
(defmulti scale :type)
(defmulti skew-x :type)

(defn from [c f p & args]
  (translate
   (apply f (translate p (vec-neg c)) args)
   c))

(defmulti draw :type)