(ns fonduta.vectors
  (:use fonduta.operations))

(def PI (Math/PI))

(defn rad [deg]
  (Math/toRadians deg))

(defn vec+ [[xa ya] [xb yb]]
  [(+ xa xb) (+ ya yb)])

(defn vec-neg [[xa ya]]
  [(- xa) (- ya)])

(defn vec- [[xa ya] [xb yb]]
  [(- xa xb) (- ya yb)])

(defn vec-scale
  ([[xa ya] f]
     [(* xa f) (* ya f)])
  ([[xa ya] fx fy]
     [(* xa fx) (* ya fy)]))

(defn vec-rotate [[xa ya] angle]
  [(- (* xa (Math/cos angle)) (* ya (Math/sin angle)))
   (+ (* xa (Math/sin angle)) (* ya (Math/cos angle)))])

(defn vec-x [[x y]] x)
(defn vec-y [[x y]] y)

(defn vec-skew-x [[x y] angle]
  (vec+ [x y] [(* -1 (Math/tan angle) y) 0]))


(defmethod translate :vector [v v1]
  (vec+ v v1))

(defmethod scale :vector
  ([v f] (vec-scale v f))
  ([v f fy] (vec-scale v f fy)))

(defmethod rotate :vector [v angle]
  (vec-rotate v angle))

(defmethod skew-x :vector [v angle]
  (vec-skew-x v angle))