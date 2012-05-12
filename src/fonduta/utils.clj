(ns fonduta.utils)

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
