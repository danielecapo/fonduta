(ns fonduta.tensionpaths
  (:require [fonduta.core :as core])
  (:use fonduta.vectors
        fonduta.operations))


;; 1. objects (points, control points, angle-control points, paths)
;; 2. predicates
;; 3. accessors
;; 4. 'change'
;; 5. transformations
;;  5.1 translations
;;  5.2 scaling
;;  5.3 rotations
;;  5.4 skew-x
;;  5.5 tense
;; 6. drawing to 'base' format
;; 7. primitives

;; 1. objects

;; objects are represented with maps
;; the type field is used for dispatching to mutimethods

(defn pt
  ([x y] {:type :point, :x x, :y y})
  ([[x y]] {:type :point, :x x, :y y}))

(defn cpt
  ([x y t] {:type :control, :x x, :y y :tension t})
  ([[x y] t] {:type :control, :x x, :y y :tension t}))

(defn acpt
  ([a1 a2 t] {:type :angle-control, :prevang a1, :nextang a2 :tension t})
  ([[a1 a2] t] {:type :angle-control, :prevang a1, :nextang a2 :tension t}))

(defn path [t & pts]
  {:type :path :closed t :points pts})

(defn closed-path [& pts]
  (apply path :closed pts))

(defn open-path [& pts]
  (apply path :open pts))

;; 2. predicates

(defn point? [p]
  (= (:type p) :point))

(defn control? [p]
  (= (:type p) :control))

(defn angle-control? [p]
  (= (:type p) :angle-control))

(defn path? [p]
  (= (:type p) :path))

(defn closed? [p]
  (= (:closed p) :closed))

;; 3. accessors

(defn coords [p]
  [(:x p) (:y p)])

(defn angles [c]
  [(:prevang c) (:nextang c)])

(defn path-type [p]
  (:closed p))

(defn path-count [p]
  (count (:points p)))

(defn path-elt [p i]
  ((:points p) i))

;; 4. 'change'

;; The functions in this section return new objects

(defn set-x [p v]
  (assoc p :x v))

(defn set-y [p v]
  (assoc p :y v))

(defn set-tension [p t]
  (assoc p :tension t))

(defn set-prevang [p v]
  (assoc p :prevang v))

(defn set-nextang [p v]
  (assoc p :nextang v))

(defn add-to-path [p & pts]
  (assoc p :points (concat (:points p) pts)))

(defn set-points [p & pts]
  (assoc p :points pts))

;; To cut a path we must be sure that the resulting (open) path
;; starts and ends with a point (not a control point)

(defn cut-path
  ([p start]
     (apply open-path
            (subvec (:points p) (if (not (point? (p start))) (+ start 1) start))))
  ([p start end]
     (apply open-path
            (subvec (:points p)
                    (if (not (point? (p start))) (+ start 1) start)
                    (if (not (point? (p (- end 1)))) (- end 1) end)))))

(defn join-paths [p & paths]
  (apply open-path
         (reduce concat
                 (map :points (conj paths p)))))

(defn set-in-path [p i new]
  (assoc-in p [:points i] new))

(defn mod-in-path [p i f & args]
  (set-in-path
   p i (apply f (path-elt p i) args)))


;; map-points map over the points list of paths

(defn map-points [f & paths]
  (apply map f (map :points paths)))

(defn close [p]
  (assoc p :closed :closed))

(defn open [p]
  (assoc p :closed :open))

;; (defn flatten-subpaths [p]
;;   (letfn [(f [p]
;;             (reduce
;;              (fn [pts x]
;;                (if (subpath? x)
;;                  (concat pts (f (points x)))
;;                  (concat pts [x])))
;;              [] p))]
;;     (apply path (path-type p) (f (points p)))))

;; (defn reverse-subpath [p]
;;   (cond (subpath? p) (apply subpath
;;                             (reverse (map reverse-subpath (points p))))
;;         (angle-control? p) (acpt (p 2) (p 1) (tension p))
;;         :else p))

;; In order to reverse a path we must reverse the order of angles
;; in every angle-control point. Then, if the first point in the resulting
;; points list isn't a point, we must place it a the end.

(defn reverse-angle-control [c]
  (acpt (:nextang c) (:prevang c) (:tension c)))

(defn reverse-path [p]
  (let [pts (reverse (map (fn [p]
                            (if (angle-control? p)
                              (reverse-angle-control p)
                              p))
                          (:points p)))]
    (apply set-points p
           (if (not (point? (first pts)))
             (concat (rest pts) [(first pts)])
             pts))))

(defmethod reverse-all :path [p]
  (reverse-path p))

;; 5. transformations

;; 5.1 translation

(defmethod translate :point [p v]
  (pt (vec+ (coords p) v)))

(defmethod translate :control [c v]
  (cpt (vec+ (coords c) v) (:tension c)))

(defmethod translate :angle-control [c v]
  c)

(defmethod translate :path [p v]
  (apply set-points p (map-points (fn [p] (translate p v)) p)))

;; 5.2 scaling

(defmethod scale :point
  ([p f] (pt (vec-scale (coords p) f)))
  ([p f fy] (pt (vec-scale (coords p) f fy))))

(defmethod scale :control
  ([c f] (cpt (vec-scale (coords c) f) (:tension c)))
  ([c f fy] (cpt (vec-scale (coords c) f fy) (:tension c))))

(defmethod scale :angle-control [c f & [fy]]
  (let [fxy (if (nil? fy) 1 (/ f fy))]
    (acpt (Math/atan (/ (Math/tan (:prevang c)) fxy))
          (Math/atan (/ (Math/tan (:nextang c)) fxy))
          (:tension c))))

(defmethod scale :path [p f & fy]
  (apply set-points p (map-points (fn [p] (apply scale p f fy)) p)))


;; 5.3 rotations

(defmethod rotate :point [p angle]
  (pt (vec-rotate (coords p) angle)))

(defmethod rotate :control [c angle]
  (cpt (vec-rotate (coords c) angle) (:tension c)))

(defmethod rotate :angle-control [c angle]
  (acpt (+ (:prevang c) angle)
        (+ (:nextang c) angle)
        (:tension c)))

(defmethod rotate :path [p angle]
  (apply set-points p (map-points (fn [p] (rotate p angle)) p)))

;; 5.4 skew-x

(defn- skew-x-angle [a angle]
  (+ (rad 90)
     (Math/atan (+ (Math/tan (- a (rad 90)))
                   (Math/tan angle)))))


(defmethod skew-x :point [p angle]
  (pt (vec-skew-x (coords p) angle)))

(defmethod skew-x :control [c angle]
  (cpt (vec-skew-x (coords c) angle) (:tension c)))

(defmethod skew-x :angle-control [c angle]
  (acpt (skew-x-angle (:prevang c) angle)
        (skew-x-angle (:nextang c) angle)
        (:tension c)))

(defmethod skew-x :path [p angle]
  (apply set-points p (map-points (fn [p] (skew-x p angle)) p)))

;; 5.5 tense

(defmulti tense :type)

(defmethod tense :point [p f]
  p)

(defmethod tense :control [c f]
  (set-tension c (* (:tension c) f)))

(defmethod tense :angle-control [c f]
  (set-tension c (* (:tension c) f)))

(defmethod tense :path [p f]
  (apply set-points p (map-points (fn [p] (tense p f)) p)))

(defmethod tense :group [g f]
  (apply core/group (core/map-paths (fn [p] (tense p f)) g)))

;; 6. drawing to 'base' format

;; To 'draw' a path we must convert 'tension' control points
;; to bezier control points. This is done interpolating the position
;; of the tension control point with the position of a near point.
;; We must also find the position of control points when we used
;; angle-control points, using angles to find the intersection.

(defn- approx-equal [a b]
  (< (Math/abs (- a b)) 0.0001))
  
(defn- normalize-angle [a]
  (let [d (/ a PI)]
    (* (- d (Math/floor d)) PI)))

(defn- intersect [[x1 y1] [x2 y2] [a1 a2]]
  (let [a1 (normalize-angle a1)
        a2 (normalize-angle a2)
        t1 (Math/tan a1)
        t2 (Math/tan a2)
        horizontal (fn [x ya yb t] (+ x (/ (- ya yb) t)))
        vertical (fn [y xa xb t] (+ (* (- xa xb) t) y))]
    (cond (approx-equal a1 0) [(horizontal x2 y1 y2 t2) y1]
          (approx-equal a2 0) [(horizontal x1 y2 y1 t1) y2]
          (approx-equal a1 (rad 90)) [x1 (vertical y2 x1 x2 t2)]
          (approx-equal a2 (rad 90)) [x2 (vertical y1 x2 x1 t1)]
          :else (let [xc (/ (+ (- (* x1 t1) (* x2 t2)) (- y2 y1))
                             (- t1 t2))]
                  [xc (+ (* t1 (- xc x1)) y1)]))))

(defn- offcurve [p1 ct p2]
  (let [p1 (coords p1)
        p2 (coords p2)
        ctc (if (control? ct)
              (coords ct)
              (intersect p1 p2 (angles ct)))]
    [(vec+ p1 (vec-scale (vec- ctc p1) (:tension ct)))
     (vec+ p2 (vec-scale (vec- ctc p2) (:tension ct)))]))


(defn- base-outline [p]
  (let [pts (:points p)]
    (reduce (fn [c [prev cur next]]
              (if (point? cur)
                (if (point? prev)
                  (into c [(coords prev) (coords cur) (coords cur)])
                  (conj c (coords cur)))
                (into c (offcurve prev cur next))))
            [(coords (first pts))]
            (map vector pts
                 (rest (if (closed? p) (cycle pts) pts))
                 (rest (rest (cycle pts)))))))


(defmethod draw :path [p]
  (base-outline p))


;; 7. primitives

(defn super-ellipse [bottom-left top-right]
  (let [[xa ya] bottom-left
        [xb yb] top-right
        xm (/ (+ xb xa) 2)
        ym (/ (+ yb ya) 2)]
    (path :closed
          (pt xm ya) (cpt xb ya 1)
          (pt xb ym) (cpt xb yb 1)
          (pt xm yb) (cpt xa yb 1)
          (pt xa ym) (cpt xa ya 1))))

(defn ellipse [bottom-left top-right]
  (tense (super-ellipse bottom-left top-right) 0.55))

(defn rect [bottom-left top-right]
  (let [[xa ya] bottom-left
        [xb yb] top-right]
    (path :closed
          (pt xa ya) (pt xb ya)
          (pt xb yb) (pt xa yb))))

(defn circle [center radius]
  (let [vr [radius radius]]
    (ellipse (vec- center vr) (vec+ center vr))))

(defn square [center side]
  (let [vr [(/ side 2) (/ side 2)]]
    (rect (vec- center vr) (vec+ center vr))))