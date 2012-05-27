(ns fonduta.tension-paths
  (:use fonduta.utils
        fonduta.operations))

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

;; predicates

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

;; accessors

(defn coords [p]
  [(:x p) (:y p)])

(defn path-type [p]
  (:closed p))

(defn path-count [p]
  (count (:points p)))

(defn path-elt [p i]
  ((:points p) i))

;; 'change'

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

(defn cut-path
  ([p start]
     (apply open-path
            (subvec (:points p) (if (not (point? (p start))) (+ start 1) start))))
  ([p start end]
     (apply open-path
            (subvec (:points p)
                    (if (not (point? (p start))) (+ start 1) start)
                    (if (not (point? (p (- end 1)))) (- end 1) end)))))

(defn set-in-path [p i new]
  (assoc-in p [:points i] new))

(defn mod-in-path [p i f & args]
  (set-in-path
   p i (apply f (path-elt p i) args)))

(defn map-points [f & paths]
  (assoc (first paths) :points
         (apply map f (map :points paths))))

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

(defn reverse-path [p]
  (let [pts (reverse (:points p))]
    (assoc p :points (if (not (point? (first pts)))
                       (concat (rest pts) [(first pts)])
                       pts))))


(defmethod translate :point [p v]
  (pt (vec+ (coords p) v)))

(defmethod translate :control [c v]
  (cpt (vec+ (coords c) v) (:tension c)))

(defmethod translate :angle-control [c v]
  c)

(defmethod translate :path [p v]
  (map-points (fn [p] (translate p v)) p))


(defmethod scale :point
  ([p f] (pt (vec-scale (coords p) f)))
  ([p f fy] (pt (vec-scale (coords p) f fy))))

(defmethod scale :control
  ([c f] (cpt (vec-scale (coords c) f) (:tension c)))
  ([c f fy] (cpt (vec-scale (coords c) f fy) (:tension c))))

(defmethod scale :angle-control [c f & [fy]]
  (let [fxy (if (nil? fy) 1 (/ f fy))]
    (acpt (Math/atan (/ (Math/tan (c 1)) fxy))
          (Math/atan (/ (Math/tan (c 2)) fxy))
          (:tension c))))

(defmethod scale :path [p f & fy]
  (map-points (fn [p] (apply scale p f fy)) p))


(defmethod rotate :point [p angle]
  (pt (vec-rotate (coords p) angle)))

(defmethod rotate :control [c angle]
  (cpt (vec-rotate (coords c) angle) (:tension c)))

(defmethod rotate :angle-control [c angle]
  (acpt (+ (:prevang c) angle)
        (+ (:nextang c) angle)
        (:tension c)))

(defmethod rotate :path [p angle]
  (map-points (fn [p] (rotate p angle)) p))

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
  (map-points (fn [p] (skew-x p angle)) p))

(defmulti tense :type)

(defmethod tense :point [p f]
  p)

(defmethod tense :control [c f]
  (set-tension c (* (:tension c) f)))

(defmethod tense :angle-control [c f]
  (set-tension c (* (:tension c) f)))

(defmethod tense :path [p f]
  (map-points (fn [p] (tense p f)) p))



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
              (intersect p1 p2 (coords ct)))]
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

;; (defn base-outlines [v t]
;;   (if (nil? t) v
;;       (if (group? t)
;;         (concat v (reduce base-outlines [] (content t)))
;;         (conj v (base-outline t)))))


(defmethod draw :path [p]
  (base-outline p))