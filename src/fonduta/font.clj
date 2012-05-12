(ns fonduta.font
  (:require [fonduta.basefont :as base])
  (:use fonduta.utils))

;;;; basic definitions of points, paths, subpaths and groups

(defn pt
  ([x y] [:point x y])
  ([[x y]] [:point x y]))

(defn cpt
  ([x y t] [:control x y t])
  ([[x y] t] [:control x y t]))

(defn acpt
  ([a1 a2 t] [:angle-control a1 a2 t])
  ([[a1 a2] t] [:angle-control a1 a2 t]))

(defn path [t & pts]
  `[:path ~t ~@pts])

(defn subpath [& pts]
  `[:subpath ~@pts])

(defn closed-path [& pts]
  (apply path :closed pts))

(defn open-path [& pts]
  (apply path :open pts))

(defn group [& paths]
  (vec (cons :group paths)))

;; predicates

(defn control? [p]
  (= (first p) :control))

(defn angle-control? [p]
  (= (first p) :angle-control))

(defn point? [p]
  (= (first p) :point))

(defn path? [p]
  (= (first p) :path))

(defn subpath? [p]
  (= (first p) :subpath))

(defn group? [p]
  (= (first p) :group))

;; accessors

(defn path-type [p]
  (p 1))

(defn closed? [p]
  (and (path? p) (= (path-type p) :closed)))

(defn x [p]
  (p 1))

(defn y [p]
  (p 2))

(defn tension [p]
  (p 3))

(defn prevang [p]
  (p 1))

(defn nextang [p]
  (p 2))

(defn coords [point]
  [(x point) (y point)])

(defn points [p]
  (subvec p (if (subpath? p) 1 2)))

(defn path-count [p]
  (count (points p)))

(defn content [g]
  (rest g))

(defn group-count [g]
  (count (content g)))

(defn path-elt [p i]
  ((points p) i))

(defn group-elt [g i]
  (get g (+ i 1)))

;; 'change'

(defn set-x [p v]
  (assoc p 1 v))

(defn set-y [p v]
  (assoc p 2 v))

(defn set-tension [p t]
  (assoc p 3 t))

(defn set-prevang [p v]
  (assoc p 1 v))

(defn set-nextang [p v]
  (assoc p 2 v))

(defn add-to [p new]
  (conj p new))

(defn cut-path
  ([p start]
     (apply subpath (subvec (points p) start)))
  ([p start end]
     (apply subpath (subvec (points p) start end))))

(defn set-in-path [p i new]
  (assoc p (+ i (if (subpath? p) 1 2)) new))

(defn mod-in-path [p i f & args]
  (set-in-path
   p i (apply f (path-elt p i) args)))

(defn set-in-group [g i n]
  (assoc g (+ i 1) n))

(defn mod-in-group [g i f & args]
  (set-in-group
   g i (apply f (group-elt g i) args)))

(defn map-points [f & paths]
  (apply map f (map points paths)))

(defn map-content [f & groups]
  (apply map f (map content groups)))

(defn close [p]
  (assoc p 1 :closed))

(defn open [p]
  (assoc p 1 :open))

(defn flatten-subpaths [p]
  (letfn [(f [p]
            (reduce
             (fn [pts x]
               (if (subpath? x)
                 (concat pts (f (points x)))
                 (concat pts [x])))
             [] p))]
  (apply path (path-type p) (f (points p)))))

;;;; detecting orientation of paths
;;;; to be rewritten for subpaths? (or I can flatten subpaths)


(defn centroid [p]
  (vec-scale
   (reduce vec+
           (map-points coords p))
   (/ 1 (count (points p)))))

(defn relative-vec [p v]
  (map-points (fn [x] (vec+ (coords x) (vec-neg v))) p))

(defn winding-number [p r]
  (let [np (relative-vec p r)]
    (reduce (fn [w v]
              (let [[x1 y1] (first v)
                    [x2 y2] (second v)]
                (cond (< (* y1 y2) 0)
                      (if (> (+ x1 (/ (* y1 (- x2 x1))
                                      (- y1 y2)))
                             0)
                        (if (< y1 0) (+ w 1) (- w 1))
                        w)
                      (and (= y1 0) (> x1 0))
                      (if (> y2 0) (+ w 1/2) (- w 1/2))
                      (and (= y2 0) (> x2 0))
                      (if (< y1 0) (+ w 1/2) (- w 1/2))
                      :else w)))
            0 (map list np (take (count np) (rest (cycle np)))))))

(defn remove-angle-control [p]
  (apply path (path-type p) (filter (complement angle-control?) (points p))))

(defn clockwise? [p]
  (let [p (map-points coords (remove-angle-control (flatten-subpaths p)))]
    (> (reduce (fn [a e]
                 (let [[x1 y1] (first e)
                       [x2 y2] (second e)]
                   (+ a (* (- x2 x1) (+ y1 y2)))))
               0 (map list p (take (count p) (rest (cycle p)))))
       0)))

(defn reverse-subpath [p]
  (cond (subpath? p) (apply subpath
                            (reverse (map reverse-subpath (points p))))
        (angle-control? p) (acpt (p 2) (p 1) (tension p))
        :else p))

(defn reverse-path [p]
  (apply path (path-type p) (reverse (map reverse-subpath (points p)))))

;; impose clockwise or counterclockwise orientation to paths

(defn cw [p]
  (if (clockwise? p) p (reverse-path p)))

(defn ccw [p]
  (if (clockwise? p) (reverse-path p) p))

;;(defn join-paths [p1 p2]
;;  (apply open-path
;;   (concat (points p1) (points p2))))

(defn join-paths [pth & pths]
  "Return an open counterclockwise path formed
   by subpaths whose points are the points
   of the original paths"
  (apply open-path
         (map (fn [p] (ccw (apply subpath (points p))))
              (concat pth pths))))

;;;; geometric transformations of objects


(defn- dispatch_fn [v & params]
  (first v))

(defmacro deftransform [name [& params] & bodies]
  `(do (defmulti ~name dispatch_fn)
       ~@(map (fn [body] `(defmethod ~name ~(first body) ~(vec params) ~@(rest body))) bodies)))


(deftransform translate [p v]
  (:point (apply pt (vec+ (coords p) v)))
  (:control (apply cpt (conj (vec+ (coords p) v) (tension p))))
  (:angle-control p)
  (:subpath (apply subpath (map-points (fn [o] (translate o v)) p)))
  (:path (apply path (path-type p) (map-points (fn [o] (translate o v)) p)))
  (:group (apply group (map-content (fn [o] (translate o v)) p))))


(defn from [v f p & params]
  "Example: (from [10 10] rotate my-path (rad 90))
   from uses v as the reference point for the transformation"
  (translate (apply f (translate p (vec-neg v)) params) v)) 

(deftransform scale [p f & fy]
  (:point (apply pt (apply vec-scale (coords p) f fy)))
  (:control (apply cpt (conj (apply vec-scale (coords p) f fy) (tension p))))
  (:angle-control (let [fxy (if (nil? (first fy))
                              1 (/ f (first fy)))]
                    (acpt (Math/atan (/ (Math/tan (p 1)) fxy))
                          (Math/atan (/ (Math/tan (p 2)) fxy))
                          (tension p))))
  (:subpath (apply subpath (map-points (fn [o] (apply scale o f fy)) p)))
  (:path (apply path (path-type p)
                (map-points (fn [o] (apply scale o f fy)) p)))
  (:group (apply group (map-content (fn [o] (apply scale o f fy)) p))))

(deftransform rotate [p angle]
  (:point (apply pt (vec-rotate (coords p) angle)))
  (:control (apply cpt (conj (vec-rotate (coords p) angle) (tension p))))
  (:angle-control (acpt (+ (p 1) angle) (+ (p 2) angle) (tension p)))
  (:subpath (apply subpath (map-points (fn [o] (rotate o angle)) p)))
  (:path (apply path (path-type p) (map-points (fn [o] (rotate o angle)) p)))
  (:group (apply group (map-content (fn [o] (rotate o angle)) p))))

(deftransform skew-x [p angle]
  (:point (translate p [(* -1 (Math/tan angle) (y p)) 0]))
  (:control (translate p [(* -1 (Math/tan angle) (y p)) 0]))
 ;; !!! (:angle-control (acpt (+ (p 1) angle) (+ (p 2) angle) (tension p)))
  (:subpath (apply subpath (map-points (fn [o] (skew-x o angle)) p)))
  (:path (apply path (path-type p) (map-points (fn [o] (skew-x o angle)) p)))
  (:group (apply group (map-content (fn [o] (skew-x o angle)) p))))

;; tense multiplies the tension parameter for a factor f

(deftransform tense [p f]
  (:point p)
  (:control (set-tension p (* f (tension p))))
  (:angle-control (set-tension p (* f (tension p))))
  (:subpath (apply subpath (map-points (fn [o] (tense o f)) p)))
  (:path (apply path (path-type p) (map-points (fn [o] (tense o f)) p)))
  (:group (apply group (map-content (fn [o] (tense o f)) p))))

(defn release [p f]
  (tense p (/ 1 f)))

(defn multiple-copy [o n f & args]
  "o is the object, n is the number of copies,
   f is a function (for example, a translation)
   used for copying and args are the other arguments for f"
  (letfn [(iter [s n]
            (if (> n 0)
              (iter (cons (apply f (first s) args) s) (- n 1))
              s))]
  (apply group (reverse (iter (list o) n)))))




;;;; transform the current font format in the base font format

(defn intersect [[x1 y1] [x2 y2] [a1 a2]]
  (let [t1 (Math/tan a1)
        t2 (Math/tan a2)]
    (cond (= t1 0.0) [(+ x2 (/ (- y1 y2) t2)) y1]
          (= t2 0.0) [(+ x1 (/ (- y2 y1) t1)) y2]
          :else (let [xc (/ (+ (- (* x1 t1) (* x2 t2)) (- y2 y1))
                             (- t1 t2))]
                   [xc (+ (* t1 (- xc x1)) y1)]))))

(defn offcurve [p1 ct p2]
  (let [p1 (coords p1)
        p2 (coords p2)
        ctc (if (control? ct)
              (coords ct)
              (intersect p1 p2 (coords ct)))]  
    [(vec+ p1 (vec-scale (vec- ctc p1) (tension ct)))
     (vec+ p2 (vec-scale (vec- ctc p2) (tension ct)))]))


(defn base-outline [p]
  (let [p (flatten-subpaths p)
        f (first (points p))
        pts (if (control? f) (conj (subvec (points p) 1) f) (points p))] 
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

(defn base-outlines [v t]
  (if (nil? t) v
      (if (group? t)
        (concat v (reduce base-outlines [] (content t)))
        (conj v (base-outline t)))))

(defmacro glyph [name [& locals] advance & contents]
  `(let [~@locals]
     (apply base/glyph
            ~name
            ~advance
            (reduce base-outlines [] [~@contents]))))

;;;; alignments

(defn build-alignments [alignments]
  (map (fn [x] (let [n (first x)]
                 [(keyword (name n)) n]))
       (partition 2 alignments)))

(defn alignment [a]
  (get a 0))

(defn overshoot [a]
  (get a 1))

(defn overshooted [a]
  (+ (alignment a) (overshoot a)))

(defn on-alignment [a x]
  [x (alignment a)])

(defn on-overshoot [a x]
  [x (+ (alignment a) (overshoot a))])

;;;; font macro

(defn make-opt [opt]
  (if (= :grid (opt 0))
    `(base/font* ~@(rest opt))
    '(identity)))

(defmacro font [name [& opts] alignments variables [& glyphs]]
  `(->
    (let [~@alignments ~@variables]
      (base/font ~name
                 [~@(map (fn [x] (assoc x 1 `(alignment ~(get x 1))))
                         (build-alignments alignments))]
                 ~@glyphs))
    ~@(map make-opt opts)))

;       {:name ~name,
;        :aligments ~(build-alignments alignments),
;        :glyphs [~@glyphs]}))

;(defmacro overshooted [vertical]
;  `(+ ~vertical ~(overshoot-symbol vertical)))

;;;; primitives

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

;;;; the idea of counterpunches should be expanded
;;;; for example, instead of just mapping every point with a transformation
;;;; we should be able to refer to previous points etc

;; (defmacro rule [f & args]
;;   (let [x (gensym)]
;;     `(fn [~x] (~f ~x ~@args))))

;;(defn ctpunch [rules counterpunch]
;;  (let [new-points (map (fn [r p] (r p)) rules (points counterpunch))]
;;    new-points))

(defn rule [f & args]
  (fn [x] (apply f x args)))

(defn map-rules [rules points]
  (map (fn [r p]
         (if (vector? r)
           (apply subpath (map (fn [r] (r p)) r))
           (r p)))
       rules points))


(defn use-ctpunch [rules counterpunch]
  (let [new-points (map-rules rules (points counterpunch))]
    (if (closed? counterpunch)
      (group (ccw (apply closed-path new-points)) (cw counterpunch))
      (ccw (closed-path (apply subpath (points counterpunch))
                        (reverse-subpath (apply subpath new-points)))))))

(defmacro ctpunch [rules counterpunch]
  (let [new-rules (gensym)
        new-points (gensym)
        r (gensym)
        p (gensym)]
    `(let [~new-rules  [~@(map (fn [r] (cons 'rule r)) rules)]
           ~new-points (map (fn [~r ~p] (~r ~p)) ~new-rules (points ~counterpunch))]
       (if (closed? ~counterpunch) 
         (group (cw ~counterpunch) (ccw (apply closed-path ~new-points)))
         (ccw
          (closed-path (apply subpath (points ~counterpunch))
                       (reverse-subpath (apply subpath ~new-points))))))))
  
;;(defmacro ctpunch [rules counterpunch]
;;  (let [new (gensym)]
;;    `(let [~new (list ~@(map (fn [r] (make-rule r)) rules))]
;;       ~new)))

;; (font
;;  :abc
;;  [descender  [-240 -10]
;;   baseline   [0 -10]
;;   xheight    [500 10]
;;   small-caps [550 10]
;;   caps       [700 12]
;;   ascender   [740 10]]
;;   [:o [600 0]
;;    (tense (ccw (circle 100 (overshoot :baseline) 500 (overshoot :xheight))) 1.2)
;;    (tense (cw (circle 150 20 450 480)))])
   
   
;; [:erase [[10 20] [10 100 .3] [100 100]]]

(defn parameters-list [params]
  {:keys (vec (map first (partition 2 params)))
    :or (apply hash-map params)})

(defmacro deffoundry [fontname [& params] & body-forms]
  `(defn ~fontname [& ~(parameters-list params)]
     (font ~(keyword (name fontname)) ~@body-forms)))


;; (deffoundry abc
;;   [stem 19
;;    contrast 1
;;    width 1
;;    space 1]
;;   [h-stems (* stem contrast)
;;    v-curve (* stem 1.2)
;;    h-curves (* v-curves contrast)]
;;   [:metrics
;;    [descender -249 -10]]
;;   [:glyphs
;;    [:o
              

;; (defn ->base [f]
;;   (apply base/font (get f :name)
;;              (into [] (get f :vertical-metrics))
;;              (map base-glyph (get f :glyphs))))