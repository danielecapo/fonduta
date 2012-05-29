(ns fonduta.core
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [fonduta.sfd :as sfd])
  (:use fonduta.glyphlist))

(def PI (Math/PI))

(defn rad [deg]
  (Math/toRadians deg))


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


(defn vec-length [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn vec-angle [[x y]]
  (if (= x 0)
    (if (> y 0) (rad 90) (rad 270))
    (Math/atan (/ y x))))

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

(defn between [v1 v2 f]
  (vec+ (vec-scale (vec- v2 v1) f)
        v1))

(defn middle [v1 v2]
  (vec-scale (vec+ v1 v2) 0.5))


(defmethod translate :vector [v v1]
  (vec+ v v1))

(defmethod scale :vector
  ([v f] (vec-scale v f))
  ([v f fy] (vec-scale v f fy)))

(defmethod rotate :vector [v angle]
  (vec-rotate v angle))

(defmethod skew-x :vector [v angle]
  (vec-skew-x v angle))

(defmethod reverse-all :vector [v] v)


;; 1. transformations
;; 2. draw
;; 3. font
;;  3.1 glyph
;;  3.2 font
;; 4. write to sfd format
;;  4.1 save sfd
;; 5. font math
;; 6. font skew


;; 1. transformations

(defn transform [f p args]
  (if (number? (first p)) ;; is a geometrical vector
    (apply f p args)
    (into [] (map (fn [p] (transform f p args))
                  p))))

(defmethod  translate :outline [p v]
  (transform vec+ p [v]))

(defmethod  rotate :outline [p angle]
  (transform vec-rotate p [angle]))

(defmethod scale :outline
  ([p f]
     (transform vec-scale p [f]))
  ([p f fy]
     (transform vec-scale p [f fy])))

(defmethod skew-x :outline [p angle]
  (transform vec-skew-x p [angle]))

(defmethod reverse-all :outline [p]
  (reverse p))

;; 2. draw

(defmethod draw :outline [p] p)

;; (defn vertical-metrics [x-height capitals ascender descender & [others]]
;;   (merge {:x-height x-height,
;;           :capitals capitals,
;;           :ascender ascender,
;;           :descender descender}
;;          others))

;; 3. font

;; 3.1 glyph

(defn make-glyph [name advance & outlines]
  {:name name,
   :advance advance,
   :outlines outlines})

;; 3.2 font

(defn make-font [name alignments & glyphs]
  {:name name,
   :alignments (into {} alignments),
   :glyphs (vec glyphs)})  

;; 3.3 accessors

(defn font-name [f]
  (:name f))

(defn ascender [f]
  (:ascender (:alignments f)))

(defn descender [f]
  (:descender (:alignments f)))

(defn glyphs [f]
  (:glyphs f))

(defn get-glyph [f g]
  (first
   (filter (fn [gl] (= (:name gl) g))
          (glyphs f))))

(defn sorted-glyphs [f]
  (letfn [(g< [g1 g2]
            (< (compare (:name g1) (:name g2)) 0))]
    (sort g< (glyphs f))))


;; 4. write to sfd format


(defn- sfd-encoding [g]
  (let [e ((:name g) adobe-glyph-list)]
    [e e 0]))

(defn- sfd-cmd [pts]
  (let [[c1 c2 p] (vec pts)]
    [:c c1 c2 p]))

(defn- sfd-outline [o]
  `[[:m ~@(first o)]
    ~@(map sfd-cmd (partition 3 (rest o)))])
  
(defn- sfd-glyph [g]
  `[~(get g :name)
    ~(sfd-encoding g)
    ~(get g :advance)
    ~@(map sfd-outline (get g :outlines))])
  
(defn- sfd-glyphs [g]
  (map sfd-glyph g))


;; The following function returns a minimal sfd (as a string)
;; with a basica 'header'

(defn build-sfd [f]
  (let [fontname (name (font-name f))]
    `[[:header
       [:SplineFontDB 3.0]
       [:FontName ~fontname]
       [:FullName ~fontname]
       [:FamilyName ~fontname]
       [:Ascent ~(ascender f)]
       [:Descent ~(- (descender f))]
       [:Encoding "unicode"]
       [:Namelist "Adobe Glyph List"]]
      ~@(sfd-glyphs (glyphs f))]))

;; 4.1 save a sfd file
            
(defn ->sfd [filename f]
  (spit filename (sfd/font (build-sfd f))))


;; 5. font math

;; vector operations on fonts
;; implementing font 'math'
;; see robofab.org for the idea of glyph math
;; http://robofab.org/howto/glyphmath.html

(defn glyph-op [f g1 g2]
  (apply make-glyph (:name g1)
         (f (:advance g1) (:advance g2))
         (map (fn [o1 o2] (map f o1 o2)) (:outlines g1) (:outlines g2))))

(defn glyph+ [g1 & gs]
  (reduce (fn [a b] (glyph-op vec+ a b)) g1 gs))


(defn glyph- [g1 & gs]
  (reduce (fn [a b] (glyph-op vec- a b)) g1 gs))

(defn glyph* [g fx & [fy]]
  (let [fy (if (nil? fy) fx fy)]
    (apply make-glyph (:name g)
           (vec-scale (:advance g) fx fy)
           (map (fn [o1]
                  (map (fn [p] (vec-scale p fx fy)) o1))
                (:outlines g)))))

(defn glyph-neg [g]
  (glyph* g -1))

(defn alignments-op [f a1 a2]
  (let [ks (set/intersection (set (keys a1)) (set (keys a2)))]
    (into {} (map (fn [k] [k (f (get a1 k) (get a2 k))]) ks))))

(defn font-op [f fg f1 f2]
  (apply make-font
         (:name f1)
         (alignments-op f (:alignments f1) (:alignments f2))
         (map fg (sorted-glyphs f1) (sorted-glyphs f2))))

(defn font+ [f1 & fs]
  (reduce (fn [a b] (font-op + glyph+ a b)) f1 fs))

(defn font- [f1 & fs]
  (reduce (fn [a b] (font-op - glyph- a b)) f1 fs))

(defn font* [f fx & [fy]]
  (let [fy (if (nil? fy) fx fy)]
    (apply make-font
           (:name f)
           (map (fn [[k v]] [k (* v fy)]) (:alignments f))
           (map (fn [g] (glyph* g fx fy)) (glyphs f)))))

(defn font-neg [f]
  (font* f -1))

(defn interpolation [f1 f2 x & [y]]
  (let [y (if (nil? y) x y)]
    (font+ f1 (font* (font- f2 f1) x y))))

;; 6. font skew

;; skew functions
;; useful for obliques

(defn glyph-skew-x [g angle]
  (apply make-glyph
         (:name g)
         (:advance g)
         (map (fn [o1]
                (map (fn [p] (vec-skew-x p angle)) o1))
              (:outlines g))))

(defn font-skew-x [f angle]
  (apply make-font
         (:name f)
         (:alignments f)
         (map (fn [g] (glyph-skew-x g angle)) (glyphs f))))

;; 1. groups
;;  1.1 definition
;;  1.2 predicate
;;  1.3 accessors
;;  1.4 change
;;  1.5 transformations
;;  1.6 draw to 'base' format
;; 2. from
;; 3. orientation
;; 4. font
;; 4.1 glyph
;; 4.2 alignments
;; 4.3 font macro
;; 5. foundry
;; 6. counterpunches



;; 1. groups

;; 1.1 definition

(defn group [& paths]
  {:type :group :paths paths})

;; 1.2 predicate

(defn group? [p]
  (= (:type p) :group))

;; 1.3 accessors

(defn group-count [g]
  (count (:paths g)))

(defn group-elt [g i]
  ((:paths g) i))

;; 1.4 change

(defn set-in-group [g i n]
  (assoc-in g [:paths i] n))

(defn mod-in-group [g i f & args]
  (set-in-group
   g i (apply f (group-elt g i) args)))

(defn map-paths [f & groups]
  (apply map f (map :paths groups)))

(defn set-paths [g & paths]
  (assoc g :paths paths))

;; 1.5 transformations

(defn- transform-group [f g args]
  (apply set-paths g (map-paths (fn [p] (apply f p args)) g)))

(defmethod translate :group [g v]
  (transform-group translate g [v]))

(defmethod scale :group [g fx & fy]
  (transform-group scale g (cons fx fy)))

(defmethod rotate :group [g angle]
  (transform-group rotate g [angle]))

(defmethod skew-x :group [g angle]
  (transform-group skew-x g [angle]))

(defmethod draw :group [g]
  (map-paths draw g))

;; 2. from

(defn from [c f p & args]
  (translate
   (apply f (translate p (vec-neg c)) args)
   c))

;; 3. orientation


;; for clockwise see:
;; http://stackoverflow.com/questions/1165647/how-to-determine-if-a-list-of-polygon-points-are-in-clockwise-order

(defn clockwise? [p]
  (let [p (draw p)]
    (> (reduce (fn [a e]
                 (let [[x1 y1] (first e)
                       [x2 y2] (second e)]
                   (+ a (* (- x2 x1) (+ y1 y2)))))
               0 (map list p (take (count p) (rest (cycle p)))))
       0)))

;; cw and ccw impose clockwise or counterclockwise directions to paths
;; I think it is useful to set explicitly the direction

(defn cw [p]
  (if (clockwise? p) p (reverse-all p)))

(defn ccw [p]
  (if (clockwise? p) (reverse-all p) p))

;; 4. font

;; 4.1 glyph

(defn base-outlines [v t]
  (if (nil? t) v
      (if (group? t)
        (concat v (reduce base-outlines [] (:paths t)))
        (conj v (draw t)))))

(defmacro glyph [name [& locals] advance & contents]
  `(let [~@locals]
     (apply make-glyph
            ~name
            ~advance
            (reduce base-outlines [] [~@contents]))))

;; 4.2 alignments

(defn- build-alignments [alignments]
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

;; 4.3 font macro

(defn- make-opt [opt]
  (if (= :grid (opt 0))
    `(font* ~@(rest opt))
    '(identity)))

(defmacro font [name [& opts] alignments variables [& glyphs]]
  `(->
    (let [~@alignments ~@variables]
      (make-font ~name
                 [~@(map (fn [x] (assoc x 1 `(alignment ~(get x 1))))
                         (build-alignments alignments))]
                 ~@glyphs))
    ~@(map make-opt opts)))

;; 5. foundry

;;;; a foundry wrap a font inside a function
;;;; the arguments passed to the foundry are the parameters used by the fonts
;;;; calling the foundry evaluates the font with the parameters supplied

(defn- parameters-list [params]
  {:keys (vec (map first (partition 2 params)))
    :or (apply hash-map params)})

(defmacro deffoundry [fontname [& params] & body-forms]
  `(defn ~fontname [& ~(parameters-list params)]
     (font ~(keyword (name fontname)) ~@body-forms)))

;; 6. counterpunches

;; In puchcutting 'counterpunches' was used to simplify the work.
;; The punchcutter can produce a punch for the 'white' inside 'p',
;; for example, and the use it for other letters like 'b'.
;; In my abstraction a counterpunch is defined as a collection
;; of points (used to define the 'white'), every point of the counterpunch
;; can be mapped to zero or more points in the exterior contour of the letter.
;; For example if I define an ellipse as the counterpunch of 'o',
;; the rules can translate these points defininig another ellipse.
;; This can be expressive since the translation can be defined in terms of
;; the contrast between horizontal and vertical 'strokes'.



(defn rule [f & args]
  (fn [x] (apply f x args)))

(defn- map-rules [rules points]
  (reduce
   concat
   (map (fn [r p]
          (if (vector? r)
            (map (fn [r] (r p)) r)
            (list (r p))))
        rules points)))


(defn use-ctpunch [closed f rules counterpunch]
  (let [new-points (map-rules rules counterpunch)]
    (if (= closed :closed)
      (group (ccw (apply f new-points))
             (cw (apply f counterpunch)))
      (ccw
       (apply f
              (concat
               counterpunch
               (reverse (map reverse-all new-points))))))))