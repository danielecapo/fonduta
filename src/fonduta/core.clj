(ns fonduta.core
  (:require [fonduta.basefont :as base])
  (:use fonduta.vectors
        fonduta.operations))

;; 1. groups
;;  1.1 definition
;;  1.2 predicate
;;  1.3 accessors
;;  1.4 change
;;  1.5 transformations
;;  1.6 draw to 'base' format
;; 2. from
;; 3. clockwise and counter-clockwise directions
;; 4. font
;; 4.1 glyph
;; 4.2 alignments
;; 4.3 font macro
;; 5 foundry

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

;; 3. clockwise and counterclockwise directions

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

(defn- base-outlines [v t]
  (if (nil? t) v
      (if (group? t)
        (concat v (reduce base-outlines [] (:paths t)))
        (conj v (draw t)))))

(defmacro glyph [name [& locals] advance & contents]
  `(let [~@locals]
     (apply base/glyph
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

;; 5 foundry

;;;; a foundry wrap a font inside a function
;;;; the arguments passed to the foundry are the parameters used by the fonts
;;;; calling the foundry evaluates the font with the parameters supplied

(defn- parameters-list [params]
  {:keys (vec (map first (partition 2 params)))
    :or (apply hash-map params)})

(defmacro deffoundry [fontname [& params] & body-forms]
  `(defn ~fontname [& ~(parameters-list params)]
     (font ~(keyword (name fontname)) ~@body-forms)))