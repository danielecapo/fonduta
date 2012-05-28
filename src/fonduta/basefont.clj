(ns fonduta.basefont
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [fonduta.sfd :as sfd])
  (:use fonduta.vectors
        fonduta.glyphlist
        fonduta.operations))

;(defn get-glyph [glyph-name font]
;  (get (:glyphs font) glyph-name))

(defn points [path]
  (rest path))

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

(defmethod draw :outline [p]
  p)

;; (defn vertical-metrics [x-height capitals ascender descender & [others]]
;;   (merge {:x-height x-height,
;;           :capitals capitals,
;;           :ascender ascender,
;;           :descender descender}
;;          others))

(defn glyph [name advance & outlines]
  {:name name,
   :advance advance,
   :outlines outlines})

(defn font [name alignments & glyphs]
  {:name name,
   :alignments (into {} alignments),
   :glyphs (vec glyphs)})  

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


;;; transform to sfd format (fontforge) 
;;; encoding: this is baaaaaaad, I should load a list of glyph names
;;; with their code

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

            
(defn ->sfd [filename f]
  (spit filename (sfd/font (build-sfd f))))



;;;; vector operations on fonts
;;;; implementing font 'math'
;;;; see robofab.org for the idea of glyph math
;;;; http://robofab.org/howto/glyphmath.html

(defn glyph-op [f g1 g2]
  (apply glyph (:name g1)
         (f (:advance g1) (:advance g2))
         (map (fn [o1 o2] (map f o1 o2)) (:outlines g1) (:outlines g2))))

(defn glyph+ [g1 & gs]
  (reduce (fn [a b] (glyph-op vec+ a b)) g1 gs))


(defn glyph- [g1 & gs]
  (reduce (fn [a b] (glyph-op vec- a b)) g1 gs))

(defn glyph* [g fx & [fy]]
  (let [fy (if (nil? fy) fx fy)]
    (apply glyph (:name g)
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
  (apply font
         (:name f1)
         (alignments-op f (:alignments f1) (:alignments f2))
         (map fg (sorted-glyphs f1) (sorted-glyphs f2))))

(defn font+ [f1 & fs]
  (reduce (fn [a b] (font-op + glyph+ a b)) f1 fs))

(defn font- [f1 & fs]
  (reduce (fn [a b] (font-op - glyph- a b)) f1 fs))

(defn font* [f fx & [fy]]
  (let [fy (if (nil? fy) fx fy)]
    (apply font
           (:name f)
           (map (fn [[k v]] [k (* v fy)]) (:alignments f))
           (map (fn [g] (glyph* g fx fy)) (glyphs f)))))

(defn font-neg [f]
  (font* f -1))

(defn interpolation [f1 f2 x & [y]]
  (let [y (if (nil? y) x y)]
    (font+ f1 (font* (font- f2 f1) x y))))


;;;; skew functions
;;;; useful for obliques

(defn glyph-skew-x [g angle]
  (apply glyph
         (:name g)
         (:advance g)
         (map (fn [o1]
                (map (fn [p] (vec-skew-x p angle)) o1))
              (:outlines g))))

(defn font-skew-x [f angle]
  (apply font
         (:name f)
         (:alignments f)
         (map (fn [g] (glyph-skew-x g angle)) (glyphs f))))