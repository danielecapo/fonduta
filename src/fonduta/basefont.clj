(ns fonduta.basefont
  (:require [ufo.core :as ufo]
            [clojure.string :as string]
            [fonduta.sfd :as sfd])
  (:use fonduta.utils
        seesaw.core
        seesaw.graphics)
  (:import [java.awt Graphics2D]))

;(defn get-glyph [glyph-name font]
;  (get (:glyphs font) glyph-name))

(defn points [path]
  (rest path))

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
  (let [e (int (first (name (get g :name))))]
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


;;;; draw to canvas

(defn- draw-outline [p o]
  (let [[fx fy] (first o)]
    (do
      (.moveTo p fx fy)
      (doseq [t (partition 3 (rest o))]
        (let [[[c1x c1y] [c2x c2y] [px py]] t]
          (.curveTo p c1x c1y c2x c2y px py))))
    p))

;  (cons (cons '.moveTo (first o))
;        (map (fn [[c1 c2 p]] `(~'.curveTo ~@c1 ~@c2 ~@p))
;             (partition 3 (rest o)))))

;(defn draw-outlines [o]
;  (print o)
;  (reduce concat (map draw-outline o)))

(defn- make-path [o]
  (let [p (path [])]
    (doseq [out o]
      (draw-outline p out))
    p))


(defn- draw-glyph [c gr g ascender zero]
  (let [o (:outlines g)
        s (/ (.getHeight c) 1200.0)]
    (push gr            
          (scale gr s (- s))
          (translate gr zero (- -100 ascender))
          (draw gr  (make-path o) (style :background "#000000")))))
       ;   (translate gr (+ zero (first (:advance g))) (- ascender)))))

(defn- draw-glyphs [f zero & glyphs]
  (let [gs (remove nil? (map (fn [g] (get-glyph f g)) glyphs))
        advances (reverse (reduce (fn [l g]
                                    (cons (+ (first (:advance g))
                                             (first l)) l))
                                  (list zero)
                                  gs))
        ascender (ascender f)]
    (fn [c g]
      (doseq [gl (map list gs advances)]
        (draw-glyph c g (first gl) ascender (second gl)))))) 

(defn view-string! [v f & g]
  (config! (select v [:#canvas])
           :paint (apply draw-glyphs f 100 g)))

(defn- make-frame []
  (let [fr
        (frame :title "glyph"
               :width 1200 :height 200
               :content (border-panel :center (canvas :id :canvas
                                                      :background "#fff"
                                                      :paint nil)))]
    (do (show! fr)
        fr)))

(defn make-view []
  (let [fr (make-frame)]
    (fn [f & glyphs]
      (apply view-string! fr f glyphs)))) 

;;;; I try to provide a simple gui' with sliders to see fonts
;;;; Moving it to a separate file should be a good idea

(defn- update-font [canv sliders text f]
  (let [sliders (map (fn [x]
                       [(config x :id) (/ (config x :value) 100.0)])
                     sliders)
        text (config text :text)]
    (config! canv
             :paint (apply draw-glyphs
                           (apply f (flatten sliders))
                           100
                           (map keyword (string/split text #" "))))))

(defn- update-labels [sliders labels]
  (let [sliders (map (fn [x]
                       (/ (config x :value) 100.0))
                     sliders)]
    (doseq [[l s] (map list labels sliders)]
      (config! l :text s))))

(defn make-frame-with-sliders [title f glyphs sliders]
  (let [sl (map (fn [s] (slider :id (s 0)
                                :min (s 1)
                                :max (s 2)
                                :value (s 3)
                                :class :slider))
                sliders)
        text (text :id :text :text (string/join " " (map name glyphs)))
        labels (map (fn [s] (label (name (s 0)))) sliders)
        curval (map (fn [s] (label :text (s 3)
                                   :class :value
                                   :id (keyword (str (name (s 0)) "value")))) sliders)
        canv (canvas :id :canvas
                       :background "#fff"
                       :paint nil)
        fr (frame :title title
                  :width 1200 :height 400
                  :content (border-panel
                            :north text
                            :west (vertical-panel :id :controls
                                   :items (interleave labels sl curval))
                            :center canv))]
    (letfn [(attacher [fo]
              (let [call (fn [e] (do (update-font canv sl text fo)
                                     (update-labels sl curval)))
                    detachers [(listen text :document call)                               
                               (listen sl :change call)]]
                (call nil)
                (fn [f]
                  (doseq [d detachers] (d))
                  (attacher f))))]

      (do (show! fr)
          (attacher f)))))


;;;; vector operations on fonts
;;;; implementing font 'math'
;;;; see robofab.org for the idea of glyph math

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
  (let [ks (clojure.set/intersection (set (keys a1)) (set (keys a2)))]
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

(defn interpolation [f1 f2 x & [y]]
  (let [y (if (nil? y) x y)]
    (font+ f1 (font* (font- f2 f1) x y))))