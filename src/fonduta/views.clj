(ns fonduta.views
  (:require [clojure.string :as string]
            [fonduta.core :as core])
  (:use seesaw.core
        seesaw.graphics)
  (:import [java.awt Graphics2D]))


;;;; draw to canvas

(defn- draw-outline [p o]
  (let [[fx fy] (first o)]
    (do
      (.moveTo p fx fy)
      (doseq [t (partition 3 (rest o))]
        (let [[[c1x c1y] [c2x c2y] [px py]] t]
          (.curveTo p c1x c1y c2x c2y px py))))
    p))


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
  (let [gs (remove nil? (map (fn [g] (core/get-glyph f g)) glyphs))
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
        (frame :title "fonduta preview"
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

;;;; I try to provide a simple gui with sliders to see fonts

(defn- update-font [canv sliders text f]
  (let [sliders (map (fn [x]
                       [(config x :id) (/ (config x :value) 1000.0)])
                     sliders)
        text (config text :text)]
    (config! canv
             :paint (apply draw-glyphs
                           (apply f (flatten sliders))
                           100
                           (map keyword (string/split text #" "))))))

(defn- update-labels [sliders labels]
  (let [sliders (map (fn [x]
                       (/ (config x :value) 1000.0))
                     sliders)]
    (doseq [[l s] (map list labels sliders)]
      (config! l :text s))))

(defn make-view-with-sliders [title f sliders]
  (let [sl (map (fn [s] (slider :id (s 0)
                                :min (* (s 1) 1000)
                                :max (* (s 2) 1000)
                                :value (* (s 3) 1000)
                                :class :slider
                                :size [200 :by 30]))
                sliders)
        text (text :id :text :text "f o n d u t a")
        labels (map (fn [s] (label :size [100 :by 20] :text (name (s 0)))) sliders)
        curval (map (fn [s] (label :text (s 3)
                                   :class :value
                                   :id (keyword (str (name (s 0)) "value")))) sliders)
        sl-lab (map (fn [l v s] (vertical-panel :items [(horizontal-panel :size [200 :by 20]
                                                                          :items [l v]) s]))
                    labels curval sl)
        canv (canvas :id :canvas
                       :background "#fff"
                       :paint nil)
        fr (frame :title title
                  :width 1200 :height 400
                  :content (border-panel
                            :north text
                            :west (scrollable 
                                   (vertical-panel :id :controls
                                                   :size [250 :by 300]
                                                   :items sl-lab);;(interleave labels curval sl))
                                   :hscroll :never)
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

