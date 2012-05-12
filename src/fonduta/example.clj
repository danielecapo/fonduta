(ns fonduta.example
  (:use fonduta.font
        fonduta.utils)
  (:require [fonduta.basefont :as base]))

(defn donut [bottom-left top-right h v ext-out ext-int]
  (group (ccw (tense (super-ellipse bottom-left top-right) ext-out))
         (cw (tense (super-ellipse (vec+ bottom-left [v h])
                                   (vec- top-right [v h])) ext-out))))

(defn flared-stem [bottom-left top-right flare depth]
  (let [[l b] bottom-left
        [r t] top-right]
    (path :closed
         (pt (- l flare) b) (pt (+ r flare) b)
         (cpt r (+ depth b) 0.55) (pt r (+ (* 2 depth) b))
         (pt r (- t (* 2 depth))) (cpt r (- t depth) 0.55)
         (pt (+ r flare) t) (pt (- l flare) t)
         (cpt l (- t depth) 0.55) (pt l (- t (* 2 depth)))
         (pt l (+ (* 2 depth) b)) (cpt l (+ depth b) 0.55))))

(defn half [n] (/ n 2))

(defn twice [n] (* n 2))

(defn foot-serif [pos width height stem-width]
  (ccw (closed-path (pt  (vec- pos [width 0]))
                    (pt  (vec+ pos [(+ stem-width width) 0]))
                    (cpt (vec+ pos [(+ stem-width) (/ height 2.5)]) 0.8)
                    (pt  (vec+ pos [(+ stem-width) height]))
                    (pt  (vec+ pos [0 height]))
                    (cpt (vec+ pos [0 (/ height 2.5)]) 0.8))))

(defn half-serif [pos overshoot width height height-top stem-width]
  (ccw (closed-path (pt  pos)
                    (pt  (vec+ pos [stem-width 0]))
                    (pt  (vec+ pos [stem-width (+ height height-top overshoot)]))
                    (cpt (vec+ pos [stem-width height]) 0.3)
                    (pt  (vec+ pos [(- width) height]))
                    (cpt (vec+ pos [0 (* height 0.6)]) 0.8))))

                    
(deffoundry abc
  ;; beginnig of parameter list
  ;; each parameter has a default value
  [x-height 500 
   weight 100
   width 100
   round 100
   contrast 0
   track 1.0
   n-over-o 0.75
   serifs 0]


                   ;;(flared-stem bottom-left (vec+ bottom-left [v-stems h]) flare 110))]

  ;; then alignments
  ;; the first value is the alignment, the second is the overshoot

  []
  
  [descender [(/ (- x-height 1000) 2) -10] 
   baseline [0 -10]
   x-height [x-height 10]
   ascender [(- (alignment x-height) (alignment descender)) 10]]

  
  ;now name some variables to be used later
  [round-control (- 1.0 (* round 0.0045))
   o-width       (/ (* (alignment x-height) width) 100.0)
   n-width       (* o-width n-over-o (- (/ 1 n-over-o)
                                        (* (- 1 (/ 1 n-over-o)) (/ round -100.0))))
   v-stems       (* (/ (alignment x-height) 1000.0) weight)
   v-curves      (* v-stems (* (+ 1 (* round 0.0003))))
   h-stems       (- v-stems (* v-stems contrast))
   h-curves      (* h-stems (* (+ 1 (* round 0.0003))))
   space-lines   (* track (/ (- n-width (twice v-stems)) 2.12))
   space-curves  (* space-lines (/ (- 100 (* round 0.65)) 100.0))
   stem          (fn [bottom-left h]
                   (rect bottom-left (vec+ bottom-left [v-stems h])))
   serif-height  (* h-stems 3.2)
   f-serif       (fn [pos]
                   (foot-serif pos serifs serif-height v-stems))
   h-serif       (fn [pos]
                   (half-serif pos 15 serifs
                               serif-height (* serif-height 1.5 contrast) v-stems))
   lc-stem       (fn [from-point to-alignment]
                   (let [h (- to-alignment (from-point 1)
                              (+ (twice serif-height) (* 1.5 serif-height contrast)))
                         s (vec+ from-point [0 serif-height])]
                     (group 
                      (ccw (f-serif from-point))
                      (ccw (stem s h))
                      (ccw (h-serif (vec+ s [0 h]))))))
   n-top         (fn [pos width height]
                   (let [a (rad 60)
                         h (* width 0.2 (Math/tan a))]
                     (ccw
                      (translate 
                       (closed-path (pt   [-20 (- h)])
                                    (acpt a 0 round-control)
                                    (pt   [(* 0.5 width) 0])
                                    (acpt 0 (rad 90) (* 1.1 round-control))
                                    (pt   [width (- height)])
                                    (pt   [(- width v-stems) (- height)])
                                    (acpt (rad 90) 0 round-control)
                                    (pt   [(* 0.55 (- width v-stems)) (- (* 0.8 v-curves))])
                                    (acpt 0 a round-control)
                                    (pt   [-20 (- (+ h-stems h))]))
                       pos))))
   n-right       (fn [pos width height]
                   (let [t (/ height 3.34)]
                   (group
                    (n-top (vec+ pos [0 height]) width t)
                    (translate
                     (group (ccw (stem pos (- height t)))
                            (ccw (f-serif pos)))
                     [(- width v-stems) 0]))))
                   

   ]
  
  ;; beginning of glyph definitions
  [(glyph :h
         [space space-lines
          right-width (- n-width v-stems)
          width (+ n-width (twice space))]
         [width 0]
         (lc-stem (on-alignment baseline space) (alignment ascender))
         (n-right (on-alignment baseline (+ space v-stems))
                 right-width
                 (overshooted x-height)))
  (glyph :i
         [space space-lines
          middle (+ space (half v-stems))]
         [(+ v-stems space space) 0]
         (lc-stem (on-alignment baseline space) (alignment x-height))
         (translate (circle [middle (+ (alignment x-height) (* 0.5 v-stems) 80)]
                            (* 1.45 (half v-stems)))
                    [(* v-stems -0.05) 0]))

  (glyph :l
         [space space-lines
          middle (+ space (half v-stems))]
         [(+ v-stems space space) 0]
         (lc-stem (on-alignment baseline space) (alignment ascender)))
  
  (glyph :m
         [space space-lines
          reduction 0.95
          right-width (- (* reduction n-width) v-stems)
          width (+ (* reduction n-width) right-width (twice space))
          r (n-right (on-alignment baseline (+ space v-stems))
                     right-width
                     (overshooted x-height))]
         [width 0]
         (lc-stem (on-alignment baseline space) (alignment x-height))
         r
         (translate r [right-width 0]))

  (glyph :n
         [space space-lines
          right-width (- n-width v-stems)
          width (+ n-width (twice space))]
         [width 0]
         (lc-stem (on-alignment baseline space) (alignment x-height))
         (n-right (on-alignment baseline (+ space v-stems))
                 right-width
                 (overshooted x-height)))

  (glyph :o
         [left space-curves
          width (+ (* 2 left) o-width)
          right (- width left)
          height 0]
         ;; advance x and y
         [width 0] 
         ;; paths
         (donut (on-overshoot baseline left) 
                (on-overshoot x-height right)
                h-curves v-curves
                round-control round-control))

  (glyph :p
         [left  space-lines
          right space-curves
          width (+ left right (* 0.52 (+ o-width n-width)))]
         [width 0]
         (lc-stem (on-alignment descender left) (alignment x-height))
         (donut (on-overshoot baseline (- left (half v-stems))) 
                (on-overshoot x-height (- width right))
                h-curves v-curves
                round-control round-control))

  (glyph :t
         []
         [400 0]
         (rect [10 0] [390 h-stems]))]) 

         
                              
         