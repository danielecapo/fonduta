(ns fonduta.example2
   (:use fonduta.font
        fonduta.utils)
   (:require [fonduta.basefont :as base]))

(deffoundry sqfo
  [proportions 0.5
   weight 1.0
   contrast 0.0
   track 1.0
   width 1.0]
  []
  [x-height  [(* 1000 proportions) 0]
   descender [(/ (- (alignment x-height) 1000) 2.0) 0]
   baseline  [0 0]
   capitals  [(/ (+ 1000 (alignment x-height)) 2.0) 0]
   ascender  [(/ (+ 1000 (alignment x-height)) 2.0) 0]]

  [standard-stem    (* (alignment x-height) 0.2)
   standard-counter (* standard-stem 2 width)
   v-stems          (* standard-stem weight)
   h-stems          (* (* v-stems 0.85)  (- 1 contrast))
   counter-width    (- standard-counter
                       (* (/ (- v-stems standard-stem) 3.0) 4))
   counter-height   (- (alignment x-height) (* 2 h-stems))
   black-width      (+ counter-width (* 2 v-stems))
   space            (* track (/ counter-width 2))
   n-advance        [(+ (* 2 space) (* 2 v-stems) counter-width) 0]
   counter-margins  {:left (+ space v-stems), :right (+ space v-stems counter-width)}

   stem             (fn [left start end]
                      (ccw (rect [left (alignment start)]
                                 [(+ left v-stems) (alignment end)])))
   [h v d]          [[v-stems 0] [0 h-stems] [v-stems h-stems]]
   middle           (vec-scale [(vec-x n-advance) (alignment x-height)] 0.5)  

   n-counter        (-> (open-path (pt 0.0 1.0) ; (- 1 (/ 1 (Math/pow (+ weight 2) 3))))
                                   (pt 1.0 1.0)
                                   (pt 1.0 0.0))
                        (scale counter-width
                               (+ counter-height h-stems))
                        (translate [(+ space v-stems) 0]))
   o-counter        (-> (square [0.5 0.5] 1.0)
                        (scale counter-width counter-height)
                        (translate [(+ space v-stems) h-stems]))
   p-counter        (close
                     (add-to (set-in-path n-counter 2
                                          (translate (path-elt n-counter 2)
                                                     [0 h-stems]))
                             (pt (+ space v-stems) h-stems)))
   c-counter        (-> (open-path (pt 1.0 0.0)
                                   (pt 0.0 0.0)
                                   (pt 0.0 1.0)
                                   (pt 1.0 1.0))
                        (scale (+ (* counter-width 0.75) (* 0.8 v-stems)) counter-height)
                        (translate (vec+ d [space 0])))
   n-right          (use-ctpunch [[(rule translate (vec-scale v 0.5))
                                   (rule translate
                                         [(min (* (+ counter-width v-stems) 0.5)
                                               h-stems)
                                          h-stems])]
                                  (rule translate d)
                                  (rule translate h)]
                             n-counter)
   p-right          (use-ctpunch [[(rule translate (vec-scale v 0.5))
                                   (rule translate
                                         [(min (* (+ counter-width v-stems) 0.5)
                                               h-stems)
                                          h-stems])]
                                  (rule translate d)
                                  (rule translate (vec-scale d 1 -1))
                                  (rule translate (vec-neg v))]
                             p-counter)
   c                (ctpunch [(translate (vec-neg v))
                              (translate (vec-scale d -1))
                              (translate (vec-scale d -1 1))
                              (translate v)]
                             c-counter)
   i-dot            (translate (rect [0 0] d)
                               [space (min (+ (alignment x-height) h-stems)
                                           (- (alignment ascender) h-stems))])]

                        
  [(glyph :a
          [counter-width  (* 0.8 counter-width)
           black-width    (+ (* 2 v-stems) counter-width)
           bar-height     (min (/ counter-height 1.4) h-stems)
           opening        (/ (- counter-height bar-height) 2.0)]
          [(+ (* 2 space) black-width) 0]
          (translate (rect [0 0] [(- black-width v-stems) h-stems]) [space (- (alignment x-height) h-stems)])
          (stem (+ space counter-width v-stems) baseline x-height)
          (use-ctpunch
           [(rule translate [0 bar-height])
            (rule translate [(- v-stems) bar-height])
            (rule translate (vec-neg d))
            [(rule translate
                   [(max (* (+ counter-width v-stems) -0.5)
                         (- h-stems))
                    (- h-stems)])
             (rule translate (vec-scale v -0.5))]]
           (-> (open-path (pt counter-width opening)
                          (pt 0 opening)
                          (pt 0 0)
                          (pt counter-width 0))
               (translate [(+ space v-stems) h-stems]))))
                  
   
   (glyph :b
          []
          n-advance
          (stem space baseline ascender)
          p-right)
   
   (glyph :c
          []
          [(+ (* 2 space) (* 0.75 counter-width) (* 1.8 v-stems)) 0]
          c
          (translate
           (ccw (rect [(* -0.8 v-stems) (max (- h-stems) (* -0.3 counter-height))] [0 0]))
           [(+ space (* 1.8 v-stems) (* 0.75 counter-width))
            (- (alignment x-height) h-stems)]))
   
   (glyph :d
          []
          n-advance
          (stem (:right counter-margins) baseline ascender)
          (from middle rotate p-right (rad 180)))

   (glyph :e
          [counter-width (* 0.8 counter-width)
           black-width   (+ (* 2 v-stems ) counter-width)
           bar-height    (min (/ counter-height 1.4)  h-stems)
           opening       (/ (- counter-height bar-height) 2.0)
           open-ratio    (/ opening counter-height)]
          [(+ (* 2 space) black-width) 0]
          (ctpunch [(translate (vec-neg v))
                    (translate (vec-neg d))
                    (translate (vec-scale d -1 1))
                    (translate d)
                    (translate h)]
                   (-> (open-path (pt (- black-width v-stems) 0)
                                  (pt 0 0)
                                  (pt 0 counter-height)
                                  (pt counter-width counter-height)
                                  (pt counter-width (- counter-height opening)))
                       (translate [(+ space v-stems) h-stems])))
          (translate (rect [0 0] [black-width bar-height])
                     [space (+ opening h-stems)]))
;;          (translate
;;           (ccw (rect [(* -0.8 v-stems) (max (- h-stems) (* -0.3 counter-height))] [0 0]))
;;           [(+ space (* 1.8 v-stems) (* 0.75 counter-width)

   (glyph :g
          []
          n-advance
          (stem (:right counter-margins) descender x-height)
          (from middle rotate p-right (rad 180))
          (ccw
           (rect (on-alignment descender (+ space (/ black-width 6)))
                 [(:right counter-margins) (+ (alignment descender) h-stems)])))
    (glyph :h
          []
          n-advance
          (stem space baseline ascender)
          n-right)
    
   (glyph :i
          []
          [(+ (* 2 space) v-stems) 0]
          (stem space baseline x-height)
          i-dot)

   (glyph :l
          []
          [(+ (* 2 space) v-stems) 0]
          (stem space baseline ascender))
   
   (glyph :m
          []
          (vec+ n-advance [(+ counter-width v-stems) 0])
          (stem space baseline x-height)
          n-right
          (translate n-right [(+ counter-width v-stems) 0]))

   (glyph :n
          []
          n-advance
          (stem space baseline x-height)
          n-right)
   
   (glyph :o
          [d [v-stems h-stems]]
          n-advance
          (ctpunch [(translate (vec-scale d -1.0 -1.0))
                    (translate (vec-scale d 1.0 -1.0))
                    (translate d)
                    (translate (vec-scale d -1.0 1.0))]
                   o-counter))
   (glyph :p
          []
          n-advance
          (stem space descender x-height)
          p-right)

   (glyph :q
          []
          n-advance
          (stem (:right counter-margins) descender x-height)
          (from middle rotate p-right (rad 180)))

   (glyph :u
          []
          n-advance
          (from middle rotate (group
                               (stem space baseline x-height)
                               n-right)
                (rad 180)))
                 

   ])
          ;; (-> (group
          ;;      (rect [0 (alignment baseline)]
          ;;            [v-stems (alignment x-height)])
          ;;      (rect [(- width v-stems) (alignment baseline)]
          ;;            [width (alignment x-height)]))
          ;;     (translate [space 0])))])
          
