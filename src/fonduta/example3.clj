(ns fonduta.example3
   (:use fonduta.font
        fonduta.utils)
   (:require [fonduta.basefont :as base]))

(defn exp-inv [a x]
  (/ 1 (Math/pow a x)))

(defn bound-to-max [a m speed]
  (fn [x]
    (* m (- 1 (exp-inv a (* x speed))))))

(defn comp-rule [rule & rules]
  (fn [x] (reduce (fn [p r] (r p)) x (cons rule rules))))

(defn spacing-lines [counter weight]
  (* (* counter 0.34) (+ 1 (* (- weight 1) 0))))

(defn spacing-curves [counter weight]
  (* (spacing-lines counter weight) 0.4))

(defn place [p x]
  (translate p [x 0]))

(defn stem [thickness start end]
  (ccw (rect (on-alignment start 0) (on-alignment end thickness))))

(defn make-n-bowl [thickness hor-curves x-height depth contrast]
  (fn [counter-width]
    (let [counter-height (- (overshooted x-height) hor-curves)
          top-depth (* counter-height depth)
          i (- thickness (* 0.95 ((bound-to-max 4 thickness 2)  (/ thickness counter-width))))]
          ;;(* (- 1 contrast) (- thickness (* thickness (- 1 (/ 1 (+ 2.5 (/ thickness counter-width)))))))]
      (use-ctpunch
       [(rule translate [(- i) (* hor-curves 0.1)])
        (rule translate [(- i) hor-curves])
        (rule translate [(* (- thickness i) 0.52) hor-curves])
        (rule translate [thickness hor-curves])
        (rule translate [thickness (* hor-curves 0.2)])
        (rule translate [thickness 0])]
       (open-path
        (pt 0 (- counter-height top-depth))
        (acpt (rad 90) 0 0.55)
        (pt (* counter-width 0.5) counter-height)
        (acpt 0 (rad 90) 0.55)
        (pt counter-width (- counter-height top-depth))
        (pt counter-width 0))))))

(defn make-a-shoulder [thickness hor-curves x-height depth]
  (fn [counter-width]
    (let [counter-height (- (overshooted x-height) hor-curves)
          top-depth (* counter-height depth)]
      (use-ctpunch
       [(rule translate [(- thickness) 0])
        identity
        (rule translate [0 hor-curves])
        (rule translate [thickness hor-curves])
        (rule translate [thickness 0])
        (rule translate [thickness 0])]
       (open-path
        (pt 0 (- counter-height top-depth))
        (acpt (rad 90) 0 0.55)
        (pt (* counter-width 0.5) counter-height)
        (acpt 0 (rad 90) 0.55)
        (pt counter-width (- counter-height top-depth))
        (pt counter-width 0))))))

(defn make-a-bowl [thickness baseline contrast]
  (fn [counter-width counter-height curves hor-curves]
    (let [i (- thickness (* 0.9 ((bound-to-max 6 thickness 2)  (/ thickness counter-width))))]

          ;;i  (* (- 1 contrast) (- thickness (* thickness (- 1 (/ 1 (+ 1.8 (/ thickness counter-width)))))))]
      (use-ctpunch
       [(rule translate [i hor-curves])
        (rule translate [0 hor-curves])
        identity
        (rule translate [(- curves) (* hor-curves -0)])
        identity
        (rule translate [(* (- i thickness) 0.55) (- hor-curves)])
        (rule tense 1.0)
        (rule translate [i 0])]
       (closed-path (pt 0 (+ hor-curves counter-height (overshoot baseline)))
                    (pt (* counter-width -0.4) (+ hor-curves counter-height (overshoot baseline)))
                    (acpt 0 (rad 90) 0.57)
                    (pt (- counter-width) (+ hor-curves (* counter-height 0.5) (overshoot baseline)))
                    (acpt (rad 90) 0 0.55)
                    (pt (* counter-width -0.56) (+ hor-curves (overshoot baseline)))
                    (acpt 0 (rad 90) 0.55)
                    (pt 0 (+ hor-curves (* counter-height 0.6))))))))
                    


(defn make-b-bowl [thickness curves hor-curves base x-height contrast]
  (fn [counter-width]
    (let [counter-height (- (overshooted x-height) (overshoot base) (* 2 hor-curves))
          i (* (- 1 contrast) (- thickness (* thickness (- 1 (/ 1 (+ 1.9 (/ thickness counter-width)))))))
          bottom-left [0 (+ (overshooted base) hor-curves)]
          top-right [counter-width (- (overshooted x-height) hor-curves)]]
       (group
        (ccw (ellipse (vec- bottom-left [i hor-curves])
                      (vec+ top-right [curves hor-curves])))
        (cw (ellipse bottom-left top-right))))))

(defn letter-c [curves hor-curves base x-height aperture]
  (fn [counter-width]
    (let [counter-height (- (overshooted x-height) (overshoot base) (* 2 hor-curves))
          left curves
          right (+ curves counter-width)
          bottom (+ hor-curves (overshoot base))
          up (- (overshooted x-height) hor-curves)]
      (use-ctpunch
       [(rule translate [curves 0])
        identity
        (rule translate [0  hor-curves])
        identity
        (rule translate [(- curves) 0])
        identity
        (rule translate [0 (- hor-curves)])
        identity
        (rule translate [curves 0])]
       (open-path
        (pt right (- up (* (- 1 aperture) 0.5 counter-height)))
        (acpt (rad 90) 0 0.5)
        (pt (- right (* counter-width 0.45)) up)
        (acpt 0 (rad 90) 0.55)
        (pt left (- up (* counter-height 0.5)))
        (acpt (rad 90) 0 0.55)
        (pt (- right (* counter-width 0.45)) bottom)
        (acpt 0 (rad 90) 0.5)
        (pt (+ right (* counter-width 0.02)) (+ bottom (* (- 1 aperture) 0.5 counter-height))))))))
     

(defn letter-e [curves hor-curves base x-height depth]
  (fn [counter-width]
    (let [depth (* depth 0.6)
          counter-height (- (overshooted x-height) (overshoot base) (* 2 hor-curves))
          left curves
          right (+ curves counter-width)
          bottom (+ hor-curves (overshoot base))
          up (- (overshooted x-height) hor-curves)]
      (use-ctpunch
       [(rule translate [curves 0])
        identity
        (rule translate [0  hor-curves])
        identity
        (rule translate [(- curves) 0])
        identity
        (rule translate [0 (- hor-curves)])
        identity
        (rule translate [curves 0])]
       (open-path
        (pt right (- up (* depth counter-height 1.2)))
        (acpt (rad 90) 0 0.5)
        (pt (- right (* counter-width 0.45)) up)
        (acpt 0 (rad 90) 0.55)
        (pt left (- up (* counter-height 0.5)))
        (acpt (rad 90) 0 0.55)
        (pt (- right (* counter-width 0.45)) bottom)
        (acpt 0 (rad 90) 0.5)
        (pt (+ right (* counter-width 0.02)) (+ bottom (* depth counter-height))))))))
     
      
(defn letter-o [curves hor-curves counter-width base xh]
  (let [bottom-left (on-overshoot base 0)
        top-right (on-overshoot xh (+ curves counter-width curves))]
     (group (ccw (ellipse bottom-left top-right)) 
            (cw (ellipse (vec+ bottom-left [curves hor-curves])
                         (vec- top-right [curves hor-curves]))))))
                 

(deffoundry sans
  [weight   1.0                ;; parameters
   width    1.0
   contrast 0.0
   round 1.12]
  []                           ;; options
  [descender  [-240 -10]       ;; alignments [position overshooting]
   baseline   [0    -10]
   x-height   [500   10]
   cap-height [700   10]
   ascender   [740   10]]
  [xh (alignment x-height)     ;; variables
   stems (* xh 0.15 weight)
   curves (* stems 1.03)
   hor (* ((bound-to-max Math/E (* xh 0.37) 0.4) weight)
          (- 1 contrast))
   ;;(* stems 0.85 (- 1 contrast))
   hor-curves (* hor 1.03)
   n-counter-width (- (* xh 0.70 width) (* 2 0.6 stems))
   depth (* (/ (- (* xh 0.37) hor)  (- (overshooted x-height) hor-curves))
            (if (<= width 1) width (+ 1 (* (- width 1) 0.4))))

   ;; (* (/ (/ (* (/ (overshooted x-height) 3.5 0.7) (/ n-counter-width (- (overshooted x-height) hor-curves)))
   ;;                (- (overshooted x-height) hor-curves))
   ;;             (if (< width 1) (Math/pow width 1.3) width))
   ;;          (+ contrast 1))
                                        ;(* 0.25 (/ xh 0.7) (/ n-counter-width (- xh hor-curves)))
   aperture 0.28
   line-space (spacing-lines n-counter-width weight)
   curve-space (spacing-curves n-counter-width weight)
   lc-stem (fn [start end] (stem stems start end))
   n-bowl (make-n-bowl stems (* hor-curves 1.07) x-height depth contrast)
   b-bowl (make-b-bowl stems curves hor-curves baseline x-height contrast)
   a-bowl (make-a-bowl stems baseline contrast)
   c (letter-c curves hor-curves baseline x-height aperture)
   elc (letter-e curves hor-curves baseline x-height depth)]
   
  [(glyph :a
          [counter-width (* n-counter-width 0.9)
           stems (* stems 0.97)
           curves (* curves 0.97)
           depth (/ depth 1.1  (if (<= width 1) width (+ 1 (* (- width 1) 0.4))))
           hor-curves (* hor-curves 0.8);(Math/pow 0.84 weight))
           bowl-counter-height (* 0.6 (- xh (* 2 hor-curves) hor-curves))
           bowl-counter-width (* counter-width 1.102)
           black-width (+ curves bowl-counter-width stems)]
          [(+ curve-space black-width line-space) 0]
          (tense
           (group
            (place ((make-a-shoulder stems hor-curves x-height depth) counter-width)
                   (+ curve-space (- bowl-counter-width counter-width) curves))
            (place (a-bowl bowl-counter-width bowl-counter-height curves hor-curves)
                   (+ curve-space black-width (- stems))))
           round))
   
   (glyph :b
          [counter-width (* n-counter-width 1.06)
           black-width (+ stems counter-width curves)]
          [(+ line-space black-width curve-space) 0]
          (tense
           (group
            (place (lc-stem baseline ascender) line-space)
            (place (b-bowl counter-width) (+ line-space stems)))
           round))

   (glyph :c
          [counter-width (* n-counter-width 1)
           black-width (+ curves counter-width curves)]
          [(+ curve-space black-width curve-space) 0]
          (tense (place (c counter-width) curve-space) round))

   (glyph :e
          [counter-width (* n-counter-width 1)
           black-width (+ curves counter-width curves)]
          [(+ curve-space black-width curve-space) 0]
          (tense (place (elc counter-width) curve-space) round))
          
   (glyph :d
          [counter-width (* n-counter-width 1.06)
           black-width (+ curves counter-width stems)
           rot-center [(/ counter-width 2.0) (/ xh 2)]]
          [(+ curve-space black-width line-space) 0]
          (tense
           (group
            (place (lc-stem baseline ascender) (+ curve-space curves counter-width))
            (place (from rot-center
                         rotate (b-bowl counter-width) (rad 180))
                   (+ curve-space curves)))
           round))

   (glyph :h
          [counter-width n-counter-width
           black-width (+ (* stems 2) counter-width)]
          [(+ line-space black-width line-space) 0]
          (tense
           (group
            (place (lc-stem baseline ascender) line-space)
            (place (n-bowl counter-width) (+ line-space stems)))
           round))
   
   (glyph :i
          []
          [(+ line-space stems line-space) 0]
          (place (lc-stem baseline x-height) line-space))

   (glyph :l
          []
          [(+ line-space stems line-space) 0]
          (place (lc-stem baseline ascender) line-space))
   
   (glyph :m
          [counter-width (* 0.9 n-counter-width)
           black-width (+ (* stems 3) (* 2 counter-width))]
          [(+ line-space black-width line-space) 0]
          (place (lc-stem baseline x-height) line-space)
          (tense
           (group
            (place (n-bowl counter-width) (+ line-space stems))
            (place (n-bowl counter-width) (+ line-space (* 2 stems) counter-width)))
           round))
   
   (glyph :n
          [counter-width n-counter-width
           black-width (+ (* stems 2) counter-width)]
          [(+ line-space black-width line-space) 0]
          (place (lc-stem baseline x-height) line-space)
          (tense (place (n-bowl counter-width) (+ line-space stems)) round))

   (glyph :o                   ;; glyphs
          [counter-width (* n-counter-width 1.17)
           black-width (+ (* curves 2) counter-width)]
          [(+ curve-space black-width curve-space) 0]
          (tense
           (place
            (letter-o curves hor-curves counter-width baseline x-height)
            curve-space)
           round))

   (glyph :p
          [counter-width (* n-counter-width 1.06)
           black-width (+ stems counter-width curves)]
          [(+ line-space black-width curve-space) 0]
          (place (lc-stem descender x-height) line-space)
          (tense
           (place (b-bowl counter-width) (+ line-space stems))
           round))
          

   (glyph :q
          [counter-width (* n-counter-width 1.06)
           black-width (+ curves counter-width stems)
           rot-center [(/ counter-width 2.0) (/ xh 2)]]
          [(+ curve-space black-width line-space) 0]
           (place (lc-stem descender x-height) (+ curve-space curves counter-width))
           (tense
            (place (from rot-center
                         rotate (b-bowl counter-width) (rad 180))
                   (+ curve-space curves))
            round))

   (glyph :u
          [counter-width (* 0.95 n-counter-width)
           black-width (+ (* stems 2) counter-width)
           middle [(+ line-space (/ black-width 2.0)) (/ xh 2.0)]]
          [(+ line-space black-width line-space) 0]
          (tense
           (from middle
                 rotate
                 (group
                  (place (lc-stem baseline x-height) line-space)
                  (place (n-bowl counter-width) (+ line-space stems)))
                 (rad 180))
           round))
    (glyph :v
           [counter-width (* n-counter-width 1.1)
            diag-width (* stems 1.1)
            black-width (+ (* diag-width 2) counter-width)
            diag (ccw (closed-path (pt (on-alignment x-height 0))
                                   (pt (on-alignment x-height diag-width))
                                   (pt (on-alignment baseline (/ (+ black-width diag-width) 2)))
                                   (pt (on-alignment baseline (/ (- black-width diag-width) 2)))))]
           [black-width 0]
           diag
           (ccw (from [(/ black-width 2) 0] scale diag  -1 1)))])
          