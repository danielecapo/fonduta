(ns fonduta.example3
   (:use fonduta.font
        fonduta.utils)
   (:require [fonduta.basefont :as base]))

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

(defn make-n-bowl [thickness hor-curves x-height depth]
  (fn [counter-width]
    (let [counter-height (- (overshooted x-height) hor-curves)
          top-depth (* counter-height depth)
          i (- thickness (* thickness (- 1 (/ 1 (+ 3 (/ thickness counter-width))))))]
      (use-ctpunch
       [(rule translate [(- i) (* hor-curves 0.3)])
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

(defn make-a-bowl [thickness baseline]
  (fn [counter-width counter-height curves hor-curves]
    (let [i (- thickness (* thickness (- 1 (/ 1 (+ 2 (/ thickness counter-width))))))]
      (use-ctpunch
       [(rule translate [i hor-curves])
        (rule translate [0 hor-curves])
        identity
        (rule translate [(- curves) (* hor-curves -0)])
        identity
        (rule translate [(* (- i thickness) 0.55) (- hor-curves)])
        (rule tense 1.03)
        (rule translate [i hor-curves])]
       (closed-path (pt 0 (+ hor-curves counter-height (overshoot baseline)))
                    (pt (* counter-width -0.4) (+ hor-curves counter-height (overshoot baseline)))
                    (acpt 0 (rad 90) 0.57)
                    (pt (- counter-width) (+ hor-curves (* counter-height 0.5) (overshoot baseline)))
                    (acpt (rad 90) 0 0.55)
                    (pt (* counter-width -0.56) (+ hor-curves (overshoot baseline)))
                    (acpt 0 (rad 90) 0.55)
                    (pt 0 (+ hor-curves (* counter-height 0.6))))))))
                    


(defn make-b-bowl [thickness curves hor-curves base x-height]
  (fn [counter-width]
    (let [counter-height (- (overshooted x-height) (overshoot base) (* 2 hor-curves))
          i (- thickness (* thickness (- 0.85 (/ 1 (+ 4 (/ thickness counter-width))))))
          bottom-left [0 (+ (overshooted base) hor-curves)]
          top-right [counter-width (- (overshooted x-height) hor-curves)]]
       (group
        (ccw (ellipse (vec- bottom-left [i hor-curves])
                      (vec+ top-right [curves hor-curves])))
        (cw (ellipse bottom-left top-right))))))

(defn letter-c [curves hor-curves base x-height depth]
  (fn [counter-width]
    (let [
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
        (pt right (- up (* depth (+ hor-curves counter-width))))
        (acpt (rad 90) 0 0.5)
        (pt (- right (* counter-width 0.45)) up)
        (acpt 0 (rad 90) 0.55)
        (pt left (- up (* counter-height 0.5)))
        (acpt (rad 90) 0 0.55)
        (pt (- right (* counter-width 0.45)) bottom)
        (acpt 0 (rad 90) 0.5)
        (pt (+ right (* counter-width 0.02)) (+ bottom (* depth (+ hor-curves counter-width)))))))))
     

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
   contrast 0.0]
  []                           ;; options
  [descender  [-240 -10]       ;; alignments [position overshooting]
   baseline   [0    -10]
   x-height   [500   10]
   cap-height [700   10]
   ascender   [740   10]]
  [xh (alignment x-height)     ;; variables
   stems (* xh 0.15 weight)
   curves (* stems 1.03)
   hor (* stems 0.85 (- 1 contrast))
   hor-curves (* hor 1.03)
   n-counter-width (- (* xh 0.70 width) (* 2 0.6 stems))
   depth (/ (/ (* (/ (overshooted x-height) 3.5 0.7) (/ n-counter-width (- (overshooted x-height) hor-curves)))
               (- (overshooted x-height) hor-curves))
            (if (< width 1) (Math/pow width 1.4) width))
                                        ;(* 0.25 (/ xh 0.7) (/ n-counter-width (- xh hor-curves)))
   line-space (spacing-lines n-counter-width weight)
   curve-space (spacing-curves n-counter-width weight)
   lc-stem (fn [start end] (stem stems start end))
   n-bowl (make-n-bowl stems (* hor-curves 1.07) x-height depth)
   a-shoulder (make-a-shoulder (* stems 0.95) (* hor-curves 0.8) x-height  (* depth 0.7))
   b-bowl (make-b-bowl stems curves hor-curves baseline x-height)
   a-bowl (make-a-bowl stems baseline)
   c (letter-c curves hor-curves baseline x-height (* 1.2 depth))
   elc (letter-e curves hor-curves baseline x-height depth)]
   
  [(glyph :a
          [counter-width (* n-counter-width 0.9)
           bowl-curves (* curves 0.97)
           bowl-hor-curves (* hor-curves (Math/pow 0.84 weight))
           bowl-counter-height (* 0.6 (- xh (* 2 bowl-hor-curves) hor-curves))
           bowl-counter-width (* counter-width 1.102)
           black-width (+ bowl-curves bowl-counter-width stems)]
          [(+ curve-space black-width line-space) 0]
          (tense
           (group
            (place (a-shoulder counter-width)
                   (+ curve-space (- bowl-counter-width counter-width) bowl-curves))
            (place (a-bowl bowl-counter-width bowl-counter-height bowl-curves bowl-hor-curves)
                   (+ curve-space black-width (- stems))))
           1.12))
   
   (glyph :b
          [counter-width (* n-counter-width 1.06)
           black-width (+ stems counter-width curves)]
          [(+ line-space black-width curve-space) 0]
          (tense
           (group
            (place (lc-stem baseline ascender) line-space)
            (place (b-bowl counter-width) (+ line-space stems)))
           1.12))

   (glyph :c
          [counter-width (* n-counter-width 1)
           black-width (+ curves counter-width curves)]
          [(+ curve-space black-width curve-space) 0]
          (tense (place (c counter-width) curve-space) 1.12))

   (glyph :e
          [counter-width (* n-counter-width 1)
           black-width (+ curves counter-width curves)]
          [(+ curve-space black-width curve-space) 0]
          (tense (place (elc counter-width) curve-space) 1.12))
          
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
           1.12))

   (glyph :h
          [counter-width n-counter-width
           black-width (+ (* stems 2) counter-width)]
          [(+ line-space black-width line-space) 0]
          (tense
           (group
            (place (lc-stem baseline ascender) line-space)
            (place (n-bowl counter-width) (+ line-space stems)))
           1.12))
   
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
           1.12))
   
   (glyph :n
          [counter-width n-counter-width
           black-width (+ (* stems 2) counter-width)]
          [(+ line-space black-width line-space) 0]
          (place (lc-stem baseline x-height) line-space)
          (tense (place (n-bowl counter-width) (+ line-space stems)) 1.12))

   (glyph :o                   ;; glyphs
          [counter-width (* n-counter-width 1.17)
           black-width (+ (* curves 2) counter-width)]
          [(+ curve-space black-width curve-space) 0]
          (tense
           (place
            (letter-o curves hor-curves counter-width baseline x-height)
            curve-space)
           1.12))

   (glyph :p
          [counter-width (* n-counter-width 1.06)
           black-width (+ stems counter-width curves)]
          [(+ line-space black-width curve-space) 0]
          (place (lc-stem descender x-height) line-space)
          (tense
           (place (b-bowl counter-width) (+ line-space stems))
           1.12))
          

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
            1.12))

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
           1.12))])
          