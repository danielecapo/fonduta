(ns fonduta.example3
   (:use fonduta.font
        fonduta.utils)
   (:require [fonduta.basefont :as base]))

(defn comp-rule [rule & rules]
  (fn [x] (reduce (fn [p r] (r p)) x (cons rule rules))))

(defn spacing-lines [counter weight]
  (+ (* counter 0.4) (* (- 1 weight) 0.05)))

(defn spacing-curves [counter weight]
  (* (spacing-lines counter weight) 0.34))

(defn letter-o [space curves hor-curves counter-width base xh]
  (let [up [0 hor-curves]
        right [curves 0]
        down (vec-neg up)
        left (vec-neg right)
        out-tense (rule tense 0.99)]
    (use-ctpunch
     [(rule translate down)
      (comp-rule (rule translate (vec+ down right))
                 out-tense)
      (rule translate right)
      (comp-rule (rule translate (vec+ up right))
                 out-tense)
      (rule translate up)
      (comp-rule (rule translate (vec+ up left))
                 out-tense)
      (rule translate left)
      (comp-rule (rule translate (vec+ left down))
                 out-tense)]
     (tense
      (ellipse [(+ space curves) (+ hor-curves (overshooted base))]
               [(+ space curves counter-width) (- (overshooted xh) hor-curves)])
      1.12))))

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
   line-space (spacing-lines n-counter-width weight)
   curve-space (spacing-curves n-counter-width weight)]
   
  [(glyph :o                   ;; glyphs
          [counter-width (* n-counter-width 1.2)
           black-width (+ (* curves 2) counter-width)]
          [(+ curve-space black-width curve-space) 0]
          (letter-o curve-space curves hor-curves counter-width
                    baseline x-height))])
          