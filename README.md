# Fonduta

Fonduta is a small library to experiment with parametric fonts written in Clojure. A lot of things should be fixed, improved, added, or deleted. Consider that I'm not a 'real' programmer, so you can find my code ugly. I know that we can use Metafont to make parametric fonts, but writing 'my own' code enabled me to test some (probably insane) concepts.

I wrote this just to be able to make experiments, so I tried to keep the font representation  as simple as possible:

(make-font 
      :abc                 ;; name
      [[:descender -250]
       [:x-height 500]
       [:ascender 750]]
      (make-glyph :o
                  [600 0]  ;; advance 
             	  [[300 -10]
              	   [443.0 -10.0] [560.0 107.0] [560 250]
              	   [560.0 393.0] [443.0 510.0] [300 510]
              	   [157.0 510.0] [40.0 393.0] [40 250]
              	   [40.0 107.0] [157.0 -10.0] [300 -10]]))

(this draws a circle in 'o')

Outlines are clojure vectors containing points. There's no difference between 'on curve' and 'off curve' points., the order of appearance in the outline says if a point is a control point,to make an example:

[p1 cp12 cp21 p2 cp23 cp32 p3 ...]

where cp is a control point. Thus a line segment looks like this:

[[0 0] [0 0] [10 10] [10 10]]

with control point equal to points.

Three macro can be used write fonts, font, glyph and deffoundry

Font is like make-font with some variations:
'options' (but only the :grid option is available)
'alignments'
'variables'

Options: is a (possibly empty) vector of options (but, at the momento, only :grid can be used)
[[:grid 100 100]]

Alignments: is a vector of 'alignments', every alignment is defined by the position and the offset of overshooting
[descender [-250 -10]
 x-height  [500 10]
 ascender  [750 10]]

Then, inside the font, you can refer to an aligment using:

(alignment x-height)        -> 500
(overshoot x-height)        -> 10
(overshooted x-height)      -> 510
(on-alignment x-height 100) -> [100 500]
(on-overshoot x-height 100) -> [100 510]

Variables: a (possibly empty) vector of variables with their values

[contrast 0.5
 stems    100]

the glyph macro has a vector for local variables

There's also another level of outline description (in tensionpaths.clj) written on top of the first one, where I have points, and 'tension' control points.
'Tension' contol points are placed between two points with a value (the tension) used to fnd the position of 'actual' control points (find interpolating between the tension control point and the point), There are also 'angle' controls where you define the tangents to the curve.
Path are build using the open-path and closed-path functions.

I hope in the future to add different kind of 'paths'.

(font :abc 
      []			;; options
      [descender [-250 -10]	;; alignments
       x-height  [500 10]
       ascender  [750 10]]
      []			;; variables
      [(glyph :o
              []		;; local variables
              [600 0] 
              (closed-path
               (pt 300 -10) (cpt 560 -10 0.55)
               (pt 560 250) (cpt 560 510 0.55)
               (pt 300 510) (cpt 40 510 0.55)
               (pt 40 250) (cpt 40 -10 0.55)))])

We can write a fancier example using alignments

(font :abc 
      [] 
      [descender [-250 -10]
       baseline  [0 -10]
       x-height  [500 10]
       ascender  [750 10]]
      [] 
      [(glyph :o
              [middle-height (/ (alignment x-height) 2)]
              [600 0] 
              (closed-path
               (pt (on-overshoot baseline 300))
               (cpt (on-overshoot baseline 560)  0.55)
               (pt 560 middle-height)
               (cpt (on-overshoot x-height 560) 0.55)
               (pt (on-overshoot x-height 300))
               (cpt (on-overshoot x-height 40) 0.55)
               (pt 40 middle-height)
               (cpt (on-overshoot baseline 40) 0.55)))])

Or we can use the circle function

(font :abc 
      [] 
      [descender [-250 -10]
       baseline  [0 -10]
       x-height  [500 10]
       ascender  [750 10]]
      [] 
      [(glyph :o
              [width 600
               middle-height (/ (alignment x-height) 2)
               middle-width (/ width 2)]
              [width 0] 
              (circle [middle-width middle-height]
                      (+ middle-height (overshoot x-height))))])

The deffoundry macro is used to make a function that returns fonts

(deffoundry abc ;; name of the font
  [xh 500]	;; a parameter with a default value
  []
  [descender [(* (- xh 1000) 0.5) -10]
   baseline  [0 -10]
   x-height  [xh 10]
   ascender  [(+ xh (* (- 1000 xh) 0.5)) -10]]
  []
  [(glyph :o
          [width (+ xh 100)
           middle-height (/ (alignment x-height) 2)
           middle-width (/ width 2)]
          [width 0]
          (circle [middle-width middle-height]
                  (+ middle-height (overshoot x-height))))])


and now you can call abc

(abc)
(abc :xh 400)

Probably you would like to see what you are doing. In fonduta.views you have two functions, make-view, and make-view-with-sliders:

(def v (make-view))
(v (abc) :o :o :o)

Since you have defined a foundry you can enjoy sliders:

(make-view-with-sliders "abc" abc [[:xh 0 1000 500]])

To save a font (in a minimal fontforge sfd format), just call:
(->sfd "path/to/your/file.sfd" your-font)

Looking at the file example.clj you can see a complex (and silly) example of use.
I will write a better documentation in the future.


Copyright © 2012 Daniele Capo

Distributed under the Eclipse Public License.

