# Fonduta

Fonduta is a small library to experiment with parametric fonts written in Clojure. A lot of things should be fixed, improved, added.
Fonts can be saved as (minimal) .sfd files (the format used by [fontforge](http://fontforge.sourceforge.net/))

At the moment, the 'main' way to write a font with fonduta, is to use the font and foundry macros defined in fonduta.font.

## font macro example

(font name [& opts] [& alignments] [& variables] [& glyphs])

* *name* is a keyword
* *opts*: at the moment the only option available is grid [:grid x y]
* *alignments*: here you define common alignments (like ascender, descender, etc.). The format is: alignment-name [position overshoot]. To access to the position use (alignment x-height), for the overshoot use (overshoot x-height), to place a point on an alignment line use (on-alignment x-height 10), etc.
* *glyphs*: a glyph is built using the glyph macro

### glyph macro

(glyph name variables advance & paths)
* *name* is a keyword
* *variables*: a vector width variables names and values
* *advance*: a vector where the first value is the horizontal advance and the second value is the vertical advance.
* *paths*: where you define actual shapes

### how paths are built
A path is constructed with the path functions (or the closed-path and open-path functions)

(path :closed (pt 0 0) (pt 100 0) (pt 100 100) (pt 0 100))

Or (closed-path (pt 0 0) ...)

If you want curves you have to insert a control point between two points

(closed-path (pt 0 0) (cpt 100 0 0.55) (pt 100 100) ...)

The third parameter of a control point is the 'tension'. The curves are actually cubic bezier splines, the tension parameter is used to control where the real control points of the bezier spline are placed.

You can explicitly set the direction of a path using the ccw (counterclockwise) and cw (clockwise) functions. 
 
A small example of font usage

(font :abc
  [[: grid 10 10]]
  [descender [-24 -1]
   baseline  [0 -1]
   x-height  [50 1]
   ascender  [74 1]]
  [o-width (* (alignment x-height) 0.75)
   weight  10]]
  [(glyph :o
          []
	  [(+ o-width 12) 0]
          (ccw (ellipse (on-overshoot baseline 6) 
                        (on-overshoots x-height (+ 6 o-width))))
	  (cw (ellipse (vec+ [weight weight]
      	      	       	     (on-overshoot baseline 6))
		       (vec- [weight weight]
		       	     (on-overshoot x-height (+ 6 o-width))))))])


## foundry macro use

Now, if you don't want to change manually your parameters in the source code you can use the foundry macro.
Foundry 'wrap' a font inside a function, the parameters defined (with default values) become the argument of the function


(foundry abc
  [weight 100
   width  0.75]
  [[: grid 10 10]]
  [descender [-24 -1]
   baseline  [0 -1]
   x-height  [50 1]
   ascender  [74 1]]
  [o-width (* (alignment x-height) width)]]
  [(glyph :o
          []
	  [(+ o-width 12) 0]
          (ccw (ellipse (on-overshoot baseline 6) 
                        (on-overshoots x-height (+ 6 o-width))))
	  (cw (ellipse (vec+ [weight weight]
      	      	       	     (on-overshoot baseline 6))
		       (vec- [weight weight]
		       	     (on-overshoot x-height (+ 6 o-width))))))])

That produces an abc functionNow you can ask your 'foundry' to produce a font

(abc :weight 80)

## Interactive development

Probably you want to 'see' your font. You can make a view and then call it with your font and the glyphs to be displayed.

(def v (fonduta.basefont/make-view))

(v (abc :weight 80) :o :o :o)

### I want 'sliders'!

(make-view-with-sliders abc [[:weight 0 400 100] [:width 0 5 0.75]] :o :o :o)

