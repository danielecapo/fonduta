# Fonduta

Fonduta is a small library to experiment with parametric fonts written in Clojure. A lot of things should be fixed, improved or added.
At the moment fonts can be saved as (minimal) .sfd files (the format used by [fontforge](http://fontforge.sourceforge.net/)) or [UFO(http://unifiedfontobject.org/)] (format 3) 'directories'.

The description of fonts (in fonduta.basefont) is really simple (maybe it is possible to make it even simpler): the name, basic alignments (such as descender, ascender, etc.) and glyphs; glyphs are described by a name, the advance data (width and height) and outlines (vectors of points).

(font :abc
      [[:descender -250]
       [:x-height 500]
       [:ascender 750]]
      (glyph :o [600 0] 
             [[300 -10]
              [443.0 -10.0] [560.0 107.0] [560 250]
              [560.0 393.0] [443.0 510.0] [300 510]
              [157.0 510.0] [40.0 393.0] [40 250]
              [40.0 107.0] [157.0 -10.0] [300 -10]]))

(this draws a circle in 'o')

Something about outlines: they are cubic bezier splines, the first point is always 'on curve', then there are two control point ('off curve') and the next 'on curve' point, and so on. We can decide if a point is a control point by looking at its position, in this way we can represent all the points as couples. If the first point is equal to the last one, the outline is closed.

However, there's a second 'level' (fonduta.font) where I have described macros to generate these fonts.
Here the curves are represented in a different way: between two points we can place a control point with a tension parameter, the 'actual' control points of the resulting bezier spline are found interpolating between 'on curve' points and the control point.
If a point is directly followed by another point, then we have a line.
Another kind of control point is provided: angle control point. The values before tension are read as the tangents of the curve on the first and second 'on curve' points.

Using the font macro in fonduta.font is simple, the code in the example 'translates' into the code above.

(font :abc 
      [] 
      [descender [-250 -10]
       x-height  [500 10]
       ascender  [750 10]]
      [] 
      [(glyph :o
              []
              [600 0] 
              (closed-path
               (pt 300 -10) (cpt 560 -10 0.55)
               (pt 560 250) (cpt 560 510 0.55)
               (pt 300 510) (cpt 40 510 0.55)
               (pt 40 250) (cpt 40 -10 0.55)))])

Let's see how it works:
In the first line I gave the name (:abc)
Second line is an empty vector (you can use to define 'options', but at the moment the only option available is :grid)
Then we have a vector of alignment (the first value is the alignment, the second is for 'overshoots')
Then we have a vector (in my case empty) where we can define variables to be used in the rest of the font.
Then a vector of glyphs. The empty vector in the glyph definition is for defining local variables.
Notice that alignements are variables and can be used in the body of font. You can refer to alignment using:

(alignment x-height)        -> 500
(overshoot x-height)        -> 10
(overshooted x-height)      -> 510
(on-alignment x-height 100) -> [100 500]
(on-overshoot x-height 100) -> [100 510]

We can rewrite the previous example:

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

In fonduta.font you can find the circle 'primitive', and we rewrite it again

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


##Foundries

The other macro in fonduta.font is deffoundry. A foundry is a 'factory' of fonts: it wraps a font inside a function. For example, if we want to produce fonts with different x-heights we can use deffoundry:

(deffoundry abc
  [xh 500]
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

Notice in the second line a vector with parameters and their default values.
The macro make a function named abc for you, then you can call it:

(abc :xh 600)
(abc :xh 400)

##Views

With 'views' you can see what you are doing. In fonduta.views you can find make-view and make-view-with-sliders.

make-view returns a function that require a font and a the glyphs to be displayed:

(def v (make-view))
(v (abc :xh 400) :o :o :o)

For a more dynamic experience you can use make-view-with-sliders:

(make-view-with-sliders "abc" abc [[:xh 0 1000 500]])

the arguments are: name, a function with keyword arguments that produces a font, a vector of sliders for each argument with minimum, maximum and default values.

##Saving
To save you can use the the functions defined in fonduta.basefont, ->sfd and ->ufo:

(->sfd filename font)

##Font Math
[Robofab(http://robofab.org/)] introduced the idea of [glyph math(http://robofab.org/howto/glyphmath.html)], in fonduta I've extended the same kind of operations to fonts.
The functions are in fonduta.basefont (font+, font-, font*, font-neg). They are useful to implement interpolations.


##Counterpunches
In fonduta.font there's a function named use-ctpunch that implement the idea of counterpunches. A counterpunch is a path (that define the 'white' inside a letter), to 'use' it you need a list of 'rules' that map the points of the counterpunch to the points of the exterior path.

(use-ctpunch [(rule translate [-10 -10])	
	      (rule translate [10 -10])
	      (rule translate [10 10])
	      (rule translate [-10 10])]
	     (closed-path (pt 0 0) (pt 100 0) (pt 100 100) (pt 0 100)))





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

