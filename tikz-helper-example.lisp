;;;; Install and run using asdf:
;;;; (pushnew "/path-to-tikz/tikz-helper/" asdf:*central-registry* :test #'equal)
;;;; (asdf:operate 'asdf:load-op 'tikz-helper)
;;;; (asdf:operate 'asdf:load-op 'tikz-levmar)
;;;; (asdf:operate 'asdf:load-op 'tikz-utils)
;;;; Or, if quicklisp is installed:
;;;; (pushnew "/path-to-tikz/tikz-helper/" asdf:*central-registry* :test #'equal)
;;;; (ql:quickload 'tikz-helper)
;;;; (ql:quickload 'tikz-levmar)
;;;; (ql:quickload 'tikz-utils)

(defpackage :tikz-helper-example
  (:use :cl :tikz-helper))
(in-package :tikz-helper-example)

(defparameter *plotting-dir* (make-pathname :defaults "/home/haavagj/src/tikz-helper/example/")
  "The plots produced in the code below will end up in this directory")
(defparameter *viewer* "evince" "A program to view the resulting pdf file.")
(defparameter *compilep* nil "The plots will be compiled with pdflatex in path, and viewed with *viewer*")

;;Code for  generating the documentation file examples.tex
(eval-when (:compile-toplevel)
  (defparameter *examples* nil "Stuff that makes up the example tex file."))

(defun comment (type data)
  "Adding stuff to documentation tex file."
  (push (list type data) *examples*))
  
(comment :section "Introduction")
(comment :text "tikz-helper is a set of common lisp functions and macros to make plots. This is done by 
generating \\LaTeX \\ code using pgf and {Ti\\textit{k}Z}.")
(comment :text "To generate a plot, one of the macros with-tikz-to-file, with-tikz-to-string or with-tikz-to-stream is called.
with-tikz-to-file and with-tikz-to-string are just wrappers for with-tikz-to-stream.
The macros set up the latex environment needed by the figures, collects information needed to perform
transformations between the data frame and a default frame, and draws axis for the plot. The transformations are
linear and works so that (plot-x-min,plot-y-min) is at (0cm,0cm) in the default frame, and  (plot-x-max,plot-x-min) is at (width in cm, height in cm)")
(comment :text "The axis-style should be one of :rectangle :cross :left-bottom :popped-out or :none.
Examples of all the different axis styles are below. Axis ticks are added to the axis. The position of the ticks is so
that they are placed with a spacing of 1,2 or 5 times 10 to a power such that you get between 4 and 10 ticks on the
axis. If custom ticks are needed, or ticks with names, not numbers, use :none, and call the corresponding draw-axis-*
function.")

(defmacro with-example-plot ((name plot-x-min plot-x-max plot-y-min plot-y-max axis-style 
				   &key (tikz-arg "") (width 10) (height 5)) caption &body body)
  "A macro wrapping the with-tikz-to-file macro for the example plots. It makes a plot with name in the 
*plotting-dir* directory, with a width of 10cm, a height of 5 cm. If *compilep* is T, the produced file is
compiled with pdflatex, the results are viewed with *viewer*. Also adds the figure to example.tex, with caption."
  (let ((fname (gensym)))
    `(let ((,fname (namestring (merge-pathnames (make-pathname :name ,name :type "tex") *plotting-dir*))))
       (with-tikz-to-file (tikz ,fname ,width ,height ,plot-x-min ,plot-x-max ,plot-y-min ,plot-y-max ,axis-style :tikz-arg ,tikz-arg)
	 ,@body)
       (when *compilep* (pdflatex-compile-view ,fname *viewer*))
       (push (list :figure ,fname ,caption) *examples*))))

(with-example-plot ("transform-and-clip" -1.5 0.5 -2 1 :rectangle)
    "By default paths and nodes are drawn in a frame where origin is the lower left corner of the plot,
and the units in x and y is 1cm. The transform macro generates tikz transformations, so that all points
within the scope are drawn in the plot frame, defined by plot- x-min x-max and y-min y-max.
 The clip-and-transform macro also clips the plotting area. Values with units like cm or pt are not scaled, 
but all points are translated. The black star path is here drawn in the current frame, the red one 
is shifted by (0.1,0.1) in the current frame, then drawn in units of cm."
  (labels ((draw-star (x x-cm y y-cm style)
	     (scope (tikz style)
	       (make-path-mixed-units tikz x (mapcar (lambda (x) (format nil "~acm" x)) x-cm)
				      y (mapcar (lambda (x) (format nil "~acm" x)) y-cm))
	       (path-use tikz)))
	   (draw-stuff ()
	     (let ((star-x (list -2 0 -2 0 -1 -2))
		   (star-y (list -2 0 0 -2 1 -2))
		   (no-offset (list 0 0 0 0 0 0))
		   (offset (list 0.1 0.1 0.1 0.1 0.1 0.1)))
	       ;;Black star, fully transformed
	       (draw-star star-x no-offset star-y no-offset "black") 
	       ;;Red star drawn from 0.1,0.1 in the current framed, but not scaled.
	       (draw-star offset star-x offset star-y "red")
	       ;;star node, shape and size not transformed.
	       (draw-node tikz 0 0 "draw=green,fill=yellow" (make-node-string "star" 0.4 0.4 0 "cm")))))
    (draw-stuff)
    (clip-and-transform (tikz)
      (draw-stuff))))

(comment :section "Simple plots")

(with-example-plot ("plots" -1.5 1.5 0 2 :left-bottom)
    "A histogram, a function and some data points. Most functions 
dealing with sets of data points call the clip-and-transform macro themselves, so calling it from 
top level is not necessary."
  (draw-histogram tikz (make-histogram -1.5 0.25 (make-range 0.0 0.1 12)) "draw=gray,fill=blue!30" 
		  :fill t :legend (legend 6.0 4.5 "Histogram"))
  (draw-function tikz (lambda (x) (* x x)) 100 "red" :legend (legend 6.0 4.1 "Function"))
  (draw-datapoints tikz (make-range -1.5 0.25 12) (make-range 2.0 -0.15 12) "fill=orange" :legend (legend 6.0 3.7 "Data points")))

(with-example-plot ("test-styles" 0 10 0 12 :rectangle)
    "Different styles of lines and nodes. The styles are just regular tikz options."
  (let ((x-vals (make-range 1 2 6)))
    ;;Function that translates y-vals, and draws datapoints connected with a line.
    (labels ((get-y (shift)
	       (mapcar (lambda (x) (+ shift (* (* 0.25 x) (* 0.25 x)))) x-vals))
	     (draw-translate (shift line-style mark-style node-name size-x size-y)
	       (draw-graph-spline tikz x-vals (get-y shift)
				  line-style mark-style :node (make-node-string node-name size-x size-y))))
      (draw-translate 0.0 "blue" "fill=blue,draw=blue" "circle" 2 2)
      ;;nested calls to transform do no damage. Nothing happens except in the outer most call
      (transform (tikz) (transform (tikz) (draw-translate 1.0 "red,thick" "fill=red" "circle" 3 3)))
      (draw-translate 2.0 "black,dashed" "draw=black,fill=yellow" "star,star points=5" 7 3)
      (draw-translate 3.0 "green!80!black" "draw=green!80!black,fill=green" "rectangle" 3 3)
      (draw-translate 4.0 "orange!80!black" "draw=orange!80!black,fill=orange" "diamond" 3 3)
      (draw-translate 5.0 "black,thick" "draw=black,fill=black!20" "ellipse" 2 4)
      (draw-translate 6.0 "purple!80!black,thick" "draw=purple!80!black,fill=purple!20"
		      "regular polygon,regular polygon sides=5" 4 4)
      ;;Data points with error bars
      (clip-and-transform (tikz) 
	(draw-function tikz (spline:get-spline-fun x-vals (get-y 7.0)) 100 "red"))
      (draw-profilepoints tikz x-vals (get-y 7.0) (make-range 0.5 0 11) "draw=red,fill=red"))))

(defun pick-one (list)
  "Get a random element from list"
  (elt list (random (length list))))

(with-example-plot ("bubbles" 0 10 0 10 :popped-out)
    "Data points of varying sizes, shapes and colors. Draw node does not automatically transform, 
since it can be useful in the default frame."
  (let* ((x (make-range 0 0.1 100))
	 (y (mapcar (lambda (x)  (+ x (tut:gaussian-random))) x))
	 (size (mapcar (lambda (x) (max 0.1 (+ 5.0 (* x 2.0)))) (tut:make-random-list 100))))
    (clip-and-transform (tikz)
      (mapc (lambda (x y size)
	      (draw-node tikz x y (format nil "draw=black,fill=~a,fill opacity=0.3"
					  (pick-one (list "red" "blue" "green")))
			 (make-node-string (pick-one (list "rectangle" "star" "circle")) size size 0 "mm")))
	    x y size))))

(defun make-gaussian-histogram (min bin-size nbins mean sigma ndraws)
  "Make a histograms from Gaussian random numbers."
  (let ((data (make-array nbins)))
    (flet ((add (g1)
	     (let ((bin (floor (- (+ mean (* sigma g1)) min) bin-size)))
	       (and (>= bin 0) (< bin nbins) (incf (aref data bin))))))
      (dotimes (i (floor ndraws 2))
	(multiple-value-bind (g1 g2) (tut:gaussian-random)
	  (add g1) (add g2))))
    (make-histogram min bin-size data)))

(comment 
 :text 
 (format nil 
	 "The with-tikz-to-string macro is nice for mixing text and drawings(like so ~a). It's possible to draw stuff in captions
by including the following:.
\\begin{verbatim}
%The preamble needs:
\\usepackage[singlelinecheck=off]{caption}
%Inside the figure environment
\\captionsetup{singlelinecheck=off}
\\caption[foo bar]\{\\node at (0,0) ...}
\\end{verbatim}" 
	 (with-tikz-to-string (tikz 1 1 0 0 0 0 :none) (draw-node tikz 0 0 "draw=black,fill=gray" (make-node-string "star" 0.3 0.3 0 "cm")))))

(flet ((c-legend (style fill)
	 (with-tikz-to-string (tikz 0 0 0 0 0 0 :none)
	   (tikz::legend-histo tikz (legend 0 0 "") style fill))))
  (with-example-plot ("test-histo2" 0 10 0 150 :rectangle)
      (format nil "Some Gaussian histograms with different styles and with legend entries. 
The legend entries are placed in the default cm frame, unless draw-histogram 
is called within (transform (tikz) ...). With some trickery it is also possible to get 
legends in captions: ~a Histogram 1, ~a Histogram 2, ~a Histogram 3."
	      (c-legend "draw=gray,fill=blue!50" t) (c-legend "red!80,thick" nil) (c-legend "draw=black,fill=green" t))
    (flet ((draw-histo (sigma ndraws style fill sep legend-y-pos number)
	     (draw-histogram tikz (make-gaussian-histogram 0.0 0.25 49 5.0 sigma ndraws)
			     style :fill fill :separate-bins sep
			     :legend (legend 0.5 legend-y-pos (format nil "Histogram ~a" number)))))
      (draw-histo 2.0 2500 "draw=gray,fill=blue!50" t nil 4.5 1)
      (draw-histo 1.8 1600 "red!80,thick" nil nil 4.1 2)
      (draw-histogram-columns tikz (make-gaussian-histogram 0.0 0.25 49 5.0 1.6 1000)
				    "draw=black,fill=green" 0.15 :legend (legend 0.5 3.7 "Histogram 3")))))
  
(comment :text 
	 (format nil "Simple sparkline: ~a"
		 (with-tikz-to-string (tikz 3 0.3 0 5 0 100 :none)
		   (draw-histogram-columns tikz (make-gaussian-histogram 0 0.25 20  4.5 2 1000)
						 "gray" 0.2))))

(comment :text
  (let ((histo (make-histogram 0 0.5 (tut:make-random-list 20))))
    (format nil "More complex sparkline: ~a \\textcolor{red}{~4,1f}, ~a indicates the $1 \\sigma$ band"
	    (with-tikz-to-string (tikz 3 0.4 0.1 10 -3 3 :none)
	      (clip-and-transform (tikz)
		(scope (tikz "gray!30")
		  (make-rectangle-path tikz 0 -1 10 1)
		  (path-use tikz t t)))
	      (draw-histogram tikz histo "black")
	      (transform (tikz)
		(draw-node tikz 10 (car (last (getf histo :data)))
			   "red,fill=red" (make-node-string "rectangle" 2 2))))
	    (car (last (getf histo :data)))
	    (with-tikz-to-string (tikz 0 0 0 0 0 0 :none)
	      (tikz::legend-histo tikz (legend 0 0 "") "draw=gray!30,fill=gray!30" t)))))

(with-example-plot ("test-histo3" 0 10 0 350 :none :tikz-arg "rotate=-90" :width 5 :height 10)
    "Histogram with bins extending in the horizontal direction. 
This is just a tikzpicture with the transformation rotate=-90. The text is not rotated, so axis ticks
require extra care, and text in legend entries will not be properly aligned.
The bins are named with the draw-axis-ticks function."
  (let* ((histo (make-gaussian-histogram 0.0 1.0 10 5.0 2.0 1000))
	 (data (map 'list (lambda (x) (+ 100 x)) (sort (getf histo :data) #'>))))
    ;; Data is from a Gaussian histo, but the bins are sorted and incremented by 100.
    (draw-histogram-columns tikz (make-histogram (getf histo :min) (getf histo :bin-size) data)
				  "draw=white,fill=blue!20" 0.8))
  ;;Use the axis ticks function to name bins
  (draw-axis-ticks-x tikz (make-range 0.5 1 10) :numberp nil :start 0 :stop 0 :text-style "right"
		     :names (mapcar (lambda (x) (format nil "Thing ~a" (floor x))) (make-range 1 1 11)))
  ;;Draw a line and som text in the data frame
  (transform (tikz)
    (draw-line tikz -0.3 250 10.3 250 "gray")
    (draw-node tikz -0.3 250 "above" "" "Threshold")))

(with-example-plot ("functions" -7 7 -1.2 1.2 :none)
    "Plotting sin(x) and cos(x), with grid lines and tick names on the x-axis."
  ;;Grid lines are specific values in the x-direction, automatic in the y-direct ion
  (draw-grid-lines tikz :x-list (list (* -2 pi) (* -1 pi) 0.0 pi (* 2 pi)))
  ;;Draw a rectangle around the plottingarea, with no axes.
  (draw-axis-popped-out tikz :x-list (list (* -2 pi) (* -1 pi) 0 pi (* 2 pi))
			:x-names (list "$-2\\pi$" "$-\\pi$" "0.0" "$\\pi$" "$2\\pi$"))
  (draw-function tikz #'sin 100 "red" :legend (legend 0.7 5.2 "sin(x)"))
  (draw-function tikz #'cos 100 "blue" :legend (legend 2.9 5.2 "cos(x)")))
  
(with-example-plot ("gaussian-distribution" -3.2 3.2 0 0.45 :none)
    "Gaussian function, made by closing, drawing and filling function segments."
  (flet ((clip-draw (x-min x-max x-pos-height color text)
	   (draw-function tikz (lambda (x) (tut:gauss x #( 1.0 0.0 1.0))) 100
			  (format nil "draw=black,thick,fill=~a" color) 
			  :x-min x-min :x-max x-max :fill t :close t)
	   ;;A text node with an somewhat transparent background box.
	   (draw-node tikz (/ (+ x-min x-max) 2) (* 0.5 (tut:gauss x-pos-height #(1.0 0.0 1.0)))
		      "draw=black,fill=white,fill opacity=0.3,text opacity=1.0"
		      (make-node-string "rectangle,rounded corners" 2 2 2) text)))
    (let ((stops (list 0 1 2 3.5))
	  (height (list 0.5 1.7 1.6))
	  (col (list "blue!40" "orange!60" "red!80"))
	  (perc  (list "34.1\\%" "12.6\\%" "2.2\\%")))
      (clip-and-transform (tikz)
	(mapc #'clip-draw (mapcar #'- stops) (mapcar #'- (cdr stops)) height col perc)
	(mapc #'clip-draw stops (cdr stops) height col perc))))
  (draw-axis-ticks-x tikz (make-range -2 1 5) :numberp nil
		     :names (list "$-2\\sigma$" "$-\\sigma$" "$\\mu$" "$\\sigma$" "$2\\sigma$")))

(comment :section "Fitting with Levenberg-Mmarquart")
(comment :text "The Levenberg-Marquart algorithm minimizes the squared distance in the y-direction
between a function and a set of data points by changing function parameters. 
If errors are supplied the $\\chi^2$, or the squared normalized differences, is minimized.")

(with-example-plot ("test-fitter" 0 10 0 20 :popped-out)
    "Some Gauss smeared data points, fitted with the Gaussian function. Fit parameters are printed in the plot. 
A spline fit is also plotted."
  (let* ((x-poses (make-range 0 0.25d0 41))
	 (y-poses (mapcar (lambda (x) (tut:gauss x #(90.0d0 5.0d0 2.0d0))) x-poses))
	 (smeared-y (mapcar (lambda (x) (+ x (tut:gaussian-random))) y-poses))
	 (fit-params (levmar:levmar-optimize #'tut:gauss #(10.0d0 0.0d0 1.0d0) x-poses y-poses)))
    (draw-function tikz (spline:get-spline-fun x-poses smeared-y)
		   400 "green,thick" :legend (legend 0.5 4.1 "Spline fit"))
    (draw-function tikz (lambda (x) (tut:gauss x fit-params)) 200 "thick,gray" 
		   :legend (legend 0.5 3.7 "Gauss fit"))
    (draw-datapoints tikz x-poses smeared-y "draw=blue,fill=blue"
		     :legend (legend 0.5 4.5 "Noisy data"))
    (draw-node tikz 9.5 4.5 "left" "" (format nil "Fitted mean: ~5,2f" (aref fit-params 1)))
    (draw-node tikz 9.5 4.1 "left" "" (format nil "Fitted sigma: ~5,2f" (aref fit-params 2)))))

(with-example-plot ("test-fitter2" 0 10 0 300 :left-bottom)
    "Same as above, except the data points have errors. The error bars are calculated from bin content.
Empty bins are discarded in the fit."
  (let* ((histo (make-gaussian-histogram 0 0.5 20 5.0 1.5 2000))
	 (x-poses (make-range 0.25 0.5 21))
	 (y-poses (getf histo :data))
	 (y-errors (map 'vector #'sqrt y-poses))
	 (parameters (levmar:levmar-optimize-errors #'tut:gauss #(300.0d0 3.0d0 1.0d0)
						    x-poses y-poses y-errors t)))
    (draw-histogram tikz histo "draw=blue!10,fill=blue!20" :fill t :separate-bins t
		    :legend (legend 0.5 4.5 "Data"))
    (draw-profilepoints tikz x-poses y-poses y-errors "draw=gray,fill=gray" 
			:node (make-node-string "rectangle" 3 0) :legend (legend 0.5 4.1 "Bin uncertainty"))
    (draw-function tikz (lambda (x) (tut:gauss x parameters)) 200 "thick,red"
		   :legend (legend 0.5 3.7 "Gauss fit"))
    (draw-node tikz 9.5 4.5 "left" "" (format nil "Fitted mean: ~5,2f" (aref parameters 1)))
    (draw-node tikz 9.5 4.1 "left" "" (format nil "Fitted sigma: ~5,2f" (aref parameters 2)))))

(defun polynomial (x params) 
  "Polynomial of degree (- (length params) 1)"
  (reduce #'+ (map 'vector (lambda (param degree) 
			     (* param (expt x degree))) params 
			     (make-range (- (length params) 1) -1 (length params)))))

(with-example-plot ("test-fitter3" -7 7 -100 100 :none)
    "Noisy data points, with known errors, fitted with a polynomial of the third degree. 
The ''Simulated parameter'' legend is placed in the default frame, the ''Fitted parameters'' 
in the data frame."
  (let* ((x-poses (make-range -10 0.5 41))
	 (y-poses (mapcar (lambda (x) (polynomial x #(0.5 -1.0d0 -2.0d0 3.0d0))) x-poses))
	 (y-errors (mapcar (lambda (x) (+ 4.0  (sqrt (abs x)))) y-poses))
	 (y-smeared (mapcar (lambda (x err) (+ x (* err (tut:gaussian-random)))) y-poses y-errors))
	 (params (levmar:levmar-optimize-errors #'polynomial #(1.0d0 1.0d0 1.0d0 1.0d0)
						x-poses y-smeared y-errors)))
    (flet ((print-params (params)
	     (format nil "~{$~3,1fx^3 ~3,1@fx^2 ~3,1@fx ~3,1@f$~}" (coerce params 'list))))
      (draw-profilepoints tikz x-poses y-smeared y-errors "draw=red,fill=red"
			  :legend (legend 5.3 1.4 "Simulated parameters:"))
      (draw-node tikz 5.3 1.0 "right" "" (print-params #(0.5 -1.0 -2.0 3.0))) 
      (transform (tikz) ;;The legend/text node is placed in the data frame.
	(draw-function tikz (lambda (x) (polynomial x params)) 200 "blue"
		       :legend (legend -6.5 90 "Fitted parameters:"))
	(draw-node tikz -6.5 75 "right" "" (print-params params))))
    (draw-axis-cross tikz :y-ticks-max 8 :y-ticks-min 4)))

(with-example-plot ("spline" 4.0 6.0 4.0 7.0 :popped-out)
    "Cubic splines, with different end point conditions."
  (let ((x (list 4.0d0  4.35d0 4.57d0 4.76d0 5.26d0 5.88d0))
	(y (list 4.19d0 5.77d0 6.57d0 6.23d0 4.90d0 4.77d0)))
    (draw-graph-spline tikz x y "red" "fill=black,draw=black" :legend (legend 5.5 4.0 "Not-a-know splint"))
    (draw-graph-spline tikz x y "blue" "fill=black,draw=black" :legend (legend 5.5 3.6 "Natural spline") :natural t)))

(comment :section "Sub figures")

(comment :text "The with-sub-figure macro makes it possible to draw with a new set of transformations
within the same tikz figure. In addition to the with-tikz-to-stream arguments, the sub-figure also
expects the offset from (0,0) in the default cm frame.")

(with-example-plot ("sub-fig" -3.0 3.0 -1.0 1.0 :none)
    "More than one plot can be plotted in the same figure by using sub figures.
Sub figures are basically a new set of transformations, and do not affect the default cm frame at all.
Here is a function with a zoomed view of a region of interest."
  (flet ((erf-gauss (x) (+ (tut:erf x) (tut:gauss x (vector 0.001 -0.15 0.01)))))
    (draw-axis-cross tikz :y-ticks-max 6)
    (draw-function tikz #'erf-gauss 400 "red")
    (with-subfigure (tikz tikz2 7.5 0.1 3 1.5 -0.2 -0.05 -0.20 -0.1)
      (region-of-interest-zoom tikz tikz2 "gray" nil t t nil)
      (draw-axis-rectangle tikz2 :fill t  :x-ticks-min 2 :x-ticks-max 4
			   :y-ticks-min 2 :y-ticks-max 4)
      (draw-function tikz2 #'erf-gauss 100 "red"))))

(with-example-plot ("sub-histo" 0.0 10.0 -6.0 6.0 :none)
    "Horizontal histograms, in sub figures side by side. The mean and $\\sigma$ are indicated by red verical bars."
  (flet ((draw-sub-histo (offset mean sigma)
	   (with-subfigure (tikz tikz2 offset 0.0 2.0 5 0.0 1000 -6.0 6.0)
	     (let ((histo (make-gaussian-histogram -6.0 0.25 48 mean sigma 10000)))
	       (transform (tikz2)
		 (multiple-value-bind (y x) (tikz::make-histogram-path-points histo)
		   (draw-path tikz x y "fill=black!20,draw=black!80" t))
		 (draw-line tikz 0 -6 0 6 "thick,black")
		 (draw-profilepoint tikz2 (* 0.5 (reduce #'max (getf histo :data)))
				    mean sigma "red,fill=red,thick" :node (make-node-string "circle" 4 4)))))))
    (mapc #'draw-sub-histo (list 0 2 4 6 8) (list -2.0 -1 0.5 0.2 0.0) (list 2.2 1.8 1.6 1.4 1.2))
    (draw-grid-lines tikz :x-list nil :y-list (make-range -6.0 2.0 7) :line-style "thin,black,draw opacity=0.1")
    ;;(draw-axis-ticks-y tikz (make-range -6.0 2.0 7) :precision 1 :stop "10cm" :style "gray")
    (draw-axis-ticks-x tikz (make-range 0.5 2.0 5) :numberp nil
		       :stop 0 :start 0
		       :names (mapcar (lambda (x) (format nil "Step ~a" x)) (make-range 1 1 5)))))


(with-example-plot ("bubbles2" 0 10 0 10 :none)
    "Two data sets with different transformations are plotted on top of ech other."
  (flet ((draw-axis (tikz x-cm color start stop)
	   (draw-line tikz x-cm 0 x-cm 5 (format nil "thick,~a" color))
	   (multiple-value-bind (ax pr) (tikz::auto-ticks-y tikz t 4 10)
	     (draw-axis-ticks-y tikz  ax :x-shift x-cm :precision pr
				:style color :text-style color :start start :stop stop))))
    (let* ((x (make-range 0 0.1 101))
	   (y1 (mapcar (lambda (x)  (+ x (tut:gaussian-random))) x))
	   (y2 (mapcar (lambda (x)  (* 100 (- 10 (+ x (tut:gaussian-random))))) x)))
      (draw-datapoints tikz x y1 "draw=black,fill=blue" :node (make-node-string "circle" 4 4)
		       :legend (legend 2.5 5.1 "Blue data"))
      (with-subfigure (tikz tikz2 0 0 10 5 0 10 0 1000)
	(draw-datapoints tikz2 x y2 "draw=black,fill=red" :node (make-node-string "rectangle" 4 4)
			 :legend (legend 5.5 5.1 "Red data"))
	(draw-axis-left-bottom tikz2 :y-list nil)
	(draw-node tikz2 11 2.5 "rotate=-90,red" ""  "Red axis")
	(draw-axis tikz2 "10cm" "right,red" "2pt" "-2pt"))
      (draw-node tikz -0.7 2.5 "rotate=90,blue" ""  "Blue axis")
      (draw-axis tikz "0cm" "blue,left" "-2pt" "2pt"))))

(with-example-plot ("log-scale" 0 10 -0.5 2 :none)
    "Plot with log scale in the y direction. Explicit transformation."
  (let* ((x (make-range 0.0 0.1 101))
	 (err (mapcar (lambda (x) (+ (* (expt 10 (* 0.2 x)) 0.4 (tut:gaussian-random)))) x))
	 (y (mapcar (lambda (x err) (max 0.01 (+ (expt 10 (* 0.20 x)) err))) x err)))
    (draw-axis-popped-out tikz :y-list nil)
    (draw-grid-lines tikz :x-list nil
		     :y-list (mapcar (lambda (x) (log x 10)) (list 0.5 0.75 1 2.5 5.0 7.5 10 25 50 75 100)))
    (draw-axis-ticks-y tikz (mapcar (lambda (x) (log x 10)) (list 1 10 100))
		       :x-shift "-0.15cm" :numberp nil :stop 0
		       :names (mapcar (lambda (x) (format nil "$10^{~a}$" x)) (list 0 1 2)))
    (draw-function tikz (lambda (x) (log (* (expt 10 (* 0.2 x))) 10)) 100 "blue,thick" 
		   :legend (legend 0.5 5.3 "$10^{0.2 x}$"))
    (draw-datapoints tikz x (mapcar (lambda (x) (log x 10)) y) "fill=red,draw=red!20!black" 
		     :node (make-node-string "circle" 4 4))))

(comment :section "2D histograms")
(comment :text "2D representations of 2D histograms.")
 
(defun make-2d-histo ()
  "Make and fill a histogram"
  (let* ((histo (make-histogram2d -2.5 0.2 25 -2.5 0.2 25)))
    (flet ((make-cross (x y)
	     (dotimes (i 100000)
	       (multiple-value-bind (g1 g2) (tut:gaussian-random)
		 (histo2d-incf histo (+ (* g1 0.3) x)  (- (random 5.0) 2.5))
		 (histo2d-incf histo (- (random 5.0) 2.5) (+ (* g2 0.3) y))))))
      (make-cross 1 1)
      (make-cross -1 -1))
    histo))

(with-example-plot ("histo-rect" -2.5 2.5 -2.5 2.5 :popped-out)
    "2D histogram drawn as filled rectangles. Takes a while to compile with pdflatex, 
especially if the binning is fine."
  (let* ((histo (make-2d-histo)))
    (draw-histo2d-rectangles tikz histo 0 (histo2d-get-max histo))
    (color-palette tikz 10.2 0 0.5 5.0 0 (* 0.9 (histo2d-get-max histo)))))

(with-example-plot ("histo-cont" -2.5 2.5 -2.5 2.5 :popped-out)
    "2D histogram drawn as filled contour regions. The points making up the contour lines 
are just linear interpolation between neighbors on either side of the contour height."
  (let* ((histo (make-2d-histo)))
    (draw-histo2d-contour tikz histo 0 (* 0.95 (histo2d-get-max histo)) 50 t :color-lines t)
    (color-palette tikz 10.2 0 0.5 5.0 1 (* 0.9 (histo2d-get-max histo)))))

(with-example-plot ("histo-isolines" -2.5 2.5 -2.5 2.5 :popped-out)
    "2D histogram drawn as isolines."
  (let* ((histo (make-2d-histo)))
    (draw-histo2d-contour tikz histo 0 (* 0.95 (histo2d-get-max histo)) 10 nil :color-lines t)))

(with-example-plot ("histo-opacity" -2.5 2.5 -2.5 2.5 :popped-out)
    "2D histogram drawn as colored layers with opacity of less than one."
  (let* ((histo (make-2d-histo)))
    (draw-histo2d-contour tikz histo 100 (* 0.95 (histo2d-get-max histo)) 10 t :color-lines t :layer-opacity 0.11 :cols (list "black"))))

(with-example-plot ("histo-nodes" -2.5 2.5 -2.5 2.5 :popped-out)
    "2D histograms drawn as nodes of varying sizes."
  (let* ((histo (make-2d-histo)))
    (draw-histo2d-nodes tikz histo 0 (+ 500 (histo2d-get-max histo)) "rectangle" "draw=blue!80,fill=blue!50")))

(defun get-v (r x)
  (cond ((>= (* x x) (* r r)) 0.0)
	(t (* 1 (sqrt (- (* r r) (* x x)))))))

(with-example-plot ("velocity-field2" -3 3 0 1 :popped-out :width 7 :height 7)
    "Velocity field with streamlines as parametrized functions."
  (let ((vectorfield (make-vectorfield2d -3 0.5 13 0.0 0.1 10
					 :function (lambda (x z) (vector (get-v 3 x) z)))))
    (draw-node tikz -1.0 3.5 "" "" "$z$")
    (draw-node tikz 3.5 -1 "" "" "$x$")
    (clip (tikz)
      (scope (tikz "draw=blue,thick")
	(draw-vectorfield2d tikz vectorfield 0.1 :scale 0.1))
      (transform (tikz)
	(map nil (lambda (z0)
		   (tikz::draw-parameter-path tikz (lambda (theta) (vector (* 3 (cos theta))
									   (* z0 (exp theta))))
					      pi (* 2 pi) 25 "red,thick"))
	     (make-range 0.0 0.001 25))))))

(comment :section "3D scalar fields")

(comment :text "3D histograms can be visualized by projecting 3D scalar data into a 2D
histogram in a projection plane. The camera is pointed towards a reference point. The
projection has a horizontal axis parallel to the cross product of the difference vector
between the camera and the reference point, and the z-axis. The vertical axis is parallel to
the cross product between the difference and the horizontal axis. The hight and width of the
projection is measured along the axes around the reference point. Rays are emitted from the
bins in the projection plane, moving away from the camera. The distance from the camera to
the projection plane is specified in units of the distance from the camera to the
reference. The reference point is only in the projection plane if this distance in 1. If the
projection plane is inside the 3D scalar field, only data further away from the camera will
be used in the visualization. The plots are generated in the file volumes-example.lisp.

Three projection methods are implemented. The first method uses a line integral along the ray
emitting from the projection bin. The data is from \\\\
http://www.cg.tuwien.ac.at/research/publications/2005/dataset-stagbeetle/")

(let ((text (format nil "\\includegraphics\{~a\}
\\includegraphics\{~a\}
\\includegraphics\{~a\}"
		    (namestring (merge-pathnames (make-pathname :name "beetle1" :type "pdf") *plotting-dir*))
		    (namestring (merge-pathnames (make-pathname :name "beetle2" :type "pdf") *plotting-dir*))
		    (namestring (merge-pathnames (make-pathname :name "beetle3" :type "pdf") *plotting-dir*)))))
  (comment :text text))

(comment :text "The second method is the Maximum Intensity Projection (MIP). The maximum
value the ray passes through is used. The data is from \\\\ http://graphics.stanford.edu/data/voldata/CThead.tar.gz.")

(let ((text (format nil "\\includegraphics\{~a\}
\\includegraphics\{~a\}
\\includegraphics\{~a\}"
		    (namestring (merge-pathnames (make-pathname :name "head1" :type "pdf") *plotting-dir*))
		    (namestring (merge-pathnames (make-pathname :name "head2" :type "pdf") *plotting-dir*))
		    (namestring (merge-pathnames (make-pathname :name "head3" :type "pdf") *plotting-dir*)))))
  (comment :text text))


(comment :text "The third method is the Local Maximum Intensity Projection (LMIP). This is
similar to MIP, but the first value above a threshold passed by each ray is returned. If the
treshold is set very low, the projection plane can be studied as a cut plane. The data is from \\\\
http://graphics.stanford.edu/data/voldata/MRbrain.tar.gz.")

(let ((text (format nil "\\includegraphics\{~a\}
\\includegraphics\{~a\}
\\includegraphics\{~a\}"
		    (namestring (merge-pathnames (make-pathname :name "brain1" :type "pdf") *plotting-dir*))
		    (namestring (merge-pathnames (make-pathname :name "brain2" :type "pdf") *plotting-dir*))
		    (namestring (merge-pathnames (make-pathname :name "brain3" :type "pdf") *plotting-dir*)))))
  (comment :text text))


(defun make-example-tex ()
  "Generate a tex file containing all the example plots above. 
Make sure *examples* looks right before use, or just C-c C-k."
  (let ((fname (namestring (merge-pathnames 
			    (make-pathname :name "example" :type "tex") *plotting-dir*))))
    (with-open-file (tex fname :direction :output :if-exists :supersede)
      (tikz::print-preamble tex :documentclass "article" :use-standalone t)
      (tikz::latex-command tex "usepackage" nil "float")
      (tikz::latex-command tex "usepackage" "singlelinecheck=off" "caption")
      (tikz::latex-environs (tex "document")
	(mapc (lambda (entry)
		(ecase (first entry)
		  (:figure (tikz::latex-environ-with-arg (tex "figure" "H")
			     (tikz::latex-command tex "centering")
			     (tikz::latex-command tex "input" nil (second entry))
			     (tikz::latex-command tex "captionsetup" nil "singlelinecheck=off")
			     (tikz::latex-command tex "caption" "asdf" (third entry)))) ;;caption needs a [argument] for some reason
		  (:section  (format tex "\\section{~a}~%" (second entry)))
		  (:text (format tex "~%~%~a~%~%" (second entry)))))
	      (remove-duplicates (reverse *examples*) :test #'equal))))
    (pdflatex-compile-view fname "evince")))

(make-example-tex)
