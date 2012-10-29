#|
Install and run using quicklisp:
(pushnew "/path-to-tikz/tikz-helper/" asdf:*central-registry* :test #'equal)
Load the tikz-helper package:
(ql:quickload 'tikz-helper)
Load the package for function fitting:
(ql:quickload 'tikz-levmar)
Some functions used in the examples
(ql:quickload 'tikz-utils)
|#

(use-package :tikz-helper)

(defparameter *plotting-dir* "/home/haavagj/src/tikz-helper/example/"
  "The plots produced in the code below will end up in this directory")
(defparameter *compilep* t "The plots will be compiled with pdflatex in path, and viewed with *viewer*")
(defparameter *viewer* "evince" "A program to view the resulting pdf file.")

(defmacro with-fname ((name) &body body)
  "Add plotting dir to filename"
  `(let ((fname 
	  #+sbcl(sb-ext:native-namestring (make-pathname :name ,name :defaults (pathname *plotting-dir*)))
	  #-sbcl(concatenate 'string *plotting-dir* ,name)))
     ,@body
     (when *compilep* (pdflatex-compile-view fname *viewer*))))

(defun make-random-list (size)
  "returns a list of size gaussian random numbers"
  (let ((rands nil))
    (dotimes (i size)
      (push (tikz-utils:gaussian-random) rands))
    rands))

#|
Different styles of graphs
|#
(with-fname ("test-styles.tex")
  (with-tikz-plot (tikz fname 10 5 0 10 0 12)
    (let ((x-vals (make-range 0 1 10))
	  (y-vals (make-range 0 0.5 10)))
      ;;Clip the figure area, and draw lines with different styles
      (flet ((draw-translate (shift line-style mark-style node-string)
	       (draw-graph tikz x-vals (mapcar (lambda (x) (+ shift x)) y-vals) line-style mark-style node-string)))
	(clip-and-transform (tikz)
	  (draw-translate 0.0 "blue" "fill=blue,draw=blue" 
			  (make-node-string "circle" 2 2))
	  (draw-translate 1.0 "red,thick" "fill=red" 
			  (make-node-string "circle" 3 3))
	  (draw-translate 2.0 "black,dashed" "draw=black,fill=yellow" 
			  (make-node-string "star,star points=5" 7 7))
	  (draw-translate 3.0 "green!80!black" "draw=green!80!black,fill=green" 
			  (make-node-string "rectangle" 3 3))
	  (draw-translate 4.0 "orange!80!black" "draw=orange!80!black,fill=orange" 
			  (make-node-string "diamond" 3 3))
	  (draw-translate 5.0 "black,thick" "draw=black,fill=black!20" 
			  (make-node-string "ellipse" 2 4))
	  (draw-translate 6.0 "purple!80!black,thick" "draw=purple!80!black,fill=purple!20" 
			  (make-node-string "regular polygon,regular polygon sides=5" 4 4))
	  (let ((y-vals (mapcar (lambda (x) (+ x 7.0)) y-vals)))
	    (draw-path tikz x-vals y-vals "red" nil)
	    (draw-profilepoints tikz x-vals y-vals (make-range 0.5 0 10) "draw=red,fill=red")))))
    (transform (tikz)
      ;;Draw tick marks on axis
      (draw-axis-ticks-x tikz (make-range 0 2 5) :y-shift "-0.15cm" :start "-3pt" :stop 0)
      (draw-axis-ticks-y tikz (make-range 0 2 6) :x-shift "-0.15cm" :start "-3pt" :stop 0)    
      ;;Draw a thick frame around the plot
      (draw-plottingarea-rectangle tikz))
    ;;Draw lines for the axis tick marks
    (draw-line tikz 0 -0.15 10 -0.15 "black,thick")
    (draw-line tikz -0.15 0 -0.15 5 "black,thick")))

(defun make-gaussian-histogram (min bin-size nbins mean sigma ndraws)
  "Generate a Gaussian histogram with ndraws Gaussian random numbers."
  (let ((data (make-array nbins)))
    (flet ((add (g1)
	     (let ((bin (floor (- (+ mean (* sigma g1)) min) bin-size)))
	       (and (>= bin 0) (< bin nbins) (incf (aref data bin))))))
      (dotimes (i (floor ndraws 2))
	(multiple-value-bind (g1 g2) (tikz-utils:gaussian-random)
	  (add g1) (add g2))))
    (make-histogram min bin-size (coerce data 'list))))

#|
Some Gaussian histograms
|#
(with-fname ("test-histo2.tex")
  (with-tikz-plot (tikz fname 10 5 0 10 0 150)
    (let ((histo1 (make-gaussian-histogram 0.0 0.25 40 5.0 2.0 2000))
	  (histo2 (make-gaussian-histogram 0.0 0.25 40 6.0 1.0 1300))
	  (histo3 (make-gaussian-histogram 0.0 0.25 40 7.0 1.0 900)))
      (clip-and-transform (tikz)
	;;Draw a histogram, outlined dark blue filled light blue
	(draw-histogram tikz histo1 "draw=blue!80!black,fill=blue!20" t)
	;;Transparent, red
	(draw-histogram tikz histo2 "opacity=0.5,red!80!black,fill=red!20" t)
	;;Transparent, green
	(draw-histogram tikz histo3 "opacity=0.5,draw=green!80!black,fill=green!20" t)))
    (transform (tikz)
      (draw-axis-ticks-y tikz (make-range 0 15 10))
      (draw-axis-ticks-x tikz (make-range 0 1 10) :names (make-range 0 1 10))
      (draw-plottingarea-rectangle tikz))
    (draw-legend-rectangle tikz 0.5 4.5 1 0.2 "Histogram 1" "blue!80!black,fill=blue!20" "")
    (draw-legend-rectangle tikz 0.5 4.1 1 0.2 "Histogram 2" "opacity=0.5,red!80!black,fill=red!20" "")
    (draw-legend-rectangle tikz 0.5 3.7 1 0.2 "Histogram 3" "opacity=0.5,draw=green!80!black,fill=green!20" "")))
  
#|
Datapoints of varying sizes
|#
(with-fname ("bubbles.tex")
  (with-tikz-plot (tikz fname 10 5 0 10 0 10)
    (clip-and-transform (tikz)
      (let* ((x (make-range 0 0.1 100))
	     (y (mapcar (lambda (x)  (+ x (tikz-utils:gaussian-random))) x))
	     (shapes (list "rectangle" "star" "circle"))
	     (colors (list "red" "blue" "green"))
	     (size (mapcar (lambda (x) (max 0.1 (+ 5.0 (* x 2.0)))) (make-random-list 100))))
	(mapc (lambda (x y size) (draw-node tikz x y (format nil "draw=black,fill=~a!40,opacity=0.2" (elt colors (random 3)))
					    (make-node-string (elt shapes (random 3)) size size 0 "mm"))) x y size)))
    (transform (tikz)
      ;;Print one digit after .
      (draw-axis-ticks-y tikz (make-range 0 2.0 5) :precision 1)
      (draw-axis-ticks-x tikz (make-range 0 2 5)))
    (draw-path tikz (list 0 0 10) (list 5 0 0) "thick,black")))

#|
Histogram with horizontal bins.
|#
(with-fname ("test-histo1.tex")
  (with-tikz-plot (tikz fname 10 5 0 350 0 10)
    (let ((histo (make-gaussian-histogram 0.0 1.0 10 5.0 2.0 1000)))
      (clip-and-transform (tikz)
	;;Filled horizontal histogram. Data is from a Gaussian histo, but the bins are sorted and incremented by 100.
	(draw-histogram-horizontal tikz (make-histogram (getf histo :min) (getf histo :bin-size)
							(mapcar (lambda (x) (+ x 100))
								(sort (getf histo :data) #'<)))
				   "draw=white,fill=blue!20" t t)))
    ;;Use the axis ticks function to name bins
    (draw-axis-ticks-y tikz (make-range 0.25 0.5 9)
		       :names (mapcar (lambda (x) (format nil "Thing ~a" (floor x)))
				      (make-range 1 1 10)) :numberp nil :start 0 :stop 0 :text-style "right")
    (draw-line tikz 7 -0.3 7 5.3 "gray")
    (draw-node tikz 7 5.3 "above" "" "Threshold")))

#|
Plotting some functions
|#
(with-fname ("functions.tex")
  (with-tikz-plot (tikz fname 10 5 -7 7 -1.2 1.2)
    (clip-and-transform (tikz)
      ;;Vertical grid lines, text just below horizontal axis
      (draw-axis-ticks-x tikz  (list (* -2 pi) (* -1 pi) pi (* 2 pi))
			 :names (list "$-2\\pi$" "$-\\pi$" "$\\pi$" "$2\\pi$")
			 :numberp nil :y-shift "2.5cm" :start "-2.5cm" :stop "2.5cm"
			 :style "ultra thin,gray" :text-style "black,midway,below")
      ;;Horizontal grid lines, text just left of vertical axis. Text is rised a little to prevent - sign from dissappearing
      (draw-axis-ticks-y tikz (make-range -1.0 0.5 4)
			 :precision 1 :x-shift "5cm" :start "-5cm" :stop "5cm" 
			 :style "ultra thin,gray":text-style "above=3pt,black,midway,left")
      (tikz-helper::draw-axis-cross tikz)
      (draw-function tikz #'sin 100 "red")
      (draw-function tikz #'cos 100 "blue"))
    (draw-legend-line tikz 5.5 4.8 0.6 "sin(x)" "red")
    (draw-legend-line tikz 7.5 4.8 0.6 "cos(x)" "blue")))

#|
Gaussian function, with some clipping, filling, text and more
|#
(with-fname ("gaussian-distribution.tex")
  (with-tikz-plot (tikz fname 10 5 -3.2 3.2 0 0.45)
    (let* ((x (make-range -3.2 0.05 128))
	   (y (mapcar (lambda (x) (tikz-utils:gauss x #( 1.0 0.0 1.0))) x))
	   (x-vals (append (list -3.2) x (list 3.2)))
	   (y-vals (append (list 0)  y (list 0))))
      (flet ((clip-draw (x-min x-max x-pos x-pos-height color text)
	       (clip-and-transform (tikz)
		 (scope (tikz)
		   (make-rectangle-path tikz x-min 0 x-max 5)
		   (path-stroke tikz nil nil t)
		   (draw-path tikz x-vals y-vals color t))
		 (draw-node tikz
			    x-pos (* 0.5 (tikz-utils:gauss x-pos-height #(1.0 0.0 1.0)))
			    "fill=white,opacity=0.3,draw=black, rounded corners"
			    (make-node-string "rectangle" 2 2 2) text)
		 (draw-node tikz
			    x-pos (* 0.5 (tikz-utils:gauss x-pos-height #(1.0 0.0 1.0)))
			    "text=black" (make-node-string "rectangle" 2 2 ) text))))
	(clip-draw -3.5 -2  -2.5 -1.6 "fill=red!80" "2.2\\%")
	(clip-draw   -2 -1  -1.5 -1.7 "fill=orange!60" "13.6\\%")
	(clip-draw   -1  0  -0.5 -0.5 "fill=blue!40" "34.1\\%")
	(clip-draw    0  1   0.5  0.5 "fill=blue!40" "34.1\\%")
	(clip-draw    1  2   1.5  1.7 "fill=orange!60" "13.6\\%")
	(clip-draw    2  3.5 2.5  1.6 "fill=red!80" "2.2\\%")))
    (transform (tikz)
      (draw-axis-ticks-x tikz (make-range -3 1 6)
			 :names (mapcar (lambda (x) (if (= x 0) "$\\mu$"
							(format nil "$~a\\sigma$" x)))
					(make-range -3 1 6)) :numberp nil)
      (tikz-helper::draw-axis-cross tikz))))

#|
Som Gauss smeared datapoints, with a fitted function
|#
(with-fname ("test-fitter.tex")
  (with-tikz-plot (tikz fname 10 5 0 10 0 20)
    (let* ((x-poses (make-range 0 0.5d0 20))
	   (y-poses (mapcar (lambda (x) (tikz-utils:gauss x #(90.0d0 5.0d0 2.0d0))) x-poses))
	   (smeared-y (mapcar (lambda (x) (+ x (* .5 (tikz-utils:gaussian-random)))) y-poses))
	   ;;Get the fit-parameters from the #'gauss function, with initial gueass #(10 0 1)
	   (fit-params (levmar:levmar-optimize #'tikz-utils:gauss #(10.0d0 0.0d0 1.0d0) x-poses y-poses)))
      (clip-and-transform (tikz)
	(draw-function tikz (spline:get-spline-fun (coerce x-poses 'vector) (coerce smeared-y 'vector))
		       100 "green,thick" 0 10)
	(draw-function tikz (lambda (x) (tikz-utils:gauss x fit-params)) 200 "thick,gray")
	(draw-datapoints tikz x-poses smeared-y "draw=blue,fill=blue"))
      (transform (tikz)
	(draw-axis-ticks-x tikz (make-range 0 1 10))
	(draw-axis-ticks-y tikz (make-range 0 2 10))
	(draw-plottingarea-rectangle tikz))
      (draw-legend-line tikz 0.5 4.5 1 "Noisy data" "" "draw=blue,fill=blue")
      (draw-legend-line tikz 0.5 4.1 1 "Spline fit" "draw=green,thick")
      (draw-legend-line tikz 0.5 3.7 1 "Gauss fit" "thick,gray")
      ;;Print the fit parameters to the plot.
      (draw-text-node tikz 9.5 4.5 (format nil "Fitted mean: ~5,2f" (aref fit-params 1)) "left")
      (draw-text-node tikz 9.5 4.1 (format nil "Fitted sigma: ~5,2f" (aref fit-params 2)) "left"))))
  
#|
Gaussian histogram, with a fitted function
|#
(with-fname ("test-fitter2.tex")
  (with-tikz-plot (tikz fname 10 5 0 10 0 300)
    ;;Make and draw histo
    (let ((histo (make-gaussian-histogram 0 0.5 20 5.0 1.5 2000)))
      (clip-and-transform (tikz)
	(draw-histogram tikz histo "draw=blue"))
      (let* ((x-poses (make-range 0.25 0.5 20))
	     (y-poses (getf histo :data))
	     (y-errors (map 'vector #'sqrt y-poses))
	     ;;Get the estimated parameters
	     (parameters (levmar:levmar-optimize-errors #'tikz-utils:gauss #(300.0d0 3.0d0 1.0d0)
							x-poses y-poses y-errors t)))
	(draw-text-node tikz 9.5 4.5 (format nil "Fitted mean: ~5,2f" (aref parameters 1)) "left")
	(draw-text-node tikz 9.5 4.1 (format nil "Fitted sigma: ~5,2f" (aref parameters 2)) "left")
	(clip-and-transform (tikz)
	  (draw-function tikz (lambda (x) (tikz-utils:gauss x parameters)) 200 "thick,red")))
      (transform (tikz)
	(draw-axis-ticks-x tikz (make-range 0 1 10))
	(draw-axis-ticks-y tikz (make-range 0 30 10))
	(draw-plottingarea-rectangle tikz))
      (draw-legend-line tikz 0.5 4.5 1 "2000 Gauss-rand" "draw=blue,fill=blue")
      (draw-legend-line tikz 0.5 4.1 1 "Gauss fit" "thick,red"))))

#|
Make some noisy datapoints from polynomial, fit and plot.
|#
(with-fname ("test-fitter3.tex")
  (with-tikz-plot (tikz fname 10 5 -7 7 -100 100)
    (flet ((polynomial (x params) (+ (* (aref params 0) x x x) (* (aref params 1) x x)
				     (* (aref params 2) x) (aref params 3))))
      (let* ((x-poses (make-range -10 0.5 40))
	     (y-poses (mapcar (lambda (x) (polynomial x #(0.5 -1.0d0 -2.0d0 3.0d0))) x-poses))
	     (y-errors (mapcar (lambda (x) (+ 2.0  (sqrt (abs x)))) y-poses))
	     (y-smeared (mapcar (lambda (x err) (+ x (* err (tikz-utils:gaussian-random))))
				y-poses y-errors))
	     (params (levmar:levmar-optimize-errors #'polynomial #(1.0 1.0d0 1.0d0 1.0d0)
						    x-poses y-smeared y-errors)))
	(clip-and-transform (tikz)
	  (draw-profilepoints tikz x-poses y-smeared y-errors "draw=red,fill=red")
	  (draw-function tikz (lambda (x) (polynomial x params)) 200 "blue")
	  (draw-function tikz (lambda (x) (polynomial x #(0.5 -1.0d0 -2.0d0 3.0d0)))
			 200 "green!80!black"))
	(draw-text-node tikz 5.1 1.4 "Fitted parameters: " "right")
	(draw-text-node tikz 5.1 1.0 (format nil "$~2,1fx^3 ~2,1@fx^2 ~2,1@fx ~2,1@f$"
					     (aref params 0) (aref params 1)
					     (aref params 2)	(aref params 3)) "right")))
    (transform (tikz)
      (draw-axis-ticks-x tikz (remove 0 (make-range -7 1 14)) :precision 1 :y-shift "2.5cm")
      (draw-axis-ticks-y tikz (remove 0 (make-range -100 20 10)) :precision 1 :x-shift "5.0cm")
      (tikz-helper::draw-axis-cross tikz))
    (draw-legend-line tikz 0.0 4.6 1 "$0.5x^3 - x^2 - 2x + 3$" "green!80!black")
    (draw-legend-line tikz 0.0 4.2 1 "noisy measurements" "" "draw=red,fill=red"
		      (make-node-string "circle" 3 3) "" "red" 0.2)
    (draw-legend-line tikz 0.0 3.8 1 "Fitted polynomial" "blue")))
  
#|
Qubic splines, with different end conditions.
|#
(with-fname ("spline.tex")
  (with-tikz-plot (tikz fname 10 5 4.0 6.0 4.0 7.0)
    (let ((x #(4.0d0  4.35d0 4.57d0 4.76d0 5.26d0 5.88d0))
	  (y #(4.19d0 5.77d0 6.57d0 6.23d0 4.90d0 4.77d0)))
      (draw-text-node tikz 5 5.0 "Cubic splines" "")
      (clip-and-transform (tikz)
	;;Using diamonds instead of circles for datapoints
	(draw-datapoints tikz x y "draw=black!80,fill=black!80" (make-node-string "diamond" 3 3))
	(let ((fun (spline:get-spline-fun x y)))
	  (draw-function tikz fun 100 "blue!80" 3.5d0 6.0d0))
	(let ((fun (spline:get-spline-fun x y t)))
	  (draw-function tikz fun 100 "red!80" 3.5d0 6.0d0)))
      (transform (tikz)
	(draw-axis-ticks-x tikz (make-range 4 0.2 10) :precision 1)
	(draw-axis-ticks-y tikz (make-range 4 0.5 6) :precision 1))
      (draw-path tikz (list 0 0 10) (list 5 0 0) "thick")
      (draw-legend-line tikz 5.5 4.0 1 "Not-a-knot spline" "blue!80")
      (draw-legend-line tikz 5.5 3.4 1 "Natural spline" "red!80"))))
  
#|
A function with a zoomed view of a region of interest.
|#
(with-fname ("sub-fig.tex")
  (with-tikz-plot (tikz fname 10 5 -3.0 3.0 -1.0 1.0)
    (flet ((erf-gauss (x) (+ (tikz-utils:erf x) (tikz-utils:gauss x (vector 0.001 -0.15 0.01)))))
      (transform (tikz)
	(draw-axis-ticks-x tikz (remove 0.0 (make-range -3.0 1 6)) :precision 1 :y-shift "2.5cm")
	(draw-axis-ticks-y tikz (remove 0.0 (make-range -1 0.5 4)) :precision 1 :x-shift "5.0cm")
	(tikz-helper::draw-axis-cross tikz)
	(draw-function tikz #'erf-gauss 400 "red"))
      (with-sugfigure (tikz tikz2 7.5 0.1 3 1.5 -0.2 -0.05 -0.20 -0.1)
	(region-of-interest-zoom tikz tikz2 "gray" nil t t nil)
	(transform (tikz2)
	  (draw-axis-ticks-y tikz2 (make-range -0.2 0.05 2))
	  (draw-axis-ticks-x tikz2 (make-range -0.2 0.05 3))
	  (draw-plottingarea-rectangle tikz2 t "black,fill=white"))
	(clip-and-transform (tikz2)
	  (draw-function tikz2 #'erf-gauss 100 "red"))))))

#|
Horizontal histograms, side by side.
|#
(with-fname ("sub-histo.tex")
  (with-tikz-plot (tikz fname 10 5 0.0 10.0 0.0 5.0)
    (flet ((draw-sub-histo (offset mean sigma)
	     (with-sugfigure (tikz tikz2 offset 0.0 2.0 5 0.0 1000 -6.0 6.0)
	       (let ((histo (make-gaussian-histogram -6.0 0.25 48 mean sigma 10000)))
		 (clip-and-transform (tikz2)
		   (draw-histogram-horizontal tikz2 histo "fill=blue!20,draw=blue!20" t)
		   (draw-profilepoint tikz2 (* 0.5 (reduce #'max (getf histo :data)))
				      mean sigma "red,fill=red,thick" (make-node-string "circle" 4 4))
		   (draw-plottingarea-rectangle tikz2 nil "black"))))))
      (draw-sub-histo 0.0 -2.0 2.2)
      (draw-sub-histo 2.0 -1.0 1.8)
      (draw-sub-histo 4.0  0.5 1.6)
      (draw-sub-histo 6.0  0.0 1.4)
      (draw-sub-histo 8.0  0.0 1.2)
      (with-sugfigure (tikz tikz2 0.0 0.0 2.0 5 0.0 200 -6.0 6.0)
	(transform (tikz2)
	  (draw-axis-ticks-y tikz2 (make-range -6.0 2.0 6)
			     :precision 1 :stop "10cm" :style "gray")))
      (draw-axis-ticks-x tikz (make-range 1.0 2.0 4) 
			 :names (mapcar (lambda (x) (format nil "Step ~a" x))
					(make-range 1 2 4))
			 :numberp nil))))

#|
Overlaying plots, different scales
|#
(with-fname ("bubbles2.tex")
  (with-tikz-plot (tikz fname 10 5 0 10 0 10)
    (let* ((x (make-range 0 0.1 100))
	   (y1 (mapcar (lambda (x)  (+ x (tikz-utils:gaussian-random))) x))
	   (y2 (mapcar (lambda (x)  (* 100 (- 10 (+ x (tikz-utils:gaussian-random))))) x)))
      (draw-line tikz 0 0 10 0 "thick,black")
      (draw-line tikz 0 0 0 5 "thick,blue")
      (draw-text-node tikz -0.7 2.5 "Blue axis" "rotate=90,blue") 
      (transform (tikz)
	(draw-axis-ticks-x tikz (make-range 0 1 10))
	(draw-axis-ticks-y tikz (make-range 0 1 10) :style "blue"))
      (clip-and-transform (tikz)
	(draw-datapoints tikz x y1 "draw=black,fill=blue!20" (make-node-string "circle" 4 4)))
      (with-sugfigure (tikz tikz2 0 0 10 5 0 10 0 1000)
	(draw-text-node tikz 11 2.5 "Red axis" "rotate=-90,red") 
	(draw-line tikz2 10 0 10 5 "thick,red")
	(transform (tikz2)
	  (draw-axis-ticks-y tikz (make-range 0 100 10) :x-shift "10cm" :style "red" :text-style "right"
			     :start "2pt" :stop "-2pt"))
	(clip-and-transform (tikz2)
	  (draw-datapoints tikz2 x y2 "draw=black,fill=red!20" (make-node-string "rectangle" 4 4)))))))
	
      
#|
Draw plot with log axis, explicit transformation. Also sub-ticks.
|#
(with-fname ("log-scale.tex")
  (with-tikz-plot (tikz fname 10 5 0 10 -1 2)
    (let* ((x (make-range 0.0 0.2 50))
	   (y (mapcar (lambda (x) (max 0.1 (expt 10 (+ (* 0.20 x)  (* 0.5 (tikz-utils:gaussian-random)))))) x)))
      (transform (tikz)
	(draw-plottingarea-rectangle tikz)
	(draw-axis-ticks-x tikz (make-range 0 2 5))
	(tikz::draw-axis-subticks-y tikz (mapcar (lambda (x) (log x 10)) (list 0.25 0.5 0.75 2.5 5.0 7.5 25 50 75))
				    :style "ultra thin,gray"
				    :start 0 :stop "10cm")
	(draw-axis-ticks-y tikz (mapcar (lambda (x) (log x 10)) (list 0.1 1 10 100))
			   :numberp nil :start 0 :stop "10cm"
			   :style "gray"
			   :names (mapcar (lambda (x) (format nil "$10^{~a}$" x)) (list -1 0 1 2))))
      (clip-and-transform (tikz)
	(draw-datapoints tikz x (mapcar (lambda (x) (log x 10)) y) "fill=blue!20,draw=black" (make-node-string "rectangle" 4 4))))))

