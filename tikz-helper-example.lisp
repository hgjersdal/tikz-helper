;; Install and run using asdf:
;; (pushnew "/path-to-tikz/tikz-helper/" asdf:*central-registry* :test #'equal)
;; (asdf:operate 'asdf:load-op 'tikz-helper)
;; (asdf:operate 'asdf:load-op 'tikz-levmar)
;; (asdf:operate 'asdf:load-op 'tikz-utils)
;; Or, if quicklisp is installed:
;; (pushnew "/path-to-tikz/tikz-helper/" asdf:*central-registry* :test #'equal)
;; (ql:quickload 'tikz-helper)
;; (ql:quickload 'tikz-levmar)
;; (ql:quickload 'tikz-utils)

(use-package :tikz-helper)

(defparameter *plotting-dir* "/home/haavagj/src/tikz-helper/example/"
  "The plots produced in the code below will end up in this directory")
(defparameter *compilep* t "The plots will be compiled with pdflatex in path, and viewed with *viewer*")
(defparameter *viewer* "emacsclient" "A program to view the resulting pdf file.")

(defmacro with-example-plot ((name plot-x-min plot-x-max plot-y-min plot-y-max) &body body)
  "A macro wrapping the with-tikz-plot macro for the example plots. It makes a plot with name in the 
*plotting-dir* directory, with a width of 10cm, a height of 5 cm. If *compilep* is T, the produced file is
compiled with pdflatex, the reults are viewed with *viewer*."
  (let ((fname (gensym)))
    `(let ((,fname 
	    #+sbcl(sb-ext:native-namestring (make-pathname :name ,name :defaults (pathname *plotting-dir*)))
	    #-sbcl(concatenate 'string *plotting-dir* ,name)))
       (with-tikz-plot (tikz ,fname 10 5 ,plot-x-min ,plot-x-max ,plot-y-min ,plot-y-max)
	 ,@body)
       (when *compilep* (pdflatex-compile-view ,fname *viewer*)))))

(defun make-random-list (size)
  "returns a list of size gaussian random numbers"
  (let ((rands nil))
    (dotimes (i size) (push (tut:gaussian-random) rands))
    rands))

(with-example-plot ("transform-and-clip.tex" -5 5 -3 2)
  (flet ((draw-stuff ()
	   (make-path tikz (list -5 5) (list -2.5 2.5))
	   (make-path tikz (list -5 5) (list 2.5 -2.5))
	   (path-stroke tikz)
	   (draw-node tikz 0 2 "draw=black,fill=yellow" (make-node-string "star" 1 1 0 "cm"))))
    (draw-stuff)
    (clip-and-transform (tikz) (draw-stuff))
    (draw-axis-rectangle tikz)))

#|
Different styles of graphs
|#
(with-example-plot ("test-styles.tex" 0 10 0 12)
  (let ((x-vals (make-range 0 1 11))
	(y-vals (make-range 0 0.5 11)))
    ;;Function that translates y-vals, and draws datapoints connected with a line.
    (flet ((draw-translate (shift line-style mark-style node-name size-x size-y)
	     (draw-graph tikz x-vals (mapcar (lambda (x) (+ shift x)) y-vals)
			 line-style mark-style (make-node-string node-name size-x size-y))))
      ;;Clip the figure area, and draw lines with different styles
      (draw-translate 0.0 "blue" "fill=blue,draw=blue" "circle" 2 2)
      (draw-translate 1.0 "red,thick" "fill=red" "circle" 3 3)
      (draw-translate 2.0 "black,dashed" "draw=black,fill=yellow" "star,star points=5" 7 3)
      (draw-translate 3.0 "green!80!black" "draw=green!80!black,fill=green" "rectangle" 3 3)
      (draw-translate 4.0 "orange!80!black" "draw=orange!80!black,fill=orange" "diamond" 3 3)
      (draw-translate 5.0 "black,thick" "draw=black,fill=black!20" "ellipse" 2 4)
      (draw-translate 6.0 "purple!80!black,thick" "draw=purple!80!black,fill=purple!20" 
		      "regular polygon,regular polygon sides=5" 4 4)
      ;;Data points with error bars
      (let ((y-vals (mapcar (lambda (x) (+ x 7.0)) y-vals)))
	;; The transform macro adds tikz code to transform data points within a scope
	;; draw-path does not autoimatically transform, since it is often needed in the cm frame.
	(transform (tikz) (draw-path tikz x-vals y-vals "red" nil))
	(draw-profilepoints tikz x-vals y-vals (make-range 0.5 0 11) "draw=red,fill=red")))
    ;;Draw a rectangle and axes with ticks.
    (draw-axis-rectangle tikz)))

(defun make-gaussian-histogram (min bin-size nbins mean sigma ndraws)
  "Generate a Gaussian histogram with ndraws Gaussian random numbers."
  (let ((data (make-array nbins)))
    (flet ((add (g1)
	     (let ((bin (floor (- (+ mean (* sigma g1)) min) bin-size)))
	       (and (>= bin 0) (< bin nbins) (incf (aref data bin))))))
      (dotimes (i (floor ndraws 2))
	(multiple-value-bind (g1 g2) (tut:gaussian-random)
	  (add g1) (add g2))))
    (make-histogram min bin-size (coerce data 'list))))

#|
Some Gaussian histograms
|#
(let ((histo1 (make-gaussian-histogram 0.0 0.25 40 5.0 2.0 2500))
      (histo2 (make-gaussian-histogram 0.0 0.25 40 5.0 2.0 1700))
      (histo3 (make-gaussian-histogram 0.0 0.25 40 5.0 2.0 1000)))
  (with-example-plot ("test-histo2.tex" 0 10 0 150)
    ;;Draw the histograms with different styles
    (draw-histogram tikz histo1 "draw=gray,fill=blue!50" t)
    (draw-histogram tikz histo2 "red,ultra thick" nil)
    (draw-histogram tikz histo3 "draw=blue!50,fill=green,ultra thick" t t)
    (draw-axis-rectangle tikz)
    ;;Legend entries for the histograms
    (draw-legend-entry tikz 0.5 4.5 "Histogram 1" :mark-style "gray,fill=blue!50" :histogram-node-p t)
    (draw-legend-entry tikz 0.5 4.1 "Histogram 2" :line-style "red,ultra thick")
    (draw-legend-entry tikz 0.5 3.7 "Histogram 3" :mark-style "white,fill=green" :histogram-node-p t)))

#|
Datapoints of varying sizes,shapes and colors
|#
(let* ((x (make-range 0 0.1 100))
       (y (mapcar (lambda (x)  (+ x (tut:gaussian-random))) x))
       (shapes (list "rectangle" "star" "circle"))
       (colors (list "red" "blue" "green"))
       (size (mapcar (lambda (x) (max 0.1 (+ 5.0 (* x 2.0)))) (make-random-list 100))))
  (with-example-plot ("bubbles.tex" 0 10 0 10)
    ;; The clip-and-transform macro clips the plottingarea, and adds tikz code to transform data points within a scope
    ;; Draw node does not autoimatically transform, since it is often needed in the cm frame.
    (clip-and-transform (tikz)
      (mapc (lambda (x y size) (draw-node tikz x y (format nil "draw=black,fill=~a!40,opacity=0.5" (elt colors (random 3)))
					  (make-node-string (elt shapes (random 3)) size size 0 "mm"))) x y size))
    (draw-axis-popped-out tikz)))

#|
Histogram with horizontal bins.
|#
(with-example-plot ("test-histo1.tex" 0 350 0 10)
  (let ((histo (make-gaussian-histogram 0.0 1.0 10 5.0 2.0 1000)))
    ;;Filled horizontal histogram. Data is from a Gaussian histo, but the bins are sorted and incremented by 100.
    (draw-histogram-horizontal tikz (make-histogram (getf histo :min) (getf histo :bin-size)
						    (mapcar (lambda (x) (+ x 100))
							    (sort (getf histo :data) #'<)))
			       "draw=white,fill=blue!20" t t))
  ;;Use the axis ticks function to name bins
  (draw-axis-ticks-y tikz (make-range 0.5 1 10)
		     :names (mapcar (lambda (x) (format nil "Thing ~a" (floor x))) (make-range 1 1 11))
		     :numberp nil :start 0 :stop 0 :text-style "right")
  ;;Draw a line and som text
  (draw-line tikz 7 -0.3 7 5.3 "gray")
  (draw-node tikz 7 5.3 "above" "" "Threshold"))

#|
Plotting sin(x) and cos(x).
|#
(with-example-plot ("functions.tex" -7 7 -1.2 1.2)
  ;;Names axis, so x-axis is not drawn automatically, but manually
  (draw-axis-cross tikz :x-list nil :y-list nil)
  ;;Grid lines ar specific values in the x-direction, automatic in the y-direct ion
  (draw-grid-lines tikz :x-list (list (* -2 pi) (* -1 pi) pi (* 2 pi)))
  ;;Draw a rectangle around the plorringarea, with no axes.
  (draw-axis-rectangle tikz :x-list nil)
  (draw-axis-ticks-x tikz  (list (* -2 pi) (* -1 pi) 0 pi (* 2 pi))
		     :names (list "$-2\\pi$" "$-\\pi$" "0.0" "$\\pi$" "$2\\pi$")
		     :numberp nil)
  (draw-function tikz #'sin 100 "red")
  (draw-function tikz #'cos 100 "blue")
  (draw-legend-entry tikz 0.5 5.2 "sin(x)" :line-style "red")
  (draw-legend-entry tikz 2.5 5.2 "cos(x)" :line-style "blue"))

#|
Gaussian function, with some clipping, filling and text boxes
|#
(with-example-plot ("gaussian-distribution.tex" -3.2 3.2 0 0.45)
  ;;A Gaussian ditribution path
  (let* ((x (make-range -3.2 0.05 128))
	 (y (mapcar (lambda (x) (tut:gauss x #( 1.0 0.0 1.0))) x))
	 (x-vals (append (list -3.2) x (list 3.2)))
	 (y-vals (append (list 0)  y (list 0))))
    (flet ((clip-draw (x-min x-max x-pos x-pos-height color text)
	     ;;The clip area is active vithin scope.
	     (clip-and-transform (tikz)
	       (scope (tikz)
		 ;;Clip a rectangle
		 (make-rectangle-path tikz x-min 0 x-max 5)
		 (path-stroke tikz nil nil t)
		 ;;Draw and fill the entire Gaussian distribution path,
		 (draw-path tikz x-vals y-vals color t))
	       ;;A text node with an opaque background box.
	       (draw-node tikz x-pos (* 0.5 (tut:gauss x-pos-height #(1.0 0.0 1.0)))
			  "fill=white,opacity=0.3,draw=black, rounded corners"
			  (make-node-string "rectangle" 2 2 2) text)
	       (draw-node tikz x-pos (* 0.5 (tut:gauss x-pos-height #(1.0 0.0 1.0)))
			  "text=black" (make-node-string "rectangle" 2 2 ) text))))
      ;;Fill different regions with different colors.
      (clip-draw -3.5 -2  -2.5 -1.6 "fill=red!80" "2.2\\%")
      (clip-draw   -2 -1  -1.5 -1.7 "fill=orange!60" "13.6\\%")
      (clip-draw   -1  0  -0.5 -0.5 "fill=blue!40" "34.1\\%")
      (clip-draw    0  1   0.5  0.5 "fill=blue!40" "34.1\\%")
      (clip-draw    1  2   1.5  1.7 "fill=orange!60" "13.6\\%")
      (clip-draw    2  3.5 2.5  1.6 "fill=red!80" "2.2\\%")))
  (draw-axis-ticks-x tikz (make-range -3 1 7)
		     :names (list "$-3\\sigma$" "$-2\\sigma$" "$-\\sigma$" 
				  "$\\mu$" "$\\sigma$" "$2\\sigma$" "$3\\sigma$")
		     :numberp nil)
  (draw-axis-cross tikz :x-list nil :y-list nil))

#|
Som Gauss smeared datapoints, with a fitted function
|#
(with-example-plot ("test-fitter.tex" 0 10 0 20)
  (let* ((x-poses (make-range 0 0.25d0 41))
	 (y-poses (mapcar (lambda (x) (tut:gauss x #(90.0d0 5.0d0 2.0d0))) x-poses))
	 (smeared-y (mapcar (lambda (x) (+ x (tut:gaussian-random))) y-poses))
	 ;;Get the fit-parameters from the #'gauss function, with initial gueass #(10 0 1)
	 (fit-params (levmar:levmar-optimize #'tut:gauss #(10.0d0 0.0d0 1.0d0) x-poses y-poses)))
    (draw-function tikz (spline:get-spline-fun (coerce x-poses 'vector) (coerce smeared-y 'vector))
		   100 "green,thick" 0 10)
    (draw-function tikz (lambda (x) (tut:gauss x fit-params)) 200 "thick,gray")
    (draw-datapoints tikz x-poses smeared-y "draw=blue,fill=blue")
    (draw-axis-rectangle tikz)
    (draw-legend-entry tikz 0.5 4.5 "Noisy data" :mark-style "draw=blue,fill=blue")
    (draw-legend-entry tikz 0.5 4.1 "Spline fit" :line-style "draw=green,thick")
    (draw-legend-entry tikz 0.5 3.7 "Gauss fit" :line-style "thick,gray")
    ;;Print the fit parameters to the plot, in the cm frame
    (draw-node tikz 9.5 4.5 "left" "" (format nil "Fitted mean: ~5,2f" (aref fit-params 1)))
    (draw-node tikz 9.5 4.1 "left" "" (format nil "Fitted mean: ~5,2f" (aref fit-params 2)))))

#|
Gaussian histogram, with a fitted function
|#
(with-example-plot ("test-fitter2.tex" 0 10 0 300)
  (let* ((histo (make-gaussian-histogram 0 0.5 20 5.0 1.5 2000))
	 (x-poses (make-range 0.25 0.5 21))
	 (y-poses (getf histo :data))
	 (y-errors (map 'vector #'sqrt y-poses))
	 (parameters (levmar:levmar-optimize-errors #'tut:gauss #(300.0d0 3.0d0 1.0d0)
						    x-poses y-poses y-errors t)))
    (draw-histogram tikz histo "draw=blue!10,fill=blue!20" t t)
    (draw-profilepoints tikz x-poses y-poses (coerce y-errors 'list) "draw=gray,fill=gray" (make-node-string "rectangle" 3 0))
    (draw-function tikz (lambda (x) (tut:gauss x parameters)) 200 "thick,red")
    (draw-legend-entry tikz 0.5 3.7 "Data" :mark-style "draw=blue!10,fill=blue!20" :histogram-node-p t)
    (draw-legend-entry tikz 0.5 4.1 "Gauss fit" :line-style "thick,red")
    (draw-node tikz  9.5 4.1 "left" "" (format nil "Fitted mean: ~5,1f" (aref parameters 1)))
    (draw-node tikz  9.5 3.7 "left" "" (format nil "Fitted sigma: ~5,1f" (aref parameters 2))))
  (draw-axis-rectangle tikz))


(defun polynomial (x params) 
  "Polynomial of degree (- (length params) 1)"
  (reduce #'+ (map 'vector (lambda (param degree) 
			     (* param (expt x degree))) params 
			     (make-range (- (length params) 1) -1 (length params)))))

#|
Make some noisy datapoints from polynomial, fit and plot.
|#
(with-example-plot ("test-fitter3.tex" -7 7 -100 100)
  (let* ((x-poses (make-range -10 0.5 41))
	 (y-poses (mapcar (lambda (x) (polynomial x #(0.5 -1.0d0 -2.0d0 3.0d0))) x-poses))
	 (y-errors (mapcar (lambda (x) (+ 2.0  (sqrt (abs x)))) y-poses))
	 (y-smeared (mapcar (lambda (x err) (+ x (* err (tut:gaussian-random)))) y-poses y-errors))
	 (params (levmar:levmar-optimize-errors #'polynomial #(1.0d0 1.0d0 1.0d0 1.0d0)
						x-poses y-smeared y-errors)))
    (draw-profilepoints tikz x-poses y-smeared y-errors "draw=red,fill=red")
    (draw-function tikz (lambda (x) (polynomial x params)) 200 "blue")
    (draw-function tikz (lambda (x) (polynomial x #(0.5 -1 -2 3)))
		   200 "green!80!black")
    (draw-node tikz 5.1 1.4 "right" "" "Fitted parameters: ")
    (draw-node tikz 5.1 1.0 "right" "" (format nil "~{$~3,1fx^3 ~3,1@fx^2 ~3,1@fx ~3,1@f$~}"
					       (coerce params 'list))))
  (draw-axis-cross tikz :y-ticks-max 8 :y-ticks-min 4)
  (draw-legend-entry tikz 0.0 4.6 "$0.5x^3 - x^2 - 2x + 3$" :line-style "green!80!black")
  (draw-legend-entry tikz 0.0 4.2 "Noisy measurements" :line-style "draw=red,fill=red"
		    :error-style "red" :error-height 0.2)
  (draw-legend-entry tikz 0.0 3.8 "Fitted polynomial" :line-style "blue"))

#|
Qubic splines, with different end conditions.
|#
(with-example-plot ("spline.tex" 4.0 6.0 4.0 7.0)
  (let ((x #(4.0d0  4.35d0 4.57d0 4.76d0 5.26d0 5.88d0))
	(y #(4.19d0 5.77d0 6.57d0 6.23d0 4.90d0 4.77d0)))
    (draw-node tikz 5 5.0 "" "" "Cubic splines")
    (draw-datapoints tikz x y "draw=black!80,fill=black!80" (make-node-string "diamond" 3 3))
    (let ((fun (spline:get-spline-fun x y)))
      (draw-function tikz fun 100 "blue!80" 3.5d0 6.0d0))
    (let ((fun (spline:get-spline-fun x y t)))
      (draw-function tikz fun 100 "red!80" 3.5d0 6.0d0))
    (draw-axis-left-bottom tikz)
    (draw-legend-entry tikz 5.5 4.0 "Not-a-knot spline" :line-style "blue!80")
    (draw-legend-entry tikz 5.5 3.4 "Natural spline" :line-style "red!80")))

#|
A function with a zoomed view of a region of interest.
|#
(with-example-plot ("sub-fig.tex" -3.0 3.0 -1.0 1.0)
  (flet ((erf-gauss (x) (+ (tut:erf x) (tut:gauss x (vector 0.001 -0.15 0.01)))))
    (draw-axis-cross tikz :y-ticks-max 6)
    (draw-function tikz #'erf-gauss 400 "red")
    (with-subfigure (tikz tikz2 7.5 0.1 3 1.5 -0.2 -0.05 -0.20 -0.1)
      (region-of-interest-zoom tikz tikz2 "gray" nil t t nil)
      (draw-axis-rectangle tikz2 :fill t  :x-ticks-min 2 :x-ticks-max 4
			   :y-ticks-min 2 :y-ticks-max 4)
      (draw-function tikz2 #'erf-gauss 100 "red"))))

#|
Horizontal histograms, side by side.
|#
(with-example-plot ("sub-histo.tex" 0.0 10.0 0.0 5.0)
  (flet ((draw-sub-histo (offset mean sigma)
	   (with-subfigure (tikz tikz2 offset 0.0 2.0 5 0.0 1000 -6.0 6.0)
	     (let ((histo (make-gaussian-histogram -6.0 0.25 48 mean sigma 10000)))
	       (draw-histogram-horizontal tikz2 histo "fill=blue!20,draw=blue!20" t)
	       (draw-profilepoint tikz2 (* 0.5 (reduce #'max (getf histo :data)))
				  mean sigma "red,fill=red,thick" (make-node-string "circle" 4 4))
	       (draw-axis-rectangle tikz2 :x-list nil :y-list nil)))))
    (draw-sub-histo 0.0 -2.0 2.2)
    (draw-sub-histo 2.0 -1.0 1.8)
    (draw-sub-histo 4.0  0.5 1.6)
    (draw-sub-histo 6.0 -0.2 1.4)
    (draw-sub-histo 8.0  0.0 1.2)
    (with-subfigure (tikz tikz2 0.0 0.0 2.0 5 0.0 200 -6.0 6.0)
      (draw-axis-ticks-y tikz2 (make-range -6.0 2.0 7)
			 :precision 1 :stop "10cm" :style "gray"))
    (draw-axis-ticks-x tikz (make-range 1.0 2.0 5) 
		       :names (mapcar (lambda (x) (format nil "Step ~a" x))
				      (make-range 1 2 5))
		       :numberp nil)))

#|
Overlaying plots, different scales
|#
(with-example-plot ("bubbles2.tex" 0 10 0 10)
  (let* ((x (make-range 0 0.1 101))
	 (y1 (mapcar (lambda (x)  (+ x (tut:gaussian-random))) x))
	 (y2 (mapcar (lambda (x)  (* 100 (- 10 (+ x (tut:gaussian-random))))) x)))
    (draw-line tikz 0 0 10 0 "thick,black")
    (draw-line tikz 0 0 0 5 "thick,blue")
    (draw-node tikz -0.7 2.5 "rotate=90,blue" ""  "Blue axis") 
    (draw-axis-ticks-x tikz (make-range 0 1 11))
    (draw-axis-ticks-y tikz (make-range 0 1 11) :style "blue")
    (draw-datapoints tikz x y1 "draw=black,fill=blue" (make-node-string "circle" 4 4))
    (with-subfigure (tikz tikz2 0 0 10 5 0 10 0 1000)
      (draw-line tikz2 10 0 10 5 "thick,red")
      (draw-node tikz2 11 2.5 "rotate=-90,red" ""  "Red axis") 
      (draw-axis-ticks-y tikz2 (make-range 0 100 11) :x-shift "10cm" :style "red" :text-style "right"
			 :start "2pt" :stop "-2pt")
      (draw-datapoints tikz2 x y2 "draw=black,fill=red" (make-node-string "rectangle" 4 4)))
    (draw-line tikz 0 5 10 5 "thick,black")
    (draw-legend-entry tikz 5.5 5.3 "Red data"  :mark-style "fill=red,draw=black" 
		       :node-string (make-node-string "rectangle" 4 4))
    (draw-legend-entry tikz 2.5 5.3 "Blue data" :mark-style "fill=blue,draw=black"
		       :node-string (make-node-string "circle" 4 4))))

#|
Draw plot with log axis, explicit transformation. Also sub-ticks.
|#
(with-example-plot ("log-scale.tex" 0 10 -0.5 2)
  (flet ((expt-10 (x params) (+ (aref params 0) (expt 10 (* (aref params 1) x)))))
    (let* ((x (make-range 0.0 0.1 101))
	   (err (mapcar (lambda (x) (+ (* (expt 10 (* 0.2 x)) 0.4 (tut:gaussian-random)))) x))
	   (y (mapcar (lambda (x err) (max 0.01 (+ (expt 10 (* 0.20 x)) err))) x err))
	   (params (levmar:levmar-optimize-errors #'expt-10 #(1.0 0.4) x y err nil)))
      (draw-axis-rectangle tikz :y-list nil)
      (draw-axis-subticks-y tikz (mapcar (lambda (x) (log x 10)) (list 0.5 0.75 2.5 5.0 7.5 25 50 75)) 
			    :style "ultra thin,gray" :start 0 :stop "10cm")
      (draw-axis-ticks-y tikz (mapcar (lambda (x) (log x 10)) (list 1 10 100))
			 :numberp nil :start 0 :stop "10cm" :style "gray" :text-style "black,left"
			 :names (mapcar (lambda (x) (format nil "$10^{~a}$" x)) (list 0 1 2)))
      (draw-function tikz (lambda (x) (log (expt-10 x params) 10)) 100 "blue,thick")
      (draw-datapoints tikz x (mapcar (lambda (x) (log x 10)) y) "fill=red,draw=red!20!black" (make-node-string "circle" 4 4))
      (draw-legend-entry tikz 0.5 5.3 (format nil "~{$y(x) = ~2,2f + 10^{~3,1fx}$~}" (coerce params 'list)) :line-style "blue,thick"))))

(defun make-2d-histo ()
  "Make and fill a histogram"
  (let* ((nbins 25)
	 (histo (make-histogram2d -2.5 0.2 nbins -2.5 0.2 nbins)))
    (dotimes (i 100000)
      (multiple-value-bind (g1 g2) (tut:gaussian-random)
	(histo2d-incf histo (- (* g1 0.3) -1.0)  (- (random 5.0) 2.5))
	(histo2d-incf histo (- (random 5.0) 2.5) (- (* g2 0.3) 1.0))))
    (dotimes (i 100000)
      (multiple-value-bind (g1 g2) (tut:gaussian-random)
	(histo2d-incf histo (+ (* g1 0.3) -1.0)  (- (random 5.0) 2.5))
	(histo2d-incf histo (- (random 5.0) 2.5) (+ (* g2 0.3) 1.0))))
    histo))

#|
Tree ways of plotting 2D histograms, filled rectangles, 
filled rectangles with contour lines, contour plot
|#
(let* ((histo (make-2d-histo)))
  (with-example-plot ("histo-rect.tex" -2.5 2.5 -2.5 2.5)
    (draw-histo2d-rectangles tikz histo 0 (histo2d-get-max histo))
    (draw-axis-rectangle tikz)
    (tikz::color-palette tikz 10.2 0 0.5 5.0 0 (* 0.9 (histo2d-get-max histo)))))

(let* ((histo (make-2d-histo)))
  (with-example-plot ("histo-rect-cont.tex" -2.5 2.5 -2.5 2.5)
    (draw-histo2d-rectangles tikz histo 0 (histo2d-get-max histo))
    (draw-histo2d-contour tikz histo 0 (* 0.9 (histo2d-get-max histo)) 15 nil)
    (draw-axis-rectangle tikz)
    (tikz::color-palette tikz 10.2 0 0.5 5.0 0 (* 0.9 (histo2d-get-max histo)))))

(with-example-plot ("histo-cont.tex" -2.5 2.5 -2.5 2.5)
  (let* ((histo (make-2d-histo)))
    (draw-histo2d-contour tikz histo 0 (* 0.9 (histo2d-get-max histo)) 10 t)
    (draw-axis-rectangle tikz)
    (tikz::color-palette tikz 10.2 0 0.5 5.0 0 (* 0.9 (histo2d-get-max histo)))))

(let* ((histo (make-histogram2d 0 (/ 22 20) 20 0 (/ 16 20) 20))
       (tikz::*colors* (list "red" "white" "blue")))
  (dotimes (i 100000)
    (multiple-value-bind (g1 g2) (tut:gaussian-random)
      (histo2d-incf histo (+ 8 (* g1 1.3)) (random 16.0))
      (histo2d-incf histo (random 22.0) (+ (* g2 1.3) 8.0))))
  (with-example-plot ("histo-cont2.tex" 0 22 0 16)
    (draw-histo2d-contour tikz histo 0 (/ (histo2d-get-max histo) 2.6) 2 t)
    (draw-axis-popped-out tikz :x-list (list 0 6 7 9 10 22)
			  :y-list (list 0 6 7 9 10 16))
    (tikz::color-palette tikz 10.2 0 0.5 5.0 0 (/ (histo2d-get-max histo) 2.6))))

;; (with-example-plot ("coffe1.tex" 0 10 0 1)
;;   (slot-value

