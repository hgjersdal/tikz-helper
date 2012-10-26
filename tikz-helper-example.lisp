;(ql:quickload "tikz-helper")
(use-package :tikz-helper)

(defparameter *plotting-dir* "/home/haavagj/src/tikz-helper/example/"
  "The plots produced in the code below will end up in this directory")

(defun make-random-list (size)
  "returns a list of size gaussian random numbers"
  (let ((rands nil))
    (dotimes (i size)
      (push (gaussian-random) rands))
    rands))

#|
Different styles of graphs
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-styles.tex") 10 5 0 10 0 12)
  (let ((x-vals (make-range 0 1 10))
	(y-vals (make-range 0 0.5 10)))
    (transform (tikz)
      ;;Draw tick marks on axis
      (draw-axis-ticks-x tikz (make-range 0 2 5) :y-shift "-0.15cm" :start "-3pt" :stop 0)
      (draw-axis-ticks-y tikz (make-range 0 2 6) :x-shift "-0.15cm" :start "-3pt" :stop 0))
    ;;Clip the figure area, and draw lines with different styles
    (clip-and-transform (tikz)
      (draw-graph tikz x-vals y-vals
		  "blue" "fill=blue,draw=blue")
      (draw-graph tikz x-vals (mapcar (lambda (x) (+ x 1.0)) y-vals)
		  "red,thick" "fill=red" (make-node-string "circle" 3 3))
      (draw-graph tikz x-vals (mapcar (lambda (x) (+ x 2.0)) y-vals)
		  "black,dashed" "draw=black,fill=yellow"
		  (make-node-string "star,star points=5" 7 7))
      (draw-graph tikz x-vals (mapcar (lambda (x) (+ x 3.0)) y-vals)
		  "green!80!black" "draw=green!80!black,fill=green"
		  (make-node-string "rectangle" 3 3))
      (draw-graph tikz x-vals (mapcar (lambda (x) (+ x 4.0)) y-vals)
		  "orange!80!black" "draw=orange!80!black,fill=orange"
		  (make-node-string "diamond" 3 3))
      (draw-graph tikz x-vals (mapcar (lambda (x) (+ 5.0 x )) y-vals)
		  "black,thick" "draw=black,fill=black!20"
		  (make-node-string "ellipse" 2 4))
      (draw-graph tikz x-vals (mapcar (lambda (x) (+ x 6.0)) y-vals)
		  "purple!80!black,thick" "draw=purple!80!black,fill=purple!20"
		  (make-node-string "regular polygon,regular polygon sides=5" 4 4))
      (let ((y-vals (mapcar (lambda (x) (+ x 7.0)) y-vals)))
	(draw-path tikz x-vals y-vals "red" nil)
	(draw-profilepoints tikz x-vals y-vals (make-range 0.5 0 10) "draw=red,fill=red"))))
  ;;Draw lines for the axis tick marks
  (draw-line tikz 0 -0.15 10 -0.15 "black,thick")
  (draw-line tikz -0.15 0 -0.15 5 "black,thick")
  ;;Draw a thick frame around the plot
  (draw-plottingarea-rectangle tikz))

(defun make-gaussian-histogram (min bin-size nbins mean sigma ndraws)
  "Generate a Gaussian histogram with ndraws Gaussian random numbers."
  (let ((data (make-array nbins)))
    (dotimes (i (floor ndraws 2))
      (multiple-value-bind (g1 g2) (gaussian-random)
	(let ((bin1 (floor (- (+ mean (* sigma g1)) min) bin-size))
	      (bin2 (floor (- (+ mean (* sigma g2)) min) bin-size)))
	  (and (>= bin1 0) (< bin1 nbins) (incf (aref data bin1)))
	  (and (>= bin2 0) (< bin2 nbins) (incf (aref data bin2))))))
    (make-histogram min bin-size (coerce data 'list))))

#|
Some Gaussian histograms
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-histo2.tex") 10 5 0 10 0 150)
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
    (draw-axis-ticks-x tikz (make-range 0.25 1 10) :names (make-range 0 1 9)))
  (draw-plottingarea-rectangle tikz)
  (draw-legend-rectangle tikz 0.5 4.5 1 0.2 "Histogram 1" "blue!80!black,fill=blue!20" "")
  (draw-legend-rectangle tikz 0.5 4.1 1 0.2 "Histogram 2" "opacity=0.5,red!80!black,fill=red!20" "")
  (draw-legend-rectangle tikz 0.5 3.7 1 0.2 "Histogram 3" "opacity=0.5,draw=green!80!black,fill=green!20" ""))

#|
Histogram with horizontal bins.
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-histo1.tex") 10 5 0 350 0 10)
  (let ((histo (make-gaussian-histogram 0.0 1.0 10 5.0 2.0 1000)))
    (clip-and-transform (tikz)
      ;;Filled horizontal histogram. Data is from a Gaussian histo, but the bins are sorted and incremented by 100.
      (draw-histogram-horizontal tikz (make-histogram (getf histo :min) (getf histo :bin-size)
						      (mapcar (lambda (x) (+ x 100))
							      (sort (getf histo :data) #'<)))
				 "draw=blue!20,fill=blue!10" t t)))
  ;;Use the axis ticks function to name bins
  (draw-axis-ticks-y tikz (make-range 0.25 0.5 9)
		     :names (mapcar (lambda (x) (format nil "Thing ~a" (floor x)))
				    (make-range 1 1 10)) :numberp nil :start 0 :stop 0 :text-style "right")
  (draw-line tikz 7 -0.3 7 5.3 "gray")
  (draw-node tikz 7 5.3 "above" "" "Threshold")
  (draw-path tikz (list 0 0) (list 0 5) "black" nil))

#|
Plotting some functions
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "functions.tex") 10 5 -7 7 -1.2 1.2)
  (clip-and-transform (tikz)
    ;;Vertical grid lines, text just below horizontal axis
    (draw-axis-ticks-x tikz  (list (* -2 pi) (* -1 pi) pi (* 2 pi))
		       :names (list "$-2\\pi$" "$-\\pi$" "$\\pi$" "$2\\pi$")
		       :numberp nil :y-shift "2.5cm" :start "-2.5cm" :stop "2.5cm"
		       :style "ultra thin,gray" :text-style "black,midway,below")
    ;;Horizontal grid lines, text just left of vertical axis. Text is rised a little to prevent - sign from dissappearing
    (draw-axis-ticks-y tikz (remove 0.0 (make-range -1.0 0.5 4))
		       :precision 1 :x-shift "5cm" :start "-5cm" :stop "5cm" 
		       :style "ultra thin,gray":text-style "above=3pt,black,midway,left")
    (draw-function tikz #'sin 100 "red")
    (draw-function tikz #'cos 100 "blue"))
  (draw-line tikz 5 0 5 5 "thick,->")
  (draw-line tikz 0 2.5 10 2.5 "thick,->")
  (draw-legend-line tikz 5.5 4.8 0.6 "sin(x)" "red")
  (draw-legend-line tikz 7.5 4.8 0.6 "cos(x)" "blue"))

#|
Gaussian function, with some clipping, filling, text and more
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "gaussian-distribution.tex")
		      10 5 -3.2 3.2 0 0.5)
  (let* ((x (make-range -3.2 0.05 128))
	 (y (mapcar (lambda (x) (gauss x #( 1.0 0.0 1.0))) x))
	 (x-vals (append (list -3.2) x (list 3.2)))
	 (y-vals (append (list 0)  y (list 0))))
    (flet ((clip-draw (x-min x-max x-pos x-pos-height color text)
	     (clip-and-transform (tikz)
	       (clip (tikz x-min x-max 0 5)
		 (draw-path tikz x-vals y-vals color t))
	       (draw-node tikz
			  x-pos (* 0.5 (gauss x-pos-height #(1.0 0.0 1.0)))
			  "fill=white,opacity=0.3,draw=black, rounded corners"
			  (make-node-string "rectangle" 2 2 2) text)
	       (draw-node tikz
			  x-pos (* 0.5 (gauss x-pos-height #(1.0 0.0 1.0)))
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
				      (make-range -3 1 6)) :numberp nil))
  (draw-line tikz 0.0 0 10.0 0 "thick,<->")
  (draw-line tikz 5 0 5 5 "thick,->"))

#|
Som Gauss smeared datapoints, with a fitted function
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-fitter.tex") 10 5 0 10 0 20)
  (let* ((x-poses (make-range 0 0.5d0 20))
	 (y-poses (mapcar (lambda (x) (gauss x #(90.0d0 5.0d0 2.0d0))) x-poses))
	 (smeared-y (mapcar (lambda (x) (+ x (* .5 (gaussian-random)))) y-poses))
	 ;;Get the fit-parameters from the #'gauss function, with initial gueass #(10 0 1)
	 (fit-params (levmar-optimize #'gauss #(10.0d0 0.0d0 1.0d0) x-poses y-poses)))
    (clip-and-transform (tikz)
      (draw-function tikz (get-spline-fun (coerce x-poses 'vector) (coerce smeared-y 'vector))
		     100 "green,thick" 0 10)
      (draw-function tikz (lambda (x) (gauss x fit-params)) 200 "thick,gray")
      (draw-datapoints tikz x-poses smeared-y "draw=blue,fill=blue"))
    (transform (tikz)
      (draw-axis-ticks-x tikz (make-range 0 1 10))
      (draw-axis-ticks-y tikz (make-range 0 2 10)))
    (draw-legend-line tikz 0.5 4.5 1 "Noisy data" "" "draw=blue,fill=blue")
    (draw-legend-line tikz 0.5 4.1 1 "Spline fit" "draw=green,thick")
    (draw-legend-line tikz 0.5 3.7 1 "Gauss fit" "thick,gray")
    ;;Print the fit parameters to the plot.
    (draw-text-node tikz 9.5 4.5 (format nil "Fitted mean: ~5,2f" (aref fit-params 1)) "left")
    (draw-text-node tikz 9.5 4.1 (format nil "Fitted sigma: ~5,2f" (aref fit-params 2)) "left")
    (draw-plottingarea-rectangle tikz)))

#|
Gaussian histogram, with a fitted function
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-fitter2.tex") 10 5 0 10 0 300)
  ;;Make and draw histo
  (let ((histo (make-gaussian-histogram 0 0.5 20 5.0 1.5 2000)))
    (clip-and-transform (tikz)
      (draw-histogram tikz histo "draw=blue"))
    (let* ((x-poses (make-range 0.25 0.5 20))
	   (y-poses (getf histo :data))
	   (y-errors (map 'vector #'sqrt y-poses))
	   ;;Get the estimated parameters
	   (parameters (levmar-optimize-errors #'gauss #(300.0d0 3.0d0 1.0d0)
					       x-poses y-poses y-errors t)))
      (draw-text-node tikz 9.5 4.5 (format nil "Fitted mean: ~5,2f" (aref parameters 1)) "left")
      (draw-text-node tikz 9.5 4.1 (format nil "Fitted sigma: ~5,2f" (aref parameters 2)) "left")
      (clip-and-transform (tikz)
	(draw-function tikz (lambda (x) (gauss x parameters)) 200 "thick,red")))
    (transform (tikz)
      (draw-axis-ticks-x tikz (make-range 0 1 10))
      (draw-axis-ticks-y tikz (make-range 0 30 10)))
    (draw-legend-line tikz 0.5 4.5 1 "2000 Gauss-rand" "draw=blue,fill=blue")
    (draw-legend-line tikz 0.5 4.1 1 "Gauss fit" "thick,red")
    (draw-plottingarea-rectangle tikz)))

#|
Make some noisy datapoints from polynomial, fit and plot.
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-fitter3.tex") 10 5 -7 7 -100 100)
  (flet ((polynomial (x params) (+ (* (aref params 0) x x x) (* (aref params 1) x x)
				   (* (aref params 2) x) (aref params 3))))
    (let* ((x-poses (make-range -10 0.5 40))
	   (y-poses (mapcar (lambda (x) (polynomial x #(0.5 -1.0d0 -2.0d0 3.0d0))) x-poses))
	   (y-errors (mapcar (lambda (x) (* 2.0 (+ 2.0  (sqrt (abs x))))) y-poses))
	   (y-smeared (mapcar (lambda (x err) (+ x (* err (gaussian-random))))
			      y-poses y-errors))
	   (params (levmar-optimize-errors #'polynomial #(1.0 1.0d0 1.0d0 1.0d0)
					   x-poses y-smeared y-errors)))
      (clip-and-transform (tikz)
	;;Draw noisy datapoints
	(draw-profilepoints tikz x-poses y-smeared y-errors "draw=red,fill=red")
	;;Draw the fitted polynomial
	(draw-function tikz (lambda (x) (polynomial x params)) 200 "blue")
	;;Draw the true polynomial
	(draw-function tikz (lambda (x) (polynomial x #(0.5 -1.0d0 -2.0d0 3.0d0)))
		       200 "green!80!black"))
      (draw-text-node tikz 5.1 1.4 "Fitted parameters: " "right")
      (draw-text-node tikz 5.1 1.0 (format nil "$~2,2fx^3 ~2,2@fx^2 ~2,2@fx ~2,2@f$"
					   (aref params 0) (aref params 1)
					   (aref params 2)	(aref params 3)) "right")))
  (transform (tikz)
    (draw-axis-ticks-x tikz (remove 0 (make-range -7 1 14)) :precision 1 :y-shift "2.5cm")
    (draw-axis-ticks-y tikz (remove 0 (make-range -100 20 10)) :precision 1 :x-shift "5.0cm"))
  (draw-legend-line tikz 0.0 4.6 1 "$0.5x^3 - x^2 - 2x + 3$" "green!80!black")
  (draw-legend-line tikz 0.0 4.2 1 "noisy measurements" "" "draw=red,fill=red"
		    (make-node-string "circle" 3 3) "" "red" 0.2)
  (draw-legend-line tikz 0.0 3.8 1 "Fitted polynomial" "blue")
  (draw-line tikz 0 2.5 10.2 2.5 "thick,->")
  (draw-line tikz 5 0 5 5.2 "thick,->"))

(defun decay-rate2 (A0 delta-time nbins half-life)
  "Get the number of decays per time"
  (let* ((events (make-array nbins))
	 (remaining (make-array nbins))
	 (decay-constant (/ (log 2) half-life))
	 (probability (exp (* -1 delta-time decay-constant)))
	 (N0 A0))
    (dotimes (bin nbins)
      (let ((decayed 0))
	(dotimes (n N0)
	  (when (> (random 1.0d0) probability) (incf decayed)))
	(setf (aref remaining bin) N0)
	(setf (aref events bin) decayed)
	(setf N0 (- N0 decayed))))
    (values events remaining)))

(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "half-life.tex") 10 5 0 5 0 10000)
    (flet ((intensity (x params)
	     (* (aref params 0) (expt 2 (/ (- x) (aref params 1)))))
	   (intensity2 (x params)
	     (* (aref params 0) 0.1 (/ (log 2) (aref params 1))
		(expt 2 (/ (- x) (aref params 1))))))
      (multiple-value-bind (decayed remaining) (decay-rate2 100000 0.1 50 1.0)
	(let* ((x-poses (make-range 0.05 0.1 50))
	       (params (levmar-optimize #'intensity2 #(0.0d0 1.0d0) x-poses decayed)))
	  (clip-and-transform (tikz)
	    (draw-histogram tikz (make-histogram 0.0 0.1 decayed) "draw=blue")
	    (draw-function tikz (lambda (x) (intensity2 x params)) 100 "black"))
	  (transform (tikz)
	    (draw-axis-ticks-x tikz (list 0 1 2 3 4 5) 
			       :names (list "$0$" "$T_{1/2}$" "$2T_{1/2}$" "$3T_{1/2}$" "$4T_{1/2}$" "$5T_{1/2}$") 
			       :numberp nil)
	    (draw-axis-ticks-y tikz (make-range 0 1000 10)))
	  (with-sugfigure (tikz tikz2 0 0 10 5 0 5 0 100000)
	    (clip-and-transform (tikz2)
	      (draw-histogram tikz2 (make-histogram -0.05 0.1 remaining) "draw=red")
	      (draw-function tikz2 (lambda (x) (intensity x params)) 100 "black"))
	    (transform (tikz2)
	      (let ((y-vals (list 100000 50000 25000 12500 6250 3125)))
		(mapcar (lambda (x y) 
			  (draw-path tikz2 (list x x 5) (list 0 y y) "gray"))
			(list 0 1 2 3 4) y-vals)
		(draw-axis-ticks-y tikz2 y-vals
				   :names (list "A0=100000" "A0/2" "A0/4" "A0/8" "A0/16") :numberp nil :x-shift "10cm" 
				   :style "gray" :text-style "right"))))
	  (draw-text-node tikz 0 5.5 "Decays:$-\\Delta N = \\lambda N \\Delta t$" "right,blue")
	  (draw-text-node tikz 10 5.5 "Remaining:$N(t) = A_0 2^{t/T_{1/2}}$" "left,red")
	  (draw-line tikz 0 0 0 5.2 "blue,thick,->")
	  (draw-line tikz 10 0 10 5.2 "red,thick,->")
	  (draw-line tikz 0 0 10.2 0 "black,thick,->")
	  (draw-legend-line tikz 3.0 3.4 1.0 "Decays" "blue")
	  (draw-text-node tikz 5.8 3.35 "$T_{1/2}$: 1.00, $A_0$: 100000" "right")
	  (draw-legend-line tikz 3.0 3.0 1.0 "Remaining" "red")
	  (draw-text-node tikz 5.8 2.95 "$T_{1/2}$: 1.00, $A_0$: 100000" "right")
	  (draw-legend-line tikz 3.0 2.6 1.0 "Estimated" "black")
	  (draw-text-node tikz 5.8 2.55 (format nil "$T_{1/2}$: ~5,2f, $A_0$: ~5,0f" 
						(aref params 1) (aref params 0)) "right")))))

;;(with-tikz-plot-standalone (tikz (concatenate 'string *plotting-dir* "spline.tex") 10 5 4.0 6.0 4.0 7.0 t)
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "spline.tex") 10 5 4.0 6.0 4.0 7.0)
  (let ((x #(4.0d0  4.35d0 4.57d0 4.76d0 5.26d0 5.88d0))
	(y #(4.19d0 5.77d0 6.57d0 6.23d0 4.90d0 4.77d0)))
    (transform (tikz)
      (draw-axis-ticks-x tikz (make-range 4 0.2 10) :precision 1)
      (draw-axis-ticks-y tikz (make-range 4 0.5 6) :precision 1))
    (draw-text-node tikz 5 5.0 "Cubic splines" "")
    (clip-and-transform (tikz)
      ;;Using diamonds instead of circles for datapoints
      (draw-datapoints tikz x y "draw=black!80,fill=black!80" (make-node-string "diamond" 3 3))
      (let ((fun (get-spline-fun x y)))
	(draw-function tikz fun 100 "blue!80" 3.5d0 6.0d0))
      (let ((fun (get-spline-fun x y t)))
	(draw-function tikz fun 100 "red!80" 3.5d0 6.0d0)))
    (draw-legend-line tikz 5.5 4.0 1 "Not-a-knot spline" "blue!80")
    (draw-legend-line tikz 5.5 3.4 1 "Natural spline" "red!80")
    (draw-line tikz 0 0 10.2 0 "thick,->")
    (draw-line tikz 0 0 0 5.2 "thick,->")))

(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "sub-fig.tex") 10 5 -3.0 3.0 -1.0 1.0)
  (flet ((erf-gauss (x) (+ (erf x) (gauss x (vector 0.001 -0.15 0.01)))))
    (draw-line tikz 0 2.5 10.3 2.5 "thick,black,->")
    (draw-line tikz 5 0 5 5.3  "thick,black,->")
    (transform (tikz)
      (draw-axis-ticks-x tikz (remove 0.0 (make-range -3.0 1 6)) :precision 1 :y-shift "2.5cm")
      (draw-axis-ticks-y tikz (remove 0.0 (make-range -1 0.5 4)) :precision 1 :x-shift "5.0cm")
      (draw-function tikz #'erf-gauss 400 "red"))
    (with-sugfigure (tikz tikz2 7.5 0.1 3 1.5 -0.2 -0.05 -0.20 -0.1)
      (connect-plots tikz tikz2 "gray" nil t t nil)
      (draw-plottingarea-rectangle tikz2 t)
      (clip-and-transform (tikz2)
	(draw-function tikz2 #'erf-gauss 100 "red"))
      (transform (tikz2)
	(draw-axis-ticks-y tikz2 (make-range -0.2 0.05 2))
	(draw-axis-ticks-x tikz2 (make-range -0.2 0.05 3))))))

(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "sub-histo.tex") 10 5 0.0 10.0 0.0 5.0)
  (flet ((draw-sub-histo (offset mean sigma)
	   (with-sugfigure (tikz tikz2 offset 0.0 2.0 5 0.0 1000 -6.0 6.0)
	     (let ((histo (make-gaussian-histogram -6.0 0.25 48 mean sigma 10000)))
	       (draw-plottingarea-rectangle tikz2 nil "black")
	       (clip-and-transform (tikz2)
		 (draw-histogram-horizontal tikz2 histo "fill=blue!20,draw=blue!20" t)
		 (draw-profilepoint tikz2 (* 0.5 (reduce #'max (getf histo :data)))
				    mean sigma "red,fill=red,thick" (make-node-string "circle" 4 4)))))))
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
		       :numberp nil)))
