;(ql:quickload "tikz-helper")
(use-package :tikz-helper)

(defparameter *plotting-dir* "/home/haavagj/src/tikz-helper/example/" "The plots produced in the code below will end up in this directory")

(defun make-random-list (size)
  "returns a list of size gaussian random numbers"
  (let ((rands nil))
    (dotimes (i size)
      (push (gaussian-random) rands))
    rands))

#|
Add a simple plot of some gauss smeared measurements around dotted lines.
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-graph.tex") 10 5 0 10 0 20) 
  ;;Clip the figure area
  (clip (tikz)
    ;;Draw data points with red errorbars
    (draw-profilepoints tikz  (make-range 0 1 10) (mapcar #'- (make-range 20 -2 10) (make-random-list 11)) (make-range 1.0 0 10) "draw=red,fill=red")
    ;;Draw 10 datapoints, and filled blue cirlces at each data point.
    (draw-datapoints tikz (make-range 0 1 10) (mapcar #'+ (make-range 0 2 10) (make-random-list 11)) "draw=blue,fill=blue")
    ;;Add legend entries
    (draw-legend-line tikz 0.5 3.0 0.5 "Expected" "thick,dotted")
    (draw-legend-line tikz 0.5 2.6 0.5 "Graph" "" "blue,fill=blue")
    (draw-legend-line tikz 0.5 2.2 0.5 "Profile" "" "" "" "" "red, fill=red" 0.1))
  ;;Axis markings in x and y. 
  (draw-axis-ticks-x-transformed tikz (make-range 0 1 10) 1) 
  (draw-axis-ticks-y-transformed tikz (make-range 0 2 10) 1)
  ;;Dotted lines representing the expected values
  (draw-line tikz 0 0 10 5 "thick,dotted")
  (draw-line tikz 0 5 10 0 "thick,dotted")
  ;;Draw a thick rectangle around the figure
  (draw-plottingarea-rectangle tikz))

#|
Add a simple histogram. Uniform with Gaussian uncertainties.
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-histo1.tex") 10 5 0 10 0 20)
  (clip (tikz)
    ;;Draw a histogram as a blue line. Bins are not separated.
    (draw-histogram tikz (make-histogram 0 1 (mapcar (lambda (x) (+ (* 2 x) 10)) (make-random-list 10))) "blue"))
  (draw-axis-ticks-x-transformed tikz (make-range 0 1 10) 1)
  (draw-axis-ticks-y-transformed tikz (make-range 0 2 10) 1)
  (draw-plottingarea-rectangle tikz)
  (draw-line tikz 0 2.5 10 2.5 "thick,dotted")
  (draw-text-node tikz 10 2.5 "Mean" "right")
  (draw-legend-line tikz 0.5 4.5 1 "Outlined histogram" "blue" "" ""))

(defun make-gaussian-histogram (min bin-size nbins mean sigma ndraws)
  "Generate a Gaussian histogram with from ndraws random numbers."
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
  (let ((histo1 (make-gaussian-histogram 0.0 0.5 20 5.0 2.0 1000))
	(histo2 (make-gaussian-histogram 0.0 0.5 20 6.0 1.0 650))
	(histo3 (make-gaussian-histogram 0.0 0.5 20 7.0 1.0 450)))
    (clip (tikz)
      ;;Draw a histogram as a blue line. Bins are not separated, but the histogram is filled with light blue.
      (draw-histogram-bins tikz  histo1 "draw=blue!20,fill=blue!20") ;Filling
      (draw-histogram tikz histo1 "blue!80!black") ;Top line
      ;;Draw a histogram where each bin is surronded by a dark green line, filled with light green.
      ;;Transparent to show the histo below
      (draw-histogram-bins tikz histo3 "opacity=0.7,draw=green!80!black,fill=green!20")
      ;;Draw a histogram as a red line. Simple and in most cases probably best.
      (draw-histogram tikz histo2 "red!80!black")))
  (draw-axis-ticks-x tikz (tikz-transform-x tikz (make-range 0.5 1 9)) (make-range 0 1 9) t 2)
  (draw-axis-ticks-y-transformed tikz (make-range 0 15 10) 1)
  (draw-plottingarea-rectangle tikz)
  (draw-legend-rectangle tikz 0.5 4.5 1 0.2 "Filled histogram" "blue!80!black" "draw=blue!20,fill=blue!20" "")
  (draw-legend-line tikz 0.5 4.1 1 "Outlined histogram" "red!80!black")
  (draw-legend-rectangle tikz 0.5 3.7 1 0.2 "Transparent histo" "" "opacity=0.7,draw=green!80!black,fill=green!20" ""))

#|
Som Gauss smeared datapoints, with a fitted function
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-fitter.tex") 10 5 0 10 0 20)
  (let* ((x-poses (make-range 0 0.5d0 20))
	 (y-poses (mapcar (lambda (x) (gauss x #(90.0d0 5.0d0 2.0d0))) x-poses))
	 (smeared-y (mapcar (lambda (x) (+ x (* .5 (gaussian-random)))) y-poses))
	 ;;Get the fit-parameters from the #'gauss function, with initial gueass #(10.0d0 0.0d0 1.0d0)
	 (fit-params (levmar-optimize #'gauss #(10.0d0 0.0d0 1.0d0) x-poses y-poses)))
    (clip (tikz)
      (draw-function tikz (get-spline-fun (coerce x-poses 'vector) (coerce smeared-y 'vector)) 100 "green,thick" 0 10)
      (draw-function tikz (lambda (x) (gauss x fit-params)) 200 "dotted, thick")
      (draw-datapoints tikz x-poses smeared-y "draw=blue,fill=blue"))
    (draw-axis-ticks-x tikz (tikz-transform-x tikz (make-range 0.5 1 9)) (make-range 0 1 9) t 2)
    (draw-axis-ticks-y-transformed tikz (make-range 0 2 10) 1)
    (draw-legend-line tikz 0.5 4.5 1 "Noisy data" "" "draw=blue,fill=blue")
    (draw-legend-line tikz 0.5 4.1 1 "Spline fit" "draw=green,thick")
    (draw-legend-line tikz 0.5 3.7 1 "Gauss fit" "thick,dotted")
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
    (clip (tikz)
      (draw-histogram tikz histo "draw=blue"))
    (let* ((x-poses (make-range 0.25 0.5 20))
	   (y-poses (getf histo :data))
	   (y-errors (map 'vector #'sqrt y-poses))
	   ;;Get the estimated parameters
	   (parameters (levmar-optimize-errors #'gauss #(300.0d0 0.0d0 1.0d0) x-poses y-poses y-errors)))
      (draw-text-node tikz 9.5 4.5 (format nil "Fitted mean: ~5,2f" (aref parameters 1)) "left")
      (draw-text-node tikz 9.5 4.1 (format nil "Fitted sigma: ~5,2f" (aref parameters 2)) "left")
      (clip (tikz)
	(draw-function tikz (lambda (x) (gauss x parameters)) 200 "thick,red")))
    (draw-axis-ticks-x tikz (tikz-transform-x tikz (make-range 0.5 1 9)) (make-range 0 1 9) t 2)
    (draw-axis-ticks-y-transformed tikz (make-range 0 30 10) 1)
    (draw-legend-line tikz 0.5 4.5 1 "2000 Gauss-rand" "draw=blue,fill=blue")
    (draw-legend-line tikz 0.5 4.1 1 "Gauss fit" "thick,red")
    (draw-plottingarea-rectangle tikz)))

#|
Make some noisy datapoints from polynomial, fit and plot.
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-fitter3.tex") 10 5 -7 7 -100 100)
  (flet ((polynomial (x params) (+ (* (aref params 0) x x x) (* (aref params 1) x x) (* (aref params 2) x) (aref params 3))))
    (let* ((x-poses (make-range -10 0.5 40))
	   (y-poses (mapcar (lambda (x) (polynomial x #(0.5 -1.0d0 -2.0d0 3.0d0))) x-poses))
	   (y-errors (mapcar (lambda (x) (* 2.0 (+ 2.0  (sqrt (abs x))))) y-poses))
	   (y-smeared (mapcar (lambda (x err) (+ x (* err (gaussian-random)))) y-poses y-errors))
	   (params (levmar-optimize-errors #'polynomial #(1.0 1.0d0 1.0d0 1.0d0) x-poses y-smeared y-errors)))
      (clip (tikz)
	;;Draw noisy datapoints
	(draw-profilepoints tikz x-poses y-poses y-errors "draw=red,fill=red")
	;;Draw the fitted polynomial
	(draw-function tikz (lambda (x) (polynomial x params)) 200 "blue")
	(draw-text-node tikz 5.1 1.4 "Fitted parameters: " "right")
	(draw-text-node tikz 5.1 1.0 (format nil "$~2,2fx^3 ~2,2@fx^2 ~2,2@fx ~2,2@f$"
					(aref params 0) (aref params 1)
					(aref params 2)	(aref params 3)) "right")
	;;Draw the true polynomial
	(draw-function tikz (lambda (x) (polynomial x #(0.5 -1.0d0 -2.0d0 3.0d0))) 200 "dotted"))))
  (draw-axis-ticks-x-transformed tikz (remove 0 (make-range -7 1 14)) 1 2.5)
  (draw-axis-ticks-y-transformed tikz (remove 0 (make-range -100 20 10)) 1 5.0)
  (draw-legend-line tikz 0.0 4.6 1 "$0.5x^3 - x^2 - 2x + 3$" "dotted")
  (draw-legend-line tikz 0.0 4.2 1 "noisy measurements" "red" "draw=red,fill=red")
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

#|
A plot trying to show the connection between half-lifes and remaining nuclei 
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "half-life.tex") 10 5 0 5 0.0d0 1.0)
  (flet ((intensity (x params)
	   (* (aref params 0) (expt 2 (/ (- x) (aref params 1))))))
    (clip (tikz)
      (draw-function tikz (lambda (x) (intensity x #(1.0d0 1.0d0))) 100 "red"))
    (draw-axis-ticks-x tikz (tikz-transform-x tikz (make-range 0 1 5))
		       (list "$0$" "$T_{1/2}$" "$2T_{1/2}$" "$3T_{1/2}$" "$4T_{1/2}$" "$5T_{1/2}$") nil) 
    (draw-text-node tikz 5.0 5.0 (format nil "$N(t) = A_0 2^{t/T_{1/2}}$") "below")
    (draw-line tikz 0 0 0 5.2 "thick,->")
    (draw-line tikz 0 0 10.2 0 "thick,->")
    (mapcar (lambda (x)
	      (let ((y-pos (* 5 (/ 1.0 (expt 2 x)))))
		(draw-line tikz -0.1 y-pos (* 2 x) y-pos "thin,gray,dashed")
		(if (> x 0)
		    (progn (draw-line tikz (* x 2) 0 (* x 2) y-pos "thin,gray,dashed")
			   (draw-text-node tikz -0.1 y-pos (format nil "\\scriptsize{$A_0/~a$}" (expt 2 x)) "left"))
		    (draw-text-node tikz -0.1 y-pos (format nil "\\scriptsize{$A_0$}") "left"))))
	    (list 0 1 2 3 4))))
  
#|
Simulating and estimating the number of decays as function of time.
|#
(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "decay-rate2.tex") 10 5 0 5 0.0d0 0.2)
  (labels ((intensity2 (x params)
	     (* (aref params 0) 0.20 (/ (log 2) (aref params 1)) (expt 2 (/ (- x) (aref params 1)))))
	   (draw-decay-rate (a0 half-life color y-pos) 
	     (let* ((x-poses (make-range 0.10 0.20 25))
		    (y-smeared (map 'vector (lambda (x) (/ x a0)) (decay-rate2 a0 0.2 25 half-life)))
		    (params (levmar-optimize (lambda (x params) (intensity2 x params))
					     (vector 1.0d0 1.0d0) x-poses y-smeared)))
	       (draw-legend-line tikz 3.0 y-pos 1.0 (format nil "Estimated half-life for ~a is ~5,2f" a0 (aref params 1)) 
				 (concatenate 'string "dashed," color))
	       (draw-histogram tikz (make-histogram 0.0 0.20 y-smeared) color)
	       (draw-function tikz (lambda (x) (intensity2 x params)) 100 (concatenate 'string "dashed," color)))))
    (draw-text-node tikz 5.0 5.2 (format nil "$-\\Delta N = \\lambda N \\Delta t$") "")
    (draw-line tikz 0 0 0 5.2 "thick,->")
    (draw-line tikz 0 0 10.2 0 "thick,->")
    (draw-axis-ticks-x tikz (tikz-transform-x tikz (make-range 0 1 5))
		       (list "$0$" "$T_{1/2}$" "$2T_{1/2}$" "$3T_{1/2}$" "$4T_{1/2}$" "$5T_{1/2}$") nil) 
    (draw-axis-ticks-y tikz (tikz-transform-y tikz (make-range 0 0.05 5))
		       (list "$0.0$" "$0.05 A_0$" "$0.10 A_0$" "$0.15 A_0$" "$0.20 A_0$") nil) 
    (clip (tikz)
      (draw-decay-rate 100 1.0 "blue!80,thick" 4.4)
      (draw-decay-rate 1000 1.0 "purple!80,thick" 4.0)
      (draw-decay-rate 1000000 1.0 "green!80,thick" 3.6))))
	     
#|
Simulating and estimating the number of decays as function of time.
|#
(defun make-half-life-plot (a0 half-life initial-guess fname)
  (with-tikz-plot (tikz (concatenate 'string *plotting-dir* fname) 10 5 0 5 0.0d0 (* 0.2 a0))
    (flet ((intensity (x params)
	     (* 0.2 (aref params 0) (expt 2 (/ (- x) (aref params 1)))))
	   (intensity2 (x params)
	     (* (aref params 0) 0.25 (/ (log 2) (aref params 1)) (expt 2 (/ (- x) (aref params 1))))))
      (multiple-value-bind (decayed remaining) (decay-rate2 a0 0.25 25 half-life)
	  (let* ((x-poses (make-range 0.125 0.25 20))
		 (params (levmar-optimize #'intensity2 initial-guess x-poses decayed)))
	    (clip (tikz)
	      (draw-histogram tikz (make-histogram 0.0 0.25 decayed) "draw=blue")
	      (draw-histogram tikz (make-histogram -0.125 0.25 (map 'vector (lambda (x) (* 0.2 x)) remaining)) "draw=red")
	      (draw-function tikz (lambda (x) (intensity2 x params)) 100 "black,dashed")
	      (draw-function tikz (lambda (x) (intensity x params)) 100 "black,dashed")
	      (draw-legend-line tikz 3.0 3.4 1.0 (format nil "Decays\\ \\ \\ \\ \\ \\ $T_{1/2}$: ~5,2f, $A_0$: ~a"
							 half-life a0) "blue")
	      (draw-legend-line tikz 3.0 3.0 1.0 (format nil "Estimated \\ $T_{1/2}$: ~5,2f, $A_0$: ~a" 
							 (aref params 1) (floor (aref params 0))) "black,thick,dashed")
	      (draw-legend-line tikz 3.0 3.8 1.0 (format nil "Remaining $T_{1/2}$: ~5,2f, $A_0$: ~a" 
							 half-life a0) "red"))))
      (draw-axis-ticks-x tikz (tikz-transform-x tikz (make-range 0 1 5))
			 (list "$0$" "$T_{1/2}$" "$2T_{1/2}$" "$3T_{1/2}$" "$4T_{1/2}$" "$5T_{1/2}$") nil) 
      (draw-axis-ticks-y-transformed tikz (make-range 0 (/ a0 50) 10) 2)
      (mapcar (lambda (x)
		(let ((y-pos (* 5 (/ 1.0 (expt 2 x)))))
		  (draw-line tikz (* 2 x) y-pos 10.1 y-pos "thin,gray")
		  (if (> x 0) 
		      (progn (draw-line tikz (* x 2) 0 (* x 2) y-pos "thin,gray")
			     (draw-text-node tikz 10 y-pos (format nil "\\scriptsize{$A_0/~a$}" (expt 2 x)) "right"))
		      (draw-text-node tikz 10 y-pos (format nil "\\scriptsize{$A_0 = ~a$}" a0) "right"))))
	      (list 0 1 2 3 4))
      (draw-text-node tikz 0.0 5.2 (format nil "Decays, $-\\Delta N = \\lambda N \\Delta t$") "right,blue")
      (draw-text-node tikz 10.0 5.2 (format nil "Remaining, $N(t) = A_0 2^{t/T_{1/2}}$") "left,red")
      (draw-text-node tikz 5.0 -0.5 (format nil "Time") "below")
      (draw-line tikz 0 0 0 5.2 "blue,thick,->")
      (draw-line tikz 10 0 10 5.2 "red,thick,->")
      (draw-line tikz 0 0 10.2 0 "thick,->"))))

(make-half-life-plot 100000 1.0 #(0.0d0 1.0d0) "half-life1000000.tex")

(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "spline.tex") 10 5 4.0 6.0 4.0 7.0)
  (let ((x #(4.0d0  4.35d0 4.57d0 4.76d0 5.26d0 5.88d0))
	(y #(4.19d0 5.77d0 6.57d0 6.23d0 4.90d0 4.77d0)))
    (draw-axis-ticks-x-transformed tikz (make-range 4 0.2 10) 1)
    (draw-axis-ticks-y-transformed tikz (make-range 4 0.2 15) 1)
    (draw-text-node tikz 5 5.0 "Cubic splines" "")
    (clip (tikz)
      ;;Using diamonds instead of circles for datapoints
      (draw-datapoints tikz x y "draw=black!80,fill=black!80" t (make-node-string "diamond" 3 3))
      (let ((fun (get-spline-fun x y)))
	(draw-function tikz fun 100 "blue!80" 3.5d0 6.0d0))
      (let ((fun (get-spline-fun x y t)))
	(draw-function tikz fun 100 "red!80" 3.5d0 6.0d0)))
    (draw-legend-line tikz 5.5 4.0 1 "Not-a-knot spline" "blue!80")
    (draw-legend-line tikz 5.5 3.4 1 "Natural spline" "red!80")
    (draw-line tikz 0 0 10.2 0 "thick,->")
    (draw-line tikz 0 0 0 5.2 "thick,->")))
