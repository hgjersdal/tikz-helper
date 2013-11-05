(in-package :tikz-helper)

(defun range->steps (range step-min step-max &optional (power 0) (nth 0))
  "Figure out the number of, and distance between tick marks."
  (when (> nth 15) (error "Auto axis failed."))
  (labels ((nsteps (step) (/ range (* step (expt 10 power))))
	   (result (step) (values (* step (expt 10 power)) power (floor (nsteps step))))
	   (withinp (step) (and (<= (nsteps step) step-max) (>= (nsteps step) step-min))))
    (cond
      ((>= (nsteps 5.0) step-max) (range->steps range step-min step-max (1+ power) (incf nth)))
      ((<= (nsteps 1.0) step-min) (range->steps range step-min step-max (1- power) (incf nth)))
      ((withinp 1.0) (result 1.0))
      ((withinp 2.0) (result 2.0))
      ((withinp 5.0) (result 5.0)))))

(defun auto-ticks (min max step-min step-max)
  "Generate a list of tick mark positions, as well as the suggested prectision."
  (multiple-value-bind (step power nsteps) (range->steps (- max min) step-min step-max)
    (let ((min-tick (* step (floor min step))))
      (values
       ;;Factor 1.0d-4 is there to take care of round off errors
       (remove-if (lambda (x) (or (< x (- min (* 1.0d-4 step))) 
				  (> x (+ max (* 1.0d-4 step)))))
		  (make-range min-tick step (* 2 nsteps)))
       (if (> power 0) 0 (- power))))))

(defun auto-ticks-x (plottingarea x-list step-min step-max)
  "Returns list of ticks, and precision if applicable"
  (if (listp x-list) (values x-list 0)
      (auto-ticks (plot-x-min plottingarea) (plot-x-max plottingarea)  
		  step-min step-max)))

(defun auto-ticks-y (plottingarea y-list step-min step-max)
  "Returns list of ticks, and precision if applicable"
  (if (listp y-list) (values y-list 0)
      (auto-ticks (plot-y-min plottingarea) (plot-y-max plottingarea)
		  step-min step-max)))

(defmacro lazy-ticks ((argsx argsy remove-0))
  "An ungly macro that saves lots of typing. Can only be used when all the parameter names below
are deined, in other words in the draw-axis* functions below."
  `(multiple-value-bind (tx prx) (auto-ticks-x plottingarea x-list x-ticks-min x-ticks-max)
     (multiple-value-bind (ty pry) (auto-ticks-y plottingarea y-list y-ticks-min y-ticks-max)
       ;;(let ((prec (max pry prx)))
       (draw-axis-ticks-x plottingarea (if ,remove-0 (remove 0.0 tx) tx)
			  :style tick-style :precision prx 
			  :numberp (not (or (listp x-list) x-names)) :names x-names
			  ,@argsx)
       (draw-axis-ticks-y plottingarea (if ,remove-0 (remove 0.0 ty) ty)
			  :style tick-style :precision pry 
			  :numberp (not (or (listp y-list) y-names)) :names y-names
			  ,@argsy))))

(defun draw-axis-rectangle (plottingarea 
			    &key (fill nil) (rectangle-style "thick,black,fill=white")
			      (x-ticks-max 10) (x-ticks-min 4) (y-ticks-max 10) (y-ticks-min 4)
			      (x-list t) (y-list t) (tick-style "")
			      (x-names nil) (y-names nil))
  "Draw a rectanle around the plottingarea, and add ticks. Keywords mean:
fill: Fill the rectangle with fill color. Helpful if the plot is overlaying another plot.
x- or y-names: list of names for the axes.
x- of y-ticks-max or -min: The minimum or maximum number of tick marks in the x or y direction. With bad numbers, auto-tick will fail.
x- or y- list: List of tick marks. If nil, no ticks are drawn. If it is a list, one tick mark per value in list. If T, ticks are automatically generated."
  (transform (plottingarea)
    (scope (plottingarea rectangle-style)
      (make-rectangle-path plottingarea
			   (plot-x-min plottingarea) (plot-y-min plottingarea)
			   (plot-x-max plottingarea) (plot-y-max plottingarea))
      (path-use plottingarea t fill))
    (lazy-ticks (() () nil))))

(defun draw-axis-cross (plottingarea
			&key (line-style "thick,black,fill=white")
			  (x-ticks-max 10) (x-ticks-min 4) (y-ticks-max 10) (y-ticks-min 4)
			  (x-list t) (y-list t) (tick-style "")
			  (x-names nil) (y-names nil))
  "Draw axes that cross at the origin.
Keywords mean:
x- or y-names: list of names for the axes.
x- of y-ticks-max or -min: The minimum or maximum number of tick marks in the x or y direction. With bad numbers, auto-tick will fail.
x- or y- list: List of tick marks. If nil, no ticks are drawn. If it is a list, one tick mark per value in list. If T, ticks are automatically generated."
  (transform (plottingarea)
    (scope (plottingarea line-style)
      (path-move-to plottingarea (plot-x-min plottingarea) 0)
      (path-line-to plottingarea (plot-x-max plottingarea) 0)
      (path-use plottingarea)
      (path-move-to plottingarea 0 (plot-y-min plottingarea))
      (path-line-to plottingarea 0 (plot-y-max plottingarea))
      (path-use plottingarea))
    (lazy-ticks ((:y-shift (format nil "~acm" (apply-transform-y plottingarea 0.0)))
		     (:x-shift (format nil "~acm" (apply-transform-x plottingarea 0.0))) t))))

(defun draw-axis-popped-out (plottingarea
			     &key (line-style "thick,black,fill=white")
			       (x-ticks-max 10) (x-ticks-min 4) (y-ticks-max 10) (y-ticks-min 4)
			       (x-list t) (y-list t) (x-shift "-0.15cm") (y-shift "-0.15cm")
			       (x-names nil) (y-names nil) (tick-style ""))
  "Draw axes that are shifted from plot-x-min and plot-y-min.
Keywords mean:
x- or y-names: list of names for the axes.
y- or x-shift: How far the axis should be shifted from plot-min.
x- of y-ticks-max or -min: The minimum or maximum number of tick marks in the x or y direction. With bad numbers, auto-tick will fail.
x- or y- list: List of tick marks. If nil, no ticks are drawn. If it is a list, one tick mark per value in list. If T, ticks are automatically generated."
  (transform (plottingarea)
    (scope (plottingarea line-style)
      (path-move-to-mixed plottingarea (plot-x-min plottingarea) 0 (plot-y-min plottingarea) y-shift)
      (path-line-to-mixed plottingarea (plot-x-max plottingarea) 0 (plot-y-min plottingarea) y-shift)
      (path-move-to-mixed plottingarea (plot-x-min plottingarea) x-shift (plot-y-min plottingarea) 0)
      (path-line-to-mixed plottingarea (plot-x-min plottingarea) x-shift (plot-y-max plottingarea) 0)
      (path-use plottingarea))
    (lazy-ticks ((:y-shift y-shift :stop 0)
		     (:x-shift x-shift :stop 0) nil))))

(defun draw-axis-left-bottom (plottingarea
			      &key (line-style "thick,black,fill=white")
				(x-ticks-max 10) (x-ticks-min 4) (y-ticks-max 10) (y-ticks-min 4)
				(x-list t) (y-list t) (tick-style "") (x-names nil) (y-names nil))
  "Draw axes along the left and bottom side of the figure.
Keywords mean:
x- or y-names: list of names for the axes.
x- of y-ticks-max or -min: The minimum or maximum number of tick marks in the x or y direction. With bad numbers, auto-tick will fail.
x- or y- list: List of tick marks. If nil, no ticks are drawn. If it is a list, one tick mark per value in list. If T, ticks are automatically generated."
  (transform (plottingarea)
    (draw-line plottingarea (plot-x-min plottingarea) (plot-y-min plottingarea) 
	       (plot-x-max plottingarea) (plot-y-min plottingarea) line-style)
    (draw-line plottingarea (plot-x-min plottingarea) (plot-y-min plottingarea)
	       (plot-x-min plottingarea) (plot-y-max plottingarea) line-style)
    (lazy-ticks (()() nil))))

(defun draw-grid-lines (plottingarea
			&key (line-style "thin,gray!20!white")
			  (x-ticks-max 10) (x-ticks-min 4) (y-ticks-max 10) (y-ticks-min 4)
			  (x-list t) (y-list t))
  "Draw grid lines.
Keywords mean:
x- of y-ticks-max or -min: The minimum or maximum number of tick marks in the x or y direction. With bad numbers, auto-tick will fail.
x- or y- list: List of tick marks. If nil, no ticks are drawn. If it is a list, one tick mark per value in list. If T, ticks are automatically generated."
  (transform (plottingarea)
    (flet ((x-lines (y-val) 
	     (path-move-to plottingarea (plot-x-min plottingarea) y-val)
	     (path-line-to plottingarea (plot-x-max plottingarea) y-val))
	   (y-lines (x-val) 
	     (path-move-to plottingarea x-val (plot-y-min plottingarea))
	     (path-line-to plottingarea x-val (plot-y-max plottingarea))))
      (scope (plottingarea line-style)
	(mapcar #'y-lines (auto-ticks-x plottingarea x-list x-ticks-min x-ticks-max))
	(mapcar #'x-lines (auto-ticks-y plottingarea y-list y-ticks-min y-ticks-max))
	(path-use plottingarea)))))

(defparameter *colors* (list "DarkBlue" "blue" "green" "yellow" "orange" "red"))

(defun color-palette (plottingarea x-pos y-pos width height z-min z-max
		      &optional (cols *colors*))
  "Draw the colors of the z-axis with ticks and a black box around it."
  (let* ((nrect (1- (length cols)))
	 (poses (make-range 0.0 (/ height nrect) (1+ nrect))))
    (format (ostream plottingarea) "\\pgfdeclareverticalshading{myshadingD}~%{~acm}{" width)
    (format (ostream plottingarea) "color(~acm)=(~a)" (car poses) (car cols))
    (mapc (lambda (pos col)
	    (format (ostream plottingarea) "; color(~acm)=(~a)" pos col)) (cdr poses) (cdr cols))
    (format (ostream plottingarea)
	    "}~%\\pgftext[at=\\pgfpoint{~acm}{~acm}] {\\pgfuseshading{myshadingD}}" 
	    (+ x-pos (* 0.5 width)) (+ y-pos (* 0.5 height)))
    ;;Use a subfigure to get z-tranformations in the y-axis direction..
    (with-subfigure (plottingarea t2 (+ x-pos width) y-pos (- width) height 0 1 z-min z-max)
      (multiple-value-bind (ticks precision) (auto-ticks-y t2 t 4 10)
	(draw-axis-rectangle t2 :x-list nil :y-list nil)
	(draw-axis-ticks-y t2 ticks :text-style "right"
			   :precision precision :start "3pt" :stop "-2pt")))))
