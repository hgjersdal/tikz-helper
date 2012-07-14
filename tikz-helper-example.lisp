;(ql:quickload "tikz-helper")
;(unuse-package :xlib)
(use-package :tikz-helper)

(defparameter *plotting-dir* "/home/haavagj/src/tikz-helper/")

(defun make-random-list (size)
  (let ((rands nil))
    (dotimes (i size)
      (push (gaussian-random) rands))
    rands))

(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-graph.tex") 10 5 0 10 0 20)
  (draw-plottingarea-rectangle tikz)
  (clip (tikz)
    (draw-graph tikz (make-range 0 1 10) (mapcar #'+ (make-range 0 2 10) (make-random-list 11)) "smooth,blue" "blue,fill=blue")
    (mapcar (lambda (x y err) (draw-profilepoint tikz x y err "draw=red,fill=red"))
	    (make-range 0 1 10)
	    (mapcar #'- (make-range 20 -2 10) (make-random-list 11))
	    (make-range 1.0 0 10))
    (draw-legend-line tikz 7 3.0 1 "Expected" "thick,dotted" "" "")
    (draw-legend-line tikz 7 2.6 1 "Graph" "blue" "blue,fill=blue" "")
    (draw-legend-line tikz 7 2.2 1 "Profile" "red" "red,fill=red" ""))
  (draw-axis-ticks-x-transformed tikz (make-range 0 1 10) 1)
  (draw-axis-ticks-y-transformed tikz (make-range 0 2 10) 1)
  (draw-line tikz 0 0 10 5 "thick,dotted")
  (draw-line tikz 0 5 10 0 "thick,dotted")
  (draw-plottingarea-rectangle tikz))

(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-histo1.tex") 10 5 0 10 0 20)
  (clip (tikz)
    (draw-histogram-top tikz (make-histogram 0 1 (mapcar (lambda (x) (+ (* 2 x) 10)) (make-random-list 10))) "blue"))
  (draw-axis-ticks-x-transformed tikz (make-range 0 1 10) 1)
  (draw-axis-ticks-y-transformed tikz (make-range 0 2 10) 1)
  (draw-plottingarea-rectangle tikz)
  (draw-line tikz 0 2.5 10 2.5 "thick,dotted")
  (draw-node tikz 10 2.5 "Mean" "right")
  (draw-legend-line tikz 0.5 4.5 1 "Outlined histogram" "blue" "" ""))

(defun make-gaussian-histogram (min bin-size nbins mean sigma ndraws)
  "Generate a Gaussian histogram with approx ndraws entries."
  (let ((data (make-array nbins)))
    (dotimes (i (floor ndraws 2))
      (multiple-value-bind (g1 g2) (gaussian-random)
	(let ((bin1 (floor (- (+ mean (* sigma g1)) min) bin-size))
	      (bin2 (floor (- (+ mean (* sigma g2)) min) bin-size)))
	  (and (>= bin1 0) (< bin1 nbins) (incf (aref data bin1)))
	  (and (>= bin2 0) (< bin2 nbins) (incf (aref data bin2))))))
    (make-histogram min bin-size (coerce data 'list))))

(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-histo2.tex") 10 5 0 10 0 150)
  (let ((histo1 (make-gaussian-histogram 0.0 0.5 20 5.0 2.0 1000))
	(histo2 (make-gaussian-histogram 0.0 0.5 20 6.0 1.0 650)))
    (clip (tikz)
      (draw-histogram tikz  histo1 "draw=blue!20,fill=blue!20")
      (draw-histogram-top tikz histo1 "blue!80!black")
      (draw-histogram-top tikz histo2 "red!80!black")))
  (draw-axis-ticks-x tikz (tikz-transform-x tikz (make-range 0.5 1 9)) (make-range 0 1 9) t 2)
  (draw-axis-ticks-y-transformed tikz (make-range 0 15 10) 1)
  (draw-plottingarea-rectangle tikz)
  (draw-legend-rectangle tikz 0.5 4.5 1 0.2 "Filled histogram" "blue!80!black" "draw=blue!20,fill=blue!20" "")
  (draw-legend-line tikz 0.5 4.1 1 "Outlined histogram" "red!80!black" "" ""))

(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-fitter.tex") 10 5 0 10 0 20)
  (let* ((x-poses (make-range 0 0.5 20))
	 (y-poses (mapcar (lambda (x) (gauss x #(90.0d0 5.0d0 2.0d0))) x-poses))
	 (smeared-y (mapcar (lambda (x) (+ x (* .5 (gaussian-random)))) y-poses))
	 (fit-params (levmar-optimize #'gauss #(10.0d0 0.0d0 1.0d0) x-poses y-poses)))
    (clip (tikz)
      (draw-graph tikz x-poses smeared-y "draw=blue,fill=blue" "draw=blue,fill=blue" t)
      (draw-function tikz (lambda (x) (gauss x fit-params)) 200 "dotted, thick"))
    (draw-axis-ticks-x tikz (tikz-transform-x tikz (make-range 0.5 1 9)) (make-range 0 1 9) t 2)
    (draw-axis-ticks-y-transformed tikz (make-range 0 2 10) 1)
    (draw-legend-line tikz 0.5 4.5 1 "Noisy data" "draw=blue,fill=blue" "draw=blue,fill=blue" "")
    (draw-legend-line tikz 0.5 4.1 1 "Gauss fit" "thick,dotted" "" "")
    (draw-node tikz 9.5 4.5 (format nil "Fitted mean: ~5,2f" (aref fit-params 1)) "left")
    (draw-node tikz 9.5 4.1 (format nil "Fitted sigma: ~5,2f" (aref fit-params 2)) "left")
    (draw-plottingarea-rectangle tikz)))

(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-fitter2.tex") 10 5 0 10 0 300)
  (let* ((data (make-array 20)))
    (dotimes (i 1000)
      (multiple-value-bind (g1 g2) (gaussian-random)
	(let ((bin1 (floor (+ 5.0 (* 1.5 g1)) 0.5))
	      (bin2 (floor (+ 5.0 (* 1.5 g2)) 0.5)))
	  (and (>= bin1 0) (< bin1 20) (incf (aref data bin1)))
	  (and (>= bin2 0) (< bin2 20) (incf (aref data bin2))))))
    (clip (tikz)
      (draw-histogram-top tikz (make-histogram 0 0.5 (map 'list (lambda (x) x) data)) "draw=blue"))
    (let* ((x-poses (make-range 0.25 0.5 20))
	   (y-poses data)
	   (y-errors (map 'vector #'sqrt data))
	   (parameters (levmar-optimize-errors #'gauss #(300.0d0 0.0d0 1.0d0) x-poses y-poses y-errors)))
      (draw-node tikz 9.5 4.5 (format nil "Fitted mean: ~5,2f" (aref parameters 1)) "left")
      (draw-node tikz 9.5 4.1 (format nil "Fitted sigma: ~5,2f" (aref parameters 2)) "left")
      (clip (tikz)
	(draw-function tikz (lambda (x) (gauss x parameters)) 200 "thick,red")))
    (draw-axis-ticks-x tikz (tikz-transform-x tikz (make-range 0.5 1 9)) (make-range 0 1 9) t 2)
    (draw-axis-ticks-y-transformed tikz (make-range 0 30 10) 1)
    (draw-legend-line tikz 0.5 4.5 1 "2000 Gauss-rand" "draw=blue,fill=blue" "" "")
    (draw-legend-line tikz 0.5 4.1 1 "Gauss fit" "thick,red" "" "")
    (draw-plottingarea-rectangle tikz)))

(with-tikz-plot (tikz (concatenate 'string *plotting-dir* "test-fitter3.tex") 10 5 -7 7 -100 100)
  (flet ((tuned-sin (x params) (+ (* (aref params 0) x x x) (* (aref params 1) x x) (* (aref params 2) x) (aref params 3))))
    (let* ((x-poses (make-range -10 0.5 40))
	   (y-poses (mapcar (lambda (x) (tuned-sin x #(0.5 -1.0d0 -2.0d0 3.0d0))) x-poses))
	   (y-errors (mapcar (lambda (x) (* 0.4 (sqrt (abs x)))) y-poses))
	   (y-smeared (mapcar (lambda (x err) (+ x (* err (gaussian-random)))) y-poses y-errors))
	   (params (levmar-optimize-errors #'tuned-sin #(1.0 1.0d0 1.0d0 1.0d0) x-poses y-smeared y-errors)))
      (clip (tikz)
	(mapcar (lambda (x y err) (draw-profilepoint tikz x y err "draw=red,fill=red"))
		x-poses y-smeared y-errors)
	(draw-function tikz (lambda (x) (tuned-sin x params)) 200 "blue")
	(draw-function tikz (lambda (x) (tuned-sin x #(0.5 -1.0d0 -2.0d0 3.0d0))) 200 "dotted"))))
  (draw-axis-ticks-x-transformed tikz (remove 0 (make-range -7 1 14)) 1 2.5)
  (draw-axis-ticks-y-transformed tikz (remove 0 (make-range -100 20 10)) 1 5.0)
  (draw-legend-line tikz 0.0 4.6 1 "$0.5x^3 - x^2 - 2x + 3$" "dotted" "" "")
  (draw-legend-line tikz 0.0 4.2 1 "noisy measurements" "red" "fill=red" "")
  (draw-legend-line tikz 0.0 3.8 1 "Fitted polynomial" "blue" "" "")
  (draw-line tikz 0 2.5 10.2 2.5 "thick,->")
  (draw-line tikz 5 0 5 5.2 "thick,->"))
