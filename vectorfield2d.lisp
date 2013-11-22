(in-package :tikz-helper)

(defun make-vectorfield2d (x-min x-bin-size x-nbin y-min y-bin-size y-nbin &key (function nil))
  (let* ((data (make-array (list x-nbin y-nbin) :element-type '(simple-vector 2)))
	 (field (list :x-min x-min :x-bin-size x-bin-size :x-nbin x-nbin 
		      :y-min y-min :y-bin-size y-bin-size :y-nbin y-nbin :data data)))
    (when (functionp function)
      (histo2d-fill-from-function field function))
    field))

(defun vectorfield2d-set (vectorfield x-bin y-bin x-val y-val)
  "Set the vector at at x y"
  (setf (aref vectorfield x-bin y-bin) (vector x-val y-val)))

(defun draw-vector (plottingarea vector x y fade zmin zmax scale centerp normalizep)
  (let* ((veclen (sqrt (+ (expt (aref vector 0) 2) (expt (aref vector 1) 2))))
	 (scale (if (and normalizep (> veclen 0.0)) (/ scale veclen) scale))
	 (alpha (if fade (format nil ",opacity=~4,2f" (/ (- veclen zmin) (- zmax zmin))) ""))
	 (x (if centerp (- x (* 0.5 scale (aref vector 0))) x))
	 (y (if centerp (- y (* 0.5 scale (aref vector 1))) y)))
    (when (> veclen  zmin)
      (draw-line plottingarea x y (+ x (* scale (aref vector 0))) (+ y (* scale (aref vector 1))) 
		 (concatenate 'string "->" alpha)))))

(defun draw-vectorfield2d (plottingarea vectorfield zmin &key (centerp t) (scale 1.0) (normalizep nil) (fade nil) (zmax nil))
  "Draw a rectangle for each bin. Colors go from cold to hot"
  (transform (plottingarea)
    (let ((x-poses (make-range (getf vectorfield :x-min) (getf vectorfield :x-bin-size) (getf vectorfield :x-nbin)))
	  (y-poses (make-range (getf vectorfield :y-min) (getf vectorfield :y-bin-size) (getf vectorfield :y-nbin)))
	  (x-bins (make-range 0 1 (getf vectorfield :x-nbin)))
	  (y-bins (make-range 0 1 (getf vectorfield :y-nbin))))
      (mapcar (lambda (x-bin x-pos)
		(mapcar (lambda (y-bin y-pos)
			  (draw-vector plottingarea (aref (getf vectorfield :data) x-bin y-bin)
				       x-pos y-pos fade zmin zmax scale centerp normalizep))
			y-bins y-poses))
	      x-bins x-poses))))
