(in-package :tikz-helper-example)

(defparameter *data-dir* (make-pathname :defaults "/home/haavagj/Desktop/cthead/")
  "This folder should contain the unzipped content of the file found here: http://graphics.stanford.edu/data/voldata/CThead.tar.gz")

(defmacro with-volume-example ((name plot-x-min plot-x-max plot-y-min plot-y-max axis-style 
				     &key (tikz-arg "") (width 4) (height 4)) &body body)
  "A macro wrapping the with-tikz-to-file macro for the volume example plots. It makes a plot with name in the 
*plotting-dir* directory, with a width of 5cm, a height of 5 cm. If *compilep* is T, the produced file is
compiled with pdflatex, the results are viewed with *viewer*."
  (let ((fname (gensym)))
    `(let ((,fname (namestring (merge-pathnames (make-pathname :name ,name :type "tex") *plotting-dir*))))
       (with-tikz-to-file (tikz ,fname ,width ,height ,plot-x-min ,plot-x-max ,plot-y-min ,plot-y-max ,axis-style :tikz-arg ,tikz-arg)
	 ,@body)
       (when *compilep* (pdflatex-compile-view ,fname *viewer*)))))


(defun read-head ()
  (let* ((histo (make-histogram3d (list 0.0d0 0.0d0 0.0d0) (list 1.0d0 1.0d0 2.0d0) (list 256 256 113)))
	 (data (getf histo :data)))
    (dotimes (z 113)
      (with-open-file (f (namestring (merge-pathnames (make-pathname :name "CThead" :type (format nil "~a" (+ z 1))) *data-dir*))
			:element-type '(unsigned-byte 8))
	(dotimes (x (* 256))
	  (dotimes (y (* 256))
	    (let ((val 0))
	      (setf (ldb (byte 8 8) val) (read-byte f))
	      (setf (ldb (byte 8 0) val) (read-byte f))
	      (setf (aref data x y z) (coerce val 'double-float)))))))
    histo))

(defparameter *head* (read-head))

(defun draw-projection (name cam projection-fun)
  (let ((*compilep* t))    
    (with-volume-example (name 0 200 0 200 :none)
      (let* ((histo (funcall projection-fun *head*
			     (coerce #(128.0 128.0 107.0) '(simple-array double-float (3)))
			     (coerce cam '(simple-array double-float (3)))
			     200.0d0 200.0d0
			     1.0d0))
	     (max (histo2d-get-max histo)))
	(format t "contouring~%")
	(draw-histo2d-contour tikz histo (* 0.00 max) (* 1.0 max) 20 t :color-lines t :opacity-gradient t :cols (list "white" "black"))))))

(let ((cam #(528.0d0 128.0d0 107.0d0)))
  (draw-projection "head1" cam #'get-integral-projection-histo)
  (draw-projection "head1-mip" cam #'get-mip-projection-histo)
  (draw-projection "head1-lmip" cam (lambda (s r c h w sp) (get-lmip-projection-histo s r c w h 1.0d0 sp))))

(let* ((dist (/ (sqrt (* 400 400)) 2.0d0))
       (cam (vector (+ 128 dist) (+ 128 dist) 107.0d0)))
  (draw-projection "head2" cam #'get-integral-projection-histo)
  (draw-projection "head2-mip" cam #'get-mip-projection-histo)
  (draw-projection "head2-lmip" cam (lambda (s r c h w sp) (get-lmip-projection-histo s r c h w 1.0d0 sp))))

(let ((cam #(128.0d0 528.0d0 107.0d0)))
  (draw-projection "head3" cam #'get-integral-projection-histo)
  (draw-projection "head3-mip" cam #'get-mip-projection-histo)
  (draw-projection "head3-lmip" cam (lambda (s r c h w sp) (get-lmip-projection-histo s r c h w 1.0d0 sp))))

