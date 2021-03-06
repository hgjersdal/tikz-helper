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

(defun read-u2 (stream)
  (let ((u2 0))
    (setf (ldb (byte 8 0) u2) (read-byte stream))
    (setf (ldb (byte 8 8) u2) (read-byte stream))
    u2))

(defun read-u2-le (stream)
  (let ((u2 0))
    (setf (ldb (byte 8 8) u2) (read-byte stream))
    (setf (ldb (byte 8 0) u2) (read-byte stream))
    u2))

(defun read-head ()
  (let* ((histo (make-histogram3d (list 0.0d0 0.0d0 0.0d0) (list 1.0d0 1.0d0 2.0d0) (list 256 256 113)))
	 (data (getf histo :data)))
    (dotimes (z 113)
      (with-open-file (f (namestring (merge-pathnames (make-pathname :name "CThead" :type (format nil "~a" (+ z 1))) *data-dir*))
			:element-type '(unsigned-byte 8))
	(dotimes (x (* 256))
	  (dotimes (y (* 256))
	    (setf (aref data x y z) (read-u2-le f))))))
    histo))

(let ((histo (read-head))
      (cam (list #(528.0d0 128.0d0 107.0d0) #(328.0d0 328.0d0 107.0d0) #(128.0d0 528.0d0 107.0d0)))
      (*compilep* t))
  (dotimes (c 3)
    (let* ((projection (get-mip-projection-histo histo 
						 (coerce #(128.0 128.0 107.0) '(simple-array double-float (3)))
						 (coerce (elt cam c) '(simple-array double-float (3)))
						 200.0d0 200.0d0 1.0d0))
	   (max (histo2d-get-max projection)))
      (with-volume-example ((format nil "head~a" (+ 1 c)) 0 200 0 200 :none)
	(draw-histo2d-contour tikz projection (* 0.0 max) (* 1.0 max) 30 t :color-lines t :cols (list "white" "black"))))))

(defun read-brain ()
  (let* ((histo (make-histogram3d (list 0.0d0 0.0d0 0.0d0) (list 1.0d0 1.0d0 2.0d0) (list 256 256 109)))
	 (data (getf histo :data)))
    (dotimes (z 109)
      (with-open-file (f (namestring (merge-pathnames (make-pathname :name "MRbrain" :type (format nil "~a" (+ z 1))) *data-dir*))
			:element-type '(unsigned-byte 8))
	(dotimes (x (* 256))
	  (dotimes (y (* 256))
	    (setf (aref data x y z) (read-u2-le f))))))
    histo))

(let ((histo (read-brain))
      (ref (list #(128.0d0 128.0d0 44.5d0) #(128.0d0 128.0d0 74.5d0) #(128.0d0 128.0d0 104.5d0)))
      ;;(ref (list #(128.0d0 68.0d0 104.5d0) #(128.0d0 88.0d0 104.5d0) #(128.0d0 118.0d0 104.5d0)))
      (*compilep* t))
  (dotimes (c 3)
    (let* ((projection (get-lmip-projection-histo histo 
						  (coerce (elt ref c) '(simple-array double-float (3)))
						  (coerce #(128.0 128.0 504.5) '(simple-array double-float (3)))
						  200.0d0 200.0d0 0 1.0d0))
	   (max 4000))
      (with-volume-example ((format nil "brain~a" (+ 1 c)) 0 200 0 200 :none)
	(draw-histo2d-contour tikz projection (* 0.32 max) (* 1.0 max) 30 t :color-lines t :cols (list "white" "black" "red"))))))

(defun read-beetle1 ()
  (with-open-file (s (namestring (merge-pathnames (make-pathname :name "stagbeetle832x832x494" :type "dat") *data-dir*))
		     :element-type '(unsigned-byte 8))
    (let* ((xx (read-u2 s))
	   (yy (read-u2 s))
	   (zz (read-u2 s))
	   (histo (make-histogram3d (list 0.0d0 0.0d0 0.0d0) (list .25d0 .25d0 .25d0) (list xx yy zz)))
	   (data (getf histo :data)))
      (dotimes (z zz)
	(dotimes (y yy)
	  (dotimes (x xx)
	    (setf (aref data x y z) (read-u2 s)))))
      histo)))

(defun read-beetle2 ()
  (with-open-file (s (namestring (merge-pathnames (make-pathname :name "stagbeetle208x208x123" :type "dat") *data-dir*))
		     :element-type '(unsigned-byte 8))
    (let* ((xx (read-u2 s))
	   (yy (read-u2 s))
	   (zz (read-u2 s))
	   (histo (make-histogram3d (list 0.0d0 0.0d0 0.0d0) (list 1.0d0 1.0d0 1.0d0) (list xx yy zz)))
	   (data (getf histo :data)))
      (dotimes (z zz)
	(dotimes (y yy)
	  (dotimes (x xx)
	    (setf (aref data x y z) (read-u2 s)))))
      histo)))

(let ((histo (read-beetle2))
      (cam (list #(604.0d0 104.0d0 61.5d0) #(104.0d0 604.0d0 61.5d0) #(104.0d0 104.0d0 560.5d0)))
      (*compilep* t))
  (dotimes (c 3)
    (let* ((projection (get-integral-projection-histo histo 
						      (coerce #(104.0 104.0 61.5) '(simple-array double-float (3)))
						      (coerce (elt cam c) '(simple-array double-float (3)))
						      200.0d0 200.0d0 0.8d0 200))
	   (max (histo2d-get-max projection)))
      (with-volume-example ((format nil "beetle~a" (+ 1 c)) 0 200 0 200 :none)
	(draw-histo2d-contour tikz projection (* 0.01 max) (* 1.0 max) 30 t :color-lines t :cols (list "LightGray" "black"))))))

