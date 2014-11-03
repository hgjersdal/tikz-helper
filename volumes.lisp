(in-package :tikz-helper)

;;(declaim (optimize (speed 3) (safety 0) (debug 0)))
(declaim (optimize (speed 0) (safety 3) (debug 3)))

#|
First define 2D histogram for projection.

Get distance vector between ref and cam. r_ - c_ = d_

Find plane orthogonal to d_, passing through r_. 

Unit vectors are 
1: norm(d_ x z_)
2: and norm (d_ x (d_ x z_))

If d_ x z_ is null, then 1: x_

Find vector b_, straight line from cam to bin. Find db_/ds, where s is arc length smaller than smallest bin.

Set bin content to line integral along b_. Terminate when outside of bounds and moving further away. 

If b_x < x_min and db_x/ds is smaller than 0, abort.
If b_x > x_max and db_x/ds is larger  than 0, abort.
|#

(defun make-histogram3d (min-list bin-size-list nbins-list)
  (let ((data (make-array nbins-list :element-type '(unsigned-byte 16) :initial-element 0)))
    (list :mins (coerce min-list '(simple-array double-float (3)))
	  :bin-sizes (coerce bin-size-list '(simple-array double-float (3)))
	  :nbins (coerce nbins-list '(simple-array integer (3)))
	  :data data)))

;;Define projection plane, some basic linear algebra

(defparameter *z-axis* (make-array 3 :element-type 'double-float :initial-contents #(0.0d0 0.0d0 1.0d0)))
(defparameter *y-axis* (make-array 3 :element-type 'double-float :initial-contents #(0.0d0 1.0d0 0.0d0)))
(defparameter *x-axis* (make-array 3 :element-type 'double-float :initial-contents #(1.0d0 0.0d0 0.0d0)))

(defun cross-product (a b result)
  (declare (type (simple-array double-float (3)) a b result))
  (let ((a1 (aref a 0))
	(a2 (aref a 1))
	(a3 (aref a 2))
	(b1 (aref b 0))
	(b2 (aref b 1))
	(b3 (aref b 2)))
    (setf (aref result 0) (- (* a2 b3) (* b2 a3))
	  (aref result 1) (- (* a3 b1) (* b3 a1))
	  (aref result 2) (- (* a1 b2) (* b1 a2)))
    result))

(defun vector-scale-add (a b scale result)
  "result = a + scale * b"
  (declare (type (simple-array double-float (3)) a b result)
	   (type double-float scale))
  (setf (aref result 0) (+ (aref a 0) (* scale (aref b 0)))
	(aref result 1) (+ (aref a 1) (* scale (aref b 1)))
	(aref result 2) (+ (aref a 2) (* scale (aref b 2)))))

(defun vector-distance (a b result)
  (declare (type (simple-array double-float (3)) a b result))
  (vector-scale-add a b -1.0d0 result))

(defun vector-step (pos dir-step)
  (declare (type (simple-array double-float (3)) pos dir-step))
  (incf (aref pos 0) (aref dir-step 0))
  (incf (aref pos 1) (aref dir-step 1))
  (incf (aref pos 2) (aref dir-step 2))
  nil)

(defun sqr (x)
  (declare (type double-float x))
  (* x x ))

(defun vector-length (vec)
  (declare (type (simple-array double-float (3)) vec))
  (sqrt (+ (sqr (aref vec 0))
	   (sqr (aref vec 1))
	   (sqr (aref vec 2)))))

(defun normalize-vector (vec)
  (declare (type (simple-array double-float (3)) vec))
  (let ((distance (vector-length vec)))
    (dotimes (i 3)
      (setf (aref vec i) (/ (aref vec i) distance)))))

(defun get-direction-step (cam pos)
  (declare (type (simple-array double-float (3)) cam pos))
  (let ((dir (make-array 3 :element-type 'double-float)))
    (vector-distance pos cam dir)
    (normalize-vector dir)
    dir))

(defun get-horizontal-axis (dir-step)
  "The horizontal axis is either dir-step cross z-axis, or the x-axis if dir-step is parallel to z-axis.
This can be problematic when scanning angles, with one point with a direction parallel to z."
  (declare (type (simple-array double-float (3)) dir-step))
  (let ((axis (make-array 3 :element-type 'double-float)))
    (cross-product dir-step *z-axis* axis)
    (if (> (vector-length axis) 0.0d0)
	(progn (normalize-vector axis)
	       axis)
	(progn 
	  (format t "Cross product failed!")
	  *x-axis*))))

;;If cross dir z

(defun get-vertical-axis (dir-step horizontal)
  (declare (type (simple-array double-float (3)) dir-step horizontal))
  (let ((axis (make-array 3 :element-type 'double-float)))
    (cross-product dir-step horizontal axis)
    (unless (> (vector-length axis) 0)
      (format t "NULL vector~%~a~%~a~%" dir-step horizontal))
    (normalize-vector axis)
    axis))

;;;Boundary checking    

(defun within-boundp (x min size n)
  (declare (type double-float x min size)
	   (type integer n))
  (if (or (< x min)
	  (> x (+ min (* size n))))
      nil
      t))

(defun vector-within-boundsp (x histo)
  (let ((mins  (getf histo :mins))
	(sizes (getf histo :bin-sizes))
	(bins  (getf histo :nbins)))
    (declare (type (simple-array double-float (3)) mins sizes x)
	     (type (simple-array integer (3)) bins))
    (and
     (within-boundp (aref x 0) (aref mins 0) (aref sizes 0) (aref bins 0))
     (within-boundp (aref x 1) (aref mins 1) (aref sizes 1) (aref bins 1))
     (within-boundp (aref x 2) (aref mins 2) (aref sizes 2) (aref bins 2)))))

(defun bin-num (x min size)
  (declare (type double-float x min size))
  (the integer (floor (/ (- x min) size))))

(defun histo3d-get-value (x histo)
  (let ((mins  (getf histo :mins))
	(sizes (getf histo :bin-sizes)))
    (declare (type (simple-array double-float (3)) mins sizes x))
    (if
     ;;inside bound?
     (vector-within-boundsp x histo)
     ;;get value
     (aref (getf histo :data)
	   (bin-num (aref x 0) (aref mins 0) (aref sizes 0))
	   (bin-num (aref x 1) (aref mins 1) (aref sizes 1))
	   (bin-num (aref x 2) (aref mins 2) (aref sizes 2)))
     ;;return 0
     0)))

(defun not-getting-closerp (x dx min size n)
  (declare (type double-float x dx min size)
	   (type integer n))
  ;;Is the line outside of bounds and moving away from data?
  (if (< dx 0)
      ;;Below bound and decreasing?
      (< x min) 
      ;;Above bound and increasing?
      (> x (+ min (* size n)))))

(defun vector-not-getting-closerp (histo x dx/ds)
  (let ((mins (getf histo :mins))
	(sizes (getf histo :bin-sizes))
	(bins (getf histo :nbins)))
    (declare (type (simple-array double-float (3)) mins sizes x dx/ds)
	     (type (simple-array integer (3)) bins))
    (or
     (not-getting-closerp (aref x 0) (aref dx/ds 0) (aref mins 0) (aref sizes 0) (aref bins 0))
     (not-getting-closerp (aref x 1) (aref dx/ds 1) (aref mins 1) (aref sizes 1) (aref bins 1))
     (not-getting-closerp (aref x 2) (aref dx/ds 2) (aref mins 2) (aref sizes 2) (aref bins 2)))))

(defun get-dx/ds (cam bin-pos)
  (declare (type (simple-array double-float (3)) cam bin-pos))
  (let ((dx/ds (make-array 3 :element-type 'double-float)))
    (vector-distance bin-pos cam dx/ds)
    (normalize-vector dx/ds)
    (dotimes (i 3)
      (setf (aref dx/ds i) (/ (aref dx/ds i) 5)))
    dx/ds))

(defun line-integral (x dx/ds histo value)
  (declare (type (simple-array double-float (3)) x dx/ds)
	   (type integer value))
  (if
   ;;Abort if outside boundaries and moving away
   (vector-not-getting-closerp histo x dx/ds)
   value
   (progn
     (vector-scale-add x dx/ds 1.0d0 x)
     (line-integral x dx/ds histo (+ value (histo3d-get-value x histo))))))

;;;Making and filling of histogram 
(defun start-line-integral (cam bin-pos histo3d)
  (line-integral bin-pos (get-dx/ds cam bin-pos)  histo3d 0))

(defun get-projection-histo (histo3d ref cam width height start-plane-pos fun bins)
  "ref is a reference point. height and width are sizes of the projection around this point.
start-plane-pos is the distance in units of (- ref cam). The projection rays emit from the plane 
around start-plane-pos, and move away from the camera.
"
  (declare (type integer bins)
	   (type double-float height width))
  (let* ((histo2d (make-histogram2d 0 (/ width bins) bins 0 (/ height bins) bins))
	 (dir (get-direction-step cam ref))
	 (horizontal (get-horizontal-axis dir))
	 (vertical (get-vertical-axis dir horizontal))
	 (height (* height start-plane-pos))
	 (width (* width start-plane-pos))
	 (distance (make-array 3 :element-type 'double-float :initial-element 0.0d0))
	 (start-ref (make-array 3 :element-type 'double-float :initial-element 0.0d0))
	 (bin-pos (make-array 3 :element-type 'double-float :initial-element 0.0d0)))
    (vector-distance ref cam distance)
    (vector-scale-add cam distance start-plane-pos start-ref)
    (dotimes (i bins)
      (dotimes (j bins)
	(vector-scale-add start-ref horizontal (+ (* -0.5 width)  (* i (/ width  bins))) bin-pos)
	(vector-scale-add bin-pos vertical (+ (* -0.5 height) (* j (/ height bins))) bin-pos)
	(setf (aref (getf histo2d :data) i j) (coerce (funcall fun cam bin-pos histo3d) 'double-float))))
    histo2d))

(defun get-integral-projection-histo (histo3d ref cam width height start-plane-pos &optional (bins 100))
  (get-projection-histo histo3d ref cam width height start-plane-pos #'start-line-integral bins))

(defun line-mip (x dx/ds histo value)
  (declare (type (simple-array double-float (3)) x dx/ds)
	   (type integer value))
  (let* ((hval (histo3d-get-value x histo))
	 (value (if (> hval value) hval value)))
    ;;Abort if outside boundaries and moving away
    (if (vector-not-getting-closerp histo x dx/ds)
     value
     (progn
       (vector-scale-add x dx/ds 1.0d0 x)
       (line-mip x dx/ds histo value)))))

;;;Making and filling of histogram
(defun start-mip (cam bin-pos histo3d)
  (line-mip bin-pos (get-dx/ds cam bin-pos)  histo3d 0))

(defun get-mip-projection-histo (histo3d ref cam width height start-plane-pos &optional (bins 100))
  (get-projection-histo histo3d ref cam width height start-plane-pos #'start-mip bins))

(defun line-lmip (x dx/ds histo value thresh)
  (declare (type (simple-array double-float (3)) x dx/ds)
	   (type integer value)
	   (type fixnum  thresh))
  (let* ((hval (histo3d-get-value x histo))
	 (value (if (> hval value) hval value)))
    ;;Abort if outside boundaries and moving away
    (if (or (vector-not-getting-closerp histo x dx/ds)
	    (<= thresh value))
	value
	(progn
	  (vector-scale-add x dx/ds 1.0d0 x)
	  (line-lmip x dx/ds histo value thresh)))))

(defun start-lmip (cam bin-pos histo3d thresh)
  (line-lmip bin-pos (get-dx/ds cam bin-pos) histo3d 0 thresh))

(defun get-lmip-projection-histo (histo3d ref cam width height thresh start-plane-pos &optional (bins 100))
  (get-projection-histo histo3d ref cam width height start-plane-pos (lambda (c b h) (start-lmip c b h thresh)) bins))


