#|
Cubic spline interpolation
Algorithms from
http://people.math.sfu.ca/~stockie/teaching/macm316/notes/splines.pdf
by John Stockie
|#

(in-package :tikz-spline)

(defun get-h (x)
  "Get the n-1 long vector h"
  (map 'vector (lambda (xi xi+1) (- xi+1 xi)) x (subseq x 1)))

(defun fill-matrix (h n matrix)
  "Fill matrix describing the linear equations from row 1 to n-1. Same for not-a-knot and natural spline"
  (loop 
     for i from 1 below (- n 1)
     do
       (setf (aref matrix i (- i 1)) (aref h (- i 1)))
       (setf (aref matrix i i)       (* 2 (+ (aref h i) (aref h (- i 1)))))
       (setf (aref matrix i (+ i 1)) (aref h i)))
  matrix)

(defun make-not-a-know-matrix (h n)
  "Make a matrix describing the not a knot system of equations"
  (let ((matrix (make-array (list n n) :initial-element 0.0d0 :element-type 'double-float)))
    ;;top row
    (setf (aref matrix 0 0) -1.0d0)
    (setf (aref matrix 0 1)  2.0d0)
    (setf (aref matrix 0 2) -1.0d0)
    ;;bottom row
    (setf (aref matrix (- n 1) (- n 3)) -1.0d0)
    (setf (aref matrix (- n 1) (- n 2))  2.0d0)
    (setf (aref matrix (- n 1) (- n 1)) -1.0d0)
    (fill-matrix h n matrix)))

(defun make-natural-spline-matrix (h n)
  "Make a matrix describing the natural spline system of equations"
  (let ((matrix (make-array (list n n) :initial-element 0.0d0 :element-type 'double-float)))
    (setf (aref matrix 0 0) 1.0d0)
    (setf (aref matrix (- n 1) (- n 1)) 1.0d0)
    (fill-matrix h n matrix)))

(defun make-right-hand-side (y h n)
  "Right hand side for the system of spline equations."
  (let ((rhs (make-array (list n 1) :element-type 'double-float :initial-element 0.0d0)))
    (setf (aref rhs 0 0) 0.0d0)
    (setf (aref rhs (- n 1) 0) 0.0d0)
    (loop for i from 1 below (- n 1) do
	 (setf (aref rhs i 0) (* 6.0d0 (- (/ (- (aref y (+ i 1)) (aref y i)) (aref h i))
					      (/ (- (aref y i) (aref y (- i 1))) (aref h (- i 1)))))))
    rhs))

(defun get-coeffs (h y m n)
  "Coefficients used for calculating the spline segments"
  (let ((a (make-array n :element-type 'double-float :initial-element 0.0d0))
	(b (make-array n :element-type 'double-float :initial-element 0.0d0))
	(c (make-array n :element-type 'double-float :initial-element 0.0d0))
	(d (make-array n :element-type 'double-float :initial-element 0.0d0)))
    (loop for i from 0 below (- n 1) do
	 (setf (aref a i) (aref y i))
	 (setf (aref b i) (+ (/ (- (aref y (+ i 1)) (aref y i)) (aref h i))
			     (- (/ (* (aref h i) (aref m i 0)) 2.0d0))
			     (- (/ (* (aref h i) (- (aref m (+ i 1) 0) (aref m i 0))) 6.0d0))))
	 (setf (aref c i) (/ (aref m i 0) 2.0d0))
	 (setf (aref d i) (/ (- (aref m (+ i 1) 0) (aref m i 0)) (* 6.0d0 (aref h i)))))
    (values a b c d)))

(defun select-region (x xx n)
  "Figure out which spline segment to draw."
  (loop
     for i from 1 below n
     when (<= xx (aref x i)) do
       (return (- i 1))
     finally (return (- n 2))))
  
(defun get-spline-fun (x y &optional (natural nil))
  "Returns the spline function fitted to x and y. If natural is T, get a natural cubic spline. If natural is NIL, a cubic spline with not-a-knot."
  (let ((x (map 'vector (lambda (x) (float x 1.d0)) x))
	(y (map 'vector (lambda (x) (float x 1.d0)) y))
	(n (min (length x) (length y))))
    (let* ((h (get-h x))
	   (ms (lla:solve (if natural 
			      (make-natural-spline-matrix h n)
			      (make-not-a-know-matrix h n)) 
			  (make-right-hand-side y h n))))
      (multiple-value-bind (a b c d) (get-coeffs h y ms n)
	(lambda (xx)
	  (let* ((index (select-region x xx n))
		 (x-diff (- xx (aref x index))))
	    (+ (aref a index)
	       (* (aref b index) x-diff)
	       (* (aref c index) x-diff x-diff)
	       (* (aref d index) x-diff x-diff x-diff))))))))
  
