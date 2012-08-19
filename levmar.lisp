#|
Implementation of Levenberg Marquart algorithm, by someone with limitedknowledge of both the algorithm and lla by t.papp.
From:
Methods for Non-Linear Least Squares Problems by K. Madsen, H.B. Nielsen, O. Tingleff
http://www2.imm.dtu.dk/pubdb/views/edoc_download.php/3215/pdf/imm3215.pdf
|#

(in-package :tikz-helper)
;(defparameter *epsillon-1* 0.0000000001d0 "Convergance parameter")
(defparameter *epsillon-2* 0.00000000001d0 "Convergance parameter")
(defparameter *tau* 1.0d-8)
(defparameter kmax 1000)
  
(defun get-diff-vector (function parameters step param)
  "Extract numerical partial derivates using the three point rule."
  (flet ((step-and-call (step)
	   (setf (aref parameters param) (+ (aref parameters param) step))
	   (prog1 (funcall function parameters)
	     (setf (aref parameters param) (- (aref parameters param) step)))))
    (let ((plus-h (step-and-call step))
	  (minus-h (step-and-call (* -1 step))))
      (map 'vector (lambda (x y) (/ (- x y) (* 2 step))) plus-h minus-h))))

(defun get-J (function parameters nparameters nmeas)
  "Get jacobian and its transverse."
  (let ((J  (lla:make-matrix 'lla:dense nmeas nparameters :element-type 'double-float))
	(JT (lla:make-matrix 'lla:dense nparameters nmeas :element-type 'double-float)))
    (dotimes (i nparameters)
      (let ((diffs (get-diff-vector function parameters (max (abs (* (aref parameters i) 0.001)) 0.000001) i)))
	(dotimes (m nmeas)
	  (setf (lla:mref J m i) (aref diffs m)
		(lla:mref JT i m) (aref diffs m)))))
    (values J JT)))
    
(defun get-g (function parameters JT nmeas)
  "g = J^T f"
  (let ((g (lla:make-matrix 'lla:dense nmeas 1 :element-type 'double-float))
	(funres (funcall function parameters)))
    (dotimes (i nmeas)
      (setf (lla:mref g i 0) (aref funres i)))
    (lla:mm JT g)))
    
(defun get-vector-abs (vec)
  "||vec||"
  (sqrt (reduce #'+ (map 'vector (lambda (x) (* x x)) vec))))

(defun L0-Lhm (h g mu)
  "L(0) - L(H_m)"
  (let ((ht (lla:make-matrix 'lla:dense 1 4))
	(hg (lla:make-matrix 'lla:dense 4 1)))
    (dotimes (i (length h))
      (setf (lla:mref ht 0 i) (aref h i))
      (setf (lla:mref hg i 0) (- (* mu (aref h i)) (lla:mref g i 0))))
    (aref (lla:mm ht hg) 0 0)))

(defun add-mu (A mu dim)
  (let ((Amu (lla:make-matrix 'lla:dense dim dim :element-type 'double-float)))
    (dotimes (i dim)
      (dotimes (j dim)
	(setf (lla:mref Amu i j) (lla:mref A i j)))
      (setf (lla:mref Amu i i) (+ (lla:mref Amu i i) mu)))
    Amu))
	
(defun levmar-update (function iteration mu nu params nparams nmeas)
  "Updating estimate, preparing next iretarion"
  (multiple-value-bind (J JT) (get-J function params nparams nmeas)
    (let ((A (lla:mm JT J)))
      (when (null mu)
	(setf mu 0.d0)
	(dotimes (i nparams)
	  (dotimes (j nparams)
	    (when (> (lla:mref A i j) mu)
	      (setf mu (lla:mref A i j)))))
	(setf mu (* *tau* mu)))
      (levmar-iterate function params iteration A (get-g function params JT nmeas)
			    mu nu nparams nmeas))))
  
(defun as-array (matrix nparams)
  "Get array from matrix vector"
  (let ((array (make-array nparams :element-type 'double-float)))
    (dotimes (i nparams)
      (setf (aref array i) (lla:mref matrix i 0)))
    array))

(defun scale-matrix (matrix nparams scale)
  (let ((matrix2 (lla:make-matrix 'lla:dense nparams 1 :element-type 'double-float)))
    (dotimes (i nparams)
      (setf (lla:mref matrix2 i 0) (* scale (lla:mref matrix i 0))))
    matrix2))
      
(defun levmar-iterate (function parameters iteration A g mu nu nparam nmeas)
  "An iteration in levenberg marquart."
  (when (> mu 1.0d15) (setf mu 10000.d0 nu 2))
  (if  (= (mod iteration 10) 0) (format t "iteration ~a, mu=~a , params=~a ~%" iteration mu parameters))
  (let* ((A-prime (add-mu A mu nparam))
	 (h (as-array (lla:solve A-prime (scale-matrix g nparam -1.0)) nparam))
	 (params (map 'vector #'+ parameters h))
	 (Q (/ (- 
		(reduce #'+ (map 'vector (lambda (x) (* x x)) (funcall function parameters)))
		(reduce #'+ (map 'vector (lambda (x) (* x x)) (funcall function params))))
	       (+ (L0-Lhm h g mu) 0.0000000000000000001))));vector sum of sorts
    (cond ((< (get-vector-abs h) (* *epsillon-2* (+ (get-vector-abs parameters) *epsillon-2*))) 
	   (progn (format t "Converged after ~a iterations!~% ~a~%" iteration parameters) parameters))
	  ((>= iteration kmax) (progn (format t "Reached maximum number of iterations") parameters))
	  ((<= Q 0) (levmar-iterate function parameters (+ 1 iteration) A g (* mu nu) (* 1.1 nu) nparam nmeas))
	  ((> Q 0) (levmar-update function (+ 1 iteration)
					(* mu (max (/ 1.0d0 3.0d0) (- 1 (expt (- (* 2 Q) 1) 3))))
					2 params nparam nmeas)))))
    
(defun levmar-optimize (function parameters meas-x meas-y)
  "Find the parameters that makes function closest to measurement.
Function must be a function (lambda x parameters) that returns a double-float."
  (levmar-update (lambda (params) (map 'vector (lambda (x-pos y-pos) (- y-pos (funcall function x-pos params))) meas-x meas-y))
		       0 nil 2.0d0 parameters (length parameters) (min (length meas-x) (length meas-y))))

(defun levmar-optimize-errors (function parameters meas-x meas-y error-y)
  "Find the parameters that makes function closest to measurement.
Function must be a function (lambda x parameters) that returns a double-float.
I am not sure this is legal."
  (levmar-update (lambda (params) (map 'vector (lambda (x-pos y-pos err) (/ (- y-pos (funcall function x-pos params)) (+ err double-float-epsilon))) meas-x meas-y error-y))
		       0 nil 2.0d0 parameters (length parameters) (min (length meas-x) (length meas-y))))
