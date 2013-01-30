(in-package :tikz-utils)

(defun gaussian-random ()
  "Returns two Gaussian random numbers"
  (let (x1 x2 (w 2.d0))
    (loop until (< w 1.d0) do
	 (setf x1 (- (random 2.d0) 1.d0) 
	       x2 (- (random 2.d0) 1.d0))
	 (setf w (+ (* x1 x1) (* x2 x2))))
    (setf w (sqrt (/ (* (log w) -2.d0) w)))
    (values (* x1 w) (* x2 w))))

(defun gauss (x params)
  "Gaussian function, the params are #(scale mean sigma)"
  (let* ((scale (aref params 0))
	 (mean (aref params 1))
	 (sigma (aref params 2))
	 (pull (/ (- x mean) sigma)))
    (* (/ scale (* sigma (sqrt (* 2 pi)))) (exp (* -0.5 pull pull)))))

(defun make-random-list (size)
  "returns a list of size gaussian random numbers"
  (let ((rands nil))
    (dotimes (i size) (push (gaussian-random) rands))
    rands))

(defvar tcoeff
  (list 1.00000000000000000000  0.57721566490153286061 -0.65587807152025388108
	-0.04200263503409523553  0.16653861138229148950 -0.04219773455554433675
	-0.00962197152787697356  0.00721894324666309954 -0.00116516759185906511
	-0.00021524167411495097  0.00012805028238811619 -0.00002013485478078824
	-0.00000125049348214267  0.00000113302723198170 -0.00000020563384169776
	0.00000000611609510448  0.00000000500200764447 -0.00000000118127457049
	0.00000000010434267117  0.00000000000778226344 -0.00000000000369680562
	0.00000000000051003703 -0.00000000000002058326 -0.00000000000000534812
	0.00000000000000122678 -0.00000000000000011813  0.00000000000000000119
	0.00000000000000000141 -0.00000000000000000023  0.00000000000000000002))
 
; number of coefficients
(defconstant numcoeff 30)

(defun gamma (x)
  "Gamma function"
  (if (< (abs (- x 7)) 0.001)
      720.d0
      (let ((y (- x 1.0))
	    (sum (coerce (nth (- numcoeff 1) tcoeff) 'double-float)))
	(loop for i from (- numcoeff 2) downto 0 do 
	     (setf sum (+ (* sum y) (nth i tcoeff))))
	(/ 1.0d0 sum))))

(defun erf (x)
  "Error function"
  (let* ((sign (if (>= x 0) 1 -1))
	 (x (abs x))
	 (a1  0.254829592)
	 (a2 -0.284496736)
	 (a3  1.421413741)
	 (a4 -1.453152027)
	 (a5  1.061405429)
	 (p   0.3275911)
	 (tc (/ 1.0 (+ 1.0 (* p x))))
	 (y (- 1.0
	       (* tc (exp (* x x -1.0d0))
		  (+ a1 (* tc (+ a2 (* tc (+ a3 (* tc (+ a4 (* a5 tc))))))))))))
    (* sign y)))
