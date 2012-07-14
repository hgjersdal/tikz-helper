(in-package :tikz-helper)

(defun gaussian-random ()
  (let (x1 x2 (w 2.d0))
    (loop until (< w 1.d0) do
	 (setf x1 (- (random 2.d0) 1.d0) 
	       x2 (- (random 2.d0) 1.d0))
	 (setf w (+ (* x1 x1) (* x2 x2))))
    (setf w (sqrt (/ (* (log w) -2.d0) w)))
    (values (* x1 w) (* x2 w))))

(defun gauss (x params)
  (let* ((scale (aref params 0))
	 (mean (aref params 1))
	 (sigma (aref params 2))
	 (pull (/ (- x mean) sigma)))
    (* (/ scale (* sigma (sqrt (* 2 pi)))) (exp (* -0.5 pull pull)))))
	
