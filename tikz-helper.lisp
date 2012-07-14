(in-package :tikz-helper)

(defclass plottingarea ()
  ((width  :initform 05 :initarg :width  :reader width)
   (height :initform 10 :initarg :height :reader height)
   (plot-x-min :initform 0 :initarg :plot-x-min :reader plot-x-min)
   (plot-x-max :initform 1 :initarg :plot-x-max :reader plot-x-max)
   (plot-y-min :initform 0 :initarg :plot-y-min :reader plot-y-min)
   (plot-y-max :initform 1 :initarg :plot-y-max :reader plot-y-max)
   (ostream :initform t :initarg :stream :accessor ostream)))

(defun make-transformation (cmax pmin pmax)
  (let ((scale (/ (- pmax pmin) cmax)))
    (lambda (x) (/ (- x pmin) scale))))

(defun make-vector-transform (cmax pmin pmax)
  (let ((scale (/ (- pmax pmin) cmax)))
    (lambda (x) (/ x scale))))

(defun make-transformation-x (plottingarea)
  (make-transformation (width plottingarea) (plot-x-min plottingarea) (plot-x-max plottingarea)))

(defun make-transformation-y (plottingarea)
  (make-transformation (height plottingarea) (plot-y-min plottingarea) (plot-y-max plottingarea)))

(defun tikz-transform-x (plottingarea data)
  (mapcar (make-transformation-x plottingarea) data))

(defun tikz-transform-y (plottingarea data)
  (mapcar (make-transformation-y plottingarea) data))

(defun draw-plottingarea-rectangle (plottingarea)
  (format (ostream plottingarea) "\\draw[thick] (0,0) rectangle (~a,~a);~%" (width plottingarea) (height plottingarea)))

(defun draw-axis-ticks-x (plottingarea x-list names &optional (numberp t) (precision 2))
  (map nil (lambda (x name) 
	     (if numberp
		 (format (ostream plottingarea)
			 "\\draw (~a,2pt) -- (~a, -1pt) node[below] {\\scriptsize{\\num[round-mode=places,round-precision=~a]{~a}}};~%"
			 x x (floor precision) name)
		 (format (ostream plottingarea)
			 "\\draw (~a,2pt) -- (~a, -1pt) node[below] {\\scriptsize ~a};~%"
			 x x name)))
       x-list names))

(defun draw-axis-ticks-y (plottingarea x-list names &optional (numberp t) (precision 2))
  (map nil (lambda (x name) 
	     (if numberp
		 (format (ostream plottingarea)
			 "\\draw (2pt,~g) -- (-1pt,~g) node[left] {\\scriptsize{\\num[round-mode=places,round-precision=~a]{~a}}};~%"
			 x x (floor precision) name)
		 (format (ostream plottingarea)
			 "\\draw (2pt,~g) -- (-1pt,~g) node[left] {\\scriptsize ~a};~%"
			 x x name)))
       x-list names))

(defun draw-axis-ticks-x-transformed (plottingarea x-list &optional (precision 2))
  (draw-axis-ticks-x plottingarea (mapcar (make-transformation-x plottingarea) x-list) x-list t precision))

(defun draw-axis-ticks-y-transformed (plottingarea y-list &optional (precision 2))
  (draw-axis-ticks-y plottingarea (mapcar (make-transformation-y plottingarea) y-list) y-list t precision))

(defmacro with-tikz-plot ((name filename width height plot-x-min plot-x-max plot-y-min plot-y-max) &body body)
  (let ((stream-name (gensym)))
    `(with-open-file (,stream-name ,filename :direction :output :if-exists :supersede)
       (let ((,name (make-instance 'plottingarea :stream ,stream-name :width ,width :height ,height
				   :plot-x-min ,plot-x-min :plot-x-max ,plot-x-max
				   :plot-y-min ,plot-y-min :plot-y-max ,plot-y-max)))
	 (format ,stream-name "\\begin{tikzpicture}~%")
	 ,@body
	 (format ,stream-name "\\end{tikzpicture}~%")
	 (format ,stream-name "%%% Local Variables: ~%%%% mode: latex ~%%%% TeX-master: \"master\" ~%%%% End:~%~%")))))

(defmacro clip ((plottingarea) &body body)
  `(progn
     (format (ostream ,plottingarea) "\\begin{scope}~%")
     (format (ostream ,plottingarea) "\\clip (0,0) rectangle (~a,~a);~%" (width ,plottingarea) (height ,plottingarea))
     ,@body
     (format (ostream ,plottingarea) "\\end{scope}~%")))

(defun make-range (min stepsize steps)
  (let ((my-list nil))
    (dotimes (n (1+ steps))
      (setf my-list (append my-list (list (+ min (* n stepsize))))))
    my-list))

(defun draw-line (plottingarea x-from y-from x-to y-to style)
  (format (ostream plottingarea) "\\draw[~a] (~f,~f) -- (~f,~f);~%" style x-from y-from x-to y-to))
(defun draw-node (plottingarea x y text style)
  (format (ostream plottingarea) "\\node[~a] at (~f,~f) {~a};~%" style x y text))
(defun draw-vert-2pt (plottingarea x y style)
  (format (ostream plottingarea) "\\draw[~a] (~f,~fcm+1pt) -- (~f,~fcm - 1pt); ~%" style x y x y))
(defun draw-circle (plottingarea x y style)
  (format (ostream plottingarea) "\\draw[~a] (~f,~f) circle(1pt); ~%" style x y))
(defun draw-rectangle (plottingarea x-from y-from x-to y-to style)
  (format (ostream plottingarea) "\\draw[~a] (~f,~f) rectangle (~f,~f);~%" style x-from y-from x-to y-to))

(defun draw-profilepoint (plottingarea x y y-error style)
  (let ((yy (apply (make-transformation-y plottingarea) (list y)))
	(yy-error (apply (make-vector-transform (height plottingarea) (plot-y-min plottingarea)
						(plot-y-max plottingarea)) (list y-error))))
    (draw-line plottingarea x (- yy yy-error) x (+ yy yy-error) style)
    (draw-circle plottingarea x yy style)
    (format (ostream plottingarea) "\\draw[~a] (~fcm -2pt,~f) -- (~fcm + 2pt,~f);~%" style x (- yy yy-error) x (- yy yy-error))
    (format (ostream plottingarea) "\\draw[~a] (~fcm -2pt,~f) -- (~fcm + 2pt,~f);~%" style x (+ yy yy-error) x (+ yy yy-error))))


(defun make-histogram (min bin-size data)
  (list :min min :bin-size bin-size :data data))

(defun draw-histogram-top (tikz histo style)
  (let* ((data (map 'vector (make-transformation-y tikz) (getf histo :data)))
	 (x-pos (map 'vector (make-transformation-x tikz)
		     (make-range (getf histo :min) (getf histo :bin-size) (length data)))))
    (draw-line tikz  (aref x-pos 0) (aref data 0) (aref x-pos 1) (aref data 0) style)
    (dotimes (n (- (length data) 1))
      (format (ostream tikz) "\\draw[~a] (~f,~f) -- (~f,~f) -- (~f,~f);~%"
	      style
	      (aref x-pos (+ n 1)) (aref data n)
	      (aref x-pos (+ n 1)) (aref data (+ n 1))
	      (aref x-pos (+ n 2)) (aref data (+ n 1))))))

(defun draw-histogram (tikz histo style)
  (let* ((data (map 'vector (make-transformation-y tikz) (getf histo :data)))
	 (x-pos (map 'vector (make-transformation-x tikz)
		     (make-range (getf histo :min) (getf histo :bin-size) (length data)))))
    (dotimes (n (length data))
      (format (ostream tikz) "\\filldraw[~a] (~f,~f) -- (~f,~f) -- (~f,~f) -- (~f,~f);~%"
	      style
	      (aref x-pos (+ n 0)) 0
	      (aref x-pos (+ n 0)) (aref data n)
	      (aref x-pos (+ n 1)) (aref data n)
	      (aref x-pos (+ n 1)) 0))))

(defun draw-graph-line (tikz x y line-style &optional (transformp t))
  (let* ((xx (if transformp (mapcar (make-transformation-x tikz) x) x))
	 (yy (if transformp (mapcar (make-transformation-y tikz) y) y)))
    (unless (or (null (cdr x)) (null (cdr y)))
      (draw-line tikz (car xx) (car yy) (cadr xx) (cadr yy) line-style)
      (draw-graph-line tikz (cdr xx) (cdr yy) line-style nil))))

(defun draw-graph (tikz x y line-style mark-style &optional (transformp t))
  (let* ((xx (if transformp (mapcar (make-transformation-x tikz) x) x))
	 (yy (if transformp (mapcar (make-transformation-y tikz) y) y)))
    (if (> (length mark-style) 0) (mapcar (lambda (px py) (draw-circle tikz px py mark-style)) xx yy))
    (if (> (length line-style) 0) (draw-graph-line tikz xx yy line-style nil))))

(defun draw-function (tikz function samples line-style)
  (let* ((x-vals (make-range (plot-x-min tikz) (/ (- (plot-x-max tikz) (plot-x-min tikz)) samples) samples))
	 (y-vals (mapcar (lambda (x) (funcall function x)) x-vals)))
    (draw-graph-line tikz x-vals y-vals line-style t)))

(defun draw-legend-point (tikz x y width line-style mark-style)
  (draw-circle tikz x y mark-style)
  (draw-line tikz (- x (* 0.5 width)) y (+ x (* 0.5 width)) y line-style))

(defun draw-legend-line (tikz x y width name line-style mark-style name-style)
  (if (> (length mark-style) 0) (draw-circle tikz (+ (* 0.5 width) x) y mark-style))
  (if (> (length line-style) 0) (draw-line tikz x y (+ x width) y line-style))
  (draw-node tikz (+ x width) y name (concatenate 'string "right," name-style)))

(defun draw-legend-rectangle (tikz x y width height name line-style fill-style name-style)
  (if (> (length fill-style) 0) (draw-rectangle tikz x (- y (* 0.5 height))
						(+ x width) (+ y (* 0.5 height)) fill-style))
  (if (> (length line-style) 0) (draw-rectangle tikz x (- y (* 0.5 height))
						(+ x width) (+ y (* 0.5 height)) line-style))
  (draw-node tikz (+ x width) y name (concatenate 'string "right," name-style)))
