(in-package :tikz-helper)

(defclass plottingarea () 
  ((width  :initform 05 :initarg :width  :reader width)
   (height :initform 10 :initarg :height :reader height)
   (plot-x-min :initform 0 :initarg :plot-x-min :reader plot-x-min)
   (plot-x-max :initform 1 :initarg :plot-x-max :reader plot-x-max)
   (plot-y-min :initform 0 :initarg :plot-y-min :reader plot-y-min)
   (plot-y-max :initform 1 :initarg :plot-y-max :reader plot-y-max)
   (ostream :initform t :initarg :stream :accessor ostream))
  (:documentation "Contains output-stream for latex file as well as ingo needed for transformations"))

(defun make-transformation (cmax pmin pmax)
  "Makes a linear transformation from data space(pmin-pmax) to figura space 0-cmax"
  (let ((scale (/ (- pmax pmin) cmax)))
    (lambda (x) (/ (- x pmin) scale))))

(defun make-vector-transform (cmax pmin pmax)
  "Same transformation as above, but without translations"
  (let ((scale (/ (- pmax pmin) cmax)))
    (lambda (x) (/ x scale))))

(defun make-transformation-x (plottingarea)
  "Returns a transformation function from data space to figure space in the x-direction"
  (make-transformation (width plottingarea) (plot-x-min plottingarea) (plot-x-max plottingarea)))

(defun make-transformation-y (plottingarea)
  "Returns a transformation function from data space to figure space in the y-direction"
  (make-transformation (height plottingarea) (plot-y-min plottingarea) (plot-y-max plottingarea)))

(defun tikz-transform-x (plottingarea data)
  "Preform transformations on sequence of data"
  (mapcar (make-transformation-x plottingarea) data))

(defun tikz-transform-y (plottingarea data)
  "Preform transformations on sequence of data"
  (mapcar (make-transformation-y plottingarea) data))

(defun draw-plottingarea-rectangle (plottingarea)
  "Draw a thick square around the ploting area"
  (format (ostream plottingarea) "\\draw[thick] (0,0) rectangle (~a,~a);~%" (width plottingarea) (height plottingarea)))

(defun draw-axis-ticks-x (plottingarea x-list names &optional (numberp t) (precision 2) (y-pos 0))
  "Draw axis tick marks"
  (map nil (lambda (x name) 
	     (if numberp
		 (format (ostream plottingarea)
			 "\\draw (~a,~acm + 2pt) -- (~a, ~acm -2pt) node[below] {\\scriptsize{\\num[round-mode=places,round-precision=~a]{~a}}};~%"
			 x y-pos x y-pos (floor precision) name)
		 (format (ostream plottingarea)
			 "\\draw (~a,~acm + 2pt) -- (~a, ~acm-2pt) node[below] {\\scriptsize ~a};~%"
			 x y-pos x y-pos name)))
       x-list names))

(defun draw-axis-ticks-y (plottingarea x-list names &optional (numberp t) (precision 2) (x-pos 0))
  "Draw axis tick marks"
  (map nil (lambda (x name) 
	     (if numberp
		 (format (ostream plottingarea)
			 "\\draw (~acm + 2pt,~g) -- (~acm-2pt,~g) node[left] {\\scriptsize{\\num[round-mode=places,round-precision=~a]{~a}}};~%"
			 x-pos x x-pos x (floor precision) name)
		 (format (ostream plottingarea)
			 "\\draw (~acm+2pt,~g) -- (~acm-2pt,~g) node[left] {\\scriptsize ~a};~%"
			 x-pos x x-pos x name)))
       x-list names))

(defun draw-axis-ticks-x-transformed (plottingarea x-list &optional (precision 2) (y-pos 0))
  "Draw axis tick marks where the tick title is the data space value"
  (draw-axis-ticks-x plottingarea (mapcar (make-transformation-x plottingarea) x-list) x-list t precision y-pos))

(defun draw-axis-ticks-y-transformed (plottingarea y-list &optional (precision 2) (x-pos 0))
  "Draw axis tick marks where the tick title is the data space value"
  (draw-axis-ticks-y plottingarea (mapcar (make-transformation-y plottingarea) y-list) y-list t precision x-pos))

(defmacro with-tikz-plot ((name filename width height plot-x-min plot-x-max plot-y-min plot-y-max &optional (master-file "master")) &body body)
  "Macro that opens a file, makes a plotting area and opens and closes tikzpicture environment"
  (let ((stream-name (gensym)))
    `(with-open-file (,stream-name ,filename :direction :output :if-exists :supersede)
       (let ((,name (make-instance 'plottingarea :stream ,stream-name :width ,width :height ,height
				   :plot-x-min ,plot-x-min :plot-x-max ,plot-x-max
				   :plot-y-min ,plot-y-min :plot-y-max ,plot-y-max)))
	 (format ,stream-name "\\begin{tikzpicture}~%")
	 ,@body
	 (format ,stream-name "\\end{tikzpicture}~%")
	 (format ,stream-name "%%% Local Variables: ~%%%% mode: latex ~%%%% TeX-master: ~s ~%%%% End:~%~%" ,master-file)))))


(defmacro scope ((plottingarea &optional (style "")) &body body)
  "Make a tikz scope."
  `(progn
     (format (ostream ,plottingarea) "\\begin{scope}[~a]~%" ,style)
     ,@body
     (format (ostream ,plottingarea) "\\end{scope}~%")))

(defmacro clip ((plottingarea &optional x-from x y-from y) &body body)
  "Clip a rectangle from origin."
  (if (or (null x) (null y))
      `(scope (,plottingarea)
	 (format (ostream ,plottingarea) "\\clip (0,0) rectangle (~a,~a);~%" (width ,plottingarea) (height ,plottingarea))
	 ,@body)
      `(scope (,plottingarea)
	 (format (ostream ,plottingarea) "\\clip (~a,~a) rectangle (~a,~a);~%" ,x-from ,x ,y-from ,y)
	 ,@body)))

(defun make-range (min stepsize steps)
  "Returns a list with steps elements, where the first is min the next is min+stepsize etc"
  (let ((my-list nil))
    (dotimes (n (1+ steps))
      (setf my-list (append my-list (list (+ min (* n stepsize))))))
    my-list))

(defun draw-line (plottingarea x-from y-from x-to y-to style)
  "Generate tikz code to draw a line."
  (format (ostream plottingarea) "\\draw[~a] (~f,~f) -- (~f,~f);~%" style x-from y-from x-to y-to))
(defun draw-node (plottingarea x y text style)
  "Generate tikz code to draw a text node."
  (format (ostream plottingarea) "\\node[~a] at (~f,~f) {~a};~%" style x y text))
(defun draw-circle (plottingarea x y style)
  "Generate tikz code to draw a text circle."
  (format (ostream plottingarea) "\\draw[~a] (~f,~f) circle(1pt); ~%" style x y))
(defun draw-rectangle (plottingarea x-from y-from x-to y-to style)
  "Generate tikz code to draw a rectangle."
  (format (ostream plottingarea) "\\draw[~a] (~f,~f) rectangle (~f,~f);~%" style x-from y-from x-to y-to))

(defun draw-profilepoint (plottingarea x y y-error style &optional (transformp t))
  "Draw a data-point with error bars in y direction"
  (let ((xx (if transformp (apply (make-transformation-x plottingarea) (list x)) x))
	(yy (if transformp (apply (make-transformation-y plottingarea) (list y)) y))
	(yy-error (if transformp (apply (make-vector-transform (height plottingarea) 
							       (plot-y-min plottingarea) 
							       (plot-y-max plottingarea)) (list y-error)) y-error)))
    (draw-line plottingarea xx (- yy yy-error) xx (+ yy yy-error) style) 
    (draw-circle plottingarea xx yy style)
    (format (ostream plottingarea) "\\draw[~a] (~fcm -2pt,~f) -- (~fcm + 2pt,~f);~%" style xx (- yy yy-error) xx (- yy yy-error))
    (format (ostream plottingarea) "\\draw[~a] (~fcm -2pt,~f) -- (~fcm + 2pt,~f);~%" style xx (+ yy yy-error) xx (+ yy yy-error))))


(defun make-histogram (min bin-size data)
  "A histogram as a simple plist"
  (list :min min :bin-size bin-size :data data))

(defun draw-histogram-top (tikz histo style)
  "Draw the top of a histogram, no explicit separation of bins"
  (let* ((data (map 'vector (make-transformation-y tikz) (getf histo :data)))
	 (x-pos (map 'vector (make-transformation-x tikz)
		     (make-range (getf histo :min) (getf histo :bin-size) (length data)))))
    (scope (tikz style)
      (draw-line tikz  (aref x-pos 0) (aref data 0) (aref x-pos 1) (aref data 0) "")
      (dotimes (n (- (length data) 1))
	(format (ostream tikz) "\\draw (~f,~f) -- (~f,~f) -- (~f,~f);~%"
		(aref x-pos (+ n 1)) (aref data n)
		(aref x-pos (+ n 1)) (aref data (+ n 1))
		(aref x-pos (+ n 2)) (aref data (+ n 1)))))))

(defun draw-histogram (tikz histo style)
  "Draw a histogram, each bin is drawn individually"
  (let* ((data (map 'vector (make-transformation-y tikz) (getf histo :data)))
	 (x-pos (map 'vector (make-transformation-x tikz)
		     (make-range (getf histo :min) (getf histo :bin-size) (length data)))))
    (scope (tikz style)
      (dotimes (n (length data))
	(format (ostream tikz) "\\filldraw[~a] (~f,~f) -- (~f,~f) -- (~f,~f) -- (~f,~f);~%"
		""
		(aref x-pos (+ n 0)) 0
		(aref x-pos (+ n 0)) (aref data n)
		(aref x-pos (+ n 1)) (aref data n)
		(aref x-pos (+ n 1)) 0)))))
  
(defun draw-graph-line (tikz x y line-style &optional (transformp t))
  "Draw a line between a bunch of points"
  (let* ((xx (if transformp (mapcar (make-transformation-x tikz) x) x))
	 (yy (if transformp (mapcar (make-transformation-y tikz) y) y)))
    (unless (or (null (cdr x)) (null (cdr y)))
      (draw-line tikz (car xx) (car yy) (cadr xx) (cadr yy) line-style)
      (draw-graph-line tikz (cdr xx) (cdr yy) line-style nil))))

(defun draw-graph (tikz x y line-style mark-style &optional (transformp t))
  "Draw a graph, either as one circle per point, a line between points, or both"
  (let* ((xx (if transformp (mapcar (make-transformation-x tikz) x) x))
	 (yy (if transformp (mapcar (make-transformation-y tikz) y) y)))
    (if (> (length mark-style) 0) (mapcar (lambda (px py) (draw-circle tikz px py mark-style)) xx yy))
    (if (> (length line-style) 0) 
	(scope (tikz line-style)
	  (draw-graph-line tikz xx yy "" nil)))))

(defun draw-function (tikz function samples line-style &optional (x-min nil) (x-max nil))
  "Draw a function y = f(x)"
  (when (null x-min) (setf x-min (plot-x-min tikz)))
  (when (null x-max) (setf x-max (plot-x-max tikz)))
  (let* ((x-vals (make-range x-min (/ (- x-max x-min) samples) samples))
	 (y-vals (mapcar (lambda (x) (funcall function x)) x-vals)))
    (scope (tikz line-style)
      (draw-graph-line tikz x-vals y-vals "" t))))

(defun draw-legend-line (tikz x y width name line-style mark-style name-style &optional (error-style "") (error-height 0.1))
  (if (> (length mark-style) 0) (draw-circle tikz (+ (* 0.5 width) x) y mark-style))
  (if (> (length line-style) 0) (draw-line tikz x y (+ x width) y line-style))
  (if (> (length error-style) 0) (draw-profilepoint tikz (+ (* 0.5 width) x) y error-height error-style nil))
  (draw-node tikz (+ x width) y name (concatenate 'string "right," name-style)))

(defun draw-legend-rectangle (tikz x y width height name line-style fill-style name-style)
  (if (> (length fill-style) 0) (draw-rectangle tikz x (- y (* 0.5 height))
						(+ x width) (+ y (* 0.5 height)) fill-style))
  (if (> (length line-style) 0) (draw-rectangle tikz x (- y (* 0.5 height))
						(+ x width) (+ y (* 0.5 height)) line-style))
  (draw-node tikz (+ x width) y name (concatenate 'string "right," name-style)))
