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

(defun draw-axis-ticks-x (plottingarea x-list names &optional (numberp t) (precision 2) (y-pos 0) (pt- 2) (pt+ 2) (style "below"))
  "Draw axis tick marks"
  (map nil (lambda (x name) 
	     (if numberp
		 (format (ostream plottingarea)
			 "\\draw (~f,~fcm + ~apt) -- (~f, ~fcm -~apt) node[below] {\\scriptsize{\\num[round-mode=places,round-precision=~a]{~a}}};~%"
			 x y-pos pt+ x y-pos pt- (floor precision) name)
		 (format (ostream plottingarea)
			 "\\draw (~f,~fcm + ~apt) -- (~f, ~fcm-~apt) node[~a] {\\scriptsize ~a};~%"
			 x y-pos pt+ x y-pos pt- style name)))
       x-list names))

(defun draw-axis-ticks-y (plottingarea x-list names &optional (numberp t) (precision 2) (x-pos 0) (pt- 2) (pt+ 2) (style "left"))
  "Draw axis tick marks"
  (map nil (lambda (x name) 
	     (if numberp
		 (format (ostream plottingarea)
			 "\\draw (~fcm + ~apt,~f) -- (~fcm-~apt,~f) node[~a] {\\scriptsize{\\num[round-mode=places,round-precision=~a]{~a}}};~%"
			 x-pos pt+ x x-pos pt- x style (floor precision) name)
		 (format (ostream plottingarea)
			 "\\draw (~fcm+~apt,~f) -- (~fcm-~apt,~f) node[~a] {\\scriptsize ~a};~%"
			 x-pos pt+ x x-pos pt- x style name)))
       x-list names))

(defun draw-axis-ticks-x-transformed (plottingarea x-list &optional (precision 2) (y-pos 0) (pt- 2) (pt+ 2))
  "Draw axis tick marks where the tick title is the data space value"
  (draw-axis-ticks-x plottingarea (mapcar (make-transformation-x plottingarea) x-list) x-list t precision y-pos pt- pt+))

(defun draw-axis-ticks-y-transformed (plottingarea y-list &optional (precision 2) (x-pos 0) (pt- 2) (pt+ 2))
  "Draw axis tick marks where the tick title is the data space value"
  (draw-axis-ticks-y plottingarea (mapcar (make-transformation-y plottingarea) y-list) y-list t precision x-pos pt- pt+))

(defmacro with-tikz-plot-standalone ((name filename width height plot-x-min plot-x-max plot-y-min plot-y-max) &body body)
  "Macro that opens a file, makes a plotting area and opens and closes tikzpicture environment"
  (let ((stream-name (gensym)))
    `(with-open-file (,stream-name ,filename :direction :output :if-exists :supersede)
       (let ((,name (make-instance 'plottingarea :stream ,stream-name :width ,width :height ,height
				   :plot-x-min ,plot-x-min :plot-x-max ,plot-x-max
				   :plot-y-min ,plot-y-min :plot-y-max ,plot-y-max)))
	 (format ,stream-name "\\documentclass{standalone}~%\\usepackage{tikz}~%\\usepackage{color}~%\\usepackage{siunitx}~%\\begin{document}~%\\begin{tikzpicture}~%")
	 ,@body
	 (format ,stream-name "\\end{tikzpicture}~%\\end{document}~%")))))
  

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
	 (format (ostream ,plottingarea) "\\clip (~a,~a) rectangle (~a,~a);~%" ,x-from ,y-from ,x ,y)
	 ,@body)))

(defun make-range (min stepsize steps)
  "Returns a list with steps elements, where the first is min the next is min+stepsize etc"
  (let ((my-list nil))
    (dotimes (n (1+ steps))
      (setf my-list (append my-list (list (+ min (* n stepsize))))))
    my-list))


(defun make-node-string (shape width height &optional (inner-sep 0))
  "Make a node string"
  (format nil "~a,inner sep=~apt,minimum width =~apt,minimum height=~apt"
	  shape inner-sep width height))
(defun draw-line (plottingarea x-from y-from x-to y-to style)
  "Generate tikz code to draw a line."
  (format (ostream plottingarea) "\\draw[~a] (~f,~f) -- (~f,~f);~%" style x-from y-from x-to y-to))
(defun draw-text-node (plottingarea x y text style)
  "Generate tikz code to draw a text node."
  (format (ostream plottingarea) "\\node[~a] at (~f,~f) {~a};~%" style x y text))
(defun draw-node (tikz x y style node-string &optional (text ""))
  "Draw a node at point."
  (format (ostream tikz) "\\node at (~f,~f) [~a] {~a}; ~%" x y (concatenate 'string node-string "," style) text))
(defun draw-circle (plottingarea x y style)
  "Generate tikz code to draw a circle."
  (draw-node plottingarea x y style (make-node-string "circle" 2 2)))
(defun draw-rectangle (plottingarea x-from y-from x-to y-to style)
  "Generate tikz code to draw a rectangle."
  (format (ostream plottingarea) "\\draw[~a] (~f,~f) rectangle (~f,~f);~%" style x-from y-from x-to y-to))

(defun draw-profilepoints (plottingarea x y y-error style &optional (transformp t))
  (mapcar (lambda (xx yy err) (draw-profilepoint plottingarea xx yy err style transformp)) x y y-error))

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

(defun make-histogram-path (histo)
  (let* ((y-pos (getf histo :data))
	 (x-pos (make-range (getf histo :min) (getf histo :bin-size) (length y-pos)))
	 (path-x nil)
	 (path-y nil))
    (push (elt x-pos 0) path-x)
    (push 0 path-y)
    (dotimes (i (- (length x-pos) 1))
      (push (elt x-pos i) path-x)
      (push (elt y-pos i) path-y)
      (push (elt x-pos (+ 1 i)) path-x)
      (push (elt y-pos i) path-y))
    (push (elt x-pos (- (length x-pos) 1)) path-x)
    (push 0 path-y)
    (values path-x path-y)))

(defun draw-histogram-horizontal (tikz histo style &optional (fill nil) (separate-bins nil))
  "Draw a histogram"
  (multiple-value-bind (y x) (make-histogram-path histo)
    (draw-path tikz x y style t fill)
    (when separate-bins 
      (mapcar (lambda (x y) (draw-path tikz (list 0 x) (list y y) style t nil)) x y))))

(defun draw-histogram (tikz histo style &optional (fill nil) (separate-bins nil))
  "Draw a histogram"
  (multiple-value-bind (x y) (make-histogram-path histo)
    (draw-path tikz x y style t fill)
    (when separate-bins 
      (mapcar (lambda (x y) (draw-path tikz (list x x) (list y 0) style t nil)) x y))))

(defun draw-histogram-bins (tikz histo style)
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
  
(defun add-path-point (tikz x y)
  (unless (or (null x) (null y))
    (format (ostream tikz) "\\pgfpathlineto{ \\pgfqpoint {~fcm} {~fcm}}~%" (car x) (car y))
    (add-path-point tikz (cdr x) (cdr y))))

(defun draw-path (tikz x y style &optional (transformp t) (fill nil))
  "Connect data points with straight lines."
  (let* ((xx (if transformp (mapcar (make-transformation-x tikz) x) x))
	 (yy (if transformp (mapcar (make-transformation-y tikz) y) y)))
    (scope (tikz style)
      (format (ostream tikz) "\\pgfpathmoveto{ \\pgfqpoint {~fcm} {~fcm}}~%" (car xx) (car yy))
      (add-path-point tikz (cdr xx) (cdr yy))
      (if fill
	  (format (ostream tikz) "\\pgfusepath{ fill,stroke }~%")
	  (format (ostream tikz) "\\pgfusepath{ stroke }~%")))))

(defun draw-graph-error (tikz x y y-error line-style mark-style error-style &optional (transformp t))
  "Draw error bars"
  (draw-graph tikz x y line-style mark-style transformp)
  (let* ((xx (if transformp (mapcar (make-transformation-x tikz) x) x))
	 (yy (if transformp (mapcar (make-transformation-y tikz) y) y))
	 (err (if transformp (mapcar (make-vector-transform (height tikz) (plot-y-min tikz) (plot-y-max tikz)) y-error) y-error)))
    (mapcar (lambda (x y er) (draw-profilepoint tikz x y er error-style)) xx yy err)))

(defun draw-datapoints (tikz x y style &optional (transformp t) (node (make-node-string "circle" 2 2)))
  "Draw a set of datapoints"
  (let* ((xx (if transformp (map 'list (make-transformation-x tikz) x) x))
	 (yy (if transformp (map 'list (make-transformation-y tikz) y) y)))
    (mapcar (lambda (x y) (draw-node tikz x y style node)) xx yy)))

(defun draw-graph (tikz x y line-style mark-style &optional (node (make-node-string "circle" 2 2)))
  "Draw a graph, with a line connecting datapoints"
  (draw-path tikz x y line-style)
  (draw-datapoints tikz x y mark-style t node))

(defun draw-graph-spline (tikz x y line-style mark-style  &optional (node (make-node-string "circle" 2 2)))
  "Draw a graph, with a spline connecting datapoints"
  (let ((n  (min (length x) (length y))))
    (draw-function tikz (get-spline-fun x y) 100 line-style (elt x 0) (elt x (- n 1))))
  (draw-datapoints tikz x y mark-style t node))

(defun draw-function (tikz function samples line-style &optional (x-min nil) (x-max nil))
  "Draw a function y = f(x)"
  (when (null x-min) (setf x-min (plot-x-min tikz)))
  (when (null x-max) (setf x-max (plot-x-max tikz)))
  (let* ((x-vals (make-range x-min (/ (- x-max x-min) samples) samples))
	 (y-vals (mapcar (lambda (x) (funcall function x)) x-vals)))
    (draw-path tikz x-vals y-vals line-style t)))

(defun draw-legend-line (tikz x y width name line-style &optional (mark-style "") (node-string (make-node-string "circle" 2 2)) (name-style "") 
							  (error-style "") (error-height 0.1))
  "Draw a legent entry for a plot, with a line, and or marks with or without error bars. For graphs, functions, datapoints, most histograms"
  (if (> (length line-style) 0) (draw-line tikz x y (+ x width) y line-style))
  (if (> (length error-style) 0) (draw-profilepoint tikz (+ (* 0.5 width) x) y error-height error-style nil))
  (if (> (length mark-style) 0) (draw-node tikz (+ (* 0.5 width) x) y mark-style node-string))
  (draw-text-node tikz (+ x width) y name (concatenate 'string "right," name-style)))

(defun draw-legend-rectangle (tikz x y width height name line-style fill-style name-style)
  "Draw a (filled) rectangle with a legend entry. This is for histograms drawn with draw-histogram-bins"
  (if (> (length fill-style) 0) (draw-rectangle tikz x (- y (* 0.5 height))
						(+ x width) (+ y (* 0.5 height)) fill-style))
  (if (> (length line-style) 0) (draw-rectangle tikz x (- y (* 0.5 height))
						(+ x width) (+ y (* 0.5 height)) line-style))
  (draw-text-node tikz (+ x width) y name (concatenate 'string "right," name-style)))
