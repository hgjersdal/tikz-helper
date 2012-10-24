(in-package :tikz-helper)

(defclass plottingarea ()
  ((width  :initform 05 :initarg :width  :reader width)
   (height :initform 10 :initarg :height :reader height)
   (x-offset  :initform 00 :initarg :x-offset  :reader x-offset)
   (y-offset  :initform 00 :initarg :y-offset  :reader y-offset)
   (plot-x-min :initform 0 :initarg :plot-x-min :reader plot-x-min)
   (plot-x-max :initform 1 :initarg :plot-x-max :reader plot-x-max)
   (plot-y-min :initform 0 :initarg :plot-y-min :reader plot-y-min)
   (plot-y-max :initform 1 :initarg :plot-y-max :reader plot-y-max)
   (ostream :initform t :initarg :stream :accessor ostream))
  (:documentation "Contains output-stream for latex file as well as ingo needed for transformations"))

(defmacro latex-environ ((plottingarea environ &optional (args "")) &body body)
  `(progn
     (format (ostream ,plottingarea) "\\begin{~a}[~a]~%" ,environ ,args)
     ,@body
     (format (ostream ,plottingarea) "\\end{~a}~%" ,environ)))

(defun make-transformation (cmax pmin pmax)
  "Makes a linear transformation from data space(pmin-pmax) to figura space 0-cmax"
  (let ((scale (/ (- pmax pmin) cmax)))
    (lambda (x) (/ (- x pmin) scale))))

(defun make-vector-transform (cmax pmin pmax)
  "Same transformation as above, but without translations"
  (let ((scale (/ (- pmax pmin) cmax)))
    (lambda (x) (/ x scale))))

;; (x - min / scale = val
;; val * scale = x - min
;; val * scale + min

(defun apply-transform-x (tikz x)
  (let ((scale (/ (- (plot-x-max tikz) (plot-x-min tikz)) (width tikz))))
    (/ (- x (plot-x-min tikz)) scale)))
	
(defun apply-transform-y (tikz y)
  (let ((scale (/ (- (plot-y-max tikz) (plot-y-min tikz)) (height tikz))))
    (/ (- y (plot-y-min tikz)) scale)))
;;    (+ (plot-y-min tikz) (* y scale))))

(defun draw-plottingarea-rectangle (plottingarea &optional (fill nil) (style "thick,black"))
  "Draw a thick square around the ploting area"
  (format (ostream plottingarea) (if fill 
				     "\\draw[~a,fill=white] (~f,~f) rectangle (~f,~f);~%"
				     "\\draw[~a] (~f,~f) rectangle (~f,~f);~%")
	  style
	  (x-offset plottingarea) (y-offset plottingarea)
	  (+ (width plottingarea) (x-offset plottingarea))
	  (+ (height plottingarea) (y-offset plottingarea))))

(defun draw-tick-mark (plottingarea numberp precision name style text-style x y xpt+ xpt- ypt+ ypt-)
  (format (ostream plottingarea)
	  "\\draw[~a] [shift={(~f,~f)}] (~a,~a) -- (~a,~a) node[~a]{ \\scriptsize{"
	  style x y xpt+ ypt+ xpt- ypt- text-style)
  (if numberp
      (format (ostream plottingarea)
	      "\\num[round-mode=places,round-precision=~a]{~a}}};~%"
	      precision name)
      (format (ostream plottingarea) "~a}};~%" name)))

(defun draw-axis-ticks-x (plottingarea x-list &key (names nil) (numberp t) (precision 2) 
						(y-shift "0cm") (start "-2pt") (stop "2pt") 
						(style "black") (text-style "below"))
  "Draw axis tick marks"
  (scope (plottingarea (format nil "yshift=~a" y-shift))
    (map nil (lambda (x name)
	       (draw-tick-mark plottingarea numberp precision name style text-style x (plot-y-min plottingarea) 0 0 stop start))
	 x-list (if (null names) x-list names))))

(defun draw-axis-ticks-y (plottingarea y-list &key (names nil) (numberp t) (precision 2) 
						(x-shift "0cm") (start "-2pt") (stop "2pt") 
						(style "black") (text-style "left"))
  "Draw axis tick marks"
  (scope (plottingarea (format nil "xshift=~a" x-shift))
    (map nil (lambda (y name)
	       (draw-tick-mark plottingarea numberp precision name style text-style (plot-x-min plottingarea) y stop start 0 0))
	 y-list (if (null names) y-list names))))

(defparameter *tikz-preamble*
"\\documentclass{standalone}
\\usepackage{tikz}
\\usepackage{color}
\\usepackage{siunitx}
\\usetikzlibrary{arrows,shapes}
")
(defmacro with-tikz-plot-standalone ((name filename width height
					   plot-x-min plot-x-max plot-y-min plot-y-max
					   &optional (compile-and-show nil) (tex-command "pdflatex")
					   (show-command "evince")) &body body)
  "Macro that opens a file, makes a plotting area and opens and closes tikzpicture environment.
Can also compile the tex file. and show the resulting pdf.
Compilation happens in output directory of plot."
  (let ((stream-name (gensym)))
    `(progn
       (with-open-file (,stream-name ,filename :direction :output :if-exists :supersede)
	 (let ((,name (make-instance 'plottingarea :stream ,stream-name :width ,width :height ,height
				     :plot-x-min ,plot-x-min :plot-x-max ,plot-x-max
				     :plot-y-min ,plot-y-min :plot-y-max ,plot-y-max)))
	   (format ,stream-name *tikz-preamble*)
	   (latex-environ (,name "document")
	     (latex-environ (,name "tikzpicture")
	       ,@body))))
       (when ,compile-and-show
	 (sb-ext:run-program ,tex-command
			     (list
			      "-output-directory"
			      (sb-ext:native-namestring
			       (make-pathname :directory (pathname-directory ,filename)))
			      ,filename)
			     :wait t :search t :output *standard-output*)
	 (sb-ext:run-program ,show-command
			     (list (sb-ext:native-namestring
				    (make-pathname :type "pdf" :defaults ,filename)))
			     :wait nil :search t)))))

(defmacro with-tikz-plot ((name filename width height
				plot-x-min plot-x-max
				plot-y-min plot-y-max &optional (master-file "master")) &body body)
  "Macro that opens a file, makes a plotting area and opens and closes tikzpicture environment"
  (let ((stream-name (gensym)))
    `(with-open-file (,stream-name ,filename :direction :output :if-exists :supersede)
       (let ((,name (make-instance 'plottingarea :stream ,stream-name :width ,width :height ,height
				   :plot-x-min ,plot-x-min :plot-x-max ,plot-x-max
				   :plot-y-min ,plot-y-min :plot-y-max ,plot-y-max)))
	 (latex-environ (,name "tikzpicture")
	   ,@body)
	 (format ,stream-name "%%% Local Variables: ~%%%% mode: latex ~%%%% TeX-master: ~s ~%%%% End:~%~%"
		 ,master-file)))))

(defmacro with-sugfigure ((plottingarea name 
					x-offset y-offset width height
					plot-x-min plot-x-max plot-y-min plot-y-max) &body body)
  "Macro that opens a file, makes a plotting area and opens and closes tikzpicture environment"
  `(let ((,name (make-instance 'plottingarea :stream (ostream ,plottingarea)
			       :x-offset ,x-offset :y-offset ,y-offset
			       :width ,width :height ,height
			       :plot-x-min ,plot-x-min :plot-x-max ,plot-x-max
			       :plot-y-min ,plot-y-min :plot-y-max ,plot-y-max)))
     ,@body))

(defmacro scope ((plottingarea &optional (style "")) &body body)
  "Make a tikz scope."
  `(latex-environ (,plottingarea "scope" ,style)
     ,@body))

(defmacro clip ((plottingarea &optional x-from x y-from y) &body body)
  "Clip a rectangle from origin."
  (if (or (null x) (null y))
      `(scope (,plottingarea)
	 (format (ostream ,plottingarea) "\\clip (~f,~f) rectangle (~f,~f);~%"
		 (x-offset ,plottingarea) (y-offset ,plottingarea)
		 (+ (width ,plottingarea) (x-offset ,plottingarea))
		 (+ (height ,plottingarea) (y-offset ,plottingarea)))
	 ,@body)
      `(scope (,plottingarea)
	 (format (ostream ,plottingarea) "\\clip (~a,~a) rectangle (~a,~a);~%"
		 ,x-from ,y-from ,x ,y)
	 ,@body)))

(defmacro transform-scale ((plottingarea) &body body)
  "Perform transformationf from data coord system to plottingarea system"
  (let ((x-scale (gensym))
	(y-scale (gensym)))
    `(let ((,x-scale (/  (width ,plottingarea) (- (plot-x-max ,plottingarea) (plot-x-min ,plottingarea))))
	   (,y-scale (/  (height ,plottingarea) (- (plot-y-max ,plottingarea) (plot-y-min ,plottingarea)))))
       (scope (,plottingarea
	       (format nil "shift={(~f,~f)}"
		       (x-offset ,plottingarea)
		       (y-offset ,plottingarea)))
	 (scope (,plottingarea
		 (format nil "shift={(~f,~f)},xscale=~f,yscale=~f"
			 (- (plot-x-min ,plottingarea))
			 (- (plot-y-min ,plottingarea))
			 ,x-scale ,y-scale))
	   ,@body)
	 (format (ostream ,plottingarea) "\\pgfsetxvec{\\pgfpoint{1cm}{0cm}}~&")
	 (format (ostream ,plottingarea) "\\pgfsetyvec{\\pgfpoint{0cm}{1cm}}~%")))))
  
(defmacro transform ((plottingarea) &body body)
  "Perform transformationf from data coord system to plottingarea system"
  (let ((x-scale (gensym))
	(y-scale (gensym)))
    `(let ((,x-scale (/  (width ,plottingarea) (- (plot-x-max ,plottingarea) (plot-x-min ,plottingarea))))
	   (,y-scale (/  (height ,plottingarea) (- (plot-y-max ,plottingarea) (plot-y-min ,plottingarea)))))
       (scope (,plottingarea
	       (format nil "shift={(~f,~f)}"
		       (x-offset ,plottingarea)
		       (y-offset ,plottingarea)))
	 (format (ostream ,plottingarea) "\\pgfsetxvec{\\pgfpoint{~fcm}{0cm}}~&" ,x-scale)
	 (format (ostream ,plottingarea) "\\pgfsetyvec{\\pgfpoint{0cm}{~fcm}}~%" ,y-scale)
	 (scope (,plottingarea
		 (format nil "shift={(~f,~f)}"
			 (- (plot-x-min ,plottingarea))
			 (- (plot-y-min ,plottingarea))))
	   ,@body)
	 (format (ostream ,plottingarea) "\\pgfsetxvec{\\pgfpoint{1cm}{0cm}}~&")
	 (format (ostream ,plottingarea) "\\pgfsetyvec{\\pgfpoint{0cm}{1cm}}~%")))))

;; (defmacro transform ((plottingarea) &body body)
;;   "Perform transformationf from data coord system to plottingarea system"
;;   (let ((x-scale (gensym))
;; 	(y-scale (gensym)))
;;     `(let ((,x-scale (/  (width ,plottingarea) (- (plot-x-max ,plottingarea) (plot-x-min ,plottingarea))))
;; 	   (,y-scale (/  (height ,plottingarea) (- (plot-y-max ,plottingarea) (plot-y-min ,plottingarea)))))
;;        (if (> (max ,x-scale ,y-scale) 400)
;; 	   (transform-vec (,plottingarea) ,@body)
;; 	   (transform-scale (,plottingarea) ,@body)))))

(defmacro clip-and-transform ((plottingarea) &body body)
  "First clip the plotting area, then perform transformations from data to plottingarea"
  `(clip (,plottingarea)
     (transform (,plottingarea)
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
  (format (ostream tikz) "\\node at (~f,~f) [~a] {~a}; ~%" x y
	  (concatenate 'string node-string "," style) text))

(defun draw-circle (plottingarea x y style)
  "Generate tikz code to draw a circle."
  (draw-node plottingarea x y style (make-node-string "circle" 2 2)))

(defun draw-rectangle (plottingarea x-from y-from x-to y-to style)
  "Generate tikz code to draw a rectangle."
  (format (ostream plottingarea) "\\draw[~a] (~f,~f) rectangle (~f,~f);~%"
	  style x-from y-from x-to y-to))

(defun draw-profilepoints (plottingarea x y y-error style)
  (mapcar (lambda (xx yy err) (draw-profilepoint plottingarea xx yy err style))
	  x y y-error))

(defun draw-profilepoint (plottingarea x y y-error style &optional (node-string (make-node-string "circle" 3 3)))
  "Draw a data-point with error bars in y direction"
  (draw-path plottingarea (list x x) (list (- y y-error) (+ y y-error)) style)
  (draw-node plottingarea x y style node-string)
  (scope (plottingarea style)
    (format (ostream plottingarea) "\\pgfpathmoveto{ \\pgfpointadd{\\pgfqpointxy {~f} {~f}} {\\pgfpoint{2pt}{0}}}~%" x (+ y y-error))
    (format (ostream plottingarea) "\\pgfpathlineto{ \\pgfpointadd{\\pgfqpointxy {~f} {~f}} {\\pgfpoint{-2pt}{0}}}~%" x (+ y y-error))
    (format (ostream plottingarea) "\\pgfpathlineto{ \\pgfqpointxy {~f} {~f}}~%" x (+ y y-error))
    (format (ostream plottingarea) "\\pgfpathlineto{ \\pgfqpointxy {~f} {~f}}~%" x (- y y-error))
    (format (ostream plottingarea) "\\pgfpathmoveto{ \\pgfpointadd{\\pgfqpointxy {~f} {~f}} {\\pgfpoint {2pt} {0} }}~%" x (- y y-error))
    (format (ostream plottingarea) "\\pgfpathlineto{ \\pgfpointadd{\\pgfqpointxy {~f} {~f}} {\\pgfpoint {-2pt} {0} }}~%" x (- y y-error))
    (format (ostream plottingarea) "\\pgfusepath{ stroke }~%")))

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
    (draw-path tikz x y style fill)
    (when separate-bins
      (mapcar (lambda (x y) (draw-path tikz (list 0 x) (list y y) style nil)) x y))))

(defun draw-histogram (tikz histo style &optional (fill nil) (separate-bins nil))
  "Draw a histogram"
  (multiple-value-bind (x y) (make-histogram-path histo)
    (draw-path tikz x y style fill)
    (when separate-bins
      (mapcar (lambda (x y) (draw-path tikz (list x x) (list y 0) style nil)) x y))))

(defun path-move-to (tikz x y)
  (format (ostream tikz) "\\pgfpathmoveto{ \\pgfpointxy {~f} {~f}}~%" x y))

(defun path-line-to (tikz x y)
  (format (ostream tikz) "\\pgfpathlineto{ \\pgfpointxy {~f} {~f}}~%" x y))

(defun path-stroke (tikz &optional (stroke t) (fill nil) (clip nil))
  (let ((action (concatenate 'string 
			     (if stroke "stroke," "") 
			     (if fill " fill," "")
			     (if clip " clip," ""))))
    (format (ostream tikz) "\\pgfusepath{ ~a }~%" action)))

(defun path-close (tikz)
  (format (ostream tikz) "\\pgfpathclose~%"))

(defun connect-plots (top sub style &optional (top-left t) (top-right t) (bottom-left t) (bottom-right t))
  (scope (top style)
    (transform (top)
      (path-move-to top (plot-x-min sub) (plot-y-min sub))
      (path-line-to top (plot-x-max sub) (plot-y-min sub))
      (path-line-to top (plot-x-max sub) (plot-y-max sub))
      (path-line-to top (plot-x-min sub) (plot-y-max sub))
      (path-close top))
    (path-stroke top)
    (format (ostream top) "\\pgfseteorule~%")
    ;;Clipping area
    (transform (sub)
      (path-move-to sub (plot-x-min sub) (plot-y-min sub))
      (path-line-to sub (plot-x-max sub) (plot-y-min sub))
      (path-line-to sub (plot-x-max sub) (plot-y-max sub))
      (path-line-to sub (plot-x-min sub) (plot-y-max sub))
      (path-close top))
    (transform (top)
      (path-move-to top (plot-x-min sub) (plot-y-min sub))
      (path-line-to top (plot-x-max sub) (plot-y-min sub))
      (path-line-to top (plot-x-max sub) (plot-y-max sub))
      (path-line-to top (plot-x-min sub) (plot-y-max sub))
      (path-close top))
    (path-move-to top (min 0 (+ (x-offset sub))) (min 0 (+ (y-offset sub))))
    (path-line-to top (max (width top) (+ (x-offset sub) (width sub))) (min 0 (+ (y-offset sub))))
    (path-line-to top (max (width top) (+ (x-offset sub) (width sub))) (max (height top) (+ (y-offset sub) (height sub))))
    (path-line-to top (min 0 (+ (x-offset sub))) (max (height top) (+ (y-offset sub) (height sub))))
    (path-close top)
    (path-stroke top nil nil t)
    ;;Draw lines
    ;;(draw-line top 0 0 10 6 "black,thick")
    (when bottom-left
      (path-move-to top (apply-transform-x top (plot-x-min sub)) (apply-transform-y top (plot-y-min sub)))
      (path-line-to top (+ (x-offset sub)) (y-offset sub)))
    (when top-left
      (path-move-to top (apply-transform-x top (plot-x-min sub)) (apply-transform-y top (plot-y-max sub)))
      (path-line-to top (+ (x-offset sub)) (+ (y-offset sub) (height sub))))
    (when top-right
      (path-move-to top (apply-transform-x top (plot-x-max sub)) (apply-transform-y top (plot-y-max sub)))
      (path-line-to top (+ (x-offset sub) (width sub)) (+ (y-offset sub) (height sub))))
    (when bottom-right
      (path-move-to top (apply-transform-x top (plot-x-max sub)) (apply-transform-y top (plot-y-min sub)))
      (path-line-to top (+ (x-offset sub) (width sub)) (+ (y-offset sub))))
    (when (or bottom-right bottom-left top-right top-left)
      (path-stroke top))))
 
(defun draw-histogram-bins (tikz histo style)
  "Draw a histogram, each bin is drawn individually"
  (let* ((data (getf histo :data))
	 (x-pos (make-range (getf histo :min) (getf histo :bin-size) (length data))))
    (scope (tikz style)
      (dotimes (n (length data))
	(format (ostream tikz) "\\filldraw[~a] (~f,~f) -- (~f,~f) -- (~f,~f) -- (~f,~f);~%"
		""
		(elt x-pos (+ n 0)) 0
		(elt x-pos (+ n 0)) (elt data n)
		(elt x-pos (+ n 1)) (elt data n)
		(elt x-pos (+ n 1)) 0)))))

(defun add-path-point (tikz x y)
  (unless (or (null x) (null y))
    (format (ostream tikz) "\\pgfpathlineto{ \\pgfqpointxy {~f} {~f}}~%" (car x) (car y))
    (add-path-point tikz (cdr x) (cdr y))))

    

(defun draw-path (tikz x y style &optional (fill nil))
  "Connect data points with straight lines."
  ;;Tex cannot work with numbers larger than 19ft, even before final transformation.
  ;;Here we scale down then up
  (scope (tikz style)
    (format (ostream tikz) "\\pgfpathmoveto{ \\pgfqpointxy {~f} {~f}}~%" (car x) (car y))
    (add-path-point tikz (cdr x) (cdr y))
    (if fill
	(format (ostream tikz) "\\pgfusepath{ fill,stroke }~%")
	(format (ostream tikz) "\\pgfusepath{ stroke }~%"))))

(defun draw-graph-error (tikz x y y-error line-style mark-style error-style)
  "Draw error bars"
  (draw-graph tikz x y line-style mark-style)
  (mapcar (lambda (x y er) (draw-profilepoint tikz x y er error-style)) x y y-error))

(defun draw-datapoints (tikz x y style &optional (node (make-node-string "circle" 2 2)))
  "Draw a set of datapoints"
  (map 'nil (lambda (x y) (draw-node tikz x y style node)) x y))

(defun draw-graph (tikz x y line-style mark-style &optional (node (make-node-string "circle" 2 2)))
  "Draw a graph, with a line connecting datapoints"
  (draw-path tikz x y line-style)
  (draw-datapoints tikz x y mark-style node))

(defun draw-graph-spline (tikz x y line-style mark-style  &optional (node (make-node-string "circle" 2 2)))
  "Draw a graph, with a spline connecting datapoints"
  (let ((n  (min (length x) (length y))))
    (draw-function tikz (get-spline-fun x y) 100 line-style (elt x 0) (elt x (- n 1))))
  (draw-datapoints tikz x y mark-style node))

(defun draw-function (tikz function samples line-style &optional (x-min nil) (x-max nil))
  "Draw a function y = f(x)"
  (when (null x-min) (setf x-min (plot-x-min tikz)))
  (when (null x-max) (setf x-max (plot-x-max tikz)))
  (let* ((x-vals (make-range x-min (/ (- x-max x-min) samples) samples))
	 (y-vals (mapcar (lambda (x) (funcall function x)) x-vals)))
    (draw-path tikz x-vals y-vals line-style)))

(defun draw-legend-line (tikz x y width name line-style
			 &optional (mark-style "") (node-string (make-node-string "circle" 2 2))
			   (name-style "") (error-style "") (error-height 0.1))
  "Draw a legent entry for a plot, with a line, and or marks with or without error bars.
For graphs, functions, datapoints, most histograms"
  (if (> (length line-style) 0) (draw-line tikz x y (+ x width) y line-style))
  (if (> (length error-style) 0) (draw-profilepoint tikz (+ (* 0.5 width) x)
						    y error-height error-style))
  (if (> (length mark-style) 0) (draw-node tikz (+ (* 0.5 width) x) y mark-style node-string))
  (draw-text-node tikz (+ x width) y name (concatenate 'string "right," name-style)))

(defun draw-legend-rectangle (tikz x y width height name style name-style)
  "Draw a (filled) rectangle with a legend entry. This is for histograms drawn with draw-histogram-bins"
  (draw-rectangle tikz x (- y (* 0.5 height))
		  (+ x width) (+ y (* 0.5 height)) style)
  (draw-text-node tikz (+ x width) y name (concatenate 'string "right," name-style)))
