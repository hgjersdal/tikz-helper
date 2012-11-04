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

(defmacro latex-environ ((plottingarea environ &optional (args nil)) &body body)
  "Place tex code generated by body within a latex environment"
  (if (null args)
      `(progn
	 (format (ostream ,plottingarea) "\\begin{~a}~%" ,environ)
	 ,@body
	 (format (ostream ,plottingarea) "\\end{~a}~%" ,environ))
      `(progn
	 (format (ostream ,plottingarea) "\\begin{~a}[~a]~%" ,environ ,args)
	 ,@body
	 (format (ostream ,plottingarea) "\\end{~a}~%" ,environ))))
  
(defun make-transformation (cmax pmin pmax)
  "Makes a linear transformation from data space(pmin-pmax) to figura space 0-cmax"
  (let ((scale (/ (- pmax pmin) cmax)))
    (lambda (x) (/ (- x pmin) scale))))

(defun make-vector-transform (cmax pmin pmax)
  "Same transformation as above, but without translations"
  (let ((scale (/ (- pmax pmin) cmax)))
    (lambda (x) (/ x scale))))

(defmacro scope ((plottingarea &optional (style "")) &body body)
  "Make a tikz scope."
  `(latex-environ (,plottingarea "scope" ,style)
     ,@body))

(defmacro clip ((plottingarea) &body body)
  "Clip a rectangle from origin."
  `(scope (,plottingarea)
     (make-rectangle-path ,plottingarea
			  (x-offset ,plottingarea) (y-offset ,plottingarea)
			  (+ (width ,plottingarea) (x-offset ,plottingarea))
			  (+ (height ,plottingarea) (y-offset ,plottingarea)))
     (path-stroke ,plottingarea nil nil t)
     ,@body))

(defmacro transform-scale ((plottingarea) &body body)
  "Perform transformationf from data coord system to plottingarea system by using scales. Also scales cm, pt, mm etc."
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
	   ,@body)))))

(defmacro transform ((plottingarea) &body body)
  "Perform transformationf from data coord system to plottingarea system. If this fails, try using transform-scale."
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

(defmacro clip-and-transform ((plottingarea) &body body)
  "First clip the plotting area, then perform transformations from data to plottingarea"
  `(clip (,plottingarea)
     (transform (,plottingarea)
       ,@body)))

(defun path-move-to (tikz x y)
  (format (ostream tikz) "\\pgfpathmoveto{ \\pgfpointxy {~f} {~f}}~%" x y))

(defun path-line-to (tikz x y)
  (format (ostream tikz) "\\pgfpathlineto{ \\pgfpointxy {~f} {~f}}~%" x y))

(defun path-stroke (tikz &optional (stroke t) (fill nil) (clip nil))
  "Stroke, fill and or clip the path"
  (let ((action (concatenate 'string
			     (if stroke "stroke," "")
			     (if fill " fill," "")
			     (if clip " clip," ""))))
    (format (ostream tikz) "\\pgfusepath{ ~a }~%" action)))

(defun path-close (tikz)
  "Close the path by connecting first and last point"
  (format (ostream tikz) "\\pgfpathclose~%"))

(defun make-path (tikz x y)
  "Connect data points with straight lines."
  (path-move-to tikz (car x) (car y))
  (mapc (lambda (x y) (path-line-to tikz x y)) (cdr x) (cdr y)))

(defun make-mixed-point (tikz x-data x-unit y-data y-unit)
  "A point in the coord system of the current transformation (x-data, y-data), shifted by another point (x-unit,y-unit).
The other point should be a string with cm, mm, pt or similar invariant unit."
  (format (ostream tikz) "\\pgfpointadd{\\pgfpointxy {~f} {~f}} {\\pgfpoint{~a}{~a}}"
	  x-data y-data x-unit y-unit))

(defun path-move-to-mixed (tikz x-data x-unit y-data y-unit)
  "Move path to mixed point."
  (format (ostream tikz) "\\pgfpathmoveto{ ~a }~%"
	  (make-mixed-point tikz x-data x-unit y-data y-unit)))

(defun path-line-to-mixed (tikz x-data x-unit y-data y-unit)
  "Extend path to mixed point."
  (format (ostream tikz) "\\pgfpathlineto{ ~a }~%"
	  (make-mixed-point tikz x-data x-unit y-data y-unit)))

(defun make-path-mixed-units (tikz x-data x-units y-data y-units)
  "Connect data points with straight lines. Data points are at x-data + x-units, y-data + y-units. Units can be cm,pt,mm, etc"
  (path-move-to-mixed tikz (car x-data) (car x-units) (car y-data) (car y-units))
  (mapc (lambda (x xx y yy) (path-line-to-mixed tikz x xx y yy)) (cdr x-data) (cdr x-units) (cdr y-data) (cdr y-units)))

(defun draw-path (tikz x y style &optional fill)
  "Make a path and draw it."
  (scope (tikz style)
    (make-path tikz x y)
    (path-stroke tikz t fill)))

(defun make-rectangle-path (plottingarea x-min y-min x-max y-max)
  "Make a rectangle path."
  (path-move-to plottingarea x-min y-min)
  (path-line-to plottingarea x-max y-min)
  (path-line-to plottingarea x-max y-max)
  (path-line-to plottingarea x-min y-max)
  (path-close plottingarea))

(defun apply-transform-x (tikz x)
  "Transfor a point to coordinate system of plottingarea"
  (let ((scale (/ (- (plot-x-max tikz) (plot-x-min tikz)) (width tikz))))
    (/ (- x (plot-x-min tikz)) scale)))

(defun apply-transform-y (tikz y)
  "Transfor a point to coordinate system of plottingarea"
  (let ((scale (/ (- (plot-y-max tikz) (plot-y-min tikz)) (height tikz))))
    (/ (- y (plot-y-min tikz)) scale)))

(defun draw-tick-mark (plottingarea numberp precision name style text-style x y xpt+ xpt- ypt+ ypt-)
  "Draw a tick mark on an axis."
  (format (ostream plottingarea)
	  "\\draw[~a] [shift={(~f,~f)}] (~a,~a) -- (~a,~a) node[~a]{ \\scriptsize{"
	  style x y xpt+ ypt+ xpt- ypt- text-style)
  (if numberp
      (format (ostream plottingarea)
	      "\\num[round-mode=places,round-precision=~a]{~a}}};~%"
	      precision (if (= 0 precision) (floor name) name))
      (format (ostream plottingarea) "~a}};~%" name)))

(defun draw-axis-ticks-x (plottingarea x-list &key (names nil) (numberp t) (precision 1)
						(y-shift "0cm") (start "-2pt") (stop "2pt")
						(style "black") (text-style "below"))
  "Draw axis tick marks. Names on the tick marks
names: Text that goes with the tick-mark. Default (nil), is to use the numbers in x-list.
numberp: Is the value a number?
precision: If the name is a number, and floatin point, how many digits should be displayed after '.'.
y-shift: Offset in y. 0 means at plot-y-min. Units like cm,mm,pt can be used.
start: The tick line will start at y-shift + start.
stop: The tick line will stop at y-shift + stop.
style: style of line
text-style: style of text node."
  (scope (plottingarea (format nil "yshift=~a" y-shift))
    (map nil (lambda (x name)
	       (draw-tick-mark plottingarea numberp precision name style text-style x (plot-y-min plottingarea) 0 0 stop start))
	 x-list (if (null names) x-list names))))

(defun draw-axis-ticks-y (plottingarea y-list &key (names nil) (numberp t) (precision 1)
						(x-shift "0cm") (start "-2pt") (stop "2pt")
						(style "black") (text-style "left"))
  "Draw axis tick marks. See draw-axis-tizks-x for details."
  (scope (plottingarea (format nil "xshift=~a" x-shift))
    (map nil (lambda (y name)
	       (draw-tick-mark plottingarea numberp precision name style text-style (plot-x-min plottingarea) y stop start 0 0))
	 y-list (if (null names) y-list names))))

(defun draw-subtick-mark (plottingarea style x y xpt+ xpt- ypt+ ypt-)
  "Draw a tick mark with no text"
  (format (ostream plottingarea)
	  "\\draw[~a] [shift={(~f,~f)}] (~a,~a) -- (~a,~a);~%"
	  style x y xpt+ ypt+ xpt- ypt-))

(defun draw-axis-subticks-x (plottingarea x-list &key (y-shift "0cm") (start "-1pt") (stop "1pt") (style "black"))
  "Draw ticks with no text."
  (scope (plottingarea (format nil "yshift=~a" y-shift))
    (mapc (lambda (x) (draw-subtick-mark plottingarea style x (plot-y-min plottingarea) 0 0 stop start)) x-list)))

(defun draw-axis-subticks-y (plottingarea y-list &key (x-shift "0cm") (start "-1pt") (stop "1pt") (style "black"))
  "Draw ticks with no text"
  (scope (plottingarea (format nil "xshift=~a" x-shift))
    (mapc (lambda (y) (draw-subtick-mark plottingarea style (plot-x-min plottingarea) y stop start 0 0)) y-list)))

(defparameter *tikz-preamble*
"\\documentclass{standalone}
\\ifx\\HCode\\UnDef\\else\\def\\pgfsysdriver{pgfsys-tex4ht.def}\\fi
\\usepackage{tikz}
\\usepackage{color}
\\usepackage{siunitx}
\\usetikzlibrary{arrows,shapes}
")

(defun pdflatex-compile (tex-file)
  "Compile tex-file with pdflatex"
  #+sbcl
  (sb-ext:process-exit-code
   (sb-ext:run-program "pdflatex"
		       (list
			"-output-directory"
			(sb-ext:native-namestring
			 (make-pathname :directory (pathname-directory tex-file)))
			tex-file)
		       :wait t :search t :output *standard-output*))
  #-sbcl (warn "Not implemented"))

(defun pdflatex-compile-view (tex-file &optional (viewer "emacsclient"))
  "Compile file, then view with viewer."
  #+sbcl
  (if (= 0 (pdflatex-compile tex-file))
      (sb-ext:run-program viewer
			  (list (sb-ext:native-namestring
				 (make-pathname :type "pdf" :defaults tex-file)))
			  :wait nil :search t)
      (warn "Process pdflatex failed"))
  #-sbcl
  (warn "Not implemented"))

(defmacro with-tikz-plot ((name filename width height
				plot-x-min plot-x-max
				plot-y-min plot-y-max) &body body)
  "Macro that opens a file, makes a plotting area and opens and closes tikzpicture environment."
  (let ((stream-name (gensym)))
    `(with-open-file (,stream-name ,filename :direction :output :if-exists :supersede)
       (let ((,name (make-instance 'plottingarea :stream ,stream-name :width ,width :height ,height
				   :plot-x-min ,plot-x-min :plot-x-max ,plot-x-max
				   :plot-y-min ,plot-y-min :plot-y-max ,plot-y-max)))
	 (format ,stream-name *tikz-preamble*)
	 (latex-environ (,name "document")
	   (latex-environ (,name "tikzpicture")
	     ,@body))))))

(defmacro with-sugfigure ((plottingarea name
					x-offset y-offset width height
					plot-x-min plot-x-max plot-y-min plot-y-max) &body body)
  "Macro that keeps details of a sub-figure. Used for cliping and transformations."
  `(let ((,name (make-instance 'plottingarea :stream (ostream ,plottingarea)
			       :x-offset ,x-offset :y-offset ,y-offset
			       :width ,width :height ,height
			       :plot-x-min ,plot-x-min :plot-x-max ,plot-x-max
			       :plot-y-min ,plot-y-min :plot-y-max ,plot-y-max)))
     ,@body))

(defun make-range (min stepsize steps)
  "Returns a list with steps elements, where the first is min the next is min+stepsize etc"
  (let ((my-list nil))
    (dotimes (n steps)
      (setf my-list (append my-list (list (+ min (* n stepsize))))))
    my-list))

(defun make-node-string (shape width height &optional (inner-sep 0) (unit "pt"))
  "Make a node string"
  (format nil "~a,inner sep=~f~a,minimum width =~f~a,minimum height=~f~a"
	  shape inner-sep unit width unit height unit))

(defun draw-line (plottingarea x-from y-from x-to y-to style)
  "Generate tikz code to draw a line."
  (format (ostream plottingarea) "\\draw[~a] (~f,~f) -- (~f,~f);~%" style x-from y-from x-to y-to))

(defun draw-node (tikz x y style node-string &optional (text ""))
  "Draw a node at point."
  (format (ostream tikz) "\\node at (~f,~f) [~a,~a] {~a};~%" x y style node-string text))
  (format (ostream tikz) "\\node at (~f,~f) [~a] {~a}; ~%" x y
	  (concatenate 'string node-string "," style) text))

(defun draw-circle (plottingarea x y style)
  "Generate tikz code to draw a circle."
  (draw-node plottingarea x y style (make-node-string "circle" 3 3)))

(defun draw-rectangle (plottingarea x-from y-from x-to y-to style)
  "Generate tikz code to draw a rectangle."
  (format (ostream plottingarea) "\\draw[~a] (~f,~f) rectangle (~f,~f);~%"
	  style x-from y-from x-to y-to))

(defun draw-profilepoint (plottingarea x y y-error style &optional (node-string (make-node-string "circle" 3 3)))
  "Draw a data-point with error bars in y direction"
  ;;(draw-path plottingarea (list x x) (list (- y y-error) (+ y y-error)) style)
  (scope (plottingarea style)
    (make-path-mixed-units plottingarea
			   (list x x x x x x) (list "-2pt" "2pt" "0pt" "0pt" "-2pt" "2pt")
			   (list (+ y y-error) (+ y y-error) (+ y y-error)
				 (- y y-error) (- y y-error) (- y y-error))
			   (list 0 0 0 0 0 0))
    (path-stroke plottingarea t)
    (draw-node plottingarea x y style node-string)))

(defun draw-profilepoints (plottingarea x y y-error style &optional (node-string (make-node-string "circle" 3 3)))
  (mapcar (lambda (xx yy err) (draw-profilepoint plottingarea xx yy err style node-string))
	  x y y-error))

(defun make-histogram (min bin-size data)
  "A histogram as a simple plist"
  (list :min min :bin-size bin-size :data data))

(defun make-histogram-path-points (histo)
  "Make the path for a histogram, the path here is just a list of points."
  (let* ((y-pos (getf histo :data))
	 (x-pos (make-range (getf histo :min) (getf histo :bin-size) (+ 1 (length y-pos))))
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
  "Draw a histogram."
  (multiple-value-bind (y x) (make-histogram-path-points histo)
    (draw-path tikz x y style fill)
    (when separate-bins
      (mapcar (lambda (x y) (draw-path tikz (list 0 x) (list y y) style nil)) x y))))

(defun draw-histogram (tikz histo style &optional (fill nil) (separate-bins nil))
  "Draw a histogram."
  (multiple-value-bind (x y) (make-histogram-path-points histo)
    (draw-path tikz x y style fill)
    (when separate-bins
      (mapcar (lambda (x y) (draw-path tikz (list x x) (list y 0) style nil)) x y))))

(defun region-of-interest-zoom (top sub style &optional (top-left t) (top-right t) (bottom-left t) (bottom-right t))
  "Draw a rectangle around region of interest, and draw lines connecting it with a sub-figure."
 (scope (top style)
    ;;Region of interest
    (transform (top)
      (make-rectangle-path top (plot-x-min sub) (plot-y-min sub) (plot-x-max sub) (plot-y-max sub)))
    (path-stroke top)
    (format (ostream top) "\\pgfseteorule~%")
    ;;Clipping area, inverted clip of the region of interest and the sub plottingarea
    (transform (sub)
      (make-rectangle-path sub (plot-x-min sub) (plot-y-min sub) (plot-x-max sub) (plot-y-max sub)))
    (transform (top)
      (make-rectangle-path top (plot-x-min sub) (plot-y-min sub) (plot-x-max sub) (plot-y-max sub)))
    (make-rectangle-path top
		  (min 0 (+ (x-offset sub))) (min 0 (+ (y-offset sub)))
		  (max (width top) (+ (x-offset sub) (width sub))) (max (height top) (+ (y-offset sub) (height sub))))
    (path-stroke top nil nil t)
    ;;Draw lines to connect the region of interest and the sub plotting area
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

(defun draw-datapoints (tikz x y style &optional (node (make-node-string "circle" 3 3)))
  "Draw a set of datapoints"
  (map 'nil (lambda (x y) (draw-node tikz x y style node)) x y))

(defun draw-graph (tikz x y line-style mark-style &optional (node (make-node-string "circle" 3 3)))
  "Draw a graph, with a line connecting datapoints"
  (draw-path tikz x y line-style)
  (draw-datapoints tikz x y mark-style node))

(defun draw-graph-spline (tikz x y line-style mark-style  &optional (node (make-node-string "circle" 3 3)))
  "Draw a graph, with a spline connecting datapoints"
  (let ((n  (min (length x) (length y))))
    (draw-function tikz (tikz-spline:get-spline-fun x y) 100 line-style (elt x 0) (elt x (- n 1))))
  (draw-datapoints tikz x y mark-style node))

(defun draw-function (tikz function samples line-style &optional (x-min nil) (x-max nil))
  "Draw a function y = f(x)"
  (when (null x-min) (setf x-min (plot-x-min tikz)))
  (when (null x-max) (setf x-max (plot-x-max tikz)))
  (let* ((x-vals (make-range x-min (/ (- x-max x-min) samples) (+ 1 samples)))
	 (y-vals (mapcar (lambda (x) (funcall function x)) x-vals)))
    (draw-path tikz x-vals y-vals line-style)))

(defun draw-legend-entry (tikz x y name &key (width 0.4) (line-style "") (mark-style "") 
					 (node-string (make-node-string "circle" 3 3))
					 (name-style "") (error-style "") (error-height 0.1)
					 (histogram-node-p nil))
  "Draw a legent entry for a plot, with a line, and or marks with or without error bars.
For graphs, functions, datapoints, most histograms"
  (if (> (length line-style) 0) (draw-line tikz x y (+ x width) y line-style))
  (if (> (length error-style) 0) (draw-profilepoint tikz (+ (* 0.5 width) x)
						    y error-height error-style))
  (if (> (length mark-style) 0) (draw-node tikz (+ (* 0.5 width) x) y mark-style 
					   (if histogram-node-p 
					       (make-node-string "rectangle" width 0.2 0 "cm")
					       node-string)))
  (draw-node tikz (+ x width) y (concatenate 'string "right," name-style) ""  name))
