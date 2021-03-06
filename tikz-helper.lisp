(in-package :tikz-helper)

(defmacro with-coerced-lists ((&rest sequences) &body body)
  `(let ,(mapcar (lambda (x) `(,x (coerce ,x 'list))) sequences)
     ,@body))

(defclass plottingarea ()
  ((width  :initform 05 :initarg :width  :reader width)
   (height :initform 10 :initarg :height :reader height)
   (x-offset  :initform 00 :initarg :x-offset  :reader x-offset)
   (y-offset  :initform 00 :initarg :y-offset  :reader y-offset)
   (plot-x-min :initform 0 :initarg :plot-x-min :reader plot-x-min)
   (plot-x-max :initform 1 :initarg :plot-x-max :reader plot-x-max)
   (plot-y-min :initform 0 :initarg :plot-y-min :reader plot-y-min)
   (plot-y-max :initform 1 :initarg :plot-y-max :reader plot-y-max)
   (transformedp :initform nil :accessor transformedp)
   (ostream :initform t :initarg :stream :accessor ostream))
  (:documentation "Contains output-stream for latex file as well as ingo needed for transformations"))

(defmacro latex-environ-with-arg ((ostream environ arg) &body body)
  "Place tex code generated by body within a latex environment"
  `(progn
     (format ,ostream "\\begin{~a}[~a]~%" ,environ ,arg)
     ,@body
     (format ,ostream "\\end{~a}~%" ,environ)))

(defmacro latex-environs ((ostream &rest environs) &body body)
  `(progn
    (mapc (lambda (env) (format ,ostream "\\begin{~a}~%" env)) (list ,@environs))
    ,@body
    (mapc (lambda (env) (format ,ostream "\\end{~a}~%" env)) (reverse (list ,@environs)))))

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
  `(latex-environ-with-arg ((ostream ,plottingarea) "scope" ,style)
     ,@body))

(defun cm (num)
  (format nil "~acm" num))

(defmacro clip ((plottingarea) &body body)
  "Clip a rectangle from origin."
  `(scope (,plottingarea)
     (let ((xmin (if (transformedp ,plottingarea) (plot-x-min ,plottingarea) 0))
	   (ymin (if (transformedp ,plottingarea) (plot-y-min ,plottingarea) 0)))
       (path-move-to-mixed ,plottingarea xmin (cm (x-offset ,plottingarea)) ymin (cm (y-offset ,plottingarea)))
       (path-line-to-mixed ,plottingarea xmin (cm (+ (x-offset ,plottingarea) (width ,plottingarea)))
			   ymin (cm (y-offset ,plottingarea)))
       (path-line-to-mixed ,plottingarea xmin (cm (+ (x-offset ,plottingarea) (width ,plottingarea)))
			   ymin (cm (+ (height ,plottingarea) (y-offset ,plottingarea))))
       (path-line-to-mixed ,plottingarea xmin (cm (x-offset ,plottingarea))
			   ymin (cm (+ (height ,plottingarea) (y-offset ,plottingarea)))))
     (path-close ,plottingarea)
     (path-use ,plottingarea nil nil t)
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

(defmacro transform-write ((plottingarea) &body body)
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
	   ,@body))
       (format (ostream ,plottingarea) "\\pgfsetxvec{\\pgfpoint{1cm}{0cm}}~&")
       (format (ostream ,plottingarea) "\\pgfsetyvec{\\pgfpoint{0cm}{1cm}}~%"))))

(defmacro transform ((plottingarea) &body body)
  "Perform transformationf from data coord system to plottingarea system. If this fails, try using transform-scale. 
If this is called within a transform scope of the same plottingarea, nothing is done."
  `(if (transformedp ,plottingarea)
       (progn ,@body)
       (progn (setf (transformedp ,plottingarea) t)
	      (transform-write (,plottingarea) ,@body)
	      (setf (transformedp ,plottingarea) nil))))

(defmacro clip-and-transform ((plottingarea) &body body)
  "First clip the plotting area, then perform transformations from data to plottingarea"
  `(clip (,plottingarea)
     (transform (,plottingarea)
       ,@body)))

(defun tikz-style (plottingarea name style)
  (format (ostream plottingarea) "\\tikzstyle{~a}=[~a]~%" name style))

(defun path-move-to (plottingarea x y)
  (format (ostream plottingarea) "\\pgfpathmoveto{ \\pgfpointxy {~f} {~f}}~%" x y))

(defun path-line-to (plottingarea x y)
  (format (ostream plottingarea) "\\pgfpathlineto{ \\pgfpointxy {~f} {~f}}~%" x y))

(defun path-use (plottingarea &optional (stroke t) (fill nil) (clip nil))
  "Stroke, fill and or clip the path"
  (let ((action (concatenate 'string
			     (if stroke "stroke," "")
			     (if fill " fill," "")
			     (if clip " clip," ""))))
    (format (ostream plottingarea) "\\pgfusepath{ ~a }~%" action)))

(defun path-close (plottingarea)
  "Close the path by connecting first and last point"
  (format (ostream plottingarea) "\\pgfpathclose~%"))

(defun make-path (plottingarea x y)
  "Connect data points with straight lines."
  (with-coerced-lists (x y)
    (path-move-to plottingarea (car x) (car y))
    (mapc (lambda (x y) (path-line-to plottingarea x y)) (cdr x) (cdr y))))

(defun make-mixed-point (x-data x-unit y-data y-unit)
  "A point in the coord system of the current transformation (x-data, y-data), shifted by another point (x-unit,y-unit).
The other point should be a string with cm, mm, pt or similar invariant unit."
  (format nil "\\pgfpointadd{\\pgfpointxy {~f} {~f}} {\\pgfpoint{~a}{~a}}"
	  x-data y-data x-unit y-unit))

(defun path-move-to-mixed (plottingarea x-data x-unit y-data y-unit)
  "Move path to mixed point."
  (format (ostream plottingarea) "\\pgfpathmoveto{ ~a }~%"
	  (make-mixed-point x-data x-unit y-data y-unit)))

(defun path-line-to-mixed (plottingarea x-data x-unit y-data y-unit)
  "Extend path to mixed point."
  (format (ostream plottingarea) "\\pgfpathlineto{ ~a }~%"
	  (make-mixed-point x-data x-unit y-data y-unit)))

(defun make-path-mixed-units (plottingarea x-data x-units y-data y-units)
  "Connect data points with straight lines. Data points are at x-data + x-units, y-data + y-units. Units can be cm,pt,mm, etc"
  (with-coerced-lists (x-data x-units y-data y-units)
    (path-move-to-mixed plottingarea (car x-data) (car x-units) (car y-data) (car y-units))
    (mapc (lambda (x xx y yy) (path-line-to-mixed plottingarea x xx y yy)) (cdr x-data) (cdr x-units) (cdr y-data) (cdr y-units))))

(defun draw-path (plottingarea x y style &optional fill)
  "Make a path and draw it."
  (scope (plottingarea style)
    (make-path plottingarea x y)
    (path-use plottingarea t fill)))

(defun make-rectangle-path (plottingarea x-min y-min x-max y-max)
  "Make a rectangle path."
  (path-move-to plottingarea x-min y-min)
  (path-line-to plottingarea x-max y-min)
  (path-line-to plottingarea x-max y-max)
  (path-line-to plottingarea x-min y-max)
  (path-close plottingarea))

(defun apply-transform-x (plottingarea x)
  "Transfor a point to coordinate system of plottingarea"
  (let ((scale (/ (- (plot-x-max plottingarea) (plot-x-min plottingarea)) (width plottingarea))))
    (/ (- x (plot-x-min plottingarea)) scale)))

(defun apply-transform-y (plottingarea y)
  "Transfor a point to coordinate system of plottingarea"
  (let ((scale (/ (- (plot-y-max plottingarea) (plot-y-min plottingarea)) (height plottingarea))))
    (/ (- y (plot-y-min plottingarea)) scale)))

(defun draw-tick-mark (plottingarea numberp precision name style text-style x y xpt+ xpt- ypt+ ypt-)
  "Draw a tick mark on an axis."
  (format (ostream plottingarea)
	  "\\draw[~a] [shift={(~f,~f)}] (~a,~a) -- (~a,~a) node[~a]{ \\scriptsize{"
	  style x y xpt+ ypt+ xpt- ypt- text-style)
  (if numberp
      (format (ostream plottingarea)
	      "\\num[round-mode=places,round-precision=~a]{~f}}};~%"
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
  (transform (plottingarea)
    (scope (plottingarea (format nil "yshift=~f" y-shift))
      (map nil (lambda (x name)
		 (draw-tick-mark plottingarea numberp precision name style text-style x (plot-y-min plottingarea) 0 0 stop start))
	   x-list (if (null names) x-list names)))))

(defun draw-axis-ticks-y (plottingarea y-list &key (names nil) (numberp t) (precision 1)
						(x-shift "0cm") (start "-2pt") (stop "2pt")
						(style "black") (text-style "left"))
  "Draw axis tick marks. See draw-axis-tizks-x for details."
  (transform (plottingarea)
    (scope (plottingarea (format nil "xshift=~f" x-shift))
      (map nil (lambda (y name)
		 (draw-tick-mark plottingarea numberp precision name style text-style (plot-x-min plottingarea) y stop start 0 0))
	   y-list (if (null names) y-list names)))))

(defun draw-subtick-mark (plottingarea style x y xpt+ xpt- ypt+ ypt-)
  "Draw a tick mark with no text"
  (format (ostream plottingarea)
	  "\\draw[~a] [shift={(~f,~f)}] (~a,~a) -- (~a,~a);~%"
	  style x y xpt+ ypt+ xpt- ypt-))

(defun draw-axis-subticks-x (plottingarea x-list &key (y-shift "0cm") (start "-1pt") (stop "1pt") (style "black"))
  "Draw ticks with no text."
  (transform (plottingarea)
    (scope (plottingarea (format nil "yshift=~f" y-shift))
      (mapc (lambda (x) (draw-subtick-mark plottingarea style x (plot-y-min plottingarea) 0 0 stop start)) x-list))))

(defun draw-axis-subticks-y (plottingarea y-list &key (x-shift "0cm") (start "-1pt") (stop "1pt") (style "black"))
  "Draw ticks with no text"
  (transform (plottingarea)
    (scope (plottingarea (format nil "xshift=~f" x-shift))
      (mapc (lambda (y) (draw-subtick-mark plottingarea style (plot-x-min plottingarea) y stop start 0 0)) y-list))))

(defun latex-command (stream command &optional ([args] nil) ({args} nil))
  (format stream "\\~a" command)
  (when [args](format stream "[~a]" [args]))
  (when {args}(format stream "{~a}" {args}))
  (format stream "~%"))

(defun print-preamble (stream &key (documentclass "standalone") (use-standalone nil))
  (format stream "%%% AUTO GENERATED CODE~%")
  (latex-command stream "documentclass" nil documentclass)
  (when use-standalone (latex-command stream "usepackage" nil "standalone"))
  (format stream "\\ifx\\HCode\\UnDef\\else\\def\\pgfsysdriver{pgfsys-tex4ht.def}\\fi~%")
  (latex-command stream "usepackage" "usenames,dvipsnames,svgnames,table" "xcolor")
  (latex-command stream "usepackage" "utf8" "inputenc")
  (latex-command stream "usepackage" "" "textcomp")
  (mapc (lambda (x) (latex-command stream "usepackage" nil x)) (list "tikz" "color" "siunitx"))
  (latex-command stream "usetikzlibrary" nil "arrows,shapes")
  (latex-command stream "usetikzlibrary" nil "decorations.markings"))

(defparameter *tikz-preamble*
"%%% AUTO GENERATED CODE
\\documentclass{standalone}
\\ifx\\HCode\\UnDef\\else\\def\\pgfsysdriver{pgfsys-tex4ht.def}\\fi
\\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\\usepackage{tikz}
\\usepackage{color}
\\usepackage{siunitx}
\\usetikzlibrary{arrows,shapes}
\\usepackage{helvet}
\\renewcommand*{\\familydefault}{\\sfdefault}
")

(defun pdflatex-compile (tex-file)
  "Compile tex-file with pdflatex"
  #+sbcl(sb-ext:process-exit-code
	 (sb-ext:run-program "pdflatex"
			     (list "-output-directory"
				   (namestring (make-pathname :directory (pathname-directory tex-file)))
				   tex-file)
			     :wait t :search t :output *standard-output*))
  #-sbcl (warn "Not implemented"))

(defun pdflatex-compile-view (tex-file &optional (viewer "emacsclient"))
  "Compile file, then view with viewer."
  #+sbcl (if (= 0 (pdflatex-compile tex-file))
	     (sb-ext:run-program viewer
				 (list (sb-ext:native-namestring
					(make-pathname :type "pdf" :defaults tex-file)))
				 :wait nil :search t)
	     (warn "Process pdflatex failed"))
  #-sbcl (warn "Not implemented"))

(defmacro with-tikz-to-stream ((name stream width height
				     plot-x-min plot-x-max plot-y-min plot-y-max
				     axis-style &key (environments (list "document")) (tikz-arg "")
				     (preamble t))&body body)
  "Macro that opens a file, makes a plotting area and opens and closes tikzpicture environment."
  `(let ((,name (make-instance 'plottingarea :stream ,stream :width ,width :height ,height
			       :plot-x-min ,plot-x-min :plot-x-max ,plot-x-max
			       :plot-y-min ,plot-y-min :plot-y-max ,plot-y-max)))
     (when ,preamble (print-preamble ,stream))
     (latex-environs (,stream ,@environments)
       (latex-environ-with-arg (,stream "tikzpicture" ,tikz-arg)
	 ,@body
	 (ecase ,axis-style
	   (:rectangle (draw-axis-rectangle ,name))
	   (:cross (draw-axis-cross ,name))
	   (:left-bottom (draw-axis-left-bottom ,name))
	   (:popped-out (draw-axis-popped-out ,name))
	   (:none nil))))))
  
(defmacro with-tikz-to-file ((name filename width height
				   plot-x-min plot-x-max plot-y-min plot-y-max
				   axis-style &key (tikz-arg "")) &body body)
  (let ((stream-name (gensym)))
    `(with-open-file (,stream-name ,filename :direction :output :if-exists :supersede)
       (with-tikz-to-stream (,name ,stream-name ,width ,height ,plot-x-min ,plot-x-max
				   ,plot-y-min ,plot-y-max ,axis-style 
				   :environments ("document") :tikz-arg ,tikz-arg :preamble t)
	 ,@body))))

(defmacro with-tikz-to-string ((name width height
				     plot-x-min plot-x-max plot-y-min plot-y-max
				     axis-style &key (tikz-arg "")) &body body)
  (let ((string-name (gensym)))
    `(with-open-stream (,string-name (make-string-output-stream))
       (with-tikz-to-stream (,name ,string-name ,width ,height ,plot-x-min ,plot-x-max
				   ,plot-y-min ,plot-y-max ,axis-style 
				   :environments () :tikz-arg ,tikz-arg :preamble nil)
	 ,@body)
       (get-output-stream-string ,string-name))))

(defmacro with-subfigure ((plottingarea name
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
  (format nil "~a,inner sep=~f~a,minimum width =~f~a,minimum height=~f~a"
	  shape inner-sep unit width unit height unit))

(defun draw-line (plottingarea x-from y-from x-to y-to style &optional (text nil) (pos "right"))
  "Generate tikz code to draw a line."
  (format (ostream plottingarea) "\\draw[~a] (~f,~f) -- (~f,~f)" style x-from y-from x-to y-to)
  (when text (format (ostream plottingarea) " node[~a] {~a}" pos text))
  (format (ostream plottingarea) ";~%"))

(defun draw-node (plottingarea x y style node-string &optional (text ""))
  (format (ostream plottingarea) "\\node at (~f,~f) [~a,~a] {~a};~%" x y style node-string text))

(defun draw-profilepoint (plottingarea x y y-error style &key (node (make-node-string "circle" 3 3)))
  "Draw a data-point with error bars in y direction"
  (scope (plottingarea style)
    (make-path-mixed-units plottingarea
			   (list x x x x x x) (list "-2pt" "2pt" "0pt" "0pt" "-2pt" "2pt")
			   (list (+ y y-error) (+ y y-error) (+ y y-error)
				 (- y y-error) (- y y-error) (- y y-error))
			   (list 0 0 0 0 0 0))
    (path-use plottingarea t)
    (draw-node plottingarea x y style node)))

(defun make-histogram (min bin-size data)
  "A histogram as a simple plist"
  (list :min min :bin-size bin-size :data data))

(defun make-histogram-path-points (histo)
  "Make the path for a histogram, the path here is just a list of points."
  (let* ((y-pos (coerce (getf histo :data) 'list))
	 (x-pos (make-range (getf histo :min) (getf histo :bin-size) (+ 1 (length y-pos))))
	 (path-x nil)
	 (path-y nil))
    (flet ((add-point (x y) 
	     (push x path-x) (push y path-y)))
      (add-point (car x-pos) 0)
      (mapc (lambda (x-from y-from x-to)
	      (add-point x-from y-from) (add-point x-to y-from))
	    x-pos y-pos (cdr x-pos))
      (add-point (first (last x-pos)) (first (last y-pos)))
      (add-point (first (last x-pos)) 0))
    (values path-x path-y)))

(defun region-of-interest-zoom (top sub style &optional (top-left t) (top-right t) (bottom-left t) (bottom-right t))
  "Draw a rectangle around region of interest, and draw lines connecting it with a sub-figure."
 (scope (top style)
    ;;Region of interest
    (transform (top) (make-rectangle-path top (plot-x-min sub) (plot-y-min sub) (plot-x-max sub) (plot-y-max sub)))
    (path-use top)
    (format (ostream top) "\\pgfseteorule~%")
    ;;Clipping area, inverted clip of the region of interest and the sub plottingarea
    (transform (sub) (make-rectangle-path sub (plot-x-min sub) (plot-y-min sub) (plot-x-max sub) (plot-y-max sub)))
    (transform (top) (make-rectangle-path top (plot-x-min sub) (plot-y-min sub) (plot-x-max sub) (plot-y-max sub)))
    (make-rectangle-path top
		  (min 0 (+ (x-offset sub))) (min 0 (+ (y-offset sub)))
		  (max (width top) (+ (x-offset sub) (width sub))) (max (height top) (+ (y-offset sub) (height sub))))
    (path-use top nil nil t)
    ;;Draw lines to connect the region of interest and the sub plotting area
    (flet ((line (x-f y-f x-t y-t)
	     (path-move-to top (apply-transform-x top x-f) (apply-transform-y top y-f))
	     (path-line-to top x-t y-t)))
    (when bottom-left (line (plot-x-min sub) (plot-y-min sub) (+ (x-offset sub)) (y-offset sub)))
    (when top-left (line (plot-x-min sub) (plot-y-max sub) (+ (x-offset sub)) (+ (y-offset sub) (height sub))))
    (when top-right (line (plot-x-max sub) (plot-y-max sub) (+ (x-offset sub) (width sub)) (+ (y-offset sub) (height sub))))
    (when bottom-right (line (plot-x-max sub) (plot-y-min sub) (+ (x-offset sub) (width sub)) (+ (y-offset sub))))
    (when (or bottom-right bottom-left top-right top-left) 
      (path-use top)))))

(defun legend (x y name &key (width 0.4) (height 0.2) (text-pos "right"))
  (list :x x :y y :name name :width width :height height :text-pos text-pos))

(defmacro expand-legend ((legend) &body body)
  (let ((symbols '(x y name width height text-pos)))
    (flet ((as-keyword (sym) (intern (string sym) :keyword)))
      `(let ,(mapcar (lambda (sym) `(,sym (getf ,legend ,(as-keyword sym)))) symbols)
	 (declare (ignorable ,@symbols))
	 ,@body))))

(defun legend-name (plottingarea legend)
  (expand-legend (legend)
    (unless (string= "" name)
      (draw-node plottingarea (+ x width) (+ y (* 0.5 height)) text-pos "" name))))

(defun legend-graph (plottingarea legend node-string line-style mark-style)
  (legend-line plottingarea legend line-style)
  (legend-node plottingarea legend node-string mark-style))

(defun legend-profile (plottingarea legend node-string style)
  (expand-legend (legend)
    (draw-profilepoint plottingarea (+ x (* 0.5 width)) (+ y (* .5 height))
		       (* 0.5 height) style :node node-string))
  (legend-name plottingarea legend))

(defun legend-histo (plottingarea legend style fill)
  "Legend entries for histograms."
  (if fill 
      (expand-legend (legend)
	(scope (plottingarea style)
	  (make-rectangle-path plottingarea x y (+ x width) (+ y height))
	  (path-use plottingarea t t))
	(legend-name plottingarea legend))
      (legend-line plottingarea legend style)))

(defun legend-node (plottingarea legend node-string style)
  "Legend entry for plots drawn as nodes."
  (expand-legend (legend)
    (draw-node plottingarea (+ x (* 0.5 width))
	       (+ y (* 0.5 height))  style node-string))
  (legend-name plottingarea legend))

(defun legend-line (plottingarea legend style)
  "Legend entry for plots represented as lines."
  (expand-legend (legend)
    (draw-line plottingarea x (+ y (* 0.5 height)) (+ x width) (+ y (* 0.5 height)) style)
    ;;Draw a transparent line to make it align with no text.
    (draw-line plottingarea x (+ y (* 1.0 height)) x (+ y (* 0.0 height)) "opacity=0.0,white"))
  (legend-name plottingarea legend))

(defun draw-profilepoints (plottingarea x y y-error style &key (node (make-node-string "circle" 3 3)) (legend nil))
  "Draw points with error bars in the y direction."
  (with-coerced-lists (x y y-error)
    (clip-and-transform (plottingarea)
      (mapcar (lambda (xx yy err) (draw-profilepoint plottingarea xx yy err style :node node))
	      x y y-error))
    (when legend (legend-profile plottingarea legend node style))))

(defun draw-histogram (plottingarea histo style &key (fill nil) (separate-bins nil) (legend nil))
  "Draw a histogram."
  (multiple-value-bind (x y) (make-histogram-path-points histo)
    (clip-and-transform (plottingarea)
      (draw-path plottingarea x y style fill)
      (when separate-bins
	(mapcar (lambda (x y) (draw-path plottingarea (list x x) (list y 0) style nil)) x y))))
  (when legend (legend-histo plottingarea legend style fill)))

(defun draw-histogram-columns (plottingarea histo style width &key (legend nil))
  (let* ((bin-size (getf histo :bin-size))
	 (y-pos (coerce (getf histo :data) 'list))
	 (x-pos (make-range (+ (getf histo :min) (* 0.5 bin-size))
			    bin-size (+ 1 (length y-pos)))))
    (scope (plottingarea style)
      (clip-and-transform (plottingarea)
	(mapcar (lambda (x y)
		  (make-rectangle-path plottingarea (- x (* 0.5 width)) 0
				       (+ x (* 0.5 width)) y)
		  (path-use plottingarea t t)) 
		x-pos y-pos))))
  (when legend (legend-histo plottingarea legend style t)))
  
(defun draw-datapoints (plottingarea x y style &key (node (make-node-string "circle" 3 3)) (legend nil))
  "Draw a set of datapoints"
  (clip-and-transform (plottingarea)
    (map 'nil (lambda (x y) (draw-node plottingarea x y style node)) x y))
  (when legend (legend-node plottingarea legend node style)))

(defun draw-graph (plottingarea x y line-style mark-style &key (node (make-node-string "circle" 3 3)) (legend nil))
  "Draw a graph, with a line connecting datapoints"
  (clip-and-transform (plottingarea)
    (draw-path plottingarea x y line-style)
    (draw-datapoints plottingarea x y mark-style :node node))
  (when legend (legend-graph plottingarea legend node line-style mark-style)))

(defun draw-graph-spline (plottingarea x y line-style mark-style  
			  &key (node (make-node-string "circle" 3 3)) (legend nil) (natural nil))
  "Draw a graph, with a spline connecting datapoints"
  (clip-and-transform (plottingarea)
    (draw-function plottingarea (tikz-spline:get-spline-fun x y natural) 100 line-style)
    (draw-datapoints plottingarea x y mark-style :node node))
  (when legend (legend-graph plottingarea legend node line-style mark-style)))

(defun get-function-points (function samples x-min x-max &optional (close nil))
  "Make a path of y = f(x)"
  (let* ((x-vals (make-range x-min (/ (- x-max x-min) samples) (+ 1 samples)))
	 (y-vals (mapcar (lambda (x) (funcall function x)) x-vals)))
    (if close
	(values (append (list x-max x-max x-min) x-vals)
		(append (list (first (last y-vals)) 0 0) y-vals))
	(values x-vals y-vals))))

(defun draw-function (plottingarea function samples line-style 
		      &key (x-min nil) (x-max nil) (legend nil) (fill nil) (close nil))
  "Draw a function y = f(x). If x-min and x-max are not supplied, it is drawn from plot-x-min
to plot-x-max. If close is t, the function path is closed by adding points (x-min,0) and (x-max,0). If 
fill is t, the path is also filled, should probably be used tohether with fill."
  (multiple-value-bind (x-vals y-vals) (get-function-points function samples
							    (if x-min x-min (plot-x-min plottingarea))
							    (if x-max x-max (plot-x-max plottingarea))
							    close)
    (clip-and-transform (plottingarea)
      (draw-path plottingarea x-vals y-vals line-style fill))
    (when legend (legend-line plottingarea legend line-style))))

