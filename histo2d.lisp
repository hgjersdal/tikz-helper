(in-package :tikz-helper)

(defun make-histogram2d (x-min x-bin-size x-nbin y-min y-bin-size y-nbin)
  "Alloacte a histogram"
  (let ((data (make-array (list x-nbin y-nbin) :element-type 'double-float)))
    (list :x-min x-min :x-bin-size x-bin-size :x-nbin x-nbin :y-min y-min :y-bin-size y-bin-size :y-nbin y-nbin :data data)))

(defun make-vectorfield2d (x-min x-bin-size x-nbin y-min y-bin-size y-nbin)
  (let ((data (make-array (list x-nbin y-nbin) :element-type '(simple-vector 2))))
    (list :x-min x-min :x-bin-size x-bin-size :x-nbin x-nbin :y-min y-min :y-bin-size y-bin-size :y-nbin y-nbin :data data)))

(defun histo2d-incf (histo x y &optional (val 1.0d0))
  "Increment the histogram bin at x y"
  (let ((x-bin (floor (- x  (getf histo :x-min)) (getf histo :x-bin-size)))
	(y-bin (floor (- y (getf histo :y-min)) (getf histo :y-bin-size))))
    (when (and (>= x-bin 0) (>= y-bin 0)
	       (< x-bin (getf histo :x-nbin)) (< y-bin (getf histo :y-nbin)))
      (incf (aref (getf histo :data) x-bin y-bin) val))))

(defun histo2d-get-max (histo)
  "Get the bin with the largest content"
  (max (reduce #'max (make-array (* (getf histo :x-nbin)
				    (getf histo :y-nbin))
				 :element-type 'double-float
				 :displaced-to (getf histo :data)))))

(defun make-color-combo (z-min z-max val cols)
  ;; "Make a color somehwere between first and last in color, with gradients in between"
  (let* ((ncol (length cols))
	 (nbins (- ncol 1))
	 (z% (/ (* 100 nbins (- val z-min)) (- z-max z-min))))
    (multiple-value-bind (zbin col) (floor z% 100)
      (cond ((< zbin 0) (first cols))
	    ((>= zbin nbins) (elt cols (- ncol 1)))
	    (t (format nil "~a!~a!~a" (elt cols (1+ zbin)) (floor col)
		       (elt cols zbin)))))))

(defun draw-histo2d-rectangles (plottingarea histo z-min z-max
				&optional (cols *colors*))
  "Draw a rectangle for each bin. Colors go from cold to hot"
  (clip-and-transform (plottingarea)
    (let ((x-poses (make-range (getf histo :x-min) (getf histo :x-bin-size) (getf histo :x-nbin)))
	  (y-poses (make-range (getf histo :y-min) (getf histo :y-bin-size) (getf histo :y-nbin)))
	  (x-bins (make-range 0 1 (getf histo :x-nbin)))
	  (y-bins (make-range 0 1 (getf histo :y-nbin))))
      (labels ((draw-bin (x-pos y-pos x-bin y-bin)
		 (scope (plottingarea (format nil "draw=black,ultra thin, fill=~a"
					      (make-color-combo z-min z-max (aref (getf histo :data) x-bin y-bin) 
								cols)))
		   (make-rectangle-path plottingarea x-pos y-pos
					(+ x-pos (getf histo :x-bin-size))
					(+ y-pos (getf histo :y-bin-size)))
		   (path-use plottingarea t t nil)))
	       (draw-row (y-bin y-pos)
		 (map nil (lambda (x-bin x-pos) (draw-bin x-pos y-pos x-bin y-bin)) x-bins x-poses)))
	(map nil #'draw-row y-bins y-poses)))))

(defun check-neighbour (x y val data cmap)
  "See if there are borders around the cell. Border means the cell is above val, the neighbour is below."
  (flet ((isbelow (x y)
	   (let ((dim (array-dimensions cmap)))
	     (cond ((or (< x 0) (< y 0)) t)
		   ((or (>= x (first dim)) (>= y (second dim))) t)
		   (t (< (aref data x y) val))))))
    (when (>= (aref data x y) val)
      ;;Vector is left up right below
      (let ((nb (vector (isbelow (- x 1) y) (isbelow x (+ y 1))
			(isbelow (+ x 1) y) (isbelow x (- y 1)))))
	(when (reduce (lambda (x y) (or x y)) nb)
	  (setf (aref cmap x y) nb))))))

(defun make-contour-cells (histo cmap z-val)
  "Count borders for all the cells in histo"
  (let ((data (getf histo :data))
	(xbins (make-range 0 1 (getf histo :x-nbin)))
	(ybins (make-range 0 1 (getf histo :y-nbin))))
    (mapc (lambda (x) (mapc (lambda (y) (check-neighbour x y z-val data cmap)) ybins)) xbins)))

(defun dir-to-num (dir)
  (case dir (:left 0) (:up 1) (:right 2) (:down 3)))

(defun num-to-dir (num)
  (elt (list :left :up :right :down) num))

(defun oposite-dir (dir)
  (let ((num (dir-to-num dir)))
    (num-to-dir (if (> num 1) (- num 2) (+ 2 num)))))

(defun turn-left (dir)
  (let ((num (dir-to-num dir)))
    (num-to-dir (if (= num 0) 3 (- num 1)))))

(defun turn-right (dir)
  (let ((num (dir-to-num dir)))
    (num-to-dir (if (= num 3) 0 (+ num 1)))))

(defun turn-left-xy (dir xy)
  (mapcar #'+ xy
	  (case dir (:left (list -1 -1)) (:up (list -1 1))
		(:right (list 1 1)) (:down (list 1 -1)))))

(defun continue-xy (dir xy)
  (mapcar #'+ xy
	  (case dir (:left  (list -1 0)) (:up    (list 0 1))
		(:right (list 1 0)) (:down  (list 0 -1)))))

(defun borderp (xy borderdir cmap)
  "Does cell xy have a border in direction borderdir?"
  (let ((dim (array-dimensions cmap)))
    (cond ((minusp (elt xy 0)) nil)
	  ((minusp (elt xy 1)) nil)
	  ((>= (elt xy 0) (elt dim 0)) nil)
	  ((>= (elt xy 1) (elt dim 1)) nil)
	  ((null (aref cmap (first xy) (second  xy))) nil)
	  ((aref (aref cmap (first xy) (second  xy)) (dir-to-num borderdir)) t))))

(defun border-correction (max-bin min-bin val)
  "Linear interpolation"
  (if (= max-bin min-bin) 0.0
      (- (/ (- val min-bin) (- max-bin min-bin))  0.5d0)))

(defun move-to-border (xy dir histo val)
  "Which side of the cell should the contour line be on? Positions are corrected with a linear interpolation."
  (flet ((get-correction (xy x-shift y-shift)
	   (let ((dim (array-dimensions (getf histo :data))))
	     (if (or (minusp (+ x-shift (first xy))) (minusp (+ y-shift (second xy)))
		     (>= (+ x-shift (first xy)) (elt dim 0)) (>= (+ y-shift (second xy)) (elt dim 1))
		     (<= val 0))
		 0.0
		 (border-correction (aref (getf histo :data) (first xy) (second xy))
				    (aref (getf histo :data) (+ x-shift (first xy)) (+ y-shift (second xy)))
				    val)))))
    (mapcar #'+ xy (case dir
		     (:left (list (get-correction xy -1 0) 0.5))
		     (:up (list 0.5 (- 1.0 (get-correction xy 0 1))))
		     (:right (list (- 1.0 (get-correction xy 1 0)) 0.5))
		     (:down (list 0.5 (get-correction xy 0 -1)))))))

(defun add-contour-path-point (xy dir histo plottingarea val)
  "Add a contour point to path,"
  (let* ((xy (move-to-border xy dir histo val))
	 (x (+ (getf histo :x-min) (* (elt xy 0) (getf histo :x-bin-size))))
	 (y (+ (getf histo :y-min) (* (elt xy 1) (getf histo :y-bin-size)))))
    (path-line-to plottingarea x y)))

(defun continue-path (xy dir cmap histo plottingarea val)
  "Stroll along path with border at left hand side."
  (let ((borderdir (turn-left dir)))
    (setf (aref (aref cmap (first xy) (second xy)) (dir-to-num borderdir)) nil)
    (add-contour-path-point xy borderdir histo plottingarea val))
  (cond ((borderp (turn-left-xy dir xy) (oposite-dir dir) cmap)
	 (continue-path (turn-left-xy dir xy) (turn-left dir) cmap histo plottingarea val))
	((borderp (continue-xy dir xy) (turn-left dir) cmap)
	 (continue-path (continue-xy dir xy) dir cmap histo plottingarea val))
	((borderp xy dir cmap)
	 (continue-path xy (turn-right dir) cmap histo plottingarea val))))

(defun start-contour-line (x y cmap histo plottingarea val)
  "Start the contour line, if cell has borders."
  (let ((cell (aref cmap x y)))
    (unless (or (null cell) (reduce (lambda (x y) (and (not x) (not y))) cell))
      (let* ((dir (num-to-dir 
		   (cond ((aref cell 0) 0) ((aref cell 1) 1) ((aref cell 2) 2) ((aref cell 3) 3))))
	     (xy (move-to-border (list x y) dir histo val))
	     (xx (+ (getf histo :x-min) (* (first xy) (getf histo :x-bin-size))))
	     (yy (+ (getf histo :y-min) (* (second xy) (getf histo :y-bin-size)))))
	(path-move-to plottingarea xx yy)
	(continue-path (list x y) (turn-right dir) cmap histo plottingarea val)
	(path-close plottingarea)
	(list x y)))))

(defun draw-histo2d-contour (plottingarea histo z-min z-max nlines fillp
			     &optional (cols *colors*))
  "Draw possibly filled contour lines."
  (clip-and-transform (plottingarea)
    (let ((cmap (make-array (list (getf histo :x-nbin) (getf histo :y-nbin)) :initial-element nil))
	  (data (getf histo :data))
	  (xbins (make-range 0 1 (getf histo :x-nbin)))
	  (ybins (make-range 0 1 (getf histo :y-nbin))))
      (dotimes (i (+ nlines 1))
	(let ((height (+ z-min (* i (/ (- z-max z-min) nlines)))))
	  (mapcar (lambda (x) (mapcar (lambda (y) (check-neighbour x y height data cmap)) ybins)) xbins)
	  (scope (plottingarea (format nil "draw=black,fill=~a" (make-color-combo z-min z-max height cols)))
	    (mapcar (lambda (x) (mapcar (lambda (y) (start-contour-line x y cmap histo plottingarea height)) ybins)) xbins)
	    (path-use plottingarea t fillp)))))))

(defun draw-histo2d-contour (plottingarea histo z-min z-max nlines fillp
			     &key (cols *colors*) (color-lines nil))
  "Draw possibly filled contour lines."
  (clip-and-transform (plottingarea)
    (let ((cmap (make-array (list (getf histo :x-nbin) (getf histo :y-nbin)) :initial-element nil))
	  (data (getf histo :data))
	  (xbins (make-range 0 1 (getf histo :x-nbin)))
	  (ybins (make-range 0 1 (getf histo :y-nbin))))
      (dotimes (i (+ nlines 1))
	(let ((height (+ z-min (* i (/ (- z-max z-min) nlines)))))
	  (mapcar (lambda (x) (mapcar (lambda (y) (check-neighbour x y height data cmap)) ybins)) xbins)
	  (scope (plottingarea (if color-lines 
				   (format nil  "~a" (make-color-combo z-min z-max height cols))
				   (format nil  "draw=black,fill=~a" (make-color-combo z-min z-max height cols))))
	    (mapcar (lambda (x) (mapcar (lambda (y) (start-contour-line x y cmap histo plottingarea height)) ybins)) xbins)
	    (path-use plottingarea t fillp)))))))

(defun val-to-size (val z-min z-max)
  (cond ((< val z-min) 0.0)
	((> val z-max) 1.0)
	(t (/ (- val z-min) (- z-max z-min)))))

(defun draw-histo2d-nodes (plottingarea histo z-min z-max shape style)
  (let* ((dim (array-dimensions (getf histo :data)))
	 (x-max (/ (width plottingarea) (first dim)))
	 (y-max (/ (height plottingarea) (second dim)))
	 (xbins (make-range 0 1 (getf histo :x-nbin)))
	 (ybins (make-range 0 1 (getf histo :y-nbin)))
	 (style-name (string (gensym "style"))))
    (clip-and-transform (plottingarea)
      (tikz-style plottingarea style-name (format nil "~a,~a,inner sep =0" style shape))
      (mapc (lambda (x) (mapc (lambda (y) 
				(let ((size (val-to-size (aref (getf histo :data) x y) z-min z-max)))
				  (when (> size 0.001)
				    (draw-node plottingarea
					       (+ (getf histo :x-min)  (* (+ 0.5 x) (getf histo :x-bin-size)))
					       (+ (getf histo :y-min) (* (+ 0.5 y) (getf histo :y-bin-size)))
					       style-name
					       (format nil "minimum height=~4,2fcm,minimum width=~4,2fcm" (* y-max size) (* x-max size))))))
			      ;;(make-node-string shape (* x-max size) (* y-max size) 0 "cm")))))
			      ybins)) xbins))))
  
    
