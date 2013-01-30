(defpackage :tikz-helper
  (:use :cl)
  (:nicknames :tikz)
  (:documentation "A package for making tikz drawings")
  (:export :plottingarea :with-tikz-to-file :with-tikz-to-stream :with-tikz-to-string :with-subfigure
	   :scope :clip :transform :clip-and-transform
	   :make-range :make-transformation :make-histogram
	   :make-node-string :draw-histogram :draw-histogram-columns
	   :make-path :make-rectangle-path :make-path-mixed-units
	   :path-close :path-stroke :draw-path :draw-graph :draw-graph-error
	   :draw-graph-spline :get-function-points :draw-function :draw-profilepoint :draw-profilepoints
	   :draw-legend-entry :legend :draw-axis-ticks-x :draw-axis-ticks-y
	   :draw-axis-subticks-x :draw-axis-subticks-y 
	   :draw-axis-rectangle :draw-axis-cross :draw-axis-popped-out :draw-grid-lines :draw-axis-left-bottom
	   :draw-line :draw-node :draw-datapoints :path-move-to
	   :path-stroke-to :path-use :region-of-interest-zoom :pdflatex-compile 
	   :pdflatex-compile-view :color-palette :*colors*
	   :histo2d-incf :histo2d-get-max :draw-histo2d-rectangles :draw-histo2d-contour :draw-histo2d-nodes :make-histogram2d
	   :make-vectorfield2d :draw-vectorfield2d))

(defsystem tikz-helper
  :name "tikz-helper"
  :version "0.0.1"
  :maintainer "haavagj"
  :description "A package for drawing tikz plots."
  :depends-on (:lla :tikz-spline)
  :components ((:file "tikz-helper")
	       (:file "axis" :depends-on ("tikz-helper"))
	       (:file "histo2d" :depends-on ("tikz-helper" "axis"))
	       (:file "vectorfield2d" :depends-on ("tikz-helper" "axis" "histo2d"))))
