(defpackage :tikz-helper
  (:use :cl)
  (:nicknames :tikz)
  (:documentation "A package for making tikz drawings")
  (:export :plottingarea :with-tikz-plot :with-sugfigure
	   :scope :clip :transform :clip-and-transform
	   :make-range :make-transformation :make-histogram
	   :make-node-string :draw-histogram :draw-histogram-horizontal
	   :make-path :make-rectangle-path :make-path-mixed-units
	   :path-close :path-stroke :draw-path :draw-graph :draw-graph-error
	   :draw-graph-spline :draw-function :draw-profilepoint :draw-profilepoints
	   :draw-legend-entry :draw-axis-ticks-x :draw-axis-ticks-y
	   :draw-axis-subticks-x :draw-axis-subticks-y 
	   :draw-axis-rectangle :draw-axis-cross :draw-axis-popped-out :draw-grid-lines :draw-axis-left-bottom
	   :draw-line :draw-node :draw-datapoints :path-move-to
	   :path-stroke-to :path-stroke :region-of-interest-zoom :pdflatex-compile 
	   :pdflatex-compile-view
	   :histo2d-incf :histo2d-get-max :draw-histo2d-rectangles :draw-histo2d-contour :make-histogram2d))

(defsystem tikz-helper
  :name "tikz-helper"
  :version "0.0.1"
  :maintainer "haavagj"
  :description "A package for drawing tikz plots."
  :depends-on (:lla :tikz-spline)
  :components ((:file "tikz-helper")
	       (:file "axis" :depends-on ("tikz-helper"))
	       (:file "histo2d" :depends-on ("tikz-helper"))))
