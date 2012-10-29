(defpackage #:tikz-helper
  (:use :cl)
  (:nicknames :tikz)
  (:documentation "A package for making tikz drawings")
  (:export plottingarea
	   with-tikz-plot
	   with-sugfigure
	   scope
	   clip
	   transform
	   clip-and-transform
	   make-range
	   make-transformation
	   make-histogram
	   make-node-string
	   draw-histogram
	   draw-histogram-horizontal
	   make-path
	   make-rectangle-path
	   make-path-mixed-units
	   path-close
	   path-stroke
	   draw-path
	   draw-graph
	   draw-graph-error
	   draw-graph-spline
	   draw-function
	   draw-profilepoint
	   draw-profilepoints
	   draw-legend-line
	   draw-legend-rectangle
	   draw-axis-ticks-x
	   draw-axis-ticks-y
	   draw-axis-subticks-x
	   draw-axis-subticks-y
	   draw-plottingarea-rectangle
	   draw-line
	   draw-text-node
	   draw-node
	   draw-datapoints
	   path-move-to
	   path-stroke-to
	   path-stroke
	   region-of-interest-zoom
	   pdflatex-compile
	   pdflatex-compile-view))

(defsystem tikz-helper
  :name "tikz-helper"
  :version "0.0.1"
  :maintainer "haavagj"
  :description "tikz-helper"
  :depends-on (:lla :tikz-spline)
  :components ((:file "tikz-helper")))
