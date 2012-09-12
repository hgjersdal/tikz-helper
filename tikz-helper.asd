(defpackage #:tikz-helper
  (:use :cl)
  (:nicknames :tikz)
  (:documentation "A package for making tikz drawings")
  (:export plottingarea
	   with-tikz-plot
	   scope
	   clip
	   make-range
	   make-transformation
	   make-histogram
	   draw-histogram
	   draw-histogram-bins
	   draw-graph
	   draw-graph-error
	   draw-graph-line
	   draw-function
	   draw-profilepoint
	   draw-profilepoints
	   draw-legend-line
	   draw-legend-rectangle
	   draw-axis-ticks-x-transformed 
	   draw-axis-ticks-y-transformed 
	   draw-axis-ticks-x
	   draw-axis-ticks-y
	   draw-plottingarea-rectangle
	   draw-line
	   draw-text-node
	   draw-node
	   draw-datapoints
	   tikz-transform-x
	   tikz-transform-y
	   gaussian-random
	   levmar-optimize
	   levmar-optimize-errors
	   gamma
	   gauss
	   erf
	   get-spline-fun))

(defsystem tikz-helper
  :name "tikz-helper"
  :version "0.0.1"
  :maintainer "haavagj"
  :description "tikz-helper"
  :depends-on (:lla)
  :components ((:file "utils")
	       (:file "levmar")
	       (:file "spline")
	       (:file "tikz-helper")))
