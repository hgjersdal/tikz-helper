(defpackage #:tikz-helper
  (:use :cl)
  (:nicknames :tikz)
  (:documentation "A package for making tikz drawings")
  (:export plottingarea
	   with-tikz-plot
	   with-tikz-plot-standalone
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
	   draw-path
	   draw-graph
	   draw-graph-error
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
	   draw-graph-spline
	   transform-x
	   transform-y
	   gaussian-random
	   levmar-optimize
	   levmar-optimize-errors
	   gamma
	   gauss
	   erf
	   get-spline-fun
	   path-move-to
	   path-stroke-to
	   path-stroke
	   connect-plots
	   ))

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
