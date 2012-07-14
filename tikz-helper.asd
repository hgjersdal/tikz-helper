(defpackage #:tikz-helper
  (:use :cl)
  (:nicknames :tikz)
  (:documentation "A package for making tikz drawings")
  (:export plottingarea
	   with-tikz-plot
	   clip
	   make-range
	   make-histogram
	   draw-histogram
	   draw-histogram-top
	   draw-graph
	   draw-graph-line
	   draw-function
	   draw-profilepoint
	   draw-legend-line
	   draw-legend-rectangle
	   draw-axis-ticks-x-transformed 
	   draw-axis-ticks-y-transformed 
	   draw-axis-ticks-x
	   draw-axis-ticks-y
	   draw-plottingarea-rectangle
	   draw-line
	   draw-node
	   tikz-transform-x
	   tikz-transform-y
	   gauss
	   gaussian-random
	   levmar-optimize
	   levmar-optimize-errors))

(defsystem tikz-helper
  :name "tikz-helper"
  :version "0.0.1"
  :maintainer "haavagj"
  :description "tikz-helper"
  :depends-on (:lla)
  :components ((:file "utils")
	       (:file "levmar")
	       (:file "tikz-helper")))
