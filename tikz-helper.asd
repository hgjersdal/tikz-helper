(defpackage #:tikz-helper
  (:use :cl)
  (:nicknames :tikz)
  (:documentation
   "A common lisp package for making tikz drwings")
  (:export :plottingarea
	   :with-tikz-plot
	   :make-transformation-x
	   :make-transformation-y
	   :tikz-clip
	   :draw-histogram
	   :draw-histogram-top
	   :draw-plottingarea-rectangle
	   :draw-tikz-x-transformed 
	   :draw-tikz-y-transformed
	   :draw-tikz-line 
	   :draw-tikz-node
	   :draw-tikz-x
	   :draw-tikz-y
	   :tikz-transform-x
	   :tikz-transform-y))



(defsystem tikz-helper
  :name "tikz-helper"
  :version "0.0.1"
  :maintainer "haavagj"
  :description "tikz-helper"
  :depends-on ()
  :components ((:file "tikz-helper")))
