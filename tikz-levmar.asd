(defpackage :tikz-levmar
  (:use :cl)
  (:nicknames :levmar)
  (:documentation "An implementtion of the Levenberg Marquart algorithm.")
  (:export :levmar-optimize :levmar-optimize-errors))

(defsystem tikz-levmar
  :name "tikz-levmar"
  :version "0.0.1"
  :maintainer "haavagj"
  :description "A package for fitting functions to data."
  :depends-on (:lla)
  :components ((:file "levmar")))
