(defpackage #:tikz-spline
  (:use :cl)
  (:nicknames :spline)
  (:documentation "A package for making quadric spline functions")
  (:export get-spline-fun))

(defsystem tikz-spline
  :name "tikz-spline"
  :version "0.0.1"
  :maintainer "haavagj"
  :description "tikz-spline"
  :depends-on (:lla)
  :components ((:file "spline")))
