(defpackage :tikz-spline
  (:use :cl)
  (:nicknames :spline)
  (:documentation "A package for making qubic spline functions")
  (:export :get-spline-fun))

(defsystem tikz-spline
  :name "tikz-spline"
  :version "0.0.1"
  :maintainer "haavagj"
  :description "A package for making qubic spline functions"
  :depends-on (:lla)
  :components ((:file "spline")))
