(defpackage :tikz-utils
  (:use :cl)
  (:documentation "Some functions used in examples")
  (:nicknames :tut)
  (:export :gamma :gaussian-random :gauss :erf))

(defsystem tikz-utils
  :name "tikz-utils"
  :version "0.0.1"
  :maintainer "haavagj"
  :description "Helerp functions"
  :components ((:file "utils")))
