(defpackage :tikz-utils
  (:use :cl :cffi)
  (:documentation "Some functions used in examples")
  (:nicknames :tut)
  (:export :gamma :gaussian-random :gauss :erf :make-random-list))

(defsystem tikz-utils
  :name "tikz-utils"
  :version "0.0.1"
  :maintainer "haavagj"
  :description "Helerp functions"
  :components ((:file "utils")))
