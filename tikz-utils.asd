(defpackage #:tikz-utils
  (:use :cl)
  (:documentation "Some functions used in examples")
  (:export gamma
	   gaussian-random
	   gauss
	   erf))

(defsystem tikz-utils
  :name "tikz-utils"
  :version "0.0.1"
  :maintainer "haavagj"
  :description "tikz-utils"
  :components ((:file "utils")))
