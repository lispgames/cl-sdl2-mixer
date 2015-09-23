(defpackage :sdl2-mixer-examples.asdf
  (:use :cl :asdf))

(in-package :sdl2-mixer-examples.asdf)

(defsystem :sdl2-mixer-examples
  :description "A few examples to demonstrate the usage of sdl2-mixer"
  :author "Bryan Baraoidan"
  :license "MIT"
  :depends-on (:sdl2 :sdl2-mixer)
  :pathname "examples"
  :serial t
  :components ((:file "simple-example")
               (:file "music")))
