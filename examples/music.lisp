(in-package :cl-user)
(defpackage :music-example
  (:use :cl :sdl2)
  (:export :run-music-example))

(in-package :music-example)

(require 'sdl2-mixer)
