(defpackage :sdl2-mixer.asdf
  (:use #:cl #:asdf))

(in-package :sdl2-mixer.asdf)

(defsystem :sdl2-mixer
  :description "Bindings for sdl2_mixer using autowrap"
  :author "Ryan Pavlik"
  :license "MIT"
  :version "1.0"

  :depends-on (:alexandria :defpackage-plus :cl-autowrap :sdl2)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "library")
   (:file "autowrap")
   (:file "conditions")
   (:file "general")

   (:module autowrap-spec
    :pathname "spec"
    :components
    ((:static-file "SDL_mixer.h")))))
