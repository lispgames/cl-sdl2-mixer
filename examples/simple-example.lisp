(in-package :cl-user)

(defpackage :simple-example
  (:use :cl :sdl2 :cl-opengl)
  (:export :run-simple-example))

(in-package :simple-example)

;;Not using shadowing-imports for the mixer init and quit functions for illustrative purposes
(require 'sdl2-mixer)

(defun run-simple-example ()
  (with-init (:everything)
    (sdl2-mixer:init :ogg)
    (sdl2-mixer:open-audio 22050 :s16sys 1 1024)
    (with-window (my-window :title "Mixer Example" :w 100 :h 100 :flags '(:shown))
      (with-renderer (my-renderer my-window :flags '(:accelerated))
        (flet ((clear-renderer (renderer)
                 (progn (set-render-draw-color renderer 0 0 0 255)
                        (render-clear renderer))))
          (let ((sound-effect (sdl2-mixer:load-wav (asdf:system-relative-pathname 'sdl2-mixer-examples "examples/sample.ogg"))))
            (with-event-loop (:method :poll)
              (:keydown
               (:keysym keysym))
              (:idle ()
                     (clear-renderer my-renderer)
                     (render-present my-renderer))
              (:quit ()
                     (sdl2-mixer:free-chunk sound-effect)
                     (sdl2-mixer:close-audio)
                     (sdl2-mixer:quit)
                     t))))))))
