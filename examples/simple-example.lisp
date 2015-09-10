(in-package :cl-user)

(defpackage :simple-example
  (:use :cl :sdl2 :cl-opengl)
  (:export :run-simple-example))

(in-package :simple-example)

;;Not using shadowing-imports for the mixer init and quit functions for illustrative purposes
(require 'sdl2-mixer)

;; Every channel's volume starts out at maximum according to the manual
(defparameter *current-volume* 128)

(defun run-simple-example ()
  (with-init (:everything)
    (sdl2-mixer:init :ogg)
    ;; These values are taken from the manual https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC11
    (sdl2-mixer:open-audio 22050 :s16sys 1 1024)
    (sdl2-mixer:allocate-channels 1)
    (with-window (my-window :title "Mixer Example" :w 100 :h 100 :flags '(:shown))
      (with-renderer (my-renderer my-window :flags '(:accelerated))
        (flet ((clear-renderer (renderer)
                 (progn (set-render-draw-color renderer 0 0 0 255)
                        (render-clear renderer))))
          (let ((sound-effect (sdl2-mixer:load-wav (asdf:system-relative-pathname 'sdl2-mixer-examples "examples/sample.ogg"))))
            (with-event-loop (:method :poll)
              (:keydown
               (:keysym keysym)
               (let ((scancode (scancode-value keysym)))
                 ;; Channels are 0 indexed
                 (cond ((scancode= scancode :scancode-space) (sdl2-mixer:play-channel 0 sound-effect 0))
                       ((scancode= scancode :scancode-up) (when (< (+ *current-volume* 20) 128)
                                                             (incf *current-volume* 20)
                                                             (format t "Current Volume: ~a~%" *current-volume*)
                                                             (sdl2-mixer:volume 0 *current-volume*)))
                       ((scancode= scancode :scancode-down) (when (> (- *current-volume* 20) 0)
                                                               (decf *current-volume* 20)
                                                               (format t "Current Volume: ~a~%" *current-volume*)
                                                               (sdl2-mixer:volume 0 *current-volume*))))))
              (:idle ()
                     (clear-renderer my-renderer)
                     (render-present my-renderer))
              (:quit ()
                     ;; Ensure all channels have been halted so to safely free any sound chunks we created
                     (sdl2-mixer:halt-channel -1)
                     (sdl2-mixer:close-audio)
                     (sdl2-mixer:free-chunk sound-effect)
                     (sdl2-mixer:quit)
                     t))))))))
