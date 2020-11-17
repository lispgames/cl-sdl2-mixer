(in-package #:cl-user)

(defpackage #:music-example
  (:use #:cl #:sdl2)
  (:export
   #:run-music-example))

(in-package #:music-example)

(require 'sdl2-mixer)

(defparameter *current-volume* 128)

(defun run-music-example ()
  (with-init (:everything)
    (sdl2-mixer:init :ogg)
    (sdl2-mixer:open-audio 22050 :s16sys 1 1024)
    (with-window (my-window :title "Mixer Example"
                            :w 100
                            :h 100
                            :flags '(:shown))
      (with-renderer (my-renderer my-window :flags '(:accelerated))
        (flet ((clear-renderer (renderer)
                 (progn (set-render-draw-color renderer 0 0 0 255)
                        (render-clear renderer))))
          (let ((music (sdl2-mixer:load-music
                        (asdf:system-relative-pathname
                         'sdl2-mixer-examples
                         "examples/example_song.ogg"))))
            (with-event-loop (:method :poll)
              (:keydown
               (:keysym keysym)
               (let ((scancode (scancode-value keysym)))
                 (cond ((scancode= scancode :scancode-space)
                        (format t "Playing Song~%")
                        (sdl2-mixer:play-music music 1))
                       ((scancode= scancode :scancode-up)
                        (when (< (+ *current-volume* 20) 128)
                          (incf *current-volume* 20)
                          (format t "Current Volume: ~a~%" *current-volume*)
                          (sdl2-mixer:volume-music *current-volume*)))
                       ((scancode= scancode :scancode-down)
                        (when (> (- *current-volume* 20) 0)
                          (decf *current-volume* 20)
                          (format t "Current Volume: ~a~%" *current-volume*)
                          (sdl2-mixer:volume-music *current-volume*))))))
              (:idle ()
                     (clear-renderer my-renderer)
                     (render-present my-renderer))
              (:quit ()
                     ;; Ensure all channels have been halted so to safely free
                     ;; any sound chunks we created
                     (sdl2-mixer:halt-music)
                     (sdl2-mixer:close-audio)
                     (sdl2-mixer:free-music music)
                     (sdl2-mixer:quit)
                     t))))))))
