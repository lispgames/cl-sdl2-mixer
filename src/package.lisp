(in-package #:cl-user)

(defpackage #:sdl2-mixer
  (:use #:cl #:alexandria #:autowrap.minimal #:plus-c #:sdl2-ffi.functions)
  (:export
   ;; Conditions
   #:sdl-mixer-error
   ;; General
   #:linked-version
   #:init
   #:quit
   #:open-audio
   #:close-audio
   #:query-spec
   #:load-wav
   #:allocate-channels
   #:volume
   #:play-channel
   #:halt-channel
   #:playing
   #:free-chunk
   #:load-music
   #:free-music
   #:play-music
   #:fade-in-music
   #:pause-music
   #:resume-music
   #:fade-out-music
   #:halt-music
   #:volume-music))
