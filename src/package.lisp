(in-package defpackage+-user-1)

(defpackage+ :sdl2-mixer
  (:use #:cl #:alexandria #:autowrap.minimal #:plus-c #:sdl2-ffi.functions)
  (:export-only

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
   #:halt-music
   #:volume-music
   #:pause
   #:resume
   #:paused
   #:get-num-paused-channels
   #:pause-music
   #:resume-music
   #:rewind-music
   #:set-music-position))

(in-package :sdl2-mixer)

 ;; Variables

