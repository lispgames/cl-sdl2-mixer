(in-package :sdl2-mixer)

(cffi:define-foreign-library libsdl2-mixer
  ;; I have no idea if this is the correct framework, file an issue
  ;; and let me know!
  (:darwin (:or (:framework "SDL2_mixer") (:default "libSDL2_mixer")))
  (:unix (:or "libSDL2_mixer-2.0.so.0" "libSDL2_mixer"))
  (:windows "SDL2_mixer.dll")
  (t (:default "libSDL2_mixer")))

(cffi:use-foreign-library libsdl2-mixer)
