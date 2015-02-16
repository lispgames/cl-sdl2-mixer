(cl:in-package :sdl2-ffi)

(autowrap:c-include
 `(sdl2-mixer autowrap-spec "SDL_mixer.h")
  :function-package :sdl2-ffi.functions
  :spec-path '(sdl2-mixer autowrap-spec)
  :exclude-sources ("/usr/local/lib/clang/([^/]*)/include/(?!stddef.h)"
                    "/usr/include/"
                    "/usr/include/arm-linux-gnueabihf"
                    "/usr/local/include/SDL2")
  :include-sources ("SDL_mixer.h")
  :exclude-constants ("^(?!MIX)")
  :symbol-exceptions (("SDL_RWops" . "SDL-RWOPS"))
  :no-accessors cl:t
  :release-p cl:t)
