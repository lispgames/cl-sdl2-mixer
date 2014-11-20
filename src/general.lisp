(in-package :sdl2-mixer)

(defun linked-version ()
  (c-let ((version sdl2-ffi:sdl-version :from (mix-linked-version)))
    (values (version :major) (version :minor) (version :patch))))

(autowrap:define-bitmask-from-enum (init-flags sdl2-ffi:mix-init-flags))

(defun init (&rest flags)
  (mix-init (mask-apply 'init-flags flags)))

(defun quit ()
  (mix-quit))

(autowrap:define-enum-from-constants (audio-format)
  sdl2-ffi:+audio-u8+
  sdl2-ffi:+audio-s8+
  sdl2-ffi:+audio-u16lsb+
  sdl2-ffi:+audio-s16lsb+
  sdl2-ffi:+audio-u16msb+
  sdl2-ffi:+audio-s16msb+
  sdl2-ffi:+audio-u16+
  sdl2-ffi:+audio-s16+
  sdl2-ffi:+audio-u16sys+
  sdl2-ffi:+audio-s16sys+)

(defun open-audio (frequency format channels chunksize)
  (check-rc (mix-open-audio frequency
                            (enum-value '(:enum (audio-format)) format)
                            channels chunksize)))

(defun close-audio ()
  (mix-close-audio))

(defun query-format ()
  (c-with ((freq :int)
           (fmt sdl2-ffi:uint16)
           (chans :int))
    (check-non-zero (mix-query-spec (freq &) (fmt &) (chans &)))
    (values freq (enum-key '(:enum (audio-format)) fmt) chans)))
