(in-package #:sdl2-mixer)

(defmacro create-sdl-free-function (free-function sdl-object)
  `(progn (tg:cancel-finalization ,sdl-object)
          (,free-function ,sdl-object)
          (autowrap:invalidate ,sdl-object)))

(defun linked-version ()
  "Returns the version number for SDL Mixer 2"
  (c-let ((version sdl2-ffi:sdl-version :from (mix-linked-version)))
    (values (version :major) (version :minor) (version :patch))))

(autowrap:define-bitmask-from-enum (init-flags sdl2-ffi:mix-init-flags))

(defun init (&rest flags)
  "Initialize the SDL mixer specifying the formats you wish to use. Must be one
of these values or a combination thereof :ogg, :wave, :mod, :mp3"
  (mix-init (mask-apply 'init-flags flags)))

(defun quit ()
  "Cleans up SDL Mixer"
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
  sdl2-ffi:+audio-s16sys+
  sdl2-ffi:+audio-s32lsb+
  sdl2-ffi:+audio-s32msb+
  sdl2-ffi:+audio-s32sys+
  sdl2-ffi:+audio-s32+
  sdl2-ffi:+audio-f32lsb+
  sdl2-ffi:+audio-f32msb+
  sdl2-ffi:+audio-f32sys+
  sdl2-ffi:+audio-f32+)

(defun open-audio (frequency format channels chunksize)
  "Initialize the mixer specifiying the output sample format, number of output
channels (1 mono or 2 for stereo), and bytes used per output sample. format must
be one of the following values, :u8, :s8, :u16lsb, :s16lsb, :u16msb, :s16msb,
:u16, :s16, :u16sys, :s16sys, :s32lsb, :s32msb, :s32sys, :s32, :f32lsb, :f32msb, :f32sys, :f32"
  (check-rc (mix-open-audio frequency
                            (enum-value '(:enum (audio-format)) format)
                            channels
                            chunksize)))

(defun close-audio ()
  "Closes the mixer"
  (mix-close-audio))

(defun query-spec ()
  "Gets the output format in use by the opened audio device"
  (c-with ((freq :int)
           (fmt sdl2-ffi:uint16)
           (chans :int))
    (check-non-zero (mix-query-spec (freq &) (fmt &) (chans &)))
    (values freq (enum-key '(:enum (audio-format)) fmt) chans)))

(defun load-wav (sample-file-name)
  "Loads the sample specified by the sample-file-name. Returns a mix-chunk.
sdl2-mixer must be initialized and open-audio must be called prior to."
  ;;Note the original Mix_LoadWAV function is actually a C preprocessor function
  ;;macro that was not collected by c2ffi. However the manual does state that
  ;;Mix_LoadWAV is equivalent to calling
  ;;Mix_LoadWAV_RW(SDL_RWFromFile(file,"rb"), 1) where file is a character array
  ;;representing the file
  ;;https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC19
  (autocollect (ptr)
               (check-null (mix-load-wav-rw
                            (sdl-rw-from-file
                             (namestring sample-file-name) "rb") 1))
    (mix-free-chunk ptr)))

(defun free-chunk (chunk)
  "Free the memory used in the chunk and then free the chunk itself. Do not free
the chunk while it is playing; halt the channel it's playing on using
halt-channel prior to freeing the chunk."
  (create-sdl-free-function mix-free-chunk chunk))

(defun allocate-channels (channels)
  "Set the number of channels to be mixed. Opening too many channels may result
in a segfault. This can be called at any time even while samples are playing.
Passing a number lower than previous calls will close unused channels. It
returns the number of channels allocated. NOTE: Channels are 0 indexed!"
  ;;This supposedly never fails so no check is in place
  (mix-allocate-channels channels))

(defun volume (channel volume)
  "Set the volume on a given channel, pass -1 to set the volume for all
channels. The volume may range from 0 to 128. Passing in a number higher than
the maximum will automatically set it to the maximum while passing in a negatiev
will automatically set it to 0. Returns the current volume of the channel. NOTE:
Channels are 0 indexed!"
  (mix-volume channel volume))

(defun play-channel (channel mix-chunk loops)
  "Plays the mix-chunk (sound effect) loops+1 times on a given channel. Passing
-1 for the channel will play it on the first unreserved channel. Returns the
channel the sample is played on. NOTE: Channels are 0 indexed!"
  ;; The original Mix_PlayChannel function is just a function-like C
  ;; preprocessor macro much like Mix_LoadWAV which was not in the spec.
  ;; According to the docs Mix_PlayChannel is simply Mix_PlayChannelTimed with
  ;; ticks set to -1
  ;; https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer_frame.html
  (check-rc (mix-play-channel-timed channel mix-chunk loops -1)))

(defun playing (channel)
  "Checks whether or not a channel is currently playing. It will return a 1 for
playing and 0 otherwise. Passing -1 for the channel will specify how many
channels are playing."
  (mix-playing channel))

(defun halt-channel (channel)
  "Halt the channel or pass -1 to halt all channels. Always returns 0. NOTE:
Channels are 0 indexed!"
  (mix-halt-channel channel))

(defun load-music (music-file-name)
  "Loads music from a file. Returns a mix-music object"
  (autocollect (ptr)
      (check-null (mix-load-mus (namestring music-file-name)))
    (mix-free-music ptr)))

(defun free-music (mix-music-object)
  (create-sdl-free-function mix-free-music mix-music-object))

(defun play-music (mix-music-object &optional (loops -1))
  "Play the music as many times as specified by the optional loops argument. By
default loops is -1 which makes the music loop indefinitely. Returns 0 on
success -1 on error"
  (check-rc (mix-play-music mix-music-object
                            loops)))

(defun fade-in-music (mix-music-object ms &optional (loops -1))
  "Play a new music object, fading in the audio for MS milliseconds, looping as
specified by the optional loops arguent. By default loops is -1 which makes the
music loop indefinitely. Returns 0 on success."
  (check-rc (mix-fade-in-music mix-music-object loops ms)))

(defun pause-music ()
  "Pause the music stream."
  (mix-pause-music))

(defun resume-music ()
  "Resume the music stream."
  (mix-resume-music))

(defun fade-out-music (ms)
  "Halt the music stream after fading it out for MS milliseconds."
  (mix-fade-out-music ms))

(defun halt-music ()
  "Halts the playback of all music"
  (mix-halt-music))

(defun volume-music (music-volume)
  "Adjust the volume of the music. Volume ranges from 0 to 128. The return value
is an integer that usually represents the previous volume setting. Passing -1 as
the music volume does not change the volume but instead returns the current
volume setting"
  (mix-volume-music music-volume))
