# cl-sdl2-mixer

This is a brief but usuable wrapper for SDL2_Mixer.

## Usage
The following functions are currently available to the users
* `(sdl2-mixer:linked-version)`: Returns the version number for SDL Mixer 2. Calls [Mix_Linked_Version](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC8)
* `(sdl2-mixer:init &rest formats)`: Initialize the SDL mixer specifying the formats you wish to use. Must be one of these values or a combination thereof `:ogg`, `:wave`, `:mod`, `:mp3`. Calls [Mix_Init](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC9).
* `(sdl2-mixer:quit)` Cleans up SDL Mixer. Calls [Mix_Quit](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC10).
* `(sdl2-mixer:open-audio frequency format channels chunksize)`: Initialize the mixer specifiying the output sample format, number of output channels (1 mono or 2 for stereo), and bytes used per output sample. format must be one of the following values, `:u8`, `:s8`, `:u16lsb`, `:s16lsb`, `:u16msb`, `:s16msb`, `:u16`, `:s16`, `:u16sys`, `:s16sys`. Calls [Mix_OpenAudio](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC11)
* `(sdl2-mixer:close-audio)`: Closes the mixer. Calls [Mix_CloseAudio](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC12)
* `(sdl2-mixer:query-format)`: Gets the output format in use by the opened audio device. Calls [Mix_QuerySpec](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC15)
* `(sdl2-mixer:load-wav sample-file-name)`: Loads the sample specified by the sample-file-name. Returns a mix-chunk. sdl2-mixer must be initialized and open-audio must be called prior to. Calls  [Mix_LoadWav_RW](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC20). Please refer to the source comments for more details if you are interested in why it calls Mix_LoadWav_RW as opposed to Mix_LoadWav
* `(sdl2-mixer:allocate-channels number-of-channels)`: Set the number of channels to be mixed. Opening too many channels may result in a segfault. This can be called at any time even while samples are playing. Passing a number lower than previous calls will close unused channels. It returns the number of channels allocated. NOTE: Channels are 0 indexed! Calls [Mix_AllocateChannels](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC26)
* `(sdl2-mixer:volume channel volume)`: Set the volume on a given channel, pass -1 to set the volume for all channels. The volume may range from 0 to 128. Passing in a number higher than the maximum will automatically set it to the maximum while passing in a negatiev will automatically set it to 0. Returns the current volume of the channel. NOTE: Channels are 0 indexed! Calls [Mix_Volume](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC27)
* `(sdl2-mixer:play-channel channel mix-chunk loops)`: Plays the mix-chunk (sound effect) loops+1 times on a given channel. Passing -1 for the channel will play it on the first unreserved channel. Returns the channel the sample is played on. NOTE: Channels are 0 indexed! Calls [Mix_PlayChannel](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC28)
* `(sdl2-mixer:halt-channel)` Halt the channel or pass -1 to halt all channels. Always returns 0. NOTE: Channels are 0 indexed! Calls [Mix_HaltChannel](https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC34)

## Examples
A simple example program has been provided. Ogg support is required to run it however. Press the space bar to play the sound effect, the up directional key to increase the volume by 20 and the down directional key to decrease the volume by 20. The current volume is displayed in standard-output

## Issues

If you cannot load `libSDL2_mixer`, please ensure that you have SDL_mixer 2.0, installed and not just 1.2. If you receive errors concerning unknown file types, please ensure that libSDL2_mixer is linked against the appropriate sound library, *e.g. libVorbis for ogg support*. As of writing (05-31-2015) the SDL_mixer 2.0 provided by brew on OSX does not link libVorbis correctly, please build it from source.


If you are sure all of this is correct, and it still will not load, please [file an issue](https://github.com/lispgames/cl-sdl2-mixer/issues/new) and specify:

* Your platform and architecture
* Your lisp
* The absolute path to your installed `.so`, `.dll`, or the appropriate OSX framework


