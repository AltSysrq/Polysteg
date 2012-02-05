# Assembles the Mp3-Storage-Engine with the MP3 storage mixins
# into proper storage engines.

dict set FILE_EXTENSIONS .mp3 Mp3-Select-Storage-Engine
lappend PROGRAM_OPTIONS -mp3audio       0 \
        {Enable modification of MP3 audio data (default if none specified).}
lappend PROGRAM_OPTIONS -mp3sideinfo    0 \
        {Enable modification of MP3 side information.}
lappend PROGRAM_OPTIONS -mp3bits        0 \
        {Enable modification of MP3 unused bits.}

foreach audioMixin      {{} Mp3-Mixin-Audio     } {
foreach sideMixin       {{} Mp3-Mixin-Side      } {
foreach bitsMixin       {{} Mp3-Mixin-Bits      } {
  class Mp3-Storage-Engine-$audioMixin-$sideMixin-$bitsMixin "
    inherit $audioMixin $sideMixin $bitsMixin Mp3-Storage-Engine
    constructor {fn} { Mp3-Storage-Engine::constructor \$fn } { }
  "
}
}
}

proc Mp3-Select-Storage-Engine {args} {
  global programOptions
  set anySpecified no
  if {[dict exists $programOptions -mp3audio]} {
    set audioMixin Mp3-Mixin-Audio
    set anySpecified yes
  } else {
    set audioMixin {}
  }
  if {[dict exists $programOptions -mp3sideinfo]} {
    set sideMixin Mp3-Mixin-Side
    set anySpecified yes
  } else {
    set sideMixin {}
  }
  if {[dict exists $programOptions -mp3bits]} {
    set bitsMixin Mp3-Mixin-Bits
    set anySpecified yes
  } else {
    set bitsMixin {}
  }

  if {!$anySpecified} {
    set audioMixin Mp3-Mixin-Audio
  }

  Mp3-Storage-Engine-$audioMixin-$sideMixin-$bitsMixin {*}$args
}
