# MP3 storage engine mixin to use the highest bit of the first byte of each
# frame's side information. Experimentation suggests that this does not have
# much of an effect on the audio. Presumably there is something special about
# the 0xFF sides, so ignore sides that start with 0xFF or 0x7F.
# (It would seem initial experimentation was lucky --- this actually often has
#  a painfully noticeable effect.)
class Mp3-Mixin-Side {
  method mp3-enumerate-bits {} {
    set lst [chain]
    foreach headeroff [$this mp3-get-headers] {
      set header [$this read-header $headeroff]
      set hsz [expr {[$this is-protected $header]? 6 : 4}]
      set off [expr {$headeroff + $hsz}]
      # Make sure not a "special" side value
      seek [$this mp3-get-handle] $off
      set byte [read [$this mp3-get-handle] 1]
      if {$byte != "\xFF" && $byte != "\x7F"} {
        lappend lst [list $off side]
      }
    }
    return $lst
  }

  method mp3-read-side {off} {
    seek [$this mp3-get-handle] $off
    set byte [read [$this mp3-get-handle] $off]
    binary scan $byte cu byte
    expr {($byte & 0x80) >> 7}
  }
  method mp3-write-side {off value} {
    set byte [$this get-opbyte $off]
    set nbyte [expr {($byte & 0x7F) | ($value << 7)}]
    if {$byte != $nbyte} {
      $this set-opbyte $off $nbyte
    } else {
      $this del-opbyte $off
    }
  }
}
