# MP3 storage mixin which twiddles with the final bit in each frame.
lappend PROGRAM_OPTIONS -mp3apadding 0 \
{Enable modification of MP3 padding bytes (only relevent with -mp3audio).}
class Mp3-Mixin-Audio {
  private method mp3a-byte-offset {headeroff} {
    set off [expr {$headeroff-1}] ;# Usually write to the last bit of previous
    # If we are set to avoid padding (the default), we need to see if the
    # previous frame was padded, and then move off back one
    if {![dict exists $::programOptions -mp3apadding]} {
      set ix [lsearch -sorted -integer [$this mp3-get-headers] $headeroff]
      incr ix -1
      if {$ix > 0} {
        set header [$this read-header [lindex [$this mp3-get-headers] $ix]]
        incr off -[$this padding-of $header]
      }
    }
    #puts "Using byte at $off"
    return $off
  }

  method mp3-enumerate-bits {} {
    set bits [chain]
    foreach hoff [$this mp3-get-headers] {
      lappend bits [list [mp3a-byte-offset $hoff] audio]
    }
    return $bits
  }

  method mp3-read-audio {off} {
    set handle [$this mp3-get-handle]
    seek $handle $off
    set byte [read $handle 1]
    binary scan $byte cu byte
    expr {$byte & 1}
  }

  method mp3-write-audio {off value} {
    set byte [$this get-opbyte $off]
    set nbyte [expr {($byte & ~1) | $value}]
    if {$byte != $nbyte} {
      $this set-opbyte $off $nbyte
    } else {
      $this del-opbyte $off
    }
  }
}
