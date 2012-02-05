# MP3 storage mixin which uses the unused bits in the MP3 frame header (private,
# copyrighted, original).
class Mp3-Mixin-Bits {
  method mp3-enumerate-bits {} {
    set lst [chain]
    foreach headeroff [$this mp3-get-headers] {
      lappend lst [list $headeroff priv] [list $headeroff copy] \
                  [list $headeroff orig]
    }
    return $lst
  }

  method mp3-read-priv {headeroff} {
    incr headeroff 2
    set handle [$this mp3-get-handle]
    seek $handle $headeroff
    set byte [read $handle 1]
    binary scan $byte cu byte
    expr {$byte & 1}
  }

  method mp3-write-priv {headeroff value} {
    incr headeroff 2
    set byte [$this get-opbyte $headeroff]
    set nbyte [expr {($byte & ~1) | $value}]
    if {$byte != $nbyte} {
      $this set-opbyte $headeroff $nbyte
    } else {
      # The private bit is the only bit in this byte that can be
      # modified by this program.
      $this del-opbyte $headeroff
    }
  }

  method mp3-read-copy {headeroff} {
    incr headeroff 3
    set handle [$this mp3-get-handle]
    seek $handle $headeroff
    set byte [read $handle 1]
    binary scan $byte cu byte
    expr {($byte & 0x08) >> 3}
  }

  method mp3-write-copy {headeroff value} {
    incr headeroff 3
    set byte [$this get-opbyte $headeroff]
    set nbyte [expr {($byte & ~0x08) | ($value << 3)}]
    if {$byte != $nbyte} {
      $this set-opbyte $headeroff $nbyte
    }
  }

  method mp3-read-orig {headeroff} {
    incr headeroff 3
    set handle [$this mp3-get-handle]
    seek $handle $headeroff
    set byte [read $handle 1]
    binary scan $byte cu byte
    expr {($byte & 0x04) >> 2}
  }

  method mp3-write-orig {headeroff value} {
    incr headeroff 3
    set byte [$this get-opbyte $headeroff]
    set nbyte [expr {($byte & ~0x04) | ($value << 2)}]
    if {$byte != $nbyte} {
      $this set-opbyte $headeroff $nbyte
    }
  }
}
