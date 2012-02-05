# The raw file storage engine for polysteg.
# It simply uses every bit in a binary file.
# Extension: .steganograph
# Security information:
#   The presense of steganography is immediately visible to both casual and
#   formal observers. This engine is intended only for testing and demonstration
#   purposes.
dict set FILE_EXTENSIONS .steganograph Raw-Storage-Engine
class Raw-Storage-Engine {
  inherit Storage-Device

  constructor {fn} {
    Storage-Device::constructor $fn
  } {
  }

  protected method init-storage-engine {} {
    expr {$fileLength*8}
  }

  protected method enumerate-bits {} {
    set ret {}
    for {set i 0} {$i < 8*$fileLength} {incr i} {
      lappend ret $i
    }
    return $ret
  }

  protected method read-physical-bit {bit} {
    set byte [expr {$bit/8}]
    set shft [expr {$bit%8}]
    seek $handle $byte
    binary scan [read $handle 1] cu val
    return [expr {($val >> $shft) & 1}]
  }
  protected method write-physical-bit {bit value} {
    set byte [expr {$bit/8}]
    set shft [expr {$bit%8}]
    set val [get-opbyte $byte]
    set val [expr {($val & ~(1 << $shft)) | ($value << $shft)}]
    set-opbyte $byte $val
  }
  # There is no out-of-band data, so the default does the only thing applicable.
}
