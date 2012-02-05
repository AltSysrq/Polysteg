# Storage engine for Microsoft Waveform Audio File Format. It supports Wave
# files of any bitrate, any sample point size, with no compression, and the
# more standard single-data format (versus LIST etc). It can cope with arbitrary
# chunk placement and properly ignores any chunks other than "fmt " and "data".
# (A mostly unnotable exception to "any sample point size" is that 9-, 17-, and
# 25-bit, etc, streams cannot be used if the -wavehd option is given.)
# The decoder is a bit more lenient than the standard; eg, it will happily work
# with a 2535-bit stream.
dict set FILE_EXTENSIONS .wav Wave-Storage-Engine
lappend PROGRAM_OPTIONS -wavhd 0 \
{Use more bits in Microloft Waveform Audio Files.}
lappend PROGRAM_OPTIONS -wavstride 1 \
{Only use every <arg>th bit within Microsoft Waveform Audio Files.}

class Wave-Storage-Engine {
  inherit Storage-Device

  variable bitshift ;# Bit shift within bytes containing data
  variable bitmask ;# Bits in bytes being used
  variable dataOffset ;# Address of first byte to use
  variable samplePointSize ;# Number of bytes in sample point
  variable channelCount ;# Number of sample points in a sample frame
  variable sampleFrameSkip ;# Number of bytes to skip after sample frame
  variable dataEnd ;# End of data chunk

  constructor {fn} {
    Storage-Device::constructor $fn
  } {
  }

  method init-storage-engine {} {
    set header [read $handle 4]
    if {$header != "RIFF"} {
      error "$filename: Not in Microsoft Waveform Audio File Format."
    }
    binary scan [read $handle 4] iu length
    if {$length+8 != $fileLength} {
      error [format \
      "%s: Warning: The file lies about its length (%d reported, %d actual)." \
      $filename [expr {$length+8}] $fileLength]
    }
    set header [read $handle 4]
    if {$header != "WAVE"} {
      error "$filename: Not in Microsoft Waveform Audio File Format."
    }

    # The RIFF header is OK, we are now at the first chunk.
    set hasData no
    set hasFmt no
    while {![eof $handle]} {
      set chunkid [read $handle 4]
      binary scan [read $handle 4] iu chunksz
      if {[eof $handle]} break

      if {$chunkid == "fmt "} {
        if {$hasFmt} {
          error "$filename: Has more than one 'fmt ' chunk."
        }
        set hasFmt yes

        if {$chunksz < 16} {
          error "$filename: 'fmt ' chunk is too small ($chunksz)."
        }
        binary scan [read $handle 16] \
        {s        su       iu         iu  su             su} \
        formatTag channels sampleRate bps blockAlignment sampleBits
        if {$formatTag != 1} {
          error "$filename: Data is compressed; Polysteg cannot use it."
        }

        set samplePointSize [expr {($sampleBits+7)/8}]
        # Bits to the right of the least significant are unused (the format is
        # right-aligned, rather oddly).
        set bitshift [expr {$sampleBits%8}]
        set channelCount $channels
        set sampleFrameSkip \
            [expr {$blockAlignment - ($samplePointSize*$channels)}]
        # Sanity check
        if {$bitshift == 7 && [dict exists $::programOptions -wavhd]} {
          error "$filename: $sampleBits-bit files cannot be used with -wavhd."
        }
        if {$channelCount == 0} {
          error "$filename: Has no channels."
        }
        if {$blockAlignment < 0} {
          error "$filename: Has overlapping sample frames."
        }
        if {$samplePointSize == 0} {
          error "$filename: Encodes no data."
        }
        if {$sampleBits < 4} {
          error "$filename: $sampleBit-bit audio is unusable."
        }

        # OK, skip rest of chunk
        seek $handle [expr {$chunksz - 16}] current
      } elseif {$chunkid == "data"} {
        if {$hasData} {
          error "$filename: Has more than one 'data' chunk."
        }
        set hasData yes

        set dataOffset [tell $handle]
        set dataEnd [expr {$dataOffset + $chunksz}]
        seek $handle $dataEnd
      } else {
        # Unknown chunk, move on
        seek $handle $chunksz current
      }
    }

    if {!$hasFmt} {
      error "$filename: Missing 'fmt ' chunk."
    }
    if {!$hasData} {
      error "$filename: Missing 'data' chunk."
    }

    # Verify that the data chunk is an appropriate size.
    # It must encode an integer number of complete frames, including inter-
    # frame skip.
    set datlen [expr {$dataEnd - $dataOffset}]
    if {$datlen % ($channelCount*$samplePointSize + $sampleFrameSkip)} {
      error "$filename: Does not contain an integer number of sample frames."
    }
    # OK, finish up
    if {[dict exists $::programOptions -wavhd]} {
      set density 2
      set bitmask [expr {3 << $bitshift}]
    } else {
      set density 1
      set bitmask [expr {1 << $bitshift}]
    }
    seek $handle 0 ;# Rewind for reading of out-of-band data
    if {[dict exists $::programOptions -wavstride]} {
      set stride [dict get $::programOptions -wavstride]
    } else {
      set stride 1
    }
    # There are (datlen/sampleFrameSize) frames, each encoding density bits
    # in each channel.
    # Only 1/stride will be used.
    expr {($datlen/($samplePointSize*$channelCount + $sampleFrameSkip)*
          $density*$channelCount + ($stride-1))/$stride}
  }

  method enumerate-bits {} {
    if {[dict exists $::programOptions -wavhd]} {
      set density 2
    } else {
      set density 1
    }
    set lst {}
    set off $dataOffset
    set ix 0
    if {[dict exists $::programOptions -wavstride]} {
      set stride [dict get $::programOptions -wavstride]
    } else {
      set stride 1
    }
    while {$off < $dataEnd} {
      for {set chan 0} {$chan < $channelCount} {incr chan} {
        for {set bit 0} {$bit < $density} {incr bit} {
          if {0 == $ix%$stride} {
            lappend lst [list $off $bit]
          }
          incr ix
        }
        incr off $samplePointSize
      }
      incr off $sampleFrameSkip
    }
    return $lst
  }

  method read-out-of-band-invariant {} {
    # Read raw data from the file; set all the bits corresponding to those
    # we are extracting.
    binary scan [read $handle 1024] cu1024 dat
    for {set i 0} {$i < 1024} {incr i} {
      lset dat $i [expr {[lindex $dat $i] | $bitmask}]
    }
    binary format cu1024 $dat
  }

  method read-physical-bit {bit} {
    lassign $bit off rs
    seek $handle $off
    binary scan [read $handle 1] cu byte
    expr {($byte & (1 << ($bitshift+$rs))) >> ($bitshift+$rs)}
  }

  method write-physical-bit {bit value} {
    lassign $bit off rs
    set byte [get-opbyte $off]
    set nbyte [expr {($byte & ~(1 << ($bitshift+$rs))) |
                     ($value << ($bitshift+$rs))}]
    if {$nbyte != $byte} {
      set-opbyte $off $nbyte
    } else {
      del-opbyte $off
    }
  }
}
