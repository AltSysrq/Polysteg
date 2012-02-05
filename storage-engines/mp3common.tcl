# This file does not contain a storage engine, but rather a base class
# for the common code needed for Polysteg to work with mp3 files.
#
# It supports MPEG Versions 1 and 2, Layer III ("Version" 2.5 is not supported.)
#
# See http://mpgedit.org/mpgedit/mpeg_format/mpeghdr.htm
# For CRCs, see
#   http://www.codeproject.com/Articles/8295/MPEG-Audio-Frame-Header#CRC

lappend PROGRAM_OPTIONS -mp3framestride 1 \
{Only use every <arg>th MP3 frame (defaults to 1).}
lappend PROGRAM_OPTIONS -mp3checkcrcs 0 {Verify CRCs of protected MP3 frames.}
class Mp3-Storage-Engine {
  inherit Storage-Device

  # Each element in this list is the byte index of a header in the input file.
  # The first header is not included.
  # The headers are guaranteed to be numerically sorted ascending according
  # to byte offset.
  protected variable headers

  # The first header found, excluded from the headers list
  variable firstHeader

  # Queued data to return from read-out-of-band-invariant.
  variable outOfBandData
  # Next header to examine for out-of-band data
  variable nextOutOfBandHeader

  constructor {fn} {
    Storage-Device::constructor $fn
  } {
  }

  destructor {
    if {![clobbers-crcs]} {
      foreach headeroff $headers {
        if {[is-protected [read-header $headeroff]]} {
          write-crc $headeroff [calculate-crc $headeroff]
        }
      }
    }
  }

  # This method only reads the file for headers.
  # Returns [llength [enumerate-bits]]
  # This method may recurse onto itself if it feels it needs to retry
  # scanning from the middle of the file.
  method init-storage-engine {} {
    global programOptions

    set foundHeader no
    set readNext yes

    while {!$foundHeader} {
      if {[eof $handle]} {
        error "$filename: could not locate a valid MP3 header."
      }

      if {$readNext} {
        set byte [read $handle 1]
      }
      set readNext yes
      if {$byte == "\xFF"} {
        # Possible header
        set byte [read $handle 1]
        if {![binary scan $byte cu byte]} {set byte 0}
        # The bit string is:
        #   AAAABCCD
        # We support either MPEG version (B=?), but only layer III (CC=01),
        # and D may or may not be set, so
        #   1111?01?
        # makes this a valid header.
        # XOR the bits that must be 1 off, zero those we don't care about here,
        # the ensure the result is zero.
        if {(($byte ^ 0xF2) & ~0x9) == 0} {
          # Further check that we can understand this header; some MP3s have
          # a large number of false positives, even with the strictness of
          # the above test.
          set off [tell $handle]
          catch {
            get-header-after [expr {[tell $handle]-2}]
            set foundHeader yes
          }
          seek $handle $off
        }
        set byte [binary format c $byte]
        set readNext no
      }
    }

    # Found the first header.
    set headeroff [tell $handle]
    incr headeroff -2 ;# We read two of its bytes already
    set firstHeader $headeroff
    set headeroff [get-header-after $headeroff]

    # Read the rest of the headers
    set headers {}
    while {$headeroff < $fileLength} {
      seek $handle $headeroff
      # Ensure next frame is valid
      set byte [read $handle 1]
      if {$byte != "\xFF"} {
        if {[llength $headers] > 8} {
          if {[dict exists $programOptions -debug]} {
            puts stderr [format "%s: Invalid header at %X, stopping" \
                         $filename $headeroff]
          }
          break
        } else {
          if {[dict exists $programOptions -debug]} {
            puts stderr [format "%s: Invalid header at %X, rescanning" \
                         $filename $headeroff]
          }
          return [init-storage-engine]
        }
      }
      set byte [read $handle 1]
      if {![binary scan $byte cu byte]} {set byte 0}
      if {(($byte ^ 0xF2) & ~0x9) != 0} {
        if {[llength $headers] > 8} {
          if {[dict exists $programOptions -debug]} {
            puts stderr [format "%s: Invalid header at %X, stopping" \
                         $filename $headeroff]
          }
          break
        } else {
          if {[dict exists $programOptions -debug]} {
            puts stderr [format "%s: Invalid header at %X, rescanning" \
                         $filename $headeroff]
          }
          return [init-storage-engine]
        }
      }

      # OK
      lappend headers $headeroff
      if {[catch {
        set headeroff [get-header-after $headeroff]
      } err]} {
        if {[llength $headers] > 8} {
          if {[dict exists $programOptions -debug]} {
            puts stderr [format "%s: Invalid header at %X, stopping: %s" \
                         $filename $headeroff $err]
          }
          break
        } else {
          if {[dict exists $programOptions -debug]} {
            puts stderr [format "%s: Invalid header at %X, rescanning: %s" \
                         $filename $headeroff $err]
          }
          return [init-storage-engine]
        }
      }
    }

    # Check CRCs of headers if so requested
    if {[dict exists $::programOptions -mp3checkcrcs] && ![clobbers-crcs]} {
      foreach headeroff $headers {
        set header [read-header $headeroff]
        if {[is-protected $header]} {
          set currCRC [read-crc $headeroff]
          set correctCRC [calculate-crc $headeroff]
          if {$currCRC != $correctCRC} {
            puts stderr [format \
"Warning: %s: Frame at %s off has bad CRC (%04X stored vs %04X calculated)" \
              $filename $currCRC $correctCRC]
            break
          }
        }
      }
    }

    # Possible striding
    if {[dict exists $::programOptions -mp3framestride]} {
      set ix 0
      set stride [dict get $::programOptions -mp3framestride]
      set hds $headers
      set headers {}
      foreach header $hds {
        if {$ix % $stride == 0} {
          lappend headers $header
        }
        incr ix
      }
    }

    if {[dict exists $::programOptions -v]} {
      puts "$filename: Found [llength $headers] usable headers"
    }

    # Seed out-of-band data with everything before the first header
    if {[llength $headers]} {
      seek $handle 0
      #puts "End of initial OOB data: $firstHeader"
      set outOfBandData [read $handle $firstHeader]
      set nextOutOfBandHeader 0
    }

    llength [enumerate-bits]
  }

  protected method read-out-of-band-invariant {} {
    while {[string length $outOfBandData] < 1024} {
      # After the unused data before the first header, include all side
      # information, excluding the first byte of each information section.
      set headeroff [lindex $headers $nextOutOfBandHeader]
      #puts "nextOutOfBandHeader: $nextOutOfBandHeader at $headeroff"
      incr nextOutOfBandHeader
      set header [read-header $headeroff]
      set sidesz [get-side-data-sz $header]
      set headersz [expr {[is-protected $header]? 6 : 4}]
      #puts "headersz: $headersz, sidesz: $sidesz"

      incr headeroff $headersz
      if {$headeroff+$sidesz+2 < [lindex $headers $nextOutOfBandHeader]} {
        seek $handle [expr {$headeroff+1}]
        append outOfBandData [read $handle [expr {$sidesz-1}]]
      }
    }

    set ret [string range $outOfBandData 0 1023]
    set outOfBandData [string range $outOfBandData 1024 end]
    return $ret
  }

  # Reads the header at the given offset and returns it as an integer,
  # which is a big-endian representation of the header.
  method read-header {headeroff} {
    seek $handle $headeroff
    binary scan [read $handle 4] Iu ret
    return $ret
  }

  # Writes the given header data (as a 32-bit integer) to the given offset in
  # the file.
  method write-header {headeroff header} {
    seek $handle $headeroff
    puts -nonewline $handle [binary format I $header]
  }

  # Returns the MPEG version indicated in the header, either 1 or 2
  method version-of {header} {
    if {$header & 0x00080000} {
      return 1
    } else {
      return 2
    }
  }

  # Returns the bitrate, in kbps, of a frame, given the frame header.
  # Raises an error if the bitrate cannot be understood.
  method bitrate-of {header} {
    set br [expr {($header & 0x0000F000) >> 12}]
    set v1dat {-1 32 40 48 56 64 80 96 112 128 160 192 224 256 320 -1}
    set v2dat {-1  8 16 24 32 40 48 56  64  80  96 112 128 144 160 -1}
    if {[version-of $header] == 1} {
      set br [lindex $v1dat $br]
    } else {
      set br [lindex $v2dat $br]
    }
    if {$br == -1} {
      error "$filename: Unsupported bitrate in frame header"
    }
    return $br
  }

  # Returns the sample rate, in Hertz, of a frame, given the frame header.
  # Raises an error if the sample rate cannot be understood.
  method sample-rate-of {header} {
    set sr [expr {($header & 0x00000C00) >> 10}]
    set v1dat { 44100 48000 32000 -1 }
    set v2dat { 22050 24000 16000 -1 }
    if {[version-of $header] == 1} {
      set sr [lindex $v1dat $sr]
    } else {
      set sr [lindex $v2dat $sr]
    }
    if {$sr == -1} {
      error "$filename: Unsupported sample rate in frame header"
    }
    return $sr
  }

  # Returns the padding of a frame (0 or 1), given the frame header.
  method padding-of {header} {
    expr {$header & 0x00000200? 1 : 0}
  }

  # Returns whether a CRC follows the given frame header.
  method is-protected {header} {
    expr {($header & 0x00010000)? 0 : 1}
  }

  # Returns the frame length of a frame, including the header, given its header.
  # Raises an error if the header is unsupported.
  method frame-length-of {header} {
    set br [bitrate-of $header]
    set sr [sample-rate-of $header]
    set padding [padding-of $header]
    # MPEG 2 frames are half the size as MPEG 1
    set version [version-of $header]

    if {[dict exists $::programOptions -debug]} {
      puts [format \
            "Frame info of %08X: bitrate = %d kbps, sample rate = %d Hz, padding: %d, version: %d" \
            $header $br $sr $padding $version]
      puts "Frame length: [expr {144*$br*1000/$sr/$version+$padding}]"
    }

    expr {144*$br*1000/$sr/$version+$padding}
  }

  # Returns the address of the header that follows the one at the given offset.
  # Raises an error if the header cannot be understood.
  method get-header-after {headeroff} {
    if {[dict exists $::programOptions -debug]} {
      puts [format "Examine header at %X" $headeroff]
    }
    set header [read-header $headeroff]
    expr {$headeroff + [frame-length-of $header]}
  }

  # Returns the number of bytes in the side information for the given header.
  # This is primarily used for CRC calculation.
  method get-side-data-sz {header} {
    set channels [expr {$header & 0x000000C0}]
    if {$channels == 0xC0} {
      # Mono
      return [expr {[version-of $header] == 1? 17 : 9}]
    } else {
      # Stereo, Dual Channel, or Joint Stereo
      return [expr {[version-of $header] == 1? 32 : 17}]
    }
  }

  # Calculates the CRC checksum for the frame whose header is at the given
  # offset. Raises an error if the frame is not protected.
  method calculate-crc {headeroff} {
    set header [read-header $headeroff]
    if {![is-protected $header]} {
      error "Attempt to calculate CRC of unprotected header."
    }
    # http://www.codeproject.com/Articles/8295/MPEG-Audio-Frame-Header#CRC
    # "The following data [are] considered for the CRC: the last two bytes
    #  of the header and a number of bits from the audio data which follows the
    #  checksum after the header. The checksum itself must be skipped for CRC
    #  calculation.
    #  ...
    #  For Layer III, you consider the complete side information for the CRC
    #  calculation. The side information follows the header or the CRC in
    #  Layer III files. ..."
    seek $handle [expr {$headeroff+2}]
    # Read last two bytes of the frame, the CRC, and the side data
    set data [read $handle [expr {2+2+[get-side-data-sz $header]}]]
    # Remove CRC
    set data [string replace $data 2 3]
    # Calculate
    return [crc::crc16 $data]
  }

  # Reads the CRC of the protected frame whose header is at the given offset.
  # This does not check whether the frame is in fact protected.
  # Returns a 16-bit integer.
  method read-crc {headeroff} {
    seek $handle [expr {$headeroff+4}]
    binary scan [read $handle 2] Su crc
    return $crc
  }

  # Writes the given CRC (a 16-bit integer) to the protected frame whose header
  # is at the given offset.
  # This does not check whether the frame is in fact protected.
  method write-crc {headeroff crc} {
    seek $handle [expr {$headeroff+4}]
    puts -nonewline $handle [binary format S $crc]
  }

  # Returns whether any underlying storage mechanism uses the CRCs for something
  # other than checksumming. If it returns true, CRCs should never be
  # recalculated or checked.
  method clobbers-crcs {} { return no }

  # Returns an enumeration of physical bits provided by the storage engine.
  # Default returns an empty list.
  # The MP3 storage mixins should concatenate their bits to the return value
  # of [chain].
  # Physical bits have the format
  #   headeroff method ...
  # Accesses to physical bits are performed as follows:
  #   $this mp3-read-$method $headeroff {*}[lrange $bit 2 end]
  #   $this mp3-write-$method $headeroff {*}[lrange $bit 2 end] $value
  # headeroff may also be a raw byte offset.
  method mp3-enumerate-bits {} {
    return {}
  }

  protected method enumerate-bits {} {
    mp3-enumerate-bits
  }

  protected method read-physical-bit {bit} {
    set args [lassign $bit headeroff method]
    $this mp3-read-$method $headeroff {*}$args
  }
  protected method write-physical-bit {bit value} {
    set args [lassign $bit headeroff method]
    $this mp3-write-$method $headeroff {*}$args $value
  }

  # Intended for use by storage mixins only.
  # Returns the file handle.
  method mp3-get-handle {} { return $handle }
  # Intended for use by storage mixins only.
  # Returns the header offset list.
  method mp3-get-headers {} { return $headers }
}
