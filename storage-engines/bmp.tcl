# Storage engine for Microsoft Device Independent Bitmap files (*.bmp, *.div).
# It supports 16-, 24-, and 32-bit colour files and data densities of
# 1 bits/colour/pixel and 2 bits/colour/pixel. (Note that it never touches the
# alpha channel unless requested, as it is usually noiseless in ordinary files.)
#
# It supports the following versions:
#   BITMAPINFOHEADER
#   BITMAPV2INFOHEADER
#   BITMAPV3INFOHEADER
#   everything later (treated as BITMAPV3INFOHEADER)
# It does not understand any compression methods.

dict set FILE_EXTENSIONS .bmp Bitmap-Storage-Engine
dict set FILE_EXTENSIONS .dib Bitmap-Storage-Engine
lappend PROGRAM_OPTIONS -bmphd 0 \
{Use more bits in Microsoft Device Independent Bitmap files.}
lappend PROGRAM_OPTIONS -bmpalpha 0 \
{Modify alpha in Microsoft Bitmap files (generally a bad idea).}
lappend PROGRAM_OPTIONS -bmprstride 1 \
{Only use every <arg>th row in Microsoft Bitmap files.}
lappend PROGRAM_OPTIONS -bmpcstride 1 \
{Only use every <arg>th column in Microsoft Bitmap files.}

class Bitmap-Storage-Engine {
  inherit Storage-Device

  # Set by init-storage-engine, returned and unset by enumerate-bits
  variable enumeratedBits

  # Bits to always set to 1 in out-of-band data
  variable oobBlacklist

  constructor {fn} {
    Storage-Device::constructor $fn
  } {
  }

  method init-storage-engine {} {
    set signature [read $handle 2]
    switch -exact -- $signature {
      BM {} ;# OK
      BA -
      CI -
      &C -
      CP -
      IC -
      *P -
      PT {
        error "$filename: This appears to be an unsupported OS/2 resource."
      }
      default {
        error "$filename: This does not appear to be a Microsoft Bitmap file."
      }
    }

    binary scan [read $handle 4] iu size
    if {$size != $fileLength} {
      puts stderr [format \
"%s: Warning: The file lies about its length (%d actual vs %d reported)." \
                   $filename $fileLength $size]
    }
    seek $handle 4 current ;# Skip reserved bytes
    binary scan [read $handle 4] iu dataOffset
    binary scan [read $handle 24] \
      {iu         iu   iu   su     su  iu    iu} \
      dibHeaderSz imgw imgh planes bpp compr imgsz

    # Examine header version
    switch -exact -- $dibHeaderSz {
      12 {
        error "$filename: Obsolete Windows or OS/2 format (v1) not supported."
      }
      64 {
        error "$filename: Obsolete OS/2 format (v2) not supported."
      }
      40 {
        # BITMAPINFOHEADER
        if {[dict exists $::programOptions -debug]} {
          puts "$filename: BITMAPINFOHEADER"
        }
        set rgbBitmasksMandatory [expr {$compr == 3}]
        set aBitmaskMandatory    [expr {$compr == 3}]
      }
      52 {
        # BITMAPV2INFOHEADER
        if {[dict exists $::programOptions -debug]} {
          puts "$filename: BITMAPV2INFOHEADER"
        }
        set rgbBitmasksMandatory yes
        set aBitmaskMandatory    [expr {$compr == 3}]
      }
      56 {
        # BITMAPV2INFOHEADER
        if {[dict exists $::programOptions -debug]} {
          puts "$filename: BITMAPV3INFOHEADER"
        }
        set rgbBitmasksMandatory yes
        set aBitmaskMandatory    yes
      }
      108 -
      124 {
        if {[dict exists $::programOptions -debug]} {
          puts "$filename: BITMAPV4HEADER or BITMAPV5HEADER"
        }
        set rgbBitmasksMandatory yes
        set aBitmaskMandatory    yes
      }

      default {
        if {$dibHeaderSz > 124} {
          puts stderr \
"$filename: Warning: Bitmap is a newer version than this program understands."
          puts stderr \
"$filename: Warning: Assuming it is forward-compatible and continuing."
        } else {
          error "$filename: Unknown version: $dibHeaderSz"
        }

        set rgbBitmasksMandatory yes
        set aBitmaskMandatory    yes
      }
    }

    # Validate other data
    if {$planes != 1} {
      puts stderr "$filename: Warning: colour planes ($planes) is not one."
    }
    if {$compr != 0 && $compr != 3} {
      error "$filename: Bitmap is compressed; Polysteg cannot use it."
    }
    set rowsz [expr {($imgw*$bpp/8+3)&~3}]
    if {$rowsz*$imgh != $imgsz} {
      error \
"$filename: Data size ($imgsz) does not match calculated [expr {$rowsz*$imgh}]"
    }

    # Default bitmasks and shifts
    if {$bpp == 16} {
      #XRRR.RRGG:GGGB.BBBB
      set bmask 0x001F
      set gmask 0x03E0
      set rmask 0x7C00
      set amask 0
    } elseif {$bpp == 24} {
      # We treat everything as little-endian.
      # (So the BGR becomes RGB, not that that matters)
      # RRRR.RRRR:GGGG.GGGG:BBBB.BBBB
      set rmask 0xFF0000
      set gmask 0x00FF00
      set bmask 0x0000FF
      set amask 0
    } elseif {$bpp == 32} {
      #[XXXX.XXXX]:RRRR.RRRR:GGGG.GGGG:BBBB.BBBB
      set rmask 0x00FF0000
      set gmask 0x0000FF00
      set bmask 0x000000FF
      set amask 0
    } else {
      error "$filename: Unsupported bits-per-pixel: $bpp"
    }

    # We don't care about resolution, colour table, or important colours
    seek $handle 16 current

    # Read bitmasks if present
    if {$rgbBitmasksMandatory} {
      binary scan [read $handle 12] {iu iu iu} rmask gmask bmask
    }
    if {$aBitmaskMandatory} {
      binary scan [read $handle 4] iu amask
    }

    # Never use the alpha channel unless requested
    if {![dict exists $::programOptions -bmpalpha]} {
      set amask 0
    }

    # At this point, we no longer care about which colour is which
    set masks {}
    foreach m {amask rmask gmask bmask} {
      if {[set $m] != 0} { lappend masks [set $m] }
    }
    if {[dict exists $::programOptions -bmphd]} {
      set bpcp 2
    } else {
      set bpcp 1
    }

    # Determine the mask-offset and -width for each channel.
    # Eliminate channels where we would be using 50% or more of the mask-width.
    # Accumulate blacklisted bits for out-of-band.
    set shifts {}
    set widths {}
    set basemasks $masks
    set masks {}
    set oobBlacklist 0
    foreach mask $basemasks {
      for {set shift 0} {0 == (($mask >> $shift) & 1)} {incr shift} {}
      for {set width 0} {0 != ($mask >> ($shift+$width))} {incr width} {}
      if {$width > $bpcp*2} {
        lappend masks $mask
        lappend shifts $shift
        lappend widths $width
        for {set ss $shift} {$ss < $shift+$bpcp} {incr ss} {
          set oobBlacklist [expr {$oobBlacklist | (1 << ($ss%8))}]
        }
      }
    }

    set rowstride 1
    set colstride 1
    if {[dict exists $::programOptions -bmprstride]} {
      set rowstride [dict get $::programOptions -bmprstride]
    }
    if {[dict exists $::programOptions -bmpcstride]} {
      set colstride [dict get $::programOptions -bmpcstride]
    }

    # Enumerate bits
    set enumeratedBits {}
    for {set row 0} {$row < $imgh} {incr row $rowstride} {
      set rowoff [expr {$dataOffset + $row*$rowsz}]
      for {set col 0} {$col < $imgw} {incr col $colstride} {
        set pxoff [expr {$rowoff + $bpp/8*$col}]
        foreach baseShift $shifts {
          for {set shift $baseShift} {$shift < $baseShift+$bpcp} {incr shift} {
            # Bytes are in reverse order due to little-endian,
            # so subtract from Bpp (ie, bpp/8)
            set byteoff [expr {$pxoff + $bpp/8 - $shift/8}]
            set bitoff [expr {$shift % 8}]
            lappend enumeratedBits [list $byteoff $bitoff]
          }
        }
      }
    }

    seek $handle 0 ;# Prepare for OOB
    llength $enumeratedBits
  }

  method enumerate-bits {} {
    set ret $enumeratedBits
    unset enumeratedBits
    return $ret
  }

  protected method read-out-of-band-invariant {} {
    binary scan [read $handle 1024] cu1024 dat
    for {set i 0} {$i < 1024} {incr i} {
      lset dat $i [expr {[lindex $dat $i] | $oobBlacklist}]
    }
    binary format cu1024 $dat
  }

  method read-physical-bit {bit} {
    lassign $bit byteoff shift
    seek $handle $byteoff
    binary scan [read $handle 1] cu byte
    expr {($byte & (1<<$shift)) >> $shift}
  }

  method write-physical-bit {bit value} {
    lassign $bit byteoff shift
    set byte [get-opbyte $byteoff]
    set nbyte [expr {($byte & ~(1<<$shift)) | ($value << $shift)}]
    if {$byte != $nbyte} {
      set-opbyte $byteoff $nbyte
    }
  }
}
