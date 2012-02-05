#! /usr/bin/tclsh
# Main implementation of Polysteg (see storage engines under storage-engines/)
#
# Libraries required:
#   Tcl 8.5 or later
#   [incr Tcl] (aka Itcl) 3.4 or later
#   Tcllib (see versions for components below) (http://tcllib.sourceforge.net/)
#   FUSE or compatible kernel interface
#   FUSE Tcl bindings 1.1 or later, (http://sourceforge.net/projects/tcl-fuse/)
#   Platform supporting Tcl UNIX-specific functionality
#
# Note that Debian (and hence Ubuntu) for some reason link /usr/bin/tclsh to
# /usr/bin/tclsh8.4 by default, so manual invocation with tclsh8.5 may be
# necessary on such systems.

package require Tcl 8.5
package require Itcl 3.4
package require fuse 1.1
# Tcllib stuff
package require aes ;# Version probably doesn't matter
package require crc16 1.1
package require sha256 ;# Version doesn't matter
package require term::receive
package require term::ansi::code
package require term::ansi::code::ctrl
# Comment out on Windows (and use the -password option)
package require term::ansi::ctrl::unix

namespace import ::itcl::*

set PROGRAM_OPTIONS {
  {-h -help --help}   0 {Display this help message and exit.}
  {-v -verbose}       0 {Show extra messages.}
  {-debug}            0 {Show debugging messages.}
  {-d -dir}           1 {Specify system root directory, defaults to .}
  {-r -root}          1 {Indicate the first media file to use.}
  {-m -mount}         1 {Specify mount directory.}
  -nourandom          0 {Use /dev/random instead of /dev/urandom.}
  -format             0 {Create a new system in the root directory.}
  -password           1 {Specify password on the command line.}
  -noshuffledevbits   0 {Don't shuffle bits within devices.}
  -noshuffleallbits   0 {Don't shuffle global bits.}
  -o                  1 {Pass options to FUSE.}
}

# Maps file extensions (lowercase) to Storage-Device subclass names
set FILE_EXTENSIONS [dict create]

# Binary string of 16 NULs
set NUL16 [binary format c16 [lrepeat 16 0]]
# Binary string of 1024 NULs
set NUL1024 [binary format c1024 [lrepeat 1024 0]]

proc printOptionsAndExit {code} {
  puts "Usage: $::argv0 options..."
  foreach {flags cnt descr} $::PROGRAM_OPTIONS {
    set left [join $flags ", "]
    append left [string repeat " arg" $cnt]
    puts [format "%-20s %s" $left $descr]
  }
  exit $code
}

proc main {} {
  global argc argv PROGRAM_OPTIONS programOptions
  # Read the input options
  set programOptions {}
  for {set i 0} {$i < [llength $argv]} {incr i} {
    set ok no
    foreach {argl parmc descr} $PROGRAM_OPTIONS {
      if {-1 != [lsearch -exact $argl [lindex $argv $i]]} {
        set baseOption [lindex $argl 0]
        if {[dict exists $programOptions $baseOption]} {
          puts stderr "Option [lindex $argv $i] specified more than once."
          printOptionsAndExit 2
        }

        if {$i + $parmc < [llength $argv]} {
          # OK
          set parms {}
          for {} {$parmc > 0} {incr parmc -1} {
            incr i
            lappend parms [lindex $argv $i]
          }
          dict set programOptions $baseOption $parms
          set ok yes
          break
        } else {
          puts stderr "Expected $parmc argument(s) after [lindex $argv $i]"
          printOptionsAndExit 1
        }
      }
    }

    if {!$ok} {
      puts stderr "Unknown option: [lindex $argv $i]"
      printOptionsAndExit 3
    }
  }

  if {[dict exists $programOptions -h]} {
    printOptionsAndExit 0
  }

  if {![dict exists $programOptions -r]} {
    puts stderr "The -root option is mandatory."
    printOptionsAndExit 4
  }

  if {![dict exists $programOptions -d]} {
    dict set programOptions -d .
  }

  if {[dict exists $programOptions -m]
  ==  [dict exists $programOptions -format]} {
    puts stderr "Exactly one of -mount, -format options must be specified."
    printOptionsAndExit 4
  }

  # Normalize mount point
  if {[dict exists $programOptions -m]} {
    dict set programOptions -m [file normalize [dict get $programOptions -m]]
  }

  cd [dict get $programOptions -d]

  if {[catch {
    if {[dict exists $programOptions -nourandom]} {
      set urandomFile /dev/random
    } else {
      set urandomFile /dev/urandom
    }
    set ::urandom [open $urandomFile rb]
  } err]} {
    puts stderr "Unable to open $urandomFile: $err"
    puts stderr \
"If your platform does not have /dev/urandom, try the -nourandom option."
    exit 5
  }

  if {![dict exists $programOptions -password]} {
    if {[catch {
      ::term::ansi::ctrl::unix::raw
      puts -nonewline "Password: "
      flush stdout
      set pwd {}
      while {![eof stdin]} {
        set ch [read stdin 1]
        if {$ch == "\0" || $ch == "\4" || $ch == "\032"
        ||  $ch == "\n" || $ch == "\r"} break
        if {$ch == "\b" || $ch == "\177"} {
          set pwd [string range $pwd 0 end-1]
        } elseif {$ch >= " " && $ch < "\177"} {
          append pwd $ch
        }
      }
      ::term::ansi::ctrl::unix::cooked
    } err]} {
      puts stderr "Warning: Could not switch the terminal to RAW mode: $err"
      puts -nonewline "Password (CHARACTERS WILL BE ECHOED!): "
      flush stdout
      set pwd [gets stdin]
    }

    dict set programOptions -password $pwd
  }

  if {[dict exists $programOptions -format]} {
    format-system
  } else {
    mount-system
  }
}

# The Storage-Device class is the base class for all Polysteg storage engines.
# It provides the common functionality of a storage device, and an abstract
# interface to the physical layer.
#
# This class additionally performs write caching. Each time a logical bit is
# to be written, it is translated to its physical address, but is then enqueued.
# When a flushing condition occurs, the queue is sorted by physical address
# (numerically, via field 0 0), duplicate writes are consolidated, and all
# writes are made. Flushing conditions are:
# o A read request is made
# o The queue exceeds 1024 in length
# o The device is deleted
#
# To further improve performance by eliminating read-write-read-write patterns,
# writes to physical bits should use the operational bytes abstraction, using
# the get-opbyte and set-opbyte methods in the write-physical-bit method
# instead of directly accessing the handle.
class Storage-Device {
  # The name of the file to operate on
  protected variable filename
  # The length of the file
  protected variable fileLength
  # A read-write handle to the operating file
  protected variable handle

  # The original mtime and atime of the file
  variable origAtime
  variable origMtime

  # Logical bit to physical bit mapping.
  # The element types themselves are subclass-defined.
  variable bitmap

  # The encryption key for the link data (first 640 bits)
  variable linkkey

  # The write queue (see class description)
  variable writeQueue

  # Current operational bytes (see class description)
  variable opbytes

  # Constructs a storage device for the given file.
  # For opening devices, the global create-storage-device proc should be
  # used instead.
  constructor {fn} {
    set filename $fn
    set fileLength [file size $filename]
    if {[catch {
      set origAtime [file atime $filename]
    } err]} {
      set origAtime {}
      puts stderr \
        "Warning: Unable to query access time of $filename: $err"
    }
    if {[catch {
      set origMtime [file mtime $filename]
    } err]} {
      set origMtime {}
      puts stderr \
        "Warning: Unable to query modification time of $filename: $err"
    }
    set handle [open $filename rb+]
    fconfigure $handle -buffering none
    set writeQueue {}
    set opbytes {}
  }

  destructor {
    flush-write-queue

    close $handle
    if {[catch {
      file mtime $filename $origMtime
    } err]} {
      puts stderr \
        "Warning: Unable to reset modification time of $filename: $err"
    }
    if {[catch {
      file atime $filename $origAtime
    } err]} {
      puts stderr \
        "Warning: Unable to reset access time of $filename: $err"
    }

    if {[info exists linkkey]} {
      ::aes::Final $linkkey
    }
  }

  # Initialises the storage engine. Raises an error if the file cannot be
  # worked with. Returns the number of usable bits.
  protected method init-storage-engine {} {
    error "Missing $this init-storage-engine"
  }

  # Enumerates the bits that can be stored with this storage engine.
  # The return value is a list, each item corresponding to a bit.
  # The format of the elements is not strongly defined; however, the
  # contents must be interpretable as a list whose first element is
  # an integer.
  protected method enumerate-bits {} {
    error "Missing $this enumerate-bits"
  }

  # Reads the given physical bit. The argument is an element from the list
  # returned by enumerate-bits.
  # Returns either 0 or 1.
  protected method read-physical-bit {bit} {
    error "Missing $this read-physical-bit bit"
  }

  # Writes the given physical bit. The bit is addressed with an element from the
  # list returned by enumerate-bits. The value is either 0 or 1.
  protected method write-physical-bit {bit value} {
    error "Missing $this write-physical-bit bit value"
  }

  # Reads and returns exactly 1024 bytes of out-of-band invariant data, in
  # binary format.
  # The default returns 1024 NUL bytes.
  protected method read-out-of-band-invariant {} {
    return $::NUL1024
  }

  # Initialises the storage device, given a 128-bit binary key.
  # Subclasses should not override this (see init-storage-engine).
  # Raises an error if the device cannot work with the file, or if it
  # has fewer than 641 usable bits.
  method init-storage-device {binkey} {
    set nbits [init-storage-engine]
    if {$nbits < 641} {
      error "$filename has fewer than 641 usable bits: $nbits"
    }

    set basebits [enumerate-bits]
    if {$nbits != [llength $basebits]} {
      puts stderr \
"Error in storage engine: $this: nbits ($nbits) != basebits [llength $basebits]"
    }

    set key [::aes::Init cbc $binkey $::NUL16]
    set indices {}
    # At the cost of somewhat lower entropy, immensely speed shuffling up
    # by XORing the lower half of the indices with the upper, so each chunk
    # gives us 128*128=16384 instead of 256 indices (speed up factor 64, minus
    # overhead of maintaining the two indices)
    set lixix 0
    set rixix 0
    set bitmap $basebits
    if {![dict exists $::programOptions -noshuffledevbits]} {
      for {set i $nbits; incr i -1} {$i > 0} {incr i -1} {
        if {$rixix >= [llength $indices]} {
          binary scan [::aes::Encrypt $key [read-out-of-band-invariant]] \
            iu256 indices
          set lixix 0
          set rixix 128
        }

        set index [expr {[lindex $indices $lixix] ^ [lindex $indices $rixix]}]
        if {128 == [incr lixix]} {
          set lixix 0
          incr rixix
        }
        # Make index unsigned, then modulo
        set index [expr {$index % ($i+1)}]
        # Swap items
        set curr [lindex $bitmap $i]
        lset bitmap $i [lindex $bitmap $index]
        lset bitmap $index $curr
      }
    }
    # Done
    ::aes::Final $key
  }

  # Flushes the write queue
  method flush-write-queue {} {
    if {[llength $writeQueue]} {
      # lsort is a stable sort, which is exactly what we want to do.
      # We can't use -unique, because it will eliminate all bits
      # with the same sorting address, and it would waste more time
      # to do another sorting pass than to just have some duplicate
      # writes.
      set wq [lsort -integer -index {0 0} $writeQueue]
      set writeQueue {}
      foreach bit $wq {
        write-physical-bit [lindex $bit 0] [lindex $bit 1]
      }

      # Write the opbytes and clear that list
      dict for {addr val} $opbytes {
        seek $handle $addr
        puts -nonewline $handle [binary format c $val]
      }
      set opbytes [dict create]
    }
  }

  # Returns the value (as an integer, 0..255) of the operational byte at
  # the given physical address. This should be used instead of reading
  # from the handle in write-physical-bit methods.
  method get-opbyte {byte} {
    seek $handle $byte
    binary scan [read $handle 1] cu val
    return $val
#     if {[dict exists $opbytes $byte]} {
#       return [dict get $opbytes $byte]
#     } else {
#       seek $handle $byte
#       binary scan [read $handle 1] cu val
#       dict set opbytes $byte $val
#       return $val
#     }
  }
  # Sets the value (as an integer, 0..255) of the operational byte at the given
  # physical address. This should be used instead of writing to the handle in
  # write-physical-bit methods.
  method set-opbyte {byte val} {
    #dict set opbytes $byte $val
    seek $handle $byte
    puts -nonewline $handle [binary format c $val]
  }
  # Removes the given operational byte. This causes it to not be written back.
  # This should only be done if it can be guaranteed that no other bits may be
  # using this byte.
  method del-opbyte {byte} {
    #dict unset opbytes $byte
  }

  # Returns the value (0 or 1) of the logical bit at the given index
  method read-logical-bit {bit} {
    flush-write-queue
    read-physical-bit [lindex $bitmap $bit]
  }

  # Writes to the logical bit at the given index the given value (0 or 1)
  method write-logical-bit {bit value} {
    lappend writeQueue [list [lindex $bitmap $bit] $value]
    if {[llength $writeQueue] > 1024} {
      flush-write-queue
    }
  }

  # Returns the number of bits available for storage.
  # The valid logical bit indices range from zero (inclusive) to this
  # value (exclusive)
  method bit-count {} {
    llength $bitmap
  }

  # Returns the name of the file the storage device operates on
  method get-filename {} {
    return $filename
  }

  # Sets the encryption key for the link data to the given 16-byte
  # binary string
  method set-link-key {key} {
    set linkkey [::aes::Init cbc $key $::NUL16]
  }

  # Returns the decrypted link data as a binary string
  method get-link-data {} {
    ::aes::Reset $linkkey $::NUL16
    set data {}
    for {set i 0} {$i < 640} {incr i} {
      append data [read-logical-bit $i]
    }
    ::aes::Decrypt $linkkey [binary format b640 $data]
  }

  # Writes the plaintext link data, provided as a binary string
  method set-link-data {data} {
    ::aes::Reset $linkkey $::NUL16
    binary scan [::aes::Encrypt $linkkey $data] b640 data
    for {set i 0} {$i < 640} {incr i} {
      write-logical-bit $i [string index $data $i]
    }
  }
}

# Examines the extension of the given filename and tries to create and
# initialises a Storage-Device on it.
# Returns a Storage-Device, or raises an error.
proc create-storage-device {filename key} {
  set ext [string tolower [file extension $filename]]
  if {[dict exists $::FILE_EXTENSIONS $ext]} {
    puts -nonewline "[::term::ansi::code::ctrl::el]Loading $filename\r"
    flush stdout
    set sd [[dict get $::FILE_EXTENSIONS $ext] #auto $filename]
    if {[catch {
      $sd init-storage-device $key
    } err]} {
      delete object $sd
      error $err
    }
    return $sd
  } else {
    error "$filename: No known storage engine for file type: $ext"
  }
}

# Recursively scans the current directory for files, shuffles them, puts the
# root file at the front, creates storage devices on the ones that are usable,
# and writes link data to all, thereby making the system ready for mounting.
proc format-system {} {
  global programOptions urandom
  set files {}
  set dirqueue [list .]
  while {[llength $dirqueue] > 0} {
    set dirqueue [lassign $dirqueue dir]
    # Enqueue readable-traversable subdirectories
    set dirqueue [concat $dirqueue \
      [glob -nocomplain -types {d r x} -directory $dir -- *]]
    # Add readable-writable files
    set files [concat $files \
      [glob -nocomplain -types {f r w} -directory $dir -- *]]
  }

  # Strip extraneous ./ from all filenames (it is present on all of them)
  for {set i 0} {$i < [llength $files]} {incr i} {
    set files [lreplace $files $i $i [string range [lindex $files $i] 2 end]]
  }

  # Ensure root file could be found
  if {[dict get $programOptions -r] ni $files} {
    puts stderr "Root file could not be found under directory."
    exit 128
  }
  # Ensure all filenames are <= 63 characters long and are ASCII
  for {set i 0} {$i < [llength $files]} {incr i} {
    set file [lindex $files $i]
    if {[string length $file] > 63} {
      puts stderr \
"Warning: $file: name length > 63 characters, excluding from file list."
      set files [lreplace $files $i $i]
      incr i -1
      continue
    }

    if {![string is ascii -strict $file]} {
      puts stderr "Warning: $file: name not ASCII, excluding from file list."
      set files [lreplace $files $i $i]
      incr i -1
      continue
    }
  }

  # Shuffle files
  set sfiles $files
  for {set i [llength $sfiles]; incr i -1} {$i > 0} {incr i -1} {
    set ix [read $urandom 4]
    binary scan $ix iu ix
    set ix [expr {$ix % ($i+1)}]
    set curr [lindex $sfiles $i]
    lset sfiles $i [lindex $sfiles $ix]
    lset sfiles $ix $curr
  }

  # Move root to front
  set sfiles [concat [list [dict get $programOptions -r]] \
                     [lsearch -all -inline -not -exact $sfiles \
                              [dict get $programOptions -r]]]
  # Create initial keys
  set pwhash [::sha2::sha256 -bin [dict get $programOptions -password]]
  set linkKey [string range $pwhash 0 15]
  set currentKey [string range $pwhash 16 31]
  # Open files
  set devices {}
  set keys {}
  foreach file $sfiles {
    if {[catch {
      set dev [create-storage-device $file $currentKey]
      $dev set-link-key $linkKey
      lappend keys $currentKey
      lappend devices $dev
      # Read new rearrangement key
      set currentKey [read $urandom 16]
    } err]} {
      puts stderr $err
      if {0 == [llength $devices]} {
        puts stderr "Root file could not be used, aborting..."
        exit 129
      }
    }
  }
  lappend keys $currentKey

  # Create links
  puts -nonewline "[::term::ansi::code::ctrl::el]Writing links...\r"
  flush stdout
  for {set i 0} {$i < [llength $devices]} {incr i} {
    set nxt $i
    incr nxt
    set linkdat [[lindex $devices $i] get-link-data]
    if {$nxt < [llength $devices]} {
      set nxtname [[lindex $devices $nxt] get-filename]
    } else {
      set nxtname {}
    }
    # Add terminating NUL to link name
    append nxtname "\0"
    set linkdat [string replace $linkdat 0 [string length $nxtname]-1 $nxtname]
    # Set next-key field
    set linkdat [string replace $linkdat end-15 end [lindex $keys $nxt]]
    # Write back
    [lindex $devices $i] set-link-data $linkdat
  }

  # Done, close and produce report
  set size 0
  foreach device $devices {
    puts -nonewline "[::term::ansi::code::ctrl::el]Closing [$device get-filename]\r"
    flush stdout
    set size [expr {$size + [$device bit-count]-640}]
    delete object $device
  }

  set prefices {{} k M G T P E Z Y}
  set size [expr {$size/8}]
  set pix 0
  while {$size >= 10240} {
    incr pix
    set size [expr {$size/1024}]
  }
  puts "[::term::ansi::code::ctrl::el]Created a system with $size [lindex $prefices $pix]B of usable space."
}

# Assembles and mounts the system if possible.
proc mount-system {} {
  global programOptions assemblyDevices assemblyBitmap assemblyKey unmount

  # Create initial keys
  set pwhash [::sha2::sha256 -bin [dict get $programOptions -password]]
  set linkKey [string range $pwhash 0 15]
  set deviceKey [string range $pwhash 16 31]
  set assemblyKeyHashString [dict get $programOptions -password]
  set globalDispersionKey [string range [::sha2::sha256 -bin \
    "[dict get $programOptions -password][dict get $programOptions -password]" \
    ] 16 31]
  binary scan $globalDispersionKey cu16 globalDispersionKey

  # Open devices
  set nextFile [dict get $programOptions -r]
  while {[string length $nextFile]} {
    # Accumulate assembly key hash string
    append assemblyKeyHashString $nextFile

    # Open file
    if {[catch {
      set dev [create-storage-device $nextFile $deviceKey]
    } err]} {
      puts stderr $err
      delete-all-devices
      exit 128
    }
    lappend assemblyDevices $dev

    # Read link data
    $dev set-link-key $linkKey
    set linkdat [$dev get-link-data]
    set deviceKey [string range $linkdat end-15 end]
    set nextFile [string range $linkdat 0 end-16]
    set endix [string first "\0" $nextFile]
    if {$endix == -1} {
      puts stderr "[$dev get-filename]: Invalid link data"
      binary scan $linkdat H* out
      puts stderr $out
      delete-all-devices
      exit 128
    }
    set nextFile [string range $nextFile 0 $endix-1]

    # Accumulate global dispersion key
    binary scan $deviceKey cu16 dkb
    for {set i 0} {$i < 16} {incr i} {
      lset globalDispersionKey $i \
        [expr {[lindex $globalDispersionKey $i] ^ [lindex $dkb $i]}]
    }
  }

  puts "[::term::ansi::code::ctrl::el]All files opened successfully."

  # Create final keys
  set assemblyKey [string range \
    [::sha2::sha256 -bin $assemblyKeyHashString] 16 31]
  set assemblyKey [::aes::Init ecb $assemblyKey {}]
  set globalDispersionKey [binary format c16 $globalDispersionKey]
  set globalDispersionKey [::aes::Init cbc $globalDispersionKey $::NUL16]

  if {[dict exists $programOptions -v]} {
    puts "Enumerating bits."
  }

  # Create bitmap
  set assemblyBitmap {}
  for {set devix 0} {$devix < [llength $assemblyDevices]} {incr devix} {
    set bcnt [[lindex $assemblyDevices $devix] bit-count]
    for {set bitix 640} {$bitix < $bcnt} {incr bitix} {
      lappend assemblyBitmap [list $devix $bitix]
    }
  }

  if {[dict exists $programOptions -v]} {
    puts "Shuffling bits."
  }

  # Shuffle
  if {![dict exists $programOptions -noshuffleallbits]} {
    set indices {}
    # At the cost of somewhat lower entropy, immensely speed shuffling up
    # by XORing the lower half of the indices with the upper, so each chunk
    # gives us 128*128=16384 instead of 256 indices (speed up factor 64, minus
    # overhead of maintaining the two indices)
    set lixix 0
    set rixix 0
    for {set i [llength $assemblyBitmap]; incr i -1} {$i > 0} {incr i -1} {
      if {$rixix >= [llength $indices]} {
        binary scan [::aes::Encrypt $globalDispersionKey $::NUL1024] \
          iu256 indices
        set lixix 0
        set rixix 128
      }

      set index [expr {[lindex $indices $lixix] ^ [lindex $indices $rixix]}]
      if {128 == [incr lixix]} {
        set lixix 0
        incr rixix
      }
      set index [expr {$index % ($i+1)}]
      set curr [lindex $assemblyBitmap $i]
      lset assemblyBitmap $i [lindex $assemblyBitmap $index]
      lset assemblyBitmap $index $curr
    }
  }
  # Bit mapping ready
  ::aes::Final $globalDispersionKey

  # Mount filesystem
  fuse create polysteg {*}{
    -getattr    fs-getattr
    -readdir    fs-readdir
    -open       fs-open
    -read       fs-read
    -write      fs-write
    -destroy    fs-destroy
    -access     fs-access
    -fgetattr   fs-fgetattr
  }

  if {[dict exists $programOptions -o]} {
    set opts [concat -o [dict get $programOptions -o]]
  } else {
    set opts {}
  }
  polysteg [dict get $programOptions -m] {*}$opts

  puts "[::term::ansi::code::ctrl::el]Press ENTER to unmount."
  ::term::receive::listen listen-for-unmount
  vwait unmount
  rename polysteg {}

  # Done
  #exit 0
  puts [::term::ansi::code::ctrl::el]
}

# Sets the unmount global
proc listen-for-unmount {args} {
  global unmount
  set unmount {}
}

# Frees all global devices, if present
proc delete-all-devices {} {
  global assemblyDevices
  if {[info exists assemblyDevices]} {
    foreach device $assemblyDevices {
      puts -nonewline "[::term::ansi::code::ctrl::el]Closing [$device get-filename]\r"
      flush stdout
      delete object $device
    }
  }
}

# Reads the encrypted value (0 or 1) of the assembly-logical bit at the given
# location.
proc read-assembly-bit {bit} {
  global assemblyDevices assemblyBitmap
  lassign [lindex $assemblyBitmap $bit] devix subbit
  [lindex $assemblyDevices $devix] read-logical-bit $subbit
}

# Writes the encrypted value (0 or 1) of the assembly-logical bit at the given
# location.
proc write-assembly-bit {bit value} {
  global assemblyDevices assemblyBitmap
  lassign [lindex $assemblyBitmap $bit] devix subbit
  [lindex $assemblyDevices $devix] write-logical-bit $subbit $value
}

# Reads and decrypts the 128-bit block at the given bit offset.
proc read-assembly-block {offset} {
  global assemblyKey
  set dat ""
  for {set i $offset} {$i < $offset+128} {incr i} {
    append dat [read-assembly-bit $i]
  }
  ::aes::Decrypt $assemblyKey [binary format b128 $dat]
}

# Encrypts and writes the 128-bit block at the given bit offset.
# The data is given in binary format.
proc write-assembly-block {offset data} {
  global assemblyKey
  binary scan [::aes::Encrypt $assemblyKey $data] b128 data
  for {set i 0} {$i < 128} {incr i; incr offset} {
    write-assembly-bit $offset [string index $data $i]
  }
}

# Returns the usable size of the internal data (removing any incomplete blocks)
proc get-system-size {} {
  global assemblyBitmap
  set sz [llength $assemblyBitmap]
  expr {($sz/8) & ~15}
}

# Load the storage engines in ASCIIbetical order.
foreach storageEngine \
        [lsort [glob "[file dirname $argv0]/storage-engines/*.tcl"]] {
  source $storageEngine
}

# Fail with a POSIX error.
# Usage: {*}[posix-error CODE]
proc posix-error {code} {
  list return -code error -errorcode [list POSIX $code {}]
}

# FUSE bindings
proc fs-access {context path mask} {
  switch -exact -- $path {
    / {
      if {"exists" in $mask} return
      if {"write" in $mask} {{*}[posix-error EACCESS]}
      # R and X ok
      return
    }
    /data {
      if {"execute" in $mask} {{*}[posix-error EACCESS]}
      # R and X ok, exists
      return
    }
    default { {*}[posix-error EACCESS] }
  }
}

proc fs-destroy {context} {
  delete-all-devices
}

proc fs-fgetattr {context path fileinfo} {
  fs-getattr $context $path
}

proc fs-getattr {context path} {
  switch -exact -- $path {
    / {
      return [dict create type directory mode 0555 nlinks 2]
    }
    /data {
      return [dict create mode 0666 nlinks 1 size [get-system-size]]
    }
    default {
      {*}[posix-error ENOENT]
    }
  }
}

proc fs-open {context path fileinfo} {
  switch -exact -- $path {
    / {
      {*}[posix-error EISDIR]
    }
    /data {
      return {}
    }
    default {
      {*}[posix-error ENOENT]
    }
  }
}

proc fs-read {context path finfo size offset} {
  if {$path != "/data"} {
    {*}[posix-error ENOENT]
  }
  set offset [expr {min($offset, [get-system-size])}]
  set end [expr {min([get-system-size], $offset+$size)}]
  set beginBlock [expr {$offset/16}]
  # End block is exclusive
  set endBlock [expr {($end+15)/16}]
  set leadingBytes [expr {$offset % 16}]
  set trailingBytes [expr {15-($end+15)%16}]
  set data {}
  for {set block $beginBlock} {$block < $endBlock} {incr block} {
    append data [read-assembly-block [expr {$block*128}]]
  }

  string range $data $leadingBytes end-$trailingBytes
}

proc fs-write {context path finfo data offset} {
  if {$path != "/data"} { {*}[posix-error ENOENT] }

  # Ensure that we stay within bounds
  if {$offset + [string length $data] >= [get-system-size]} {
    {*}[posix-error ENOSPC]
  }
  if {[catch {
    set dsz [string length $data]

    set end [expr {$offset+[string length $data]}]
    set beginBlock [expr {$offset/16}]
    # End block is exclusive
    set endBlock [expr {($end+15)/16}]
    set leadingBytes [expr {$offset % 16}]
    set trailingBytes [expr {15-($end+15)%16}]

    # If this is an incomplete write, pad data with what is currently
    # on disk.
    if {$leadingBytes != 0} {
      set curr [read-assembly-block [expr {$beginBlock*128}]]
      set data "[string range $curr 0 $leadingBytes-1]$data"
    }
    if {$trailingBytes != 0} {
      set curr [read-assembly-block [expr {($endBlock-1)*128}]]
      set data "$data[string range $curr end-[expr {$trailingBytes-1}] end]"
    }

    # Write the blocks
    for {set block $beginBlock} {$block < $endBlock} {incr block} {
      write-assembly-block [expr {$block*128}] [string range $data 0 15]
      set data [string range $data 16 end]
    }
  } err]} {
    puts stderr "Error in writing: $err"
  }

  return $dsz
}

proc fs-readdir {context path finfo} {
  if {$path == "/"} {
    return [list "." ".." "data"]
  } else {
    {*}[posix-error ENOENT]
  }
}


main
