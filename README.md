# efunge

[![Build Status](https://travis-ci.org/VorpalBlade/efunge.svg?branch=master)](https://travis-ci.org/VorpalBlade/efunge)

This is a simple Befunge-98 interpreter coded in Erlang.

efunge uses arbitrary-sized integers, this is a side effect of Erlang using that
by default.

To build and run efunge you will need:

* Erlang. Version 17 is currently recommended. Newer minor versions should
  usually work too (but that is not guaranteed). When a new stable major version
  of Erlang is released, efunge is generally upgraded to it (after a few days or
  weeks) and "support" for the older Erlang version is dropped. Older versions
  may still work (if you are lucky) but are neither tested nor "supported".
  **Warning**: Any version older than 17.0 is unlikely to work currently.
* A *nix make (or you could compile efunge manually, but details about that is
  undocumented).
* Optional: A POSIX compatible shell for the wrapper script. You can also run
  efunge from inside the Erlang shell.


## Building instructions

* Make sure the program "erl" (without quotes) is in your PATH.
* Run "make" (without the quotes) in your shell while you are in the efunge
  directory.
* To build a high performance build use:

      ERL_COMPILER_OPTIONS='[inline,native,{hipe,[o3]}]' make

  Note that your Erlang needs HIPE support for this, and debugging is near
  impossible in such a build (see below for how do properly build a debugging
  build).


## Running instructions

You can run efunge in two ways:
* Using the wrapper script efunge, by default it will only work when run inside
  build directory. Change $EFUNGE_ROOT at the top to an absolute path to the
  ebin directory if you want to run it elsewhere.
* Start an Erlang shell in the build directory using:

      erl -pa ./ebin

  Then run:
 
      efunge:start("path/to/befunge/program.b98").
  or
  
      efunge:start("path/to/befunge/program.b98", ["parameters", "to", "the program"]).

  inside the Erlang shell to run efunge. Note that the dot after the statement
  is important, do not try to use a semi-colon or such, just use a dot.
  Use "q()." (without the quotes) to exit the Erlang shell.


## Debugging build

If you want to report a bug with a backtrace, compile with:

    ERL_COMPILER_OPTIONS='[debug_info]' make clean all

Then run the resulting program from *inside* erl as shown in example two above.
The backtrace produced when running the interpreter freestanding is much less
detailed (and less useful).

Remember to also include:

 * Details on how to reproduce the issue.
 * If possible include the program causing the bug in efunge.
   Or even better: A minimal test case.
 * Which OS you use. (Linux, Windows, OS X, ...)
 * Which architecture you use. (x86, x86_64, ARM, PPC, ...)
 * Erlang version. (You are looking for something like "R13B-0" here)
 * Runtime variant. (Run erl and copy the entire first line of output, will be
   a long line beginning with something similar to:
   "`Erlang R14B (erts-5.8.1) [source] [64-bit] [rq:1] [hipe]`".)
 * efunge version or branch and revision.
 * And if the bug causes a crash don't forget the backtrace (as described above).


## Implementation oddities

These notes apply to efunge running on R13B-0. It is possible that it will
exhibit other behaviour under other versions. Some of these oddities can be
resolved by defining Erlang itself to be the OS that efunge runs under. Some
other ones are undefined in the Funge-98 standard.

 * Standard IO:
   - When running inside the shell, many non-printable characters will be
     escaped on output. This is an Erlang "feature", if it is a problem just run
     efunge freestanding.
   - Standard input is read one line at a time and buffered internally. Those
     instructions reading chars fetch one char from this buffer, leaving the
     rest (if any) including any ending newline. Instructions reading an integer
     will leave anything after the integer in the buffer with one exception: if
     the next char is a newline it will be discarded.
   - Standard IO uses Unicode. This means:
     * The argument to `,` (output character) is a Unicode code point which is
       encoded by Erlang in a platform-specific way before being output (to
       UTF-8 most of the time). Invalid codepoints will make `,` reflect. See
       the Erlang documentation for which codepoints are invalid.
     * Input will reflect on EOF (as expected) but also on invalid Unicode.
       Since Erlang normally expects the user's terminal to use UTF-8 this means
       invalid UTF-8 will make input (`~` and `&` for example) reflect. Any
       other input on the same line is lost.
     Both these points also apply to to fingerprint instructions dealing with
     standard IO (such as `S` in ORTH).
 * File IO:
   - File IO uses unsigned bytes (this means loading the program, `i` and `o`).
     Values are truncated to the range 0-255 using modulo 256 for `o`. How
     negative values are truncated is undefined.
   - In non-binary mode (this includes initial program loading) form feed is
     ignored, not advancing either x or y coordinate. This is due to form feed
     being used to increment z in Trefunge. Ignoring form feed in Befunge is
     consistent with the way newlines are treated in Unefunge.
 * Erlang adds some environment variables of it's own. It also modifies `$PATH`.
   It is not possible to fix or work around this in a reliable way, thus the
   environment variables as reported by y may differ slightly from what you
   would have expected.
 * efunge uses arbitrary precision integers for the cells.
   Due to this y reports -1 for cell size.
 * `y` pushes time in UTC (not local time).
 * `k` with a negative argument reflects.
 * `#` across edge of funge-space may or may not skip first instruction after
   wrapping depending on the exact situation.
 * `(` and `)` with a negative count reflects and doesn't pop any fingerprint.


## Known bugs and limitations

* efunge is slow, but that will not be fixed, if you want a faster Befunge
  interpreter try cfunge (https://github.com/VorpalBlade/cfunge) that is coded
  in C. cfunge also handles Funge-109. However, cfunge only works on POSIX
  platforms.

Real bugs or issues:
 * `k` on `k` is broken.
 * `k` on `"` may be broken.
 * The code is rather unreadable in many places, a clean up would be a good idea.
 * Better code documentation would be useful.

Missing features:
 * `=` isn't supported yet, and won't use the system() paradigm.
 * `t` isn't supported yet, and may never be supported.
