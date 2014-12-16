   _       _ _(_)_     |  A fresh haskell julia compiler to mips
  (_)     | (_) (_)    |  Documentation: run <jpc -h> for help
   _ _   _| |_  __ _   |
  | | | | | | |/ _` |  |  Version 0.3 (2014-12-16 23:59 UTC)
  | | |_| | | | (_| |  |  Official: http://julialang.org
 _/ |\__'_|_|_|\__'_|  |
|__/                   |  from julia to mips in less than a second!
                   _       _          _
                  (_)     | |        (_)
             _ __  _ _ __ | |__   ___ _ _ __ ___
            | '_ \| | '_ \| '_ \ / _ \ | '__/ _ \
    ______  | |_) | | | | | | | |  __/ | | | (_) |
   |______| | .__/|_|_| |_|_| |_|\___|_|_|  \___/
            | |
            |_|

	By Pedro Paredes and Filipe Figueiredo (DCC/FCUP)

If you are reading this then congratulations! You have just installed
the revolution in compiler industry, the first functional compiler
from julia to mips assembly code. Before you get started here are a
couple of important notes:

 - When using floats jpc uses the 'li.s' (load immediate float) from
mips.  Unfortunately this doesn't work on all mips assemblers (for
example, doesn't work on Mars). Since this was a minor detail the
creators chose to ignore it, but if you really want to test floats
simply change the (example) 'li.s $f0, 5.5' to 'l.s $f0, fl1" and then
add an entry in the '.data' section reading: 'fl1: .float 5.5';

 - Since this is compiled to assembly code it is not dynamic typed as
in the original julia, but it is static typed;

 - It includes: types (bools, ints, floats), expressions, println,
read, attributions, ifs (with elseif and else), whiles, functions (for
information on the syntax check samples/code.jl);

 - The syntax for functions is a little bit different from the
   original julia for the same reason it is static typed. The syntax
   is as follows: 'function name (type arg1, type arg2, ...) type'
   (for information on the syntax check samples/code.jl);

Here is the basic help file:
        
Usage:

	jpc [command-line-options] <input-file>

Available options:

	-h		Displays this amazing help file
	-o <file>	Set '<file>.asm' as output file
	--print-parse	Print internal parse code in haskell's structures
	--print-tadd	Print internal three address code in haskell's structures


     Enjoy our compiler and have fun juliying around,
        Pedro Paredes and Filipe Figueiredo
