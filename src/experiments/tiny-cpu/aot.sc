assemble := import .assembler
emulate := import .emulator
using import String

fn main (argc argv)
    if (argc == 1)
        print "missing subcommand"
        exit 1

    if ((String (argv @ 1)) == S"assemble")
        assemble (argc - 2) (& (argv @ 2))
    elseif ((String (argv @ 1)) == S"emulate")
        emulate (argc - 2) (& (argv @ 2))
    0

compile-object
    default-target-triple
    compiler-file-kind-object
    "tiny-cpu.o"
    do
        let main = (static-typify main i32 (mutable@ rawstring))
        local-scope;

using import C.stdlib
system "gcc -o tiny-cpu tiny-cpu.o -L. -lscopesrt -lstb -Wl,-rpath '-Wl,$ORIGIN'"
system "./tiny-cpu assemble hello.s"
system "./tiny-cpu emulate hello.s.bin"
