assemble := import .assembler
emulate := import .emulator
using import radl.strfmt String

fn main (argc argv)
    if (argc == 1)
        print "missing subcommand"
        exit 1

    if (('from-rawstring String (argv @ 1)) == S"assemble")
        assemble (argc - 2) (& (argv @ 2))
    elseif (('from-rawstring String (argv @ 1)) == S"emulate")
        emulate (argc - 2) (& (argv @ 2))
    0

compile-object
    default-target-triple
    compiler-file-kind-object
    .. module-dir "/tiny-cpu.o"
    do
        let main = (static-typify main i32 (mutable@ rawstring))
        local-scope;

using import C.stdlib
system f"gcc -o ./bin/tiny-cpu ${module-dir}/tiny-cpu.o -L./lib -lstb -lscopesrt -Wl,-rpath '-Wl,$ORIGIN/../lib'"
system f"./bin/tiny-cpu assemble ${module-dir}/hello.s"
# system f"./bin/tiny-cpu emulate ${module-dir}/hello.s.bin"
