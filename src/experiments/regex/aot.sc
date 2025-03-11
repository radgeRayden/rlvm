using import radl.strfmt
main := import .regex

compile-object
    default-target-triple
    compiler-file-kind-object
    .. module-dir "/regex.o"
    do
        let main = (static-typify main i32 (mutable@ rawstring))
        local-scope;

using import C.stdlib
system f"gcc -o ./bin/regex ${module-dir}/regex.o -L./lib -lstb -lm -Wl,-rpath '-Wl,$ORIGIN/../lib'"
