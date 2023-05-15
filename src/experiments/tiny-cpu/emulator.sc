using import Array
using import String

MEMORY-SIZE := 2 ** 16:usize

global RAM : (Array u8 MEMORY-SIZE)
global code : (Array u8)

static-if main-module?
    name argc argv := (script-launch-args)
    if (argc == 0)
        print "usage: scopes -e emulator.sc file.bin"
        exit 1

    strlen  := from (import C.string) let strlen
    file-io := import radl.file-io

    path := argv @ 0
    path := (String path (strlen path))

    let ROM =
        try (file-io.read-binary-file path)
        except (ex)
            error (.. "failed to read file: " (tostring ex))

    if ((countof ROM) < MEMORY-SIZE)
        error "ROM too small, at least 64kb expected"
    elseif ((countof ROM) == MEMORY-SIZE)
        error "missing code after memory image"

    'resize RAM MEMORY-SIZE

    src := (imply ROM pointer) as (@ i8)
    dst := (imply RAM pointer) as (mutable@ i8)
    memcpy dst src MEMORY-SIZE

    code-size := (countof ROM) - MEMORY-SIZE
    'resize code code-size
    src := (& (ROM @ MEMORY-SIZE)) as (@ i8)
    dst := (imply code pointer) as (mutable@ i8)
    memcpy dst src code-size
else
    ;
