using import Array
using import enum
using import String

enum Instructions plain
    LOAD
    STORE
    COPY
    SWAP
    PUSH
    POP
    ADD
    SUB
    MUL
    DIV
    AND
    OR
    XOR
    JMP
    JZ
    JNZ
    SHL
    SHR
    INT

MEMORY-SIZE := 2 ** 16:usize

global RAM : (Array u8 MEMORY-SIZE)
global code : (Array u8)
global acc : u16
global r0 : u16
global r1 : u16
global r2 : u16
global r3 : u16
global sh : u16

global stack : (Array u16)

fn execute ()
    loop (idx = 0:usize)
        if (idx >= (countof code))
            break;

        opcode := code @ idx
        register-addr? := (opcode & 0x80:u8) as bool
        # get the actual index
        opcode := opcode & (~ 0x80:u8)

        local next = idx + 1

        inline get8 ()
            if (next == (countof code))
                error "incomplete instruction at the end of code section"
            else
                result := code @ next
                next += 1
                result

        inline get16 ()
            if (next == (countof code))
                error "incomplete instruction at the end of code section"
            else
                lo hi := code @ next, code @ (next + 1)
                next += 2
                (hi as u16) << 8 | (lo as u16)

        inline get-reg ()
            index := (get8)
            switch index
            case 0 &acc
            case 1 &r0
            case 2 &r1
            case 3 &r2
            case 4 &r3
            case 5 &sh
            default
                error "tried to address invalid register"

        inline get-val ()
            if register-addr?
                reg := (get-reg)
                deref @reg
            else
                get16;

        inline mem-read (addr)
            if ((countof RAM) <= (addr + 1))
                error (.. "out of bounds memory access at " (tostring (deref next)))
            lo hi := RAM @ addr, RAM @ (addr + 1)
            (hi as u16) << 8 | (lo as u16)

        inline mem-write (addr content)
            if ((countof RAM) <= (addr + 1))
                error (.. "out of bounds memory access at " (tostring (deref next)))
            lo hi := content as u8, (content >> 8) as u8
            RAM @ addr     = lo
            RAM @ addr + 1 = hi

        inline get-mem ()
            let addr =
                if register-addr?
                    reg := (get-reg)
                    @reg as usize
                else
                    get16;

            mem-read addr

        ins := Instructions
        switch opcode
        case ins.LOAD
            dst := (get-reg)
            @dst = (get-mem)
        case ins.STORE
            src := (get-reg)
            dst := (get-val)
            mem-write dst
        case ins.COPY
        case ins.SWAP
        case ins.PUSH
        case ins.POP
        case ins.ADD
        case ins.SUB
        case ins.MUL
        case ins.DIV
        case ins.AND
        case ins.OR
        case ins.XOR
        case ins.JMP
        case ins.JZ
        case ins.JNZ
        case ins.SHL
        case ins.SHR
        case ins.INT
        default
            using import radl.strfmt
            error
                string f"unknown opcode at ${idx}; aborting"

        deref next

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

    execute;
else
    ;