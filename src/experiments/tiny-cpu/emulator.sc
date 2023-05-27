using import Array
using import enum
using import String

inline error (msg)
    print msg
    exit 1

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
    CALL

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

fn interrupt (idx)
    switch idx
    case 1 # PRINT
        using import C.stdio
        addr := acc
        printf "%s" ((& (RAM @ (addr as usize))) as (@ i8)) # can we make this safer?
    default
        error "unknown interrupt triggered"

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
                using import radl.strfmt
                error f"out of bounds memory access at ${next}"
            lo hi := RAM @ addr, RAM @ (addr + 1)
            (hi as u16) << 8 | (lo as u16)

        inline mem-write (addr content)
            if ((countof RAM) <= (addr + 1))
                using import radl.strfmt
                error "out of bounds memory access at ${next}"
            lo hi := content as u8, (content >> 8) as u8
            RAM @ addr       = lo
            RAM @ (addr + 1) = hi

        inline get-mem ()
            let addr =
                if register-addr?
                    reg := (get-reg)
                    deref (@reg as u16)
                else
                    get16;

            mem-read addr

        ins := Instructions
        switch opcode
        case ins.LOAD
            dst := (get-reg)
            src := (get-val)
            @dst = mem-read src
        case ins.STORE
            src := (get-reg)
            dst := (get-val)
            mem-write dst @src
        case ins.COPY
            dst := (get-reg)
            src := (get-val)
            @dst = src
        case ins.SWAP
            a b := (get-reg), (get-reg)
            sh = @a
            @a = @b
            @b = sh
        case ins.PUSH
            'append stack (get-val)
        case ins.POP
            dst := (get-reg)
            @dst = ('pop stack)
        case ins.ADD
            v := (get-val)
            acc += v
        case ins.SUB
            v := (get-val)
            acc -= v
        case ins.MUL
            v := (get-val)
            acc *= v
        case ins.DIV
            v := (get-val)
            acc //= v
        case ins.AND
            v := (get-val)
            acc &= v
        case ins.OR
            v := (get-val)
            acc |= v
        case ins.XOR
            v := (get-val)
            acc ^= v
        case ins.JMP
            jmp-dst := (get-val)
            next = jmp-dst
        case ins.JZ
            jmp-dst := (get-val)
            if (acc == 0)
                next = jmp-dst
        case ins.JNZ
            jmp-dst := (get-val)
            if (acc != 0)
                next = jmp-dst
        case ins.SHL
            v := (get-val)
            acc <<= v
        case ins.SHR
            v := (get-val)
            acc >>= v
        case ins.INT
            idx := (get-val)
            interrupt idx
        case ins.CALL
            jmp-dst := (get-val)
            'append stack (next as u16)
            next = jmp-dst
        default
            using import radl.strfmt
            error
                f"unknown opcode at ${idx}; aborting"

        deref next

fn main (argc argv)
    if (argc == 0)
        print "usage: scopes -e emulator.sc file.bin"
        exit 1

    strlen  := from (import C.string) let strlen
    IO := import radl.IO

    path := argv @ 0
    path := (String path (strlen path))

    let ROM =
        try
            file := IO.FileStream path IO.FileMode.Read
            'read-all-bytes file
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

sugar-if main-module?
    name argc argv := (script-launch-args)
    main argc argv
else
    main
