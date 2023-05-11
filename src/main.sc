using import enum
import sljit

inline sljit-fsig (...)
    argc := va-countof ...
    static-if (argc == 0 or argc > 4)
        static-error "sljit function signature must have at least return type and at most 3 arguments"

    types... :=
        va-map
            inline (t)
                static-if ((typeof t) != Symbol)
                    static-error
                        "type must be a symbol: void, word, word-r, i32, i32-r, pointer, pointer-r, f64 or f32"
                switch t
                case 'void
                    sljit.SLJIT_ARG_TYPE_VOID
                case 'word
                    sljit.SLJIT_ARG_TYPE_W
                case 'word-r
                    sljit.SLJIT_ARG_TYPE_W_R
                case 'i32
                    sljit.SLJIT_ARG_TYPE_32
                case 'i32-r
                    sljit.SLJIT_ARG_TYPE_32_R
                case 'pointer
                    sljit.SLJIT_ARG_TYPE_P
                case 'pointer-r
                    sljit.SLJIT_ARG_TYPE_P_R
                case 'f64
                    sljit.SLJIT_ARG_TYPE_F64
                case 'f32
                    sljit.SLJIT_ARG_TYPE_F32
                default
                    error
                        "type must be a symbol: void, word, word-r, i32, i32-r, pointer, pointer-r, f64 or f32"
            ...

    va-lifold 0
        inline (i __ value self)
            self | (sljit.SLJIT_ARG_VALUE value i)
        types...

fn add3 (a b c)
    compiler := sljit.create_compiler null null
    assert (compiler != null)

    """"
        fn add3 (word word word) -> word
            mov r0 s0
            add r0 r0 s1
            add r0 r0 s2
            return mov r0

    sljit.emit_enter compiler 0 (sljit-fsig 'word 'word 'word 'word) 1 3 0 0 0

    sljit.emit_op1 compiler sljit.SLJIT_MOV sljit.SLJIT_R0 0 sljit.SLJIT_S0 0
    sljit.emit_op2 compiler sljit.SLJIT_ADD sljit.SLJIT_R0 0 sljit.SLJIT_R0 0 sljit.SLJIT_S1 0
    sljit.emit_op2 compiler sljit.SLJIT_ADD sljit.SLJIT_R0 0 sljit.SLJIT_R0 0 sljit.SLJIT_S2 0

    sljit.emit_return compiler sljit.SLJIT_MOV sljit.SLJIT_R0 0

    code := sljit.generate_code compiler
    len := sljit.get_generated_code_size compiler

    func := code as (@ (function i64 i64 i64 i64))
    result := func a b c

    sljit.free_compiler compiler
    sljit.free_code code null

    result

fn main (argc argv)
    print (add3 1 2 3)
    0

static-if main-module?
    let name argc argv = (script-launch-args)
    main argc argv
else
    main
