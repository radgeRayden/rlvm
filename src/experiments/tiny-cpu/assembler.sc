""""tiny-cpu
    ========
    1. load assembly for a made up cpu
    2. JIT compile it
    3. run it

using import Array
using import enum
using import Map
using import Option
using import String
using import struct

import .instructions

inline letter? (c)
    c >= char"A" and c <= char"Z" or c >= char"a" and c <= char"z"

inline digit? (c)
    c >= char"0" and c <= char"9"

inline hex-digit? (c)
    (digit? c) or (c >= char"a" and c <= char"f") or (c >= char"A" and c <= char"F")

inline whitespace? (c)
    c == " " or c == "\t"

fn consume-whitespace (input idx)
    for i in (range idx (countof input))
        c := input @ i

        if (not (whitespace? c))
            return i

    countof input

fn consume-line (input idx)
    for i in (range idx (countof input))
        c := input @ i
        if (c == "\n")
            return (i + 1)
    countof input

fn build-error-message (msg input idx)
    :: find-error-line
    do
        fold (line-count line-beg = 0 0:usize) for i c in (enumerate input)
            if (i >= idx) # we're in the correct line
                if (c == "\n" or (i == ((countof input) - 1)))
                    merge find-error-line
                        _ line-count (idx - line-beg) (slice input line-beg i)

                _ line-count line-beg
            else
                new-line? := c == "\n"
                _
                    new-line? (line-count + 1) line-count
                    new-line? ((i as usize) + 1) line-beg
        _ 0 idx (rslice input idx)
    find-error-line (line column source-fragment) ::

    anchor := .. (tostring line) ":" (tostring column) ":"
    local uparrow : String
    for i in (range ((countof anchor) + column))
        'append uparrow char" "
    'append uparrow char"^"

    err := .. "PARSER ERROR: " msg "\n" "while parsing:\n" anchor source-fragment "\n" uparrow

inline parsing-error (msg input idx)
    error ((build-error-message msg input idx) as string)

fn parse-symbol (input idx)
    if (digit? (input @ idx))
        return S"" idx

    for i in (range idx (countof input))
        c := input @ i
        valid-char? := (letter? c) or (digit? c) or c == "_"
        if (not valid-char?)
            return (slice input idx i) i

    _
        rslice input idx
        countof input

fn parse-string-literal (input idx)
    local str : String
    for i in (range idx (countof input))
        c := input @ i

        if (c == "\n")
            parsing-error "unexpected end of line inside string literal" input (idx - 1)

        if (c == "\"")
            return str (i + 1)

        if (c == "\\")
            let actual-char =
                switch (input @ (i + 1))
                case char"n"
                    char"\n"
                case char"t"
                    char"\t"
                case char"\\"
                    char"\\"
                case char"\""
                    char"\""
                default
                    parsing-error "unknown character escape in string literal" input i

            'append str actual-char

            next := i + 2
            repeat (next as usize)

        'append str c

    parsing-error "incomplete string literal, lacks closing quote" input idx

fn parse-integer (input idx positive?)
    vvv bind value
    if ((input @ idx == char"0") and (input @ (idx + 1) == char"x")) # is hex?
        first-digit := input @ (idx + 2)
        if (first-digit == 0 or (whitespace? first-digit) or first-digit == "\n")
            parsing-error "incomplete integer literal" input idx
        fold (value = 0) for i in (range (idx + 2) (countof input))
            d := input @ i

            if ((whitespace? d) or d == "\n" or d == ",")
                return (positive? value -value) i

            if (not (hex-digit? d))
                parsing-error "extraneous character in integer literal" input i

            let next-value =
                if (digit? d)
                    d - char"0"
                elseif (d < char"a")
                    10:i8 + d - char"A"
                else
                    10:i8 + d - char"a"

            (value * 16) + (next-value as i32)
    else
        # decimal
        fold (value = 0) for i in (range idx (countof input))
            d := input @ i
            if ((whitespace? d) or d == "\n")
                return (positive? value -value) i
            if (not (digit? d))
                parsing-error "extraneous character in integer literal" input i
            (value * 10) + ((d - char"0") as i32)

    _ (positive? value -value) (countof input)

enum TokenKind
    Directive     : String
    Integer       : i32
    StringLiteral : String
    Symbol        : String
    Label         : String
    Delimiter
    EOL
    EOF
    None
    NotImplemented

    inline expect (...)
        va-rfold 0:u32
            inline (__ next result)
                fT := getattr this-type next
                result | (0x1:u32 << fT.Literal)
            ...

    inline any-of? (value bitmask)
        bool (bitmask & (0x1:u32 << ('literal value)))

    inline expecting? (field bitmask)
        fT := getattr this-type field
        bool ((0x1:u32 << fT.Literal) & bitmask)

    inline decode-expected (bitmask)
        local tags : (Array String)
        va-map
            inline (fT)
                if ((0x1 << fT.Literal) & bitmask)
                    'append tags (String (tostring fT.Name))
            this-type.__fields__
        tags

    inline expect-any ()
        0xFFFFFFFF:u32

    inline expect-value ()
        this-type.expect
            'Integer
            'StringLiteral
            'Symbol

    inline expect-operand ()
        this-type.expect
            'Integer
            'Symbol

    inline expect-line-start ()
        this-type.expect
            'Directive
            'Symbol
            'Label
            'EOL

global symbol-map : (Map String TokenKind)
global labels     : (Map String usize)
global RAM-image  : (Array u8)
global bytecode   : (Array u8)
global ins-info = (instructions.build-instruction-table)

fn next-token (input idx)
    idx := consume-whitespace input idx
    if (idx == (countof input))
        return (TokenKind.EOF idx) idx idx

    c := input @ idx
    if (c == ".")
        directive next-idx := parse-symbol input (idx + 1)
        _ (TokenKind.Directive directive) idx next-idx
    elseif (digit? c)
        number next-idx := parse-integer input idx true
        _ (TokenKind.Integer number) idx next-idx
    elseif (c == "-")
        number next-idx := parse-integer input (idx + 1) false
        _ (TokenKind.Integer number) idx next-idx
    elseif (c == "\"")
        str next-idx := parse-string-literal input (idx + 1)
        _ (TokenKind.StringLiteral str) idx next-idx
    elseif (c == ",")
        _ (TokenKind.Delimiter) idx (idx + 1)
    elseif (c == ";") # comment
        _ (TokenKind.EOL) idx (consume-line input idx)
    elseif (letter? c)
        sym next-idx := parse-symbol input idx
        if ((input @ next-idx) == ":")
            _ (TokenKind.Label sym) idx (next-idx + 1)
        else
            _ (TokenKind.Symbol sym) idx next-idx
    elseif (c == "$")
        _ (TokenKind.Integer ((countof bytecode) as i32)) idx (idx + 1)
    elseif (c == "\n")
        _ (TokenKind.EOL) idx (idx + 1)
    else
        _ (TokenKind.NotImplemented) idx idx

enum CompilationError plain
    UnknownRegister
    UnknownLabel
    IllegalExpansion
    WrongOperand

enum OperationKind plain
    Define
    String
    Bytes
    Instruction
    Label
    None

    __typecall := (cls) -> this-type.None

struct Operation
    kind  : OperationKind
    mnemonic : String
    arg1  : (Option TokenKind)
    arg2  : (Option TokenKind)
    bytes : (Array u8)

struct UnresolvedJump
    origin : usize
    target : String
    anchor : usize

global unresolved-jumps : (Array UnresolvedJump)

fn compile-op (op anchor)
    raising CompilationError

    inline getargs (argc)
        static-if (argc == 1)
            'force-unwrap op.arg1
        else
            _
                'force-unwrap op.arg1
                'force-unwrap op.arg2
    inline extract (v T)
        'unsafe-extract-payload v T

    inline resolve-symbol (sym)
        sym := copy sym
        if (sym == TokenKind.Symbol)
            copy
                'getdefault symbol-map (extract (copy sym) String) sym
        else
            sym

    inline info (ins)
        'getdefault ins-info ins (instructions.InstructionInfo)

    inline get-register (name)
        match name
        case "acc"
            0x00
        case "r0"
            0x01
        case "r1"
            0x02
        case "r2"
            0x03
        case "r3"
            0x04
        case "sh"
            0x05
        default
            raise CompilationError.UnknownRegister

    inline is-jump? (name)
        match name
        case "jmp" true
        case "jnz" true
        case "jz" true
        case "call" true
        default
            false

    inline emit8 (v)
        'append bytecode (v as u8)
    inline emit16 (v)
        v as:= u16
        'append bytecode (v as u8)
        'append bytecode ((v >> 8) as u8)

    switch op.kind
    case OperationKind.Define
        arg1 arg2 := getargs 2
        sym := extract arg1 String
        'set symbol-map (copy sym) (copy arg2)
    case OperationKind.String
        arg1 arg2 := getargs 2
        sym := extract arg1 String
        'set symbol-map (copy sym) (TokenKind.Integer ((countof RAM-image) as i32))

        # copy string to RAM image
        for c in (extract arg2 String)
            'append RAM-image (bitcast c u8)
        if (('last op.mnemonic) == "z")
            'append RAM-image 0:u8
    case OperationKind.Bytes
        # copy bytes to RAM image
        for b in op.bytes
            'append RAM-image b
    case OperationKind.Instruction
        ins := info op.mnemonic
        if (is-jump? op.mnemonic)
            arg := getargs 1
            if (arg == TokenKind.Symbol)
                name := extract arg String
                try # will fail if not a register
                    reg := get-register name
                    emit8 (ins.opcode | 0x80) # toggle register addressing bit
                    emit8 reg
                else
                    emit8 ins.opcode
                    'append unresolved-jumps
                        UnresolvedJump
                            origin = (countof bytecode)
                            target = (copy name)
                            anchor = anchor
                    emit16 0:u16
            else # Integer
                emit8 ins.opcode
                emit16 (extract arg i32)
            return;

        switch ins.argc
        case 0
            emit8 ins.opcode
        case 1
            arg1 := getargs 1
            arg1 := resolve-symbol arg1
            dispatch arg1
            case Integer (val)
                if (op.mnemonic == "pop")
                    # only instruction that can't take an immediate operand
                    raise CompilationError.WrongOperand
                emit8 ins.opcode
                emit16 val
            case Symbol (sym)
                emit8 (ins.opcode | 0x80) # toggle register addressing bit
                emit8 (get-register sym)
            default
                raise CompilationError.IllegalExpansion
        case 2
            arg1 arg2 := getargs 2
            arg1 := resolve-symbol arg1
            arg2 := resolve-symbol arg2

            # first arg is always register
            vvv bind first-register
            dispatch arg1
            case Symbol (sym)
                get-register sym
            default
                raise CompilationError.IllegalExpansion

            dispatch arg2
            case Symbol (sym)
                emit8 (ins.opcode | 0x80) # toggle register addressing bit
                emit8 first-register
                emit8 (get-register sym)
            case Integer (val)
                emit8 ins.opcode
                emit8 first-register
                emit16 val
            default
                raise CompilationError.IllegalExpansion
        default
            ;
    default
        ;

fn compile (input)
    local expect-stack : (Array u32)
    local current-op : Operation

    inline push-arg (arg)
        if current-op.arg1
            current-op.arg2 = arg
        else
            current-op.arg1 = arg

    TK := TokenKind
    loop (idx = 0:usize)
        token start next-idx := next-token input idx

        stack-size := (countof expect-stack)
        let expected =
            if (not (empty? expect-stack))
                'pop expect-stack
            else
                try
                    compile-op current-op start
                except (ex)
                    error (tostring ex)

                current-op = (Operation)
                TK.expect-line-start;

        eof-expected? := TK.expecting? 'EOF expected
        if (token == TK.EOF and (not eof-expected?))
            if (stack-size != 0)
                parsing-error "unexpected end of file" input start
            break;

        if (not (TK.any-of? token expected))
            possible-tk := TK.decode-expected expected
            local tk-list : String
            for tk in possible-tk
                tk-list ..= tk .. ", "

            parsing-error (.. "expected any of: " tk-list "got: " (tostring token)) input start

        dispatch (copy token)
        case Directive (directive)
            current-op.mnemonic = (copy directive)
            if (directive == "define")
                current-op.kind = OperationKind.Define
                'append expect-stack (TK.expect 'EOL)
                'append expect-stack (TK.expect-value)
                'append expect-stack (TK.expect 'Symbol)
            elseif (directive == "asciiz" or directive == "ascii")
                current-op.kind = OperationKind.String
                'append expect-stack (TK.expect 'EOL)
                'append expect-stack (TK.expect 'StringLiteral)
                'append expect-stack (TK.expect 'Symbol)
            elseif (directive == "bytes")
                current-op.kind = OperationKind.Bytes
                'append expect-stack (TK.expect 'Integer)
                'append expect-stack (TK.expect 'Symbol)
            else
                parsing-error (.. "unknown directive: " directive) input start
        case StringLiteral (str)
            push-arg token
        case Symbol (sym)
            if (stack-size == 0) # head of list
                current-op.mnemonic = (copy sym)
                current-op.kind = OperationKind.Instruction

                let info =
                    try ('get ins-info sym)
                    else
                        parsing-error (.. "unknown instruction: " sym) input start

                switch info.argc
                case 0
                    'append expect-stack (TK.expect 'EOL)
                case 1
                    'append expect-stack (TK.expect 'EOL)
                    'append expect-stack (TK.expect-operand)
                case 2
                    'append expect-stack (TK.expect 'EOL)
                    'append expect-stack (TK.expect-operand)
                    'append expect-stack (TK.expect 'Delimiter)
                    'append expect-stack (TK.expect 'Symbol)
                default
                    ;
            else
                push-arg token
        case Label (sym)
            current-op.kind = OperationKind.Label
            if ('in? labels sym)
                parsing-error (.. "duplicated label: " sym) input start
            'set labels sym (countof bytecode)
        case Integer (value)
            if (current-op.kind == OperationKind.Bytes)
                'append expect-stack (TK.expect 'Delimiter 'EOL 'EOF)
                'append current-op.bytes (value as u8)
            else
                push-arg token
        case Delimiter ()
            if (current-op.kind == OperationKind.Bytes)
                'append expect-stack (TK.expect 'Integer)
        case EOL ()
        case EOF ()
        default
            parsing-error "unexpected character in input stream" input next-idx

        next-idx

    # link labels
    for j in unresolved-jumps
        try
            loc := 'get labels j.target
            hi lo := ((loc >> 8) as u8), (loc as u8)
            bytecode @ j.origin       = lo
            bytecode @ (j.origin + 1) = hi
        else
            parsing-error "unknown label" input (deref j.anchor)

static-if main-module?
    name argc argv := (script-launch-args)
    if (argc == 0)
        print "usage: scopes -e assembler.sc file.s"
        exit 1

    strlen  := from (import C.string) let strlen
    file-io := import radl.file-io

    path := argv @ 0
    path := (String path (strlen path))

    let source-code =
        try (file-io.read-text-file path)
        except (ex)
            error (.. "failed to read file: " (tostring ex))

    compile source-code
    'resize RAM-image (0xFFFF + 1)
    try
        file-io.write-file (path .. ".bin") RAM-image
        file-io.append-file (path .. ".bin") bytecode
    except (ex)
        error (.. "failed to write file: " (tostring ex))
else
    ;
