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

spice Scope->StringMap (valueT scope)
    scope as:= Scope

    expr :=
        spice-quote
            local strmap : (Map String valueT)

    vvv bind expr
    fold (expr = expr) for k v in scope
        k := k as Symbol as string
        spice-quote
            expr
            ('set strmap (String [k]) v)

    spice-quote
        expr
        strmap

run-stage;

enum TokenKind
    Directive     : String
    Integer       : i32
    StringLiteral : String
    Symbol        : String
    Label         : String
    Delimiter
    EOL
    EOF
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

struct InstructionInfo
    mnemonic : String
    opcode : u8
    argc : i32

global program1 : String
    """".define PRINT 0x01
        .asciiz msg "\thello, \"world\"\n"
        .bytes  arr 0x20, 0x93, 0x97

        start:
        mov acc, msg ;; this is a comment
        int PRINT

fn execute-instruction (ins)
    switch ins.opcode
    case 'mov
    case 'int
    default
        hide-traceback;
        error "invalid instruction"

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
    elseif (c == "\n")
        _ (TokenKind.EOL) idx (idx + 1)
    else
        _ (TokenKind.NotImplemented) idx idx


global define-map : (Map String TokenKind)
global labels : (Map String usize)
global bytecode : (Array u8)

fn compile-op (op)
    inline getargs (argc)
        static-if (argc == 1)
            'force-unwrap op.arg1
        else
            _
                'force-unwrap op.arg1
                'force-unwrap op.arg2
    inline extract (v T)
        'unsafe-extract-payload v T

    mnemonic := (bitcast (storagecast (hash op.mnemonic)) Symbol)
    switch mnemonic
    case 'define
        arg1 arg2 := getargs 2
        sym := extract arg1 String
        'set define-map (copy sym) (copy arg2)
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

    let ins-argc =
        Scope->StringMap i32
            do
                mov := 2
                int := 1
                locals;

    TK := TokenKind
    loop (idx = 0:usize)
        token start next-idx := next-token input idx

        stack-size := (countof expect-stack)
        let expected =
            if (not (empty? expect-stack))
                'pop expect-stack
            else
                compile-op current-op
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
                let argc =
                    try
                        'get ins-argc sym
                    else
                        parsing-error (.. "unknown instruction: " sym) input start

                switch argc
                case 0
                    'append expect-stack (TK.expect 'EOL)
                case 1
                    'append expect-stack (TK.expect 'EOL)
                    'append expect-stack (TK.expect-operand)
                case 2
                    'append expect-stack (TK.expect 'EOL)
                    'append expect-stack (TK.expect 'Symbol)
                    'append expect-stack (TK.expect 'Delimiter)
                    'append expect-stack (TK.expect-operand)
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
            push-arg token
            if (current-op.kind == OperationKind.Bytes)
                'append expect-stack (TK.expect 'Delimiter 'EOL 'EOF)
        case Delimiter ()
            if (current-op.kind == OperationKind.Bytes)
                'append expect-stack (TK.expect 'Integer)
        case EOL ()
        case EOF ()
        default
            parsing-error "unexpected character in input stream" input next-idx

        next-idx

bytecode := compile program1
# interpret bytecode
