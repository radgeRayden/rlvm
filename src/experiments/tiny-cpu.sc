""""tiny-cpu
    ========
    1. load assembly for a made up cpu
    2. JIT compile it
    3. run it

using import Array
using import enum
using import Map
using import String
using import struct

enum TokenKind
    Directive     : String
    Integer       : i32
    StringLiteral : String
    Symbol        : String
    Delimiter
    EOL
    EOF
    NotImplemented

enum SourceOperand
    None

enum DestinationOperand
    None

struct Instruction
    opcode : u64
    src : SourceOperand
    dst : DestinationOperand

global program1 : String
    """"#define PRINT 0x01
        .asciiz msg "\thello, \"world\"\n"

        mov acc, msg
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
            error "parser error: unexpected end of line inside string literal"

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
                    error "parser error: unknown character escape in string literal"

            'append str actual-char

            next := i + 2
            repeat (next as usize)

        'append str c

    error "parser error: incomplete string literal, lacks closing quote"

fn parse-integer (input idx positive?)
    vvv bind value
    if ((input @ idx == char"0") and (input @ (idx + 1) == char"x")) # is hex?
        first-digit := input @ (idx + 2)
        if (first-digit == 0 or (whitespace? first-digit) or first-digit == "\n")
            error "parser error: incomplete integer literal"
        fold (value = 0) for i in (range (idx + 2) (countof input))
            d := input @ i

            if ((whitespace? d) or d == "\n")
                return (positive? value -value) i

            if (not (hex-digit? d))
                error "parser error: extraneous character in integer literal"

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
                error "parser error: extraneous character in integer literal"
            (value * 10) + ((d - char"0") as i32)

    _ (positive? value -value) (countof input)

fn next-token (input idx)
    idx := consume-whitespace input idx
    if (idx == (countof input))
        return (TokenKind.EOF) idx

    c := input @ idx
    if (c == "#")
        directive next-idx := parse-symbol input (idx + 1)
        _ (TokenKind.Directive directive) next-idx
    elseif (digit? c)
        number next-idx := parse-integer input idx true
        _ (TokenKind.Integer number) next-idx
    elseif (c == "-")
        number next-idx := parse-integer input (idx + 1) false
        _ (TokenKind.Integer number) next-idx
    elseif (c == "\"")
        str next-idx := parse-string-literal input (idx + 1)
        _ (TokenKind.StringLiteral str) next-idx
    elseif (c == ",")
        _ (TokenKind.Delimiter) (idx + 1)
    elseif (letter? c)
        sym next-idx := parse-symbol input idx
        _ (TokenKind.Symbol sym) next-idx
    else
        _ (TokenKind.NotImplemented) (idx + 1)

fn compile (input)
    local RAM : (Array u8)
    'resize RAM (2 ** 16 - 1) # 64kb

    loop (idx = 0:usize)
        token next-idx := next-token input idx

        if (token == TokenKind.EOF)
            break;

        print token

        next-idx

bytecode := compile program1
# interpret bytecode
