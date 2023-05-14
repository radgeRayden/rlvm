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
    Preprocessor  : String
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

        mov acc, msg ;; this is a comment
        int PRINT

        ; "this shouldn't count as a string
        " and this shouldn't ;count as a comment"

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

            if ((whitespace? d) or d == "\n")
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
    if (c == "#")
        pre next-idx := parse-symbol input (idx + 1)
        _ (TokenKind.Preprocessor pre) idx next-idx
    elseif (c == ".")
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
        _ (TokenKind.Symbol sym) idx next-idx
    elseif (c == "\n")
        _ (TokenKind.EOL) idx (idx + 1)
    else
        _ (TokenKind.NotImplemented) idx idx

fn compile (input)
    loop (idx = 0:usize)
        token start next-idx := next-token input idx

        if (token == TokenKind.EOF)
            break;

        dispatch token
        case Preprocessor (pre)
        case Directive (directive)
        case Integer (value)
        case StringLiteral (str)
        case Symbol (sym)
        case Delimiter ()
        case EOL ()
        case EOF ()
        default
            parsing-error "unexpected character in input stream" input next-idx

        next-idx

bytecode := compile program1
# interpret bytecode
