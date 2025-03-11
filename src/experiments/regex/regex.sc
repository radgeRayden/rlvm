using import Array enum Option print radl.IO.FileStream radl.strfmt String struct slice

sugar not-implemented ()
    qq
        do
            local v = false
            if v
                assert v "not implemented"
run-stage;

exit := (extern 'exit (function void i32))
inline exit (status)
    exit status
    unreachable;

u256 := (integer 256 false)

let character-class-digit = 
    fold (class = (u256)) for c in (range (char "0") (char "9"))
        class | ((u256 1) << c)

let character-class-whitespace =
    fold (class = (u256)) for c in " \t"
        class | ((u256 1) << c)

run-stage;

fn build-character-set (input)
    fold (set = (u256)) for c in input
        set | ((u256 1) << c)

enum ErrorKind plain
    TruncatedPattern
    UnbalancedBracket
    UnknownSpecifier

struct RegexMatch
    line : i32
    column : i32
    text : String

enum AtomKind
    LineStart
    LineEnd
    Any
    Literal : char
    CharacterSet : u256
    CharacterSetNegation : u256

enum RepetitionCount
    Optional
    ZeroOrMore
    ZeroOrMoreLazy
    OneOrMore
    OneOrMoreLazy
    ExplicitCount : i32 i32

struct Atom
    kind : AtomKind
    repetition : RepetitionCount = (RepetitionCount.ExplicitCount 1 1)

fn parse-repetition (subpattern)
    not-implemented;
    if (empty? subpattern)
        return 0:usize (RepetitionCount.ExplicitCount 1 1)

    match (subpattern @ 0)
    case "+"
    case "*"
    case "{"
    default
        ()

    _
        0:usize
        RepetitionCount.ExplicitCount 1 1

fn parse-character-class (subpattern)
    if (empty? subpattern)
        raise ErrorKind.TruncatedPattern

    match (subpattern @ 0)
    case "\\"
        AtomKind.Literal "\\"
    case "d"
        AtomKind.CharacterSet character-class-digit
    case "s"
        AtomKind.CharacterSet character-class-whitespace
    case "S"
        AtomKind.CharacterSetNegation character-class-whitespace
    default
        raise ErrorKind.UnknownSpecifier

fn parse-character-set (subpattern)
    not-implemented;
    _ 0:usize (u256 0) false

fn parse-pattern (pattern)
    local result : (Array Atom)

    loop (idx = 0:usize)
        if (idx == ((countof pattern) - 1))
            break;

        inline unexpected-end? ()
            if ((idx + 1) == (countof pattern))
                raise ErrorKind.TruncatedPattern

        c := pattern @ idx
        match c
        case "\\"
            unexpected-end?;
            kind := parse-character-class (rslice (view pattern) (idx + 1))
            consumed repetition := parse-repetition (rslice (view pattern) (idx + 2))
            'append result (Atom kind repetition)
            idx + 2 + consumed
        case "^"
            'append result (Atom (AtomKind.LineStart) (RepetitionCount.ExplicitCount 1))
            idx + 1
        case "$"
            'append result (Atom (AtomKind.LineEnd) (RepetitionCount.ExplicitCount 1))
            idx + 1
        case "."
            consumed repetition := parse-repetition (rslice (view pattern) (idx + 1))
            'append result (Atom (AtomKind.Any) repetition)
            idx + 1 + consumed
        case "["
            unexpected-end?;
            consumed set negation? := parse-character-set (rslice (view pattern) idx)
            let kind =
                if negation?
                    AtomKind.CharacterSetNegation set
                else
                    AtomKind.CharacterSet set

            'append result (Atom kind)
            idx + consumed
        default
            consumed repetition := parse-repetition (rslice (view pattern) (idx + 1))
            'append result (Atom (AtomKind.Literal (pattern @ idx)) repetition)
            idx + 1 + consumed
    result

fn string-match (text pattern)
    pattern := parse-pattern pattern
    local matches : (Array RegexMatch)
    if ((empty? pattern) or (empty? text))
        return matches

    loop (idx matching? match-start pattern-idx repetition-count = 0 false -1 0 0)
        c := text @ idx
        atom := pattern @ pattern-idx

        vvv bind success?
        dispatch atom.kind
        case LineStart ()
            idx == 0
        case LineEnd ()
            (idx + 1) == (countof text)
        case Literal (l)
            c == l
        case CharacterSet (set)
            ((u256 1) << c) & set
        case CharacterSetNegation (set)
            not (((u256 1) << c) & set)
        default
            assert false "Unknown atom kind"

        dispatch atom.repetition
        case Optional ()
            # because it is optional, we always match
            match-start := ? matching? match-start idx
            matching? := true
            pattern-idx := pattern-idx + 1
            repeat (idx + 1) matching? match-start pattern-idx repetition-count
        case ZeroOrMore ()
            false
        case ZeroOrMoreLazy ()
            false
        case OneOrMore ()
            false
        case OneOrMoreLazy ()
            false
        default
            assert false "Unknown RepetitionCount"

        if matching?
            if success?
                repeat (idx + 1) true match-start (pattern-idx + 1) repetition-count
            else
                repeat (match-start + 1) false -1 0 repetition-count
        else
            if success?
                repeat (idx + 1) true idx (pattern-idx + 1) repetition-count
            else
                repeat (idx + 1) false -1 0 repetition-count

fn main (argc argv)
    if (argc != 3)
        print S"USAGE: regex PATTERN FILE"
        exit 1

    arg := (s) -> ('from-rawstring String s)
    pattern filename := (arg (argv @ 1)), (arg (argv @ 2))

    let file =
        try (FileStream filename FileMode.Read)
        else
            print f"Could not open file: ${filename}"
            exit 1

    try
        for line in ('lines file)
            matches := string-match line pattern
            for m in matches
                print f"MATCH: ${m.line}:${m.column}: ${m.text}"
    else 
        print "Unexpected error while reading input"
        exit 1

    0
