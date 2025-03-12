using import String
using import struct
using import Map

itable :=
    sugar-quote
        load   2 {i r}
        store  2 {i r}
        copy   2 {i r}
        swap   2 {i r}
        push   1 {i r}
        pop    1 r
        add    1 {i r}
        sub    1 {i r}
        mul    1 {i r}
        div    1 {i r}
        and    1 {i r}
        or     1 {i r}
        xor    1 {i r}
        jmp    1 {i r}
        jz     1 {i r}
        jnz    1 {i r}
        shl    1 {i r}
        shr    1 {i r}
        int    1 {i r}
        call   1 {i r}

struct InstructionInfo plain
    opcode   : u8
    argc     : i32

expr :=
    spice-quote
        local opcodes : (Map String InstructionInfo)
expr :=
    fold (expr = expr) for i el in (enumerate itable)
        el as:= list
        mnemonic argc rest := decons el 2
        spice-quote
            expr
            'set opcodes (String [(mnemonic as Symbol as string)])
                InstructionInfo
                    opcode = [i]
                    argc = argc
            opcodes

spice-quote
    fn build-instruction-table ()
        expr

do
    let build-instruction-table InstructionInfo
    locals;
