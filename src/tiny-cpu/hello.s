.define PRINT 0x01
.define HALT 0x02
.asciiz msg "\thello, \"world\"\n"
.asciiz number_display "000000  "

jmp start
int_to_string:
    push r1
    ;; r0 = ptr into end of string
    copy acc, number_display
    add 5
    copy r0, acc
    ;; r1 = value we want to print (arg)
    ;; r2 = digit we're reducing
    ;; this is the value we'll use to mask half of r2 so we can copy one byte at a time
    copy r3, 0x2000 ;; space
    ;; while (arg > 0)
    loop0:
        ;; x = arg
        copy r2, r1
        copy acc, r2
        ;; x = x - ((x // 10) * 10)
        div 10
        mul 10
        copy r2, acc
        copy acc, r1
        sub r2
        copy r2, acc
        ;; number_display[idx] = (number_display[idx+1] & 0xFF00) | ('0' + x)
        copy acc, r3
        and 0xFF00
        copy r3, acc
        copy acc, r2
        add 48 ;; '0'
        or r3
        store acc, r0
        ;; r3 = number_display[idx]
        shl 8
        copy r3, acc
        ;; idx -= 1
        copy acc, r0
        sub 1
        copy r0, acc

        ;; arg = arg // 10
        copy acc, r1
        div 10
        copy r1, acc
        jnz loop0

        copy acc, r0
        sub 1
        copy r0, acc
        ; store r0, 0x3030

        pop r1
        pop r0
        jmp r0

subroutine:
    ;; we know the number of iterations is in r1.
    ;; This is messy, but I don't want to write a lot of register juggling code.
    call int_to_string
    copy acc, number_display
    int PRINT
    copy acc, msg
    int PRINT
    pop r0
    jmp r0

start:
    call subroutine
    ;; for i in (range 100)
    copy acc, r1
    add 1
    copy r1, acc
    sub 100
    jz finish

    jmp start

finish:
    int HALT
