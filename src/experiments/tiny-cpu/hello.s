.define PRINT 0x01
.asciiz msg "\thello, \"world\"\n"
.bytes  arr 0x20, 0x93, 0x97

jmp start
subroutine:
    copy acc, msg ;; this is a comment
    int PRINT
    pop r0
    jmp r0

start:
    copy acc, $
    add 12 ;; copy(3) + add(3) + push(3) + jmp(3)
    push acc
    jmp subroutine
    jmp start
