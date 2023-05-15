.define PRINT 0x01
.asciiz msg "\thello, \"world\"\n"
.bytes  arr 0x20, 0x93, 0x97

start:
copy acc, msg ;; this is a comment
int PRINT
jmp start
