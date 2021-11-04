escape:
    clr v0
    clr f0
    mov #$1000, r0
    mov #256, r2
1:  mov (r0), r1
    bit #$0100, r1
    beq 3
    not r1
    bit #$0013, r1
    bne 2
    mov 1(r0), v1
    add 2(r0), v1
    mov 3(r0), f1
    mul f1, v1
    add f1, f0
    add v1, v0
2:  add #16, r0
    dec r2
    bne 1
    


