.global _main
.align 4

_main:
    stp x29, x30, [SP, #-16]!

    sub sp, sp, #16


    adrp x0, template@PAGE
    add x0, x0, template@PAGEOFF
    adrp x11, num@PAGE
    add x11, x11, num@PAGEOFF
    str x11, [SP, #-16]!
    bl _scanf
    add sp, sp, #16
    adrp x10, num@PAGE
    add x10, x10, num@PAGEOFF
    ldr x19, [x10]
    str x19, [sp, #0]!
    sub sp, sp, #0

    adrp x0, template@PAGE
    add x0, x0, template@PAGEOFF
    adrp x11, num@PAGE
    add x11, x11, num@PAGEOFF
    str x11, [SP, #-16]!
    bl _scanf
    add sp, sp, #16
    adrp x10, num@PAGE
    add x10, x10, num@PAGEOFF
    ldr x19, [x10]
    str x19, [sp, #8]!
    sub sp, sp, #8
    ldr x20, [sp, #0]!
    sub sp, sp, #0
    ldr x21, [sp, #8]!
    sub sp, sp, #8
    add x19, x20, x21

    ADRP X0, message@PAGE
    ADD X0, X0, message@PAGEOFF
    STR x19, [SP, #-16]!
    BL  _printf
    ADD SP, SP, #16

    add sp, sp, #16


    mov X0, #0
    LDP X29, X30, [SP], #16
    ret

.data
.balign 4
message:    .asciz "Result: %lld\n"
.balign 4
num:    .quad 0
.balign 4
template:   .asciz "%lld"
