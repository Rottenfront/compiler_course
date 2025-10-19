.text
.global _print_bool
_print_bool:
    stp fp, lr, [sp, #-16]!
    sub sp, sp, #16
    cmp w0, #0
    adrp x0, fmt_false@PAGE
    add  x0, x0, fmt_false@PAGEOFF
    b.eq 1f
    adrp x0, fmt_true@PAGE
    add  x0, x0, fmt_true@PAGEOFF
1:
    bl _printf
    add sp, sp, #16
    ldp fp, lr, [sp], #16
    ret

.text
.global _print_int
_print_int:
    stp fp, lr, [sp, #-16]!
    mov x19, x0
    ADRP X0, fmt_int@PAGE
    ADD X0, X0, fmt_int@PAGEOFF
    STR x19, [SP, #-16]!
    BL  _printf
    ADD SP, SP, #16
    ldp fp, lr, [sp], #16
    ret

.text
.global _read
_read:
    stp fp, lr, [sp, #-16]!
    adrp x0, fmt_read@PAGE
    add x0, x0, fmt_read@PAGEOFF
    adrp x11, num@PAGE
    add x11, x11, num@PAGEOFF
    str x11, [SP, #-16]!
    bl _scanf
    add sp, sp, #16
    adrp x11, num@PAGE
    add x11, x11, num@PAGEOFF
    ldr x0, [x11]
    ldp fp, lr, [sp], #16
    ret


.data

.balign 4
fmt_true:
    .asciz "true\n"
.balign 4
fmt_false:
    .asciz "false\n"
.balign 4
fmt_read:
        .asciz "%lld"
.balign 4
fmt_int:
    .asciz "%lld\n"
.balign 4
num:    .quad 0

.text
.global _main
_main:
    stp fp, lr, [sp, #-16]!
start_main:
    sub sp, sp, #16
    bl _read
    str x0, [sp, #8]
    ldr x0, [sp, #8]
    bl _square
    str x0, [sp, #16]
    ldr x0, [sp, #16]
    bl _print_int
    mov x0, x0
    add sp, sp, #16
    ldp fp, lr, [sp], #16
    ret

.text
.global _square
_square:
    stp fp, lr, [sp, #-16]!
start_square:
    sub sp, sp, #16
    str x0, [sp, #8]
    ldr x16, [sp, #8]
    ldr x21, [sp, #8]
    mul x0, x16, x21
    add sp, sp, #16
    ldp fp, lr, [sp], #16
    ret
