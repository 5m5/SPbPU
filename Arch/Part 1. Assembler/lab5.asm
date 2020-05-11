org 100h
finit
 
    fld dword [x]
    fld dword [y]
    fadd
    fld dword [z]
    fmul
    fst dword [t]
    fstcw word[k]
    not word[k]
    or word[k],20h
    not word[k]
    fldcw word[k]
    fld1
    fld dword[y]

    fdiv
    fstp dword[z]
ret
x dd 1.02
y dd 0.04
z dd 0.01
t dd 0.0
k dw 0.0
