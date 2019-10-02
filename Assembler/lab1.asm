org 0x100

add eax, ebx ; 01d8
and eax, ebx ; 21d8
add ebx, eax ; 01c3

xor cx, cx
xor si, si
mov cx, 5

while:
mov ax,[a+si]
mov [b+si], ax
add si, 2
dec cx
jnz while

xor cx, cx
xor si, si
mov cx, 5

_loop:
mov ax,[a+si]
mov [b+si], ax
add si, 2
loop _loop

xor si, si
mov si, [a]
mov di, [b]
movsb

ret

a dw 1, 2, 3, 4, 5
b dw 0, 0, 0, 0, 0
r dw 0

