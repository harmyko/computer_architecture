.model small
.stack 100h

.data
    valueToConvert  dw 9 dup ("$")
    convertedValue  db 5 dup ("$")
    modrm           db ?

    warning     db "Zingsnio rezimo pertraukimas! $"
    ne_msg      db "Si programa negali apdoroti tokios mov komandos!", 13, 10, "$"

    address1    dw 5 dup ("$")
    address12   db ":$"
    address2    dw 5 dup ("$")

    opcode      dw ?

    mnemonic    db "  mov $"

    ax1 dw "xa"
    al1 dw "la"
    ah1 dw "ha"
    bx1 dw "xb"
    bl1 dw "lb"
    bh1 dw "hb"
    cx1 dw "xc"
    cl1 dw "lc"
    ch1 dw "hc"
    dx1 dw "xd"
    dl1 dw "ld"
    dh1 dw "hd"
    bp1 dw "pb"
    es1 dw "se"
    ds1 dw "sd"
    sp1 dw "ps"
    si1 dw "is"
    di1 dw "id"

    operand1 dw ?
    end1     db "$"
    operand2 dw ?
    end2     db "$"

    newline db 13, 10, "$"
    space   db " $"
    comma   db ",$"
    open    db "[$"
    close   db "]$"
    semicolon db " ; $"
    equal   db " = $"

    value1 dw ?
    end3   db "$"
    value2 dw ?
    end4   db "$"

    value11 db ?
    end5    db "$"
    value22 db ?
    end6    db "$"

    axvalue dw ?
    bxvalue dw ?
    cxvalue dw ?
    dxvalue dw ?
    spvalue dw ?
    bpvalue dw ?
    sivalue dw ?
    divalue dw ?

    alvalue db ?
    ahvalue db ?
    blvalue db ?
    bhvalue db ?
    clvalue db ?
    chvalue db ?
    dlvalue db ?
    dhvalue db ?

.code

main:
    mov ax, @data
    mov ds, ax

    mov ax, 0
    mov es, ax

    push es:[4]
    push es:[6]

    mov word ptr es:[4], offset interrupt
    mov es:[6], cs 

    pushf
    pushf
    pop ax
    or ax, 0100h
    push ax
    popf
    nop

; ***testavimo pradzia***

	xor ax, ax
	xor bx, bx
	add ax, 68Fh
    add bx, 9Ah
	inc bx
    mov ax, bx
	mov dx, ax
    inc bx
    mov al, dh
    mov si, bp
    mov cl, bl


; ***trestavimo pabaiga***

    popf

    pop es:[6]
    pop es:[4]

    mov ah, 4Ch
    mov al, 0
    int 21h

proc interrupt
    mov word ptr [axvalue], ax
	mov word ptr [bxvalue], bx
	mov word ptr [dxvalue], dx
    mov word ptr [cxvalue], cx
    mov word ptr [spvalue], sp
    mov word ptr [bpvalue], bp
    mov word ptr [sivalue], si
    mov word ptr [divalue], di 

    mov byte ptr [alvalue], al
    mov byte ptr [ahvalue], ah
    mov byte ptr [blvalue], bl
    mov byte ptr [bhvalue], bh
    mov byte ptr [clvalue], cl
    mov byte ptr [chvalue], ch
    mov byte ptr [dlvalue], dl
    mov byte ptr [dxvalue], dh

    push ax
    push bx
	push dx
    push bp
    push es
    push ds

    mov ax, @data
    mov ds, ax

    mov bp, sp          ; sp - stack pointer
    add bp, 12          ; 12, nes tiek baitu papushinome sios proceduros pradzioje
    mov bx, [bp]        ; bx turime komandos, kuri issauke pertraukima, index pointer (antra adreso dalis)
    mov address2, bx
    mov es, [bp + 2]    ; es turime kodo segmenta komandos, kuri issauke pertraukima (pirma adreso dalis)
    mov address1, es
    mov dx, [es:bx]     ; isirasome opcode (operation code)
    mov al, dl
    mov dl, dh
    mov dh, al
    mov opcode, dx
    mov modrm, dl

    cmp dh, 8Bh
    je is_word
    cmp dh, 8Ah
    je is_byte

    jmp return

is_word:
    jmp print_16bit
is_byte:
    jmp print_8bit

print_8bit:
    ; ***ispejimas, jog bus ivykdyta mov komanda***
    mov ah, 09h
    mov dx, offset warning
    int 21h

    ; ***isvedame mov komandos adresa***
    mov ax, address1
    mov valueToConvert, ax
    call convert
    mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    mov dx, offset address12
    int 21h

    mov ax, address2
    mov valueToConvert, ax
    call convert
    mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    ; ***isvedame op koda***
    mov dx, offset space
    int 21h

    mov ax, opcode
    mov valueToConvert, ax
    call convert
    mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    ; ***ieskome operandu***
    mov dx, offset mnemonic

analyse_al1:
    ; ***al bituose 1-0 italpiname mod bitus
    mov al, modrm      ; nukopijuojame mod r/m baita i al
    and al, 11000000b  ; izoliuojame reikalinkus bitus

    cmp al, 00000000b
    jne nextmod11
    call mod00 ; pvz.: mov ax, [bx] (be postumio)
    jmp return
nextmod11:
    cmp al, 01000000b
    jne nextmod22
    call mod11 ; pvz.: mov ax, [bx+8] (8 bitu postumis)
    jmp return
nextmod22:
    cmp al, 10000000b
    jne nextmod33
    jmp return
    call mod22 ; pvz.: mov ax, [bx+si] (16 bitu postumis)
    jmp return
nextmod33:
    cmp al, 11000000b
    je continuee
    jmp return
continuee:
    call mod33 ; pvz.: mov ax, bx (is registro i registra)
    jmp print_mod33

print_mod33:
    mov ah, 09h
    mov dx, offset mnemonic
    int 21h

    mov ah, 09h
    mov dx, offset operand1 
    int 21h

    mov dx, offset comma
    int 21h

    mov dx, offset space
    int 21h

    mov dx, offset operand2
    int 21h

    mov dx, offset semicolon
    int 21h

    mov dx, offset operand1
    int 21h

    mov dx, offset equal
    int 21h

    xor ax, ax
	mov al, value11
	mov valueToConvert, ax
	call convert
	mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    mov dx, offset comma
    int 21h

	mov dx, offset space
	int 21h

    mov dx, offset operand2
    int 21h

    mov dx, offset equal
    int 21h

    xor ax, ax
	mov al, value22
	mov valueToConvert, ax
	call convert
	mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    mov dx, offset newline
    int 21h

    jmp return
    
print_16bit:
    ; ***ispejimas, jog bus ivykdyta mov komanda***
    mov ah, 09h
    mov dx, offset warning
    int 21h

    ; ***isvedame mov komandos adresa***
    mov ax, address1
    mov valueToConvert, ax
    call convert
    mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    mov dx, offset address12
    int 21h

    mov ax, address2
    mov valueToConvert, ax
    call convert
    mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    ; ***isvedame op koda***
    mov dx, offset space
    int 21h

    mov ax, opcode
    mov valueToConvert, ax
    call convert
    mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    ; ***ieskome operandu***
    mov dx, offset mnemonic

analyse_al:
    ; ***al bituose 1-0 italpiname mod bitus
    mov al, modrm      ; nukopijuojame mod r/m baita i al
    and al, 11000000b  ; izoliuojame reikalinkus bitus

    cmp al, 00000000b
    jne nextmod1
    call mod0 ; pvz.: mov ax, [bx] (be postumio)
    jmp return
nextmod1:
    cmp al, 01000000b
    jne nextmod2
    call mod1 ; pvz.: mov ax, [bx+8] (8 bitu postumis)
    jmp return
nextmod2:
    cmp al, 10000000b
    jne nextmod3
    call mod2 ; pvz.: mov ax, [bx+si] (16 bitu postumis)
    jmp return
nextmod3:
    cmp al, 11000000b
    jne return
    call mod3 ; pvz.: mov ax, bx (is registro i registra)
    jmp print_mod3

print_mod3:
    mov ah, 09h
    mov dx, offset mnemonic
    int 21h

    mov ah, 09h
    mov dx, offset operand1 
    int 21h

    mov dx, offset comma
    int 21h

    mov dx, offset space
    int 21h

    mov dx, offset operand2
    int 21h

    mov dx, offset semicolon
    int 21h

    mov dx, offset operand1
    int 21h

    mov dx, offset equal
    int 21h

	mov ax, value1
	mov valueToConvert, ax
	call convert
	mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    mov dx, offset comma
    int 21h

	mov dx, offset space
	int 21h

    mov dx, offset operand2
    int 21h

    mov dx, offset equal
    int 21h

	mov ax, value2
	mov valueToConvert, ax
	call convert
	mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    mov dx, offset newline
    int 21h


return:
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
    iret

interrupt endp

proc convert
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov ax, valueToConvert
    mov bx, 16
    mov cx, 4

convert_to_string:
    xor dx, dx
    div bx
    cmp dl, 10
    jb number
    add dl, 55

digit_converted:
    push dx
    cmp ax, 0
    loop convert_to_string
    jmp conversion_finished
    
number:
    add dl, '0'
    jmp digit_converted
    
conversion_finished:
    mov cx, 4
    mov di, offset convertedValue
write_to_buffer:
    pop dx
    mov byte ptr [di], dl
    inc di
    loop write_to_buffer

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

convert endp

proc mod0
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    call message

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
mod0 endp

proc mod00
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    call message

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
mod00 endp

proc mod1
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    call message

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
mod1 endp

proc mod11
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    call message

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
mod11 endp

proc mod2
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    call message

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
mod2 endp

proc mod22
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    call message

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
mod22 endp

proc mod3
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov al, modrm
    and al, 00111000b     ; izoliuojame bitus 5-3

    cmp al, 00000000b
    jne next_reg1
    mov word ptr dx, [ax1]
    mov word ptr [operand1], dx
    mov word ptr dx, [axvalue]
    mov word ptr [value1], dx
    jmp operand_next
next_reg1:
    cmp al, 00001000b
    jne next_reg2
    mov word ptr dx, [cx1]
    mov word ptr [operand1], dx
    mov word ptr dx, [cxvalue]
    mov word ptr [value1], dx
    jmp operand_next
next_reg2:
    cmp al, 00010000b
    jne next_reg3
    mov word ptr dx, [dx1]
    mov word ptr [operand1], dx
    mov word ptr dx, [dxvalue]
    mov word ptr [value1], dx
    jmp operand_next
next_reg3:
    cmp al, 00011000b
    jne next_reg4
    mov word ptr dx, [bx1]
    mov word ptr [operand1], dx
    mov word ptr dx, [bxvalue]
    mov word ptr [value1], dx
    jmp operand_next
next_reg4:
    cmp al, 00100000b
    jne next_reg5
    mov word ptr dx, [sp1]
    mov word ptr [operand1], dx
    mov word ptr dx, [spvalue]
    mov word ptr [value2], dx
    jmp operand_next
next_reg5:
    cmp al, 00101000b
    jne next_reg6
    mov word ptr dx, [bp1]
    mov word ptr [operand1], dx
    mov word ptr dx, [bpvalue]
    mov word ptr [value2], dx
    jmp operand_next
next_reg6:
    cmp al, 00110000b
    jne next_reg7
    mov word ptr dx, [si1]
    mov word ptr [operand1], dx
    mov word ptr dx, [sivalue]
    mov word ptr [value2], dx
    jmp operand_next
next_reg7:
    mov word ptr dx, [di1]
    mov word ptr [operand1], dx
    mov word ptr dx, [divalue]
    mov word ptr [value2], dx

operand_next:
    ; ***al bituose 2-0 italpiname r/m bitus
    mov al, modrm
    and al, 00000111b

    cmp al, 00000000b
    jne next_reg11
    mov word ptr dx, [ax1]
    mov word ptr [operand2], dx
    mov word ptr dx, [axvalue]
    mov word ptr [value2], dx
    jmp over
next_reg11:
    cmp al, 00000001b
    jne next_reg22
    mov word ptr dx, [cx1]
    mov word ptr [operand2], dx
    mov word ptr dx, [cxvalue]
    mov word ptr [value2], dx
    jmp over
next_reg22:
    cmp al, 00000010b
    jne next_reg33
    mov word ptr dx, [dx1]
    mov word ptr [operand2], dx
    mov word ptr dx, [dxvalue]
    mov word ptr [value2], dx
    jmp over
next_reg33:
    cmp al, 00000011b
    jne next_reg44
    mov word ptr dx, [bx1]
    mov word ptr [operand2], dx
    mov word ptr dx, [bxvalue]
    mov word ptr [value2], dx
    jmp over
next_reg44:
    cmp al, 00000100b
    jne next_reg55
    mov word ptr dx, [sp1]
    mov word ptr [operand2], dx
    mov word ptr dx, [spvalue]
    mov word ptr [value2], dx
    jmp over
next_reg55:
    cmp al, 00000101b
    jne next_reg66
    mov word ptr dx, [bp1]
    mov word ptr [operand2], dx
    mov word ptr dx, [bpvalue]
    mov word ptr [value2], dx
    jmp over
next_reg66:
    cmp al, 00000110b
    jne next_reg77
    mov word ptr dx, [si1]
    mov word ptr [operand2], dx
    mov word ptr dx, [sivalue]
    mov word ptr [value2], dx
    jmp over
next_reg77:
    mov word ptr dx, [di1]
    mov word ptr [operand2], dx
    mov word ptr dx, [divalue]
    mov word ptr [value2], dx

over:
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
    ret
mod3 endp

proc mod33
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    xor dx, dx
    ; ***al bituose 2-0 italpiname reg bitus
    mov al, modrm
    and al, 00111000b     ; izoliuojame bitus 5-3

    cmp al, 00000000b
    jne next_reg1b
    mov word ptr dx, [al1]
    mov word ptr [operand1], dx
    xor dx, dx
    mov byte ptr dl, [alvalue]
    mov byte ptr [value1], dl
    jmp operand_nextb
next_reg1b:
    cmp al, 00001000b
    jne next_reg2b
    mov word ptr dx, [cl1]
    mov word ptr [operand1], dx
    xor dx, dx
    mov byte ptr dl, [clvalue]
    mov byte ptr [value1], dl
    jmp operand_nextb
next_reg2b:
    cmp al, 00010000b
    jne next_reg3b
    mov word ptr dx, [dl1]
    mov word ptr [operand1], dx
    xor dx, dx
    mov byte ptr dl, [dlvalue]
    mov byte ptr [value1], dl
    jmp operand_nextb
next_reg3b:
    cmp al, 00011000b
    jne next_reg4b
    mov word ptr dx, [bl1]
    mov word ptr [operand1], dx
    xor dx, dx
    mov byte ptr dl, [blvalue]
    mov byte ptr [value1], dl
    jmp operand_nextb
next_reg4b:
    cmp al, 00100000b
    jne next_reg5b
    mov word ptr dx, [ah1]
    mov word ptr [operand1], dx
    xor dx, dx
    mov byte ptr dl, [ahvalue]
    mov byte ptr [value1], dl
    jmp operand_nextb
next_reg5b:
    cmp al, 00101000b
    jne next_reg6b
    mov word ptr dx, [ch1]
    mov word ptr [operand1], dx
    xor dx, dx
    mov byte ptr dl, [chvalue]
    mov byte ptr [value1], dl
    jmp operand_nextb
next_reg6b:
    cmp al, 00110000b
    jne next_reg7b
    mov word ptr dx, [dh1]
    mov word ptr [operand1], dx
    xor dx, dx
    mov byte ptr dl, [dhvalue]
    mov byte ptr [value1], dl
    jmp operand_nextb
next_reg7b:
    mov word ptr dx, [bh1]
    mov word ptr [operand1], dx
    xor dx, dx
    mov byte ptr dl, [bhvalue]
    mov byte ptr [value1], dl

operand_nextb:
    ; ***al bituose 2-0 italpiname r/m bitus
    mov al, modrm
    and al, 00000111b

    cmp al, 00000000b
    jne next_reg11b
    mov word ptr dx, [al1]
    mov word ptr [operand2], dx
    xor dx, dx
    mov byte ptr dl, [alvalue]
    mov byte ptr [value2], dl
    jmp overb
next_reg11b:
    cmp al, 00000001b
    jne next_reg22b
    mov word ptr dx, [cl1]
    mov word ptr [operand2], dx
    xor dx, dx
    mov byte ptr dl, [clvalue]
    mov byte ptr [value2], dl
    jmp overb
next_reg22b:
    cmp al, 00000010b
    jne next_reg33b
    mov word ptr dx, [dl1]
    mov word ptr [operand2], dx
    xor dx, dx
    mov byte ptr dl, [dlvalue]
    mov byte ptr [value2], dl
    jmp overb
next_reg33b:
    cmp al, 00000011b
    jne next_reg44b
    mov word ptr dx, [bl1]
    mov word ptr [operand2], dx
    xor dx, dx
    mov byte ptr dl, [blvalue]
    mov byte ptr [value2], dl
    jmp overb
next_reg44b:
    cmp al, 00000100b
    jne next_reg55b
    mov word ptr dx, [ah1]
    mov word ptr [operand2], dx
    xor dx, dx
    mov byte ptr dl, [ahvalue]
    mov byte ptr [value1], dl
    jmp overb
next_reg55b:
    cmp al, 00000101b
    jne next_reg66b
    mov word ptr dx, [ch1]
    mov word ptr [operand2], dx
    xor dx, dx
    mov byte ptr dl, [chvalue]
    mov byte ptr [value1], dl
    jmp overb
next_reg66b:
    cmp al, 00000110b
    jne next_reg77b
    mov word ptr dx, [dh1]
    mov word ptr [operand2], dx
    xor dx, dx
    mov byte ptr dl, [dhvalue]
    mov byte ptr [value1], dl
    jmp overb
next_reg77b:
    mov word ptr dx, [bh1]
    mov word ptr [operand2], dx
    xor dx, dx
    mov byte ptr dl, [bhvalue]
    mov byte ptr [value1], dl

overb:
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
    ret
mod33 endp

proc message
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov ah, 09h
    mov dx, offset ne_msg
    int 21h

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
message endp

END main
