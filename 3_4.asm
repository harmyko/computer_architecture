.model small
.stack 100h

.data
    valueToConvert  dw 9 dup ("$")
    convertedValue  db 5 dup ("$")

    opcode db ?
    modrm  db ?

    warning     db "Zingsnio rezimo pertraukimas! $"
    notpopmsg   db "Komanda nera pop!!!", 13, 10, "$"
    stackMsg    db ", pirmas steko zodis $"

    topStackValue dw ?
    endStackStr   db "$"

    address1    dw 5 dup ("$")
    address12   db ":$"
    address2    dw 5 dup ("$")

    mnemonic    db " pop $"

    ; **komandos pop i registra opcode atrodo taip: 58h + registras (jei ax, tai +0, jei cx, tai +1 ir t.t.)**
    ax1 dw "xa" ; 58h = 0101 1000
    cx1 dw "xc" ; 59h = 0101 1001
    dx1 dw "xd" ; 5Ah = 0101 1010
    bx1 dw "xb" ; 5Bh = 0101 1011
    sp1 dw "ps" ; 5Ch = 0101 1100
    bp1 dw "pb" ; 5Dh = 0101 1101
    si1 dw "is" ; 5Eh = 0101 1110
    di1 dw "id" ; 5Fh = 0101 1111

    operand1  dw ?
    end1      db "$"

    newline db 13, 10, "$"
    space   db " $"
    comma   db ",$"
    open    db "[$"
    close   db "]$"
    semicolon db " ; $"
    equal   db " = $"

    value1 dw ?
    end2   db "$"

    axvalue dw ?
    bxvalue dw ?
    cxvalue dw ?
    dxvalue dw ?
    spvalue dw ?
    bpvalue dw ?
    sivalue dw ?
    divalue dw ?

.code

main:
    mov ax, @data
    mov ds, ax

    mov ax, 0
    mov es, ax

    ; issisaugome originalias reiksmes
    push es:[4]
    push es:[6]

    ; vietoje ju nurodome savo sukurtos proceduros adresa
    mov word ptr es:[4], offset interrupt
    mov es:[6], cs 

    ; pakeiciame flagus taip, jog pries kiekviena komanda isivykdytu musu nurodyta procedura
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
    add bx, 9Ch
    push ax
    add bx, 01h
    pop bx
    mov bx, 12h
	push bx
    pop cx


; ***testavimo pabaiga***

    ; graziname originalias reiksmes
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
    mov bx, [bp]        ; bx isirasome komandos, kuri issauke pertraukima, index pointer (antra adreso dalis)
    mov address2, bx
    mov es, [bp + 2]    ; es isirasome kodo segmenta komandos, kuri issauke pertraukima (pirma adreso dalis)
    mov address1, es
    mov dx, [es:bx]     ; isirasome opcode (operation code)

    mov bx, [bp + 6]    ; isirasome reiksme, kuri buvo steko virsuje pries proceduros issaukima (t.y. reiksme kuri bus irasyta i registra po pop komandos)
    mov topStackValue, bx

    mov al, dl          
    mov dl, dh
    mov dh, al
    mov opcode, dh      ; pirma opcode dalis
    mov modrm, dl       ; antra opcode dalis, nurodo vieta atmintyje, i kuria popinti

    and al, 11111000b   ; izoliuojame pirmus 5 bitus, nes jie yra vienodi visose pop reg komandu opcode
    cmp al, 01011000b   ; jei sutampa - tai pop operacija su kazkuriuo is 8 pagr. registru
    je reg
    jmp notpop

reg:
    cmp dh, 01011000b
    je axreg
    cmp dh, 01011001b
    je cxreg
    cmp dh, 01011010b
    je dxreg
    cmp dh, 01011011b
    je bxreg
    cmp dh, 01011100b
    je spreg
    cmp dh, 01011101b
    je bpreg
    cmp dh, 01011110b
    jne notpop
    jmp sireg
    cmp dh, 01011111b
    jne notpop
    jmp direg

notpop:
    mov ah, 09h
    mov dx, offset notpopmsg
    int 21h

    jmp return

axreg:
    mov word ptr dx, [ax1]
    mov word ptr [operand1], dx
    mov word ptr dx, [axvalue]
    mov word ptr [value1], dx
    jmp print

cxreg:
    mov word ptr dx, [cx1]
    mov word ptr [operand1], dx
    mov word ptr dx, [cxvalue]
    mov word ptr [value1], dx
    jmp print

dxreg:
    mov word ptr dx, [dx1]
    mov word ptr [operand1], dx
    mov word ptr dx, [dxvalue]
    mov word ptr [value1], dx
    jmp print

bxreg:
    mov word ptr dx, [bx1]
    mov word ptr [operand1], dx
    mov word ptr dx, [bxvalue]
    mov word ptr [value1], dx
    jmp print

spreg:
    mov word ptr dx, [sp1]
    mov word ptr [operand1], dx
    mov word ptr dx, [spvalue]
    mov word ptr [value1], dx
    jmp print

bpreg:
    mov word ptr dx, [bp1]
    mov word ptr [operand1], dx
    mov word ptr dx, [bpvalue]
    mov word ptr [value1], dx
    jmp print

sireg:
    mov word ptr dx, [si1]
    mov word ptr [operand1], dx
    mov word ptr dx, [sivalue]
    mov word ptr [value1], dx
    jmp print

direg:
    mov word ptr dx, [di1]
    mov word ptr [operand1], dx
    mov word ptr dx, [divalue]
    mov word ptr [value1], dx
    jmp print

print:
    ; ***ispejimas, jog bus ivykdyta mov komanda***
    mov ah, 09h
    mov dx, offset warning
    int 21h

    ; ***isvedame pop komandos adreso pirmaja dali***
    mov ax, address1
    mov valueToConvert, ax
    call convert
    mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    ; ***isvedame dvitaskio simboli***
    mov dx, offset address12
    int 21h

    ; ***isvedame pop komandos adreso atraja dali***
    mov ax, address2
    mov valueToConvert, ax
    call convert
    mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    ; ***isvedame tarpelio simboli***
    mov dx, offset space
    int 21h

    ; ***isvedame opcode***
    xor ax, ax
    mov al, opcode
    mov valueToConvert, ax
    call convert
    mov ah, 09h
    mov dx, offset [convertedValue + 2] ; +2 tam, kad neirasineti nuliu, nes sios komandos opcode sudaro tik 1 baitas
    int 21h

    ; ***isvedame pop komandos uzrasa***
    mov dx, offset mnemonic
    int 21h

    ; ***isvedame registro pavadinima***
    mov dx, offset operand1
    int 21h

    ; ***isvedame kabliataskio simboli su tarpais is abieju pusiu***
    mov dx, offset semicolon
    int 21h

    ; ***isvedame registro pavadinima***
    mov dx, offset operand1
    int 21h

    ; ***isvedame lygu simboli***
    mov dx, offset equal
	int 21h    

    ; ***isvedame registro reiksme***
    xor ax, ax
	mov ax, value1
	mov valueToConvert, ax
	call convert
	mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    ; ***isvedame virsutines steko reiksmes zinute***
    mov dx, offset stackMsg
    int 21h

    ; ***isvedame virsutine steko reiksme***
    mov ax, topStackValue
    mov valueToConvert, ax
    call convert
    mov ah, 09h
    mov dx, offset convertedValue
    int 21h

    ; ***isvedame newline***
    mov dx, offset newline
    int 21h

    jmp return

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

END main
