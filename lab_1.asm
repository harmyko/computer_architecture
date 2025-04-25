; Kodo autorius Ugnius Teišerskis, PS 1k. 1gr. 2pgr.
; Ši programa įvestą aštuntainį skaičių užrašys dešimtainiu pavidalu https://klevas.mif.vu.lt/~julius/2011Rud/KompArch/Uzd1.html
; Laikoma, jog skaičiai (dešimtainiu pavidalu) priklauso intervalui [0; 65535] (aštuntainiu [0; 177777])

.model small 
.stack 100h

.data
    messageStart  db 'This program will convert an octal number [0; 177777] to its decimal form. ', 13, 10, '$'
    messageInput  db 'Please enter an octal counting system number: $'
    messageOutput db 'The octal number has been converted to decimal: $'
    messageError  db 'An error occured! Restarting program...', 13, 10, '$'
    newLine       db 13, 10, '$'
    bufferInput   db 255, ?, 255 dup ('$')
    bufferOutput  db 5 dup ('$')

.code

main:
    mov ax, @data
    mov ds, ax
    
    mov dx, offset messageStart
    mov ah, 09h
    int 21h

input: ; ****nuskaitome duomenis****
    mov dx, offset messageInput
    mov ah, 09h
    int 21h

    mov dx, offset bufferInput
    mov ah, 0Ah
    int 21h

    mov dx, offset newLine
    mov ah, 09h
    int 21h

    xor ax, ax
    xor bx, bx
    xor cx, cx
    xor dx, dx
    mov bl, [bufferInput + 1] ; ivestu skaiciu kieki imetame i bx registra

    cmp bl, 6
    ja error

    cmp bl, 6 ; jei skaicius sudarytas is 6 skaitmenu, yra rizika, jog jo reiksme, pavertus i desimtaine sistema, netilps i viena registra
    je validRange
    jmp assign

validRange: ; ****isitikiname, jog pavertus skaiciu i desimtaine sistema, jis tilps i viena registra****
    mov si, offset [bufferInput + 2] ; imetame sesiazenklio astuntainio skaiciaus pirmaji skaitmeni i dx registra
    mov dl, byte ptr[si]
    cmp dl, '1'                      ; pirmas skaitmuo negali buti didesnis uz 1
    ja error
    
    mov si, offset [bufferInput + 3] ; antras skaitmuo
    mov dl, byte ptr[si]
    cmp dl, '7'                      ; negali buti didesnis uz 7
    ja error

    mov si, offset [bufferInput + 4] ; trecias skaitmuo
    mov dl, byte ptr[si]
    cmp dl, '7'                      ; negali buti didesnis uz 7
    ja error

    mov si, offset [bufferInput + 5] ; ketvirtas skaitmuo
    mov dl, byte ptr[si]
    cmp dl, '7'                      ; negali buti didesnis uz 7
    ja error

    mov si, offset [bufferInput + 6] ; penktas skaitmuo
    mov dl, byte ptr[si]
    cmp dl, '7'                      ; negali buti didesnis uz 7
    ja error

    mov si, offset [bufferInput + 7] ; sestas skaitmuo
    mov dl, byte ptr[si]
    cmp dl, '7'                      ; negali buti didesnis uz 7
    ja error

    xor dx, dx

assign: ; ****po viena skaitmeni imetame i bx registra****
    cmp bl, 0 
    je calculationDone

    mov si, offset bufferInput
    add si, 1
    add si, bx
    xor ax, ax
    mov al, byte ptr[si] 
    dec bl
    
    mov dh, dl
    inc dl

validDigit: ; ****patikriname, ar ivestas simbolis yra astuntainio skaiciaus skaitmuo****
    cmp al, '0' 
    jb error

    cmp al, '7'
    ja error

    jmp conversion

error: 
    mov dx, offset messageError 
    mov ah, 09h
    int 21h

    jmp input  ; duodame vartotojui kartoti duomenu ivedima

conversion:
    sub al, '0' ; is ASCII skaitmens paverciame i skaiciu

calculation: ; ****suskaiciuojame kiekvieno skaitmens, priklausomai nuo jo pozicijos, desimtaine reiksme****
    cmp dh, 0
    je addition

    push dx
    push cx     

    mov cx, 8 
    mul cx  

    pop cx  
    pop dx

    sub dh, 1
    jmp calculation

addition:
    add cx, ax
    jmp assign

calculationDone:
    mov ax, cx ; ax registrui priskiriame desimtainio skaiciaus reiksme
    xor cx, cx ; nunuliname cx registra
    mov bx, 10 ; dalinsime desimtaini skaiciu is 10
    mov di, offset bufferOutput

convertToString:
    xor dx, dx          ; kiekvieno ciklo pradzioje nunuliname dx registra
    div bx              ; ax - sveikoji dalis, dx - liekana
    add dl, '0'         ; paverciam liekana i ASCII simboli
    push dx             ; talpinam reiksme stack'e
    inc cx              ; skaiciuojam skaitmenu kieki
    cmp ax, 0           ; patikriname, ar musu skaicius nelygus 0
    jne convertToString ; jei jis dar nelygus 0, kartojame cikla

stringEndSymbol:
    mov di, offset bufferOutput       ; di suteikiame adresa, zyminti paskutini eilutes simboli
    add di, cx
    mov byte ptr [di], '$'            ; baita, adresu di, paverciame i '$' ir taip pazymime eilutes pabaiga
    mov di, offset bufferOutput

popDigits:
    pop dx         ; is stack'o susigraziname skaitmeni
    mov [di], dl   ; italpiname skaitmenis simboli adresu di
    inc di
    loop popDigits ; kartojame si procesa tiek kartu, kiek skaitmenu suskaiciavome cx registre

print:
    mov dx, offset messageOutput
    mov ah, 09h
    int 21h

    mov dx, offset bufferOutput  ; pradedame spauzdinti nuo pirmojo skaitmens adreso
    mov ah, 09h
    int 21h

    jmp finish

finish:
    mov ah, 3Ch
    int 21h

END main