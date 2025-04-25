.model small

    bufferSize equ 255

.stack 100h

.data
	
	helpMessage1 db 'This program can compare the contents of two files!', 10, 13
    helpMessage2 db 'Enter the names of 2 files when launching the executable file.', 10, 13, '$'

    firstFileName  db 20 dup (0)
    secondFileName db 20 dup (0)
    resultFileName db 'rez.txt', 0

    firstFileBuffer     db bufferSize dup ('0'), '$'
    secondFileBuffer    db bufferSize dup ('0'), '$'

    secondFileHandle    dw ?
    firstFileHandle     dw ?
    resultFileHandle    dw ?

    differentByte       db 10 dup (0), 10, 13

.code

main:
	mov	ax, @data
	mov	ds, ax

	mov bx, 81h
    lea si, firstFileName

    xor cx, cx

input_analysis:	
	mov ax, [es:bx]	        ;pirmus du parametrų baitus nuskaitome į ax
	inc bx
	
	cmp al, 13 
	je continue
	
	cmp ax, "?/"
    je help

    cmp bx, 82h
    je input_analysis

    cmp al, 20h
	je continue
        
    mov byte ptr [si], al   ;rašom į failo vardo bufferį
    inc si
    jmp input_analysis 
    
continue:
    ;mov byte ptr [si], '$'
    lea si, secondFileName
    inc cx
    cmp cx, 2
    je open_first_file_for_reading
    jmp input_analysis

help:
	mov	ah, 09
	mov	dx, offset helpMessage1
	int	21h
    jmp the_end

; ***prasideda darbas su failais***
open_first_file_for_reading:
	mov	ah, 3Dh				            ;21h pertraukimo failo atidarymo funkcijos numeris
	mov	al, 00				            ;00 - failas atidaromas skaitymui
	mov	dx, offset firstFileName		;vieta, kur nurodomas failo pavadinimas, pasibaigiantis nuliniu simboliu
	int	21h				                ;failas atidaromas skaitymui
	jc errorWhenOpeningRead1    	    ;jei atidarant failą skaitymui įvyksta klaida, nustatomas carry flag
    mov di, offset firstFileHandle
	mov	word ptr [di], ax			    ;atmintyje išsisaugom pirmojo duomenų failo deskriptoriaus numerį

write_from_first_file_to_buffer:
    mov	bx, firstFileHandle	            ;į bx irašom duomenų failo deskriptoriaus numerį
    lea si, firstFileBuffer
	call readToBuffer			        ;iškvieciame skaitymo iš failo procedurą
	push ax  				            ;ax įrašoma, kiek baitų buvo nuskaityta

open_second_file_for_reading:
	mov	ah, 3Dh				            ;21h pertraukimo failo atidarymo funkcijos numeris
	mov	al, 00				            ;00 - failas atidaromas skaitymui
	mov	dx, offset secondFileName		;vieta, kur nurodomas failo pavadinimas, pasibaigiantis nuliniu simboliu
	int	21h				                ;failas atidaromas skaitymui
	jc errorWhenOpeningRead2   	        ;jei atidarant failą skaitymui įvyksta klaida, nustatomas carry flag
    mov di, offset secondFileHandle
	mov	word ptr [di], ax			    ;atmintyje išsisaugom antrojo duomenų failo deskriptoriaus numerį

write_from_second_file_to_buffer:
    mov	bx, secondFileHandle	        ;į bx irašom duomenų failo deskriptoriaus numerį
    lea si, secondFileBuffer
	call readToBuffer			        ;iškvieciame skaitymo iš failo procedurą
	push ax	     			            ;ax įrašoma, kiek baitų buvo nuskaityta

create_and_open_result_file_for_writing:
	mov	ah, 3Ch				            ;21h pertraukimo failo sukūrimo funkcijos numeris
	mov	cx, 0				            ;kuriamo failo atributai
	mov	dx, offset resultFileName       ;vieta, kur nurodomas failo pavadinimas, pasibaigiantis nuliniu simboliu
	int	21h				                ;sukuriamas failas; jei failas jau egzistuoja, visa jo informacija ištrinama
	jc errorWhenOpeningWrite		    ;jei kuriant failą rašymui įvyksta klaida, nustatomas carry flag
    mov di, offset resultFileHandle
	mov	word ptr [di], ax			    ;atmintyje išsisaugom rezultato failo deskriptoriaus numerį

find_max_bytes_read:
    xor dx, dx
    xor cx, cx
    pop ax
    pop bx
    cmp ax, bx
    ja ax_symbols
    mov cl, bl
    jmp compare_bytes

errorWhenOpeningRead1:
    jmp help
errorWhenOpeningRead2:
    mov	ah, 09
	mov	dx, offset helpMessage1
	int	21h
    jmp close_first_file
errorWhenOpeningWrite:
    mov	ah, 09
	mov	dx, offset helpMessage1
	int	21h
    jmp close_first_file

ax_symbols:
    mov cl, al

compare_bytes:
    lea si, firstFileBuffer
    add si, dx
    mov al, [si]
    lea si, secondFileBuffer
    add si, dx
    mov bl, [si]
    inc dx
    cmp al, bl
    jne write
    loop compare_bytes

write:
    mov di, offset differentByte
    push ax
    push bx
    push cx
    mov ax, dx
    push dx
    xor bx, bx
    xor cx, cx
    xor dx, dx

    mov bx, 10

convert_to_string:
    xor dx, dx                              ; kiekvieno ciklo pradzioje nunuliname dx registra
    div bx                                  ; ax - sveikoji dalis, dx - liekana
    add dl, '0'                             ; paverciam liekana i ASCII simboli
    push dx                                 ; talpinam reiksme stack'e
    inc cx                                  ; skaiciuojam skaitmenu kieki
    cmp ax, 0                               ; patikriname, ar musu skaicius nelygus 0
    jne convert_to_string                   ; jei jis dar nelygus 0, kartojame cikla

    jmp pop_digits

pop_digits:
    pop dx                                  ; is stack'o susigraziname skaitmeni
    mov byte ptr [di], dl                   ; italpiname skaitmenis simboli adresu di
    inc di
    loop pop_digits                         ; kartojame ši procesa tiek kartų, kiek skaitmenų suskaičiavome cx registre

    mov byte ptr [di], ' '
    jmp store_bytes

; *** turime bufferyje differentBytePos irasyta pozicija, kurioje yra skirtingi baitai!! ***

proc readToBuffer
;į bx paduodamas failo deskriptoriaus numeris
;į ax bus grąžinta, kiek simbolių nuskaityta
	push cx
	push dx
	
	mov	ah, 3Fh			            ;21h pertraukimo duomenų nuskaitymo funkcijos numeris
	mov	cx, bufferSize		        ;cx - kiek baitų reikia nuskaityti iš failo
	mov	dx, offset si               ;vieta, į kurią įrašoma nuskaityta informacija
	int	21h			                ;skaitymas iš failo
	jc errorReading 		        ;jei skaitant iš failo įvyksta klaida, nustatomas carry flag

readToBufferEnd:
	pop	dx
	pop	cx
	ret

errorReading:
	mov ax, 0			            ;Pažymime registre ax, kad nebuvo nuskaityta nei vieno simbolio
	jmp	readToBufferEnd

readToBuffer endp

store_bytes: 
    pop dx
    pop cx
    pop bx
    pop ax

    inc di
    mov byte ptr [di], al
    inc di
    mov byte ptr [di], ' '
    inc di
    mov byte ptr [di], bl
    inc di

    push ax
    push bx
    push cx
    push dx

    mov bx, resultFileHandle
    mov	cx, 12			                ;cx - kiek baitų reikia įrašyti
    mov ah, 40h                         ;21h pertraukimo duomenu irašymo funkcijos numeris
    mov dx, offset differentByte        ;pradedame rašyti nuo baito pozicijos
    int 21h

    pop dx
    pop cx
    pop bx
    pop ax

    cmp dx, cx
    jae close_result_file

    jmp compare_bytes

close_result_file:
	mov	ah, 3Eh				            ;21h pertraukimo failo uždarymo funkcijos numeris
	mov	bx, resultFileHandle	        ;į bx įrašom rezultato failo deskriptoriaus numerį
	int	21h				                ;failo uždarymas
	jc errorClosingResult		        ;jei uždarant failą įvyksta klaida, nustatomas carry flag
	
close_first_file:
	mov	ah, 3Eh		            		;21h pertraukimo failo uždarymo funkcijos numeris
	mov	bx, firstFileHandle		        ;į bx įrašom duomenų failo deskriptoriaus numerį
	int	21h				                ;failo uždarymas
	jc errorClosingRead1		        ;jei uždarant failą įvyksta klaida, nustatomas carry flag

close_second_file:
    mov	ah, 3Eh		            		;21h pertraukimo failo uždarymo funkcijos numeris
	mov	bx, secondFileHandle	    	;į bx įrašom duomenų failo deskriptoriaus numerį
	int	21h				                ;failo uždarymas
	jc errorClosingRead2		        ;jei uždarant failą įvyksta klaida, nustatomas carry flag

the_end:
    mov	ah, 4Ch
	mov	al, 00h
	int	21h

errorClosingResult: 
    jmp help
errorClosingRead1:
    mov	ah, 09
	mov	dx, offset helpMessage1
	int	21h
    jmp close_second_file
errorClosingRead2:
    mov	ah, 09
	mov	dx, offset helpMessage1
	int	21h

    jmp the_end

end main
