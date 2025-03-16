.model small
.stack 100h

.data
    ; mano parasyta procedura (ja galima rasti kodo pabaigoje) vadovaujasi siais dviems buferiais
    ; pries issaukiant procedura irasome i pirma buferi savo turima reiksme, kuria norime konvertuoti i ascii simbolius
    ; procedura i antraji buferi iraso gautu simboliu ascii reiksmes
    ; po proceduros isivykdymo antrame buferyje turime skaiciu paversta i simbolius
    konvertuoti_sia_reiksme  dw 9 dup ("$")
    konvertuota_reiksme      db 5 dup ("$")

    ispejimo_pranesimas db "Zingsnio rezimo pertraukimas! $"

    ; segmentas kartu su poslinkiu nurodo adresa formatu [segmentas:poslinkis]
    segmentas    dw 5 dup ("$")
    poslinkis    dw 5 dup ("$")

    operacijos_kodas db ?

    ; mnemonika yra tai, ka matome kode (pvz.: push ax)
    mnemonika               db " push $"
    registro_pavadinimas    dw ?
    dolerio_simbolis1       db "$" ; kai bandome printint registro_pavadinimas reiksme, mums printins viska, nuo adreso ''offset operand1'' iki dolerio simbolio
    registro_reiksme        dw ?
    dolerio_simbolis2       db "$"

    ; **komandos push is BENDROS paskirties registro (REG) operacijos kodas atrodo taip: 50h + registras (jei ax, tai +0, jei cx, tai +1 ir t.t.)**
    AX_pavadinimas dw "xa" ; 50h = 01010000b
    CX_pavadinimas dw "xc" ; 51h = 01010001b
    DX_pavadinimas dw "xd" ; 52h = 01010010b
    BX_pavadinimas dw "xb" ; 53h = 01010011b
    SP_pavadinimas dw "ps" ; 54h = 01010100b
    BP_pavadiniams dw "pb" ; 55h = 01010101b
    SI_pavadinimas dw "is" ; 56h = 01010110b
    DI_pavadinimas dw "id" ; 57h = 01010111b

    ; **komandos push is SEGMENTO registru (SREG) operacijos kodai yra biski random**
    ES_pavadinimas dw "se" ; 06h = 00000110b
    CS_pavadinimas dw "sc" ; 0Eh = 00001110b
    SS_pavadinimas dw "ss" ; 16h = 00010110b
    DS_pavadinimas dw "sd" ; 1Eh = 00011110b

    ; **cia issisaugosime visu registru reiksmes, visa tai supaprastins reiksmiu spauzdinima i ekrana**
    AX_reiksme dw ?
    BX_reiksme dw ?
    CX_reiksme dw ?
    DX_reiksme dw ?
    SP_reiksme dw ?
    BP_reiksme dw ?
    SI_reiksme dw ?
    DI_reiksme dw ?
    ES_reiksme dw ?
    CS_reiksme dw ?
    SS_reiksme dw ?
    DS_reiksme dw ?

    ; **i buferius isirasome simbolius kad butu paprasta viska isvesti i ekrana**
    newline_simbolis                    db 13, 10, "$"
    tarpo_simbolis                      db " $"
    kablelio_simbolis                   db ",$"
    atidaromojo_skliaustelio_simbolis   db "[$"
    uzdaromojo_skliaustelio_simbolis    db "]$"
    kabliataskio_simbolis               db " ; $"
    dvitaskio_simbolis                  db ":$"
    lygybes_simbolis                    db " = $"

    pirmo_steko_zodzio_pranesimas   db ", pirmas steko zodis $"
    pirmas_steko_zodis              dw ? ; cia talpinsime virsutine (pries issaukiant pertraukima) steko reiksme 
    dolerio_simbolis3               db "$"

.code

main:
    mov ax, @data
    mov ds, ax

    mov ax, 0   ; nėra komandos MOV es, 0 - tai reikia daryti per kazkuri darbinį registrą (siuo atveju ax)
    mov es, ax  ; į es įsirašome 0, nes pertraukimų vektorių lentelė yra segmente, kurio pradžios adresas yra 0000

    ; Iššisaugome tikrą pertraukimo apdorojimo procedūros adresą, kad programos gale galėtume jį atstatyti
    push es:[4] 
    push es:[6]

    ; vietoje ju nurodome savo sukurtos proceduros adresa
    mov word ptr es:[4], offset pertraukimas    ; į pertraukimų vektorių lentelę įrašome pertraukimo apdorojimo procedūros poslinkį nuo kodo segmento pradžios
    mov es:[6], cs                              ; į pertraukimų vektorių lentelę įrašome pertraukimo apdorojimo procedūros segmentą

    ; ijungiame trap flag bita, jo funkcija - pries kiekvienos komandos ivykdyma nusokti i procedura, esancia adresu es:[4]
	pushf			; Išsisaugome SF reikšmę testavimo pradžioje
	pushf			; Išsisaugome SF kad galėtume ją išimti ir nustatyti TF
	pop ax			; Išimame SF reikšmę į TF
	or ax, 0100h	; Nustatome TF = 1
	push ax			; Įdedame pakoreguotą reikšmę
	popf			; Išimame pakoreguotą reikšmę į SF; Nuo čia TF=1
	nop			    ; Pirmas pertraukimas kyla ne prieš šią komandą, o po jos; todėl tiesiog vieną komandą nieko nedarome

    ; ***testavimo pradzia*** 
    ; komandos cia gali but bet kokios, visiskai nesvarbu; as tokias surasiau, kad butu patogu testuoti programa
    ; pries kiekvienos komandos igyvendinima, programa nusoka adresu es:[4] ir ivykdo musu procedura ''interrupt''

    mov ax, 1234h
	push ax
    push bx
    mov dx, 0DADh
    push dx
    mov cx, 0BEEh
    push cx
    mov sp, 0BEDh
    push sp
    push bp
    mov si, 0CBBh
    push si
    push di
    push es

    ; ***testavimo pabaiga***

    ; graziname originalias reiksmes
    popf

    pop es:[6]
    pop es:[4]

    mov ah, 4Ch
    mov al, 0
    int 21h

proc pertraukimas

    ; *** issisaugome registru reiksmes buferiuose, kad jos butu patogiai prieinamos, jeigu jas reiketu isvesti i ekrana ***
    mov [AX_reiksme], ax
	mov [BX_reiksme], bx
	mov [DX_reiksme], dx
    mov [CX_reiksme], cx
    mov [SP_reiksme], sp
    mov [BP_reiksme], bp
    mov [SI_reiksme], si
    mov [DI_reiksme], di
    mov [ES_reiksme], es
    mov [CS_reiksme], cs
    mov [SS_reiksme], ss
    mov [DS_reiksme], ds

    ; *** darbiniu registru reiksmes issisaugome steke, kad galetume naudoti juos skaiciavimams, bet gale proceduros atstatyti pradines reiksmes ***
    push ax
    push bx
	push dx
    push bp
    push es
    push ds

    ; *** cia tik tuo atveju jei sita procedura issauktu visai kita programa, pavyzdyje buvo tai ir pas mane yra ***
    mov ax, @data
    mov ds, ax

    ; *** REIKIA ZINOTI kiekviena karta kai issaukiama procedura, i steka yra issaugomas adresas, kuriame ta procedura buvo issaukta ***
    ; *** taip yra todel, kad procedurai baigus darba jai reikia zinoti kur grizti ***
    ; *** parasius komanda "iret" procedura susigrazina is steko adresa ir i ji sugrizta!!! ***

    mov bp, sp          ; sp - stack pointer (ten talpinamas adresas i siuo metu esanti pati virsutini steko elementa)
    add bp, 12          ; 12, nes tiek baitu papushinome sios proceduros pradzioje (ax, bx, dx.....)
    mov bx, [bp]        ; bx isirasome komandos, kuri issauke pertraukima, index pointer dar vadinama poslinkiu (antra adreso dalis)
    mov poslinkis, bx
    mov es, [bp + 2]    ; es isirasome kodo segmenta komandos, kuri issauke pertraukima (pirma adreso dalis)
    mov segmentas, es
    mov dx, [es:bx]     ; isirasome opcode (operation code) (masininis kodas) (operacijos kodas) (komandos kodas)
                        ; [es:bx] = [segmentas:poslinkis] = operacijos kodas
    ; uzrasa [es:bx] galima suprasti taip - ši reiksme yra lygi reiksmei, kuri tupi es kodo segmente, poslinkyje bx

    mov bx, [bp + 6]            ; butent 6 nes kiekviena kart issaukus pertraukima, yra patalpinami 6 baitai (3 zodziai) i steka:
    mov pirmas_steko_zodis, bx  ; (1. veleveliu registras, 2. kodo segmentas (CS) 3. instruction pointer (IP))

    mov operacijos_kodas, dl    ; pirma operacijos kodo dalis

    ; *** TIKRINAME AR YRA KOMANDA PUSH IS BENDROS PASKIRTIES REGISTRO ***
    mov al, operacijos_kodas
    and al, 11111000b   ; izoliuojame pirmus 5 bitus, nes jie yra vienodi visose push REG komandu operaciju koduose (zr. 23 kodo eilute)
    cmp al, 01010000b   ; jei sutampa - tai push operacija su kazkuriuo is 8 bendros paskirties registru
    je komanda_yra_push_registras

    ; *** TIKRINAME AR YRA KOMANDA PUSH IS SEGMENTU REGISTRO ***
    mov al, operacijos_kodas
    cmp al, 06h
    je es_registras_pirmas_suolis ; darome du suolius nes kodo tiek daug kad vieno suolio neuztenka
    cmp al, 0Eh
    je cs_registras_pirmas_suolis ; labai daug kodo jadfsadshj
    cmp al, 16h
    je ss_registras_pirmas_suolis
    cmp al, 1Eh
    je ds_registras_pirmas_suolis
    
    jmp baigiam_komandos_tikrinima

    ; *** zr. 23 kodo eilute, pagal tai dabar mes bandom atpazint, kuri push komandos variacija buvo atlikta ***
komanda_yra_push_registras:
    cmp dl, 01010000b
    je ax_registras     ; jei reiksmes lygios, sokam i butent tam registrui skirta kodo fragmenta kuriame surasome to registro info i buferius
    cmp dl, 01010001b   ; kuriuos paskui spausdinsime
    je cx_registras
    cmp dl, 01010010b
    je dx_registras
    cmp dl, 01010011b
    je bx_registras
    cmp dl, 01010100b
    je sp_registras
    cmp dl, 01010101b
    je bp_registras
    cmp dl, 01010110b 
    je si_registras_pirmas_suolis
    jmp di_registras

nespausdinti:
    jmp baigiam_komandos_tikrinima ; kadangi jmp gali nusokti toliau naudojam jmp

es_registras_pirmas_suolis:
    jmp cs_registras

cs_registras_pirmas_suolis:
    jmp cs_registras

ss_registras_pirmas_suolis:
    jmp ss_registras

ds_registras_pirmas_suolis:
    jmp ds_registras

si_registras_pirmas_suolis:
    jmp si_registras

    ; *** registro pavadinima isirasom i bufferi "registro_pavadinimas", o registro reiksme - i bufferi registro_reiksme ***
    ; *** taip darom del to, nes isvesime siuos 2 bufferius i ekrana ***
ax_registras:
    mov dx, [AX_pavadinimas]
    mov [registro_pavadinimas], dx
    mov dx, [AX_reiksme]
    mov [registro_reiksme], dx
    jmp spausdinti

cx_registras:
    mov dx, [CX_pavadinimas]
    mov [registro_pavadinimas], dx
    mov dx, [CX_reiksme]
    mov [registro_reiksme], dx
    jmp spausdinti

dx_registras:
    mov dx, [DX_pavadinimas]
    mov [registro_pavadinimas], dx
    mov dx, [DX_reiksme]
    mov [registro_reiksme], dx
    jmp spausdinti

bx_registras:
    mov dx, [BX_pavadinimas]
    mov [registro_pavadinimas], dx
    mov dx, [BX_reiksme]
    mov [registro_reiksme], dx
    jmp spausdinti

sp_registras:
    mov dx, [SP_pavadinimas]
    mov [registro_pavadinimas], dx
    mov dx, [SP_reiksme]
    mov [registro_reiksme], dx
    jmp spausdinti

bp_registras:
    mov dx, [BP_pavadiniams]
    mov [registro_pavadinimas], dx
    mov dx, [BP_reiksme]
    mov [registro_reiksme], dx
    jmp spausdinti

si_registras:
    mov dx, [SI_pavadinimas]
    mov [registro_pavadinimas], dx
    mov dx, [SI_reiksme]
    mov [registro_reiksme], dx
    jmp spausdinti

di_registras:
    mov dx, [DI_pavadinimas]
    mov [registro_pavadinimas], dx
    mov dx, [DI_reiksme]
    mov [registro_reiksme], dx
    jmp spausdinti

es_registras:
    mov dx, [ES_pavadinimas]
    mov [registro_pavadinimas], dx
    mov dx, [ES_reiksme]
    mov [registro_reiksme], dx
    jmp spausdinti

cs_registras:
    mov dx, [CS_pavadinimas]
    mov [registro_pavadinimas], dx
    mov dx, [CS_reiksme]
    mov [registro_reiksme], dx
    jmp spausdinti

ss_registras:
    mov dx, [SS_pavadinimas]
    mov [registro_pavadinimas], dx
    mov dx, [SS_reiksme]
    mov [registro_reiksme], dx
    jmp spausdinti

ds_registras:
    mov dx, [DS_pavadinimas]
    mov [registro_pavadinimas], dx
    mov dx, [DS_reiksme]
    mov [registro_reiksme], dx
    jmp spausdinti

spausdinti:
    ; ***ispejimas, jog bus ivykdyta mov komanda***
    mov ah, 09h
    lea dx, ispejimo_pranesimas
    int 21h

    ; ***isvedame push komandos adreso pirmaja dali***
    mov ax, segmentas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    ; ***isvedame dvitaskio simboli***
    lea dx, dvitaskio_simbolis
    int 21h

    ; ***isvedame push komandos adreso atraja dali***
    mov ax, poslinkis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    ; ***isvedame tarpelio simboli***
    lea dx, tarpo_simbolis
    int 21h

    ; ***isvedame operacijos koda***
    xor ax, ax
    mov al, operacijos_kodas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2] ; +2 tam, kad neirasineti nuliu, nes sios komandos operacijos koda sudaro tik 1 baitas
    int 21h                           ; jei spausdintume be +2 turetume pvz.: 0050, bet siuo atveju su poslinkiu: 50

    ; ***isvedame push komandos uzrasa***
    lea dx, mnemonika
    int 21h

    ; ***isvedame registro pavadinima***
    lea dx, registro_pavadinimas
    int 21h

    ; ***isvedame kabliataskio simboli su tarpais is abieju pusiu***
    lea dx, kabliataskio_simbolis
    int 21h

    ; ***isvedame registro pavadinima***
    lea dx, registro_pavadinimas
    int 21h

    ; ***isvedame lygu simboli***
    lea dx, lygybes_simbolis
	int 21h    

    ; ***isvedame registro reiksme***
    xor ax, ax
	mov ax, registro_reiksme
	mov konvertuoti_sia_reiksme, ax
	call konvertuoti_i_ascii_simbolius
	mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    ; ***isvedame virsutines steko reiksmes zinute***
    lea dx, pirmo_steko_zodzio_pranesimas
    int 21h

    ; ***isvedame virsutine steko reiksme***
    mov ax, pirmas_steko_zodis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    ; ***isvedame newline***
    lea dx, newline_simbolis
    int 21h

    jmp baigiam_komandos_tikrinima

baigiam_komandos_tikrinima:
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ; mane valaitis prigavo ir as nezinojau skirtumo tarp "ret" ir "iret". iret - interrupt return (grizimas is pertraukimo)
    ; del detalaus paaiskinimo siulyciau pasiskaityt, galima ChatGPT paklaust pagal tokia uzklausa:
    ; "ret vs iret assembly x86 dos"
    iret

pertraukimas endp

; *** sios proceduros esme yra paversti sesioliktaini skaiciu i sesioliktaini skaiciu ASCII simboliais, kad galetume printinti
; pries issaukiant sia procedura visada isirasome i "konvertuoti_sia_reiksme" bufferi skaiciu, kuri norime konvertuoti i simbolius
; procedura, baigusi darba, i bufferi "konvertuota_reiksme" iraso ASCII simbolius, kuriuos veliau printiname
proc konvertuoti_i_ascii_simbolius
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov ax, konvertuoti_sia_reiksme
    mov bx, 16
    mov cx, 4

konvertavimo_pradzia:
    xor dx, dx
    div bx
    cmp dl, 10
    jb skaicius
    add dl, 55

skaitmuo_baigtas_konvertuoti:
    push dx
    cmp ax, 0
    loop konvertavimo_pradzia
    jmp konvertavimas_baigtas
    
skaicius:
    add dl, '0'
    jmp skaitmuo_baigtas_konvertuoti
    
konvertavimas_baigtas:
    mov cx, 4
    lea di, konvertuota_reiksme
    
irasyti_i_bufferi:
    pop dx
    mov byte ptr [di], dl
    inc di
    loop irasyti_i_bufferi

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

konvertuoti_i_ascii_simbolius endp

END main
