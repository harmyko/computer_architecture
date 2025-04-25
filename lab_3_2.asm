; 7-toji trecios uzduoties salyga reikalauja mus disasemblinti ir isanalizuoti zemiau esancia komanda:
;                       |
;                       |
;                       V
; 1000 1111 mod 000 r/m [poslinkis] – POP registras/atmintis
;
; dabar as apacioj parasiau daug zodziu kurie paaiskina kiekviena dali is auksciau parasytos komandos
;
; xxxx xxxx <--- sis pirmasis baitas yra vadinamas operacijos kodu, komandos kodu, operation code, opcode zodziu vadink kaip nori
; 1000 1111 <--- musu programoje jie turi atrodyti butent sitaip!!! hexu si reiksme yra lygi 8Fh, kiekvieno pertraukimo pradzioje lyginsime
; ar musu apdorojamos komandos opcode yra lygus 8Fh, jei ne, vadinasi mums sios komandos apdoroti nereikia, jei taip - dirbame su modrm baitu
;
;           mod reg r/m <--- tai yra vadinamasis modrm baitas, kuris nusako daug idomiu dalyku  
;           mod <--- tai yra modrm baito pirmoji dalis, vadinama mod, ja sudaro DU pirmieji modrm baito bitai
;           mod galimos reiksmes: 00, 01, 10, 11. paaiskinsiu kiekviena trumpai atskirai bet ir is lentutes esancios zemiau galima tas pacias isvadas daryti
;           00 - popinsime reiksme i tam tikra adresa (cia zaisime su bx, di (destination index), si (source index) ir bp (base pointer) registrais
;                atkreipiame demesi kad galima prie adreso prideti indekso registrus nes jie swag like that pvz.: pop [bx+si]
;           01 - cia irgi zaidziame su tais paciais registrais, tik dar galime prideti 8 bitu didzio poslinki [-128; 127]
;                pvz.: pop [bx+si+10h]
;           10 - cia lygiai tas pats kas 01, tik tiek, kad db zaidziame su 16 bitu poslinkiais [-32768; 32767]
;                pvz.: pop [bx+si+1234h]
;           11 - mum neaktualus variantas, cia popintume ne i atminti o i registrus
;
;              reg <--- tai yra TRYS bitai, kurie nurodo, i kuri registra popinsime, taciau musu salyga nereikalauja tikrinti i kuri registra popiname
;              000 <--- musu trys reg bitai privales visada atrodyti taip, nes mes tikriname tuos atvejus kai popiname i atminti, o ne registra
;
;                   r\m <--- tai yra trys bitai, vadinami "er em" bitais xd
;                            visos ju imanomos kombinacijos yra surasytos pirmame lenteles stulpelyje
;                            jie basically pasako kokias reiksmes, priklausomai dar ir nuo mod bitu, imsime, kad apskaiciuoti adresa,
;                            i kuri popinsime reiksmes
;
;                     [poslinkis] - sioje vietoje bus parasyta poslinkio reiksme, jei mod yra arba 01, arba 10
;                                   kitu atveju sioje vietoje jau prasides kitos komandos aprasymas

; *********** LENTUTE KURIA VADOVAUDAMIESI NAGRINEJAME MASININI KODA IR IS TO ATPAZISTAME KOMANDAS ***********

; sita lentute isikeliau i kodo failo del patogumo https://klevas.mif.vu.lt/~linas1/KompArch/KomKodaiViso.pdf 
; DEMESIO! stulepis w=0 mums yra neaktualus, nes w bitas yra paskutinis (astuntas) opcode bitas (opcode formatas atrodo taip - 0000 00dw)
; - jeigu w bitas yra ijungtas (w=1) - tai mes dirbame su 2 baitu reiksme aka visu pilnu registru pvz.: ax, bx, cx...
; - jeigu w bitas yra isjungas (w=2) - tai mes dirbame su 1 baito reiksme aka puse registro pvz.: ah, bl, al...
; taciau pop komandos veikia tik su pilno registro reiksmemis, ir negana to mums jau yra salygoje duota, jog mums reikia dirbti su tokiomis
; komandomis, kuriu opcode atrodo taip: 1000 1111 ir cia matome jog paskutinis bitas yra ijungtas (w=1) todel galime ignoruoti stulpeli w=0
; vis delto sita visada ignoruojam asdglhasjlgd nes reg 000 tai neziurim i tauos stulpelius kur virsuj reg pamirsk ka katik skaitei apie tuos w
;           V          V
;
;        |        reg         |--------------------------------------------------|
;        |  r/m, kai mod=11   |   r/m, kai mod=00      |   r/m, kai mod = 01, 10 |
;        |     w=0       w=1  |   Efektyvus adresas:   |   Efektyvus adresas:    |
;  | ----|--------------------|------------------------|-------------------------|
;  | 000 |      AL   |   AX   |          BX+SI         |     BX+SI+poslinkis     |
;  | 001 |      CL   |   CX   |          BX+DI         |     BX+DI+poslinkis     |
;  | 010 |      DL   |   DX   |          BP+SI         |     BP+SI+poslinkis     |
;  | 011 |      BL   |   BX   |          BP+DI         |     BP+DI+poslinkis     |
;  | 100 |      AH   |   SP   |          SI            |     SI+poslinkis        |  
;  | 101 |      CH   |   BP   |          DI            |     DI+poslinkis        |
;  | 110 |      DH   |   SI   |    Tiesioginis adresas |     BP+poslinkis        |
;  | 111 |      BH   |   DI   |          BX            |     BX+poslinkis        |

; Tiesioginis adresas - tai kai popinam tsg tiesiai i nurodyta adresa skaiciais pvz.: pop [1234h], pop [0DADh], pop [0CBBh] <--- VIS DELTO CIA ILLEGAL
; KOMANDOS TAIP DARYT NEGALIMA AS NEZINAU KAS CIA YRA IR NENORIU ZINOT KJASDFHIUOASHGIUOFABIPG
; tiesioginis adresas yra tiesiog bp registras i thinks 

; kaip matom dirbsim tik su 4 registrais (bx, bp, si, di) taip yra todel, nes kiti registrai yra illegal komandos
; pvz pop [ax], pop [cx]. jei bandom assemblint faila su tokiom komandom gaunam tasm errora

.model small
.stack 100h

.data
    ; mano parasyta procedura (ja galima rasti kodo pabaigoje) vadovaujasi siais dviems buferiais
    ; pries issaukiant procedura irasome i pirma buferi savo turima reiksme, kuria norime konvertuoti i ascii simbolius
    ; procedura i antraji buferi iraso gautu simboliu ascii reiksmes
    ; po proceduros isivykdymo antrame buferyje turime skaiciu paversta i simbolius
    konvertuoti_sia_reiksme  dw 9 dup ("$")
    konvertuota_reiksme      db 5 dup ("$")

    ispejimo_pranesimas db "Zingsninio rezimo pertraukimas! $"

    ; segmentas kartu su poslinkiu nurodo adresa formatu [segmentas:poslinkis]
    segmentas    dw ?
    poslinkis    dw ?

    operacijos_kodas db ?
    modrm_baitas     db ?

    poslinkis_8bit  db ? ; jei poslinkis sudarytas is 1 baito spauzdinsime tik sita
    poslinkis_16bit db ? ; jei poslinksi sudarytas is 2 baitu spauzdinsime abu

    ; mnemonika yra tai, ka matome kode (pvz.: pop [bx])
    mnemonika               db "  pop $"

    registro_pavadinimas1   dw 3 dup ("$")
    registro_reiksme1       dw 2 dup (?)
    registro_pavadinimas2   dw 3 dup ("$")
    registro_reiksme2       dw 2 dup (?)

    registro_pavadinimas dw ?
    registro_reiksme     dw ?

    BX_pavadinimas dw "xb"
    BP_pavadiniams dw "pb"
    SI_pavadinimas dw "is"
    DI_pavadinimas dw "id"

    ; **cia issisaugosime visu registru (kurie veikia adresuojant) reiksmes, visa tai supaprastins reiksmiu spauzdinima i ekrana**
    BX_reiksme dw ?
    BP_reiksme dw ?
    SI_reiksme dw ?
    DI_reiksme dw ?

    ; **i buferius isirasome simbolius kad butu paprasta viska isvesti i ekrana**
    newline_simbolis                    db 13, 10, "$"
    tarpo_simbolis                      db " $"
    kablelio_simbolis                   db ",$"
    atidaromojo_skliaustelio_simbolis   db "[$"
    uzdaromojo_skliaustelio_simbolis    db "]$"
    kabliataskio_simbolis               db " ; $"
    dvitaskio_simbolis                  db ":$"
    lygybes_simbolis                    db " = $"
    pliuso_simbolis                     db " + $"

    pirmo_steko_zodzio_pranesimas   db ", pirmas steko zodis $"
    pirmas_steko_zodis              dw ? ; cia talpinsime virsutine (pries issaukiant pertraukima) steko reiksme

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

    mov bx, 123Ah
    push bx
    push 12h
    pop [bx]
    pop [bx + si]
    push 0DADh
    pop [bx + di]
    mov si, 1234h
    pop [si]
    mov si, 12h
  ;  pop [di]
    push 0DABh
    pop [bx + si + 01h]
  ;  pop [bx + di + 01h]
    pop [si + 6h]
  ;  pop [di + 01h]
  ;  pop [bx + 01h]
    push 0BEDh
  ;  pop [bx + si + 1234h]
    push 0001h
    pop [bx + di + 1234h]
  ;  pop [si + 1234h]
  ;  pop [di + 1234h]
    pop [bx + 1234h]



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
	mov BX_reiksme, bx
    mov BP_reiksme, bp
    mov SI_reiksme, si
    mov DI_reiksme, di

    ; *** darbiniu registru reiksmes issisaugome steke, kad galetume naudoti juos skaiciavimams, bet gale proceduros atstatyti pradines reiksmes ***
    push ax
    push bx
	push dx
    push bp
    push es
    push ds

    ; *** cia tik tuo atveju jei sita procedura issauktu visai kita programa, pavyzdyje buvo tai ir pas mane yra idk ***
    mov ax, @data
    mov ds, ax

    ; ** KAD GERIAU SUPRASTI SITO BLOKO ESME ZR. bx_registras procedura (641 eilute)
    lea di, registro_pavadinimas1
    mov registro_pavadinimas, di
    lea di, registro_reiksme1
    mov registro_reiksme, di

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
    
    mov operacijos_kodas, dl    ; i buferi isirasome operacijos koda
    mov modrm_baitas, dh        ; i buferi isiraosme modrm baito reiksme

    ; jei komanda turi poslinki, iskart galimas reiksmes issisaugome
    add bx, 2 ; poslinkis yra du sekantys baitai iskart po opcode ir modrm baito
    mov dx, [es:bx]
    mov byte ptr poslinkis_8bit, dl
    mov byte ptr poslinkis_16bit, dh

    mov bx, [bp + 6]            ; butent 6 nes kiekviena kart issaukus pertraukima, yra patalpinami 6 baitai (3 zodziai) i steka:
    mov pirmas_steko_zodis, bx  ; (1. veleveliu registras, 2. kodo segmentas (CS) 3. instruction pointer (IP))

    ; *** TIKRINAME AR YRA KOMANDA POP su adresais ten asdkljgl ***
    mov dl, operacijos_kodas
    cmp dl, 8Fh
    je ieskom_registru
    jmp baigiam_komandos_tikrinima

ieskom_registru:
    mov dh, modrm_baitas
    and dh, 00000111b

    cmp dh, 00000000b
    je mod_bx_si_rastas

    cmp dh, 00000001b
    je mod_bx_di_rastas

    cmp dh, 00000010b
    je mod_bp_si_rastas

    cmp dh, 00000011b
    je mod_bp_di_rastas

    cmp dh, 00000100b
    je mod_si_rastas

    cmp dh, 00000101b
    je mod_di_rastas

    cmp dh, 00000110b
    je mod_bp_rastas

    cmp dh, 00000111b
    je mod_bx_rastas

mod_bx_si_rastas:
    call mod_bx_si
    jmp baigiam_komandos_tikrinima

mod_bx_di_rastas:
    call mod_bx_di
    jmp baigiam_komandos_tikrinima

mod_bp_si_rastas:
    call mod_bp_si
    jmp baigiam_komandos_tikrinima

mod_bp_di_rastas:
    call mod_bp_di
    jmp baigiam_komandos_tikrinima

mod_si_rastas:
    call mod_si
    jmp baigiam_komandos_tikrinima

mod_di_rastas:
    call mod_di
    jmp baigiam_komandos_tikrinima

mod_bp_rastas:
    call mod_bp
    jmp baigiam_komandos_tikrinima

mod_bx_rastas:
    call mod_bx
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






proc mod_bx_si
    push ax
    push bx
    push dx
    push bp
    push es
    push ds
    
    call bx_registras
    call si_registras

    mov al, modrm_baitas
    and al, 11000000b
    cmp al, 01000000b
    jne bx_si_neturi_8bit_poslinkio
    call spausdinti_du_registrus_su_8_bitu_poslinkiu
    jmp bx_si_viskas

    bx_si_neturi_8bit_poslinkio:
    cmp al, 10000000b
    jne bx_si_neturi_16bit_poslinkio
    call spausdinti_du_registrus_su_16_bitu_poslinkiu
    jmp bx_si_viskas

    bx_si_neturi_16bit_poslinkio:
    call spausdinti_du_registrus

    bx_si_viskas:
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

mod_bx_si endp

proc mod_bx_di
    push ax
    push bx
    push dx
    push bp
    push es
    push ds
    
    call bx_registras
    call di_registras

    mov al, modrm_baitas
    and al, 11000000b
    cmp al, 01000000b
    jne bx_di_neturi_8bit_poslinkio
    call spausdinti_du_registrus_su_8_bitu_poslinkiu
    jmp bx_di_viskas

    bx_di_neturi_8bit_poslinkio:
    cmp al, 10000000b
    jne bx_di_neturi_16bit_poslinkio
    call spausdinti_du_registrus_su_16_bitu_poslinkiu
    jmp bx_di_viskas

    bx_di_neturi_16bit_poslinkio:
    call spausdinti_du_registrus

    bx_di_viskas:
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

mod_bx_di endp

proc mod_bp_si
    push ax
    push bx
    push dx
    push bp
    push es
    push ds
    
    call bp_registras
    call si_registras

    mov al, modrm_baitas
    and al, 11000000b
    cmp al, 01000000b
    jne bp_si_neturi_8bit_poslinkio
    call spausdinti_du_registrus_su_8_bitu_poslinkiu
    jmp bp_si_viskas

    bp_si_neturi_8bit_poslinkio:
    cmp al, 10000000b
    jne bp_si_neturi_16bit_poslinkio
    call spausdinti_du_registrus_su_16_bitu_poslinkiu
    jmp bp_si_viskas

    bp_si_neturi_16bit_poslinkio:
    call spausdinti_du_registrus

    bp_si_viskas:
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

mod_bp_si endp

proc mod_bp_di
    push ax
    push bx
    push dx
    push bp
    push es
    push ds
    
    call bp_registras
    call di_registras

    mov al, modrm_baitas
    and al, 11000000b
    cmp al, 01000000b
    jne bp_di_neturi_8bit_poslinkio
    call spausdinti_du_registrus_su_8_bitu_poslinkiu
    jmp bp_di_viskas

    bp_di_neturi_8bit_poslinkio:
    cmp al, 10000000b
    jne bp_di_neturi_16bit_poslinkio
    call spausdinti_du_registrus_su_16_bitu_poslinkiu
    jmp bp_di_viskas

    bp_di_neturi_16bit_poslinkio:
    call spausdinti_du_registrus

    bp_di_viskas:
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

mod_bp_di endp

proc mod_si
    push ax
    push bx
    push dx
    push bp
    push es
    push ds
    
    call si_registras

    mov al, modrm_baitas
    and al, 11000000b
    cmp al, 01000000b
    jne si_neturi_8bit_poslinkio
    call spausdinti_registra_su_8_bitu_poslinkiu
    jmp si_viskas

    si_neturi_8bit_poslinkio:
    cmp al, 10000000b
    jne si_neturi_16bit_poslinkio
    call spausdinti_registra_su_16_bitu_poslinkiu
    jmp si_viskas

    si_neturi_16bit_poslinkio:
    call spausdinti_viena_registra

    si_viskas:
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

mod_si endp

proc mod_di
    push ax
    push bx
    push dx
    push bp
    push es
    push ds
    
    call di_registras

    mov al, modrm_baitas
    and al, 11000000b
    cmp al, 01000000b
    jne di_neturi_8bit_poslinkio
    call spausdinti_registra_su_8_bitu_poslinkiu
    jmp di_viskas

    di_neturi_8bit_poslinkio:
    cmp al, 10000000b
    jne di_neturi_16bit_poslinkio
    call spausdinti_registra_su_16_bitu_poslinkiu
    jmp di_viskas

    di_neturi_16bit_poslinkio:
    call spausdinti_viena_registra

    di_viskas:
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

mod_di endp

proc mod_bp
    push ax
    push bx
    push dx
    push bp
    push es
    push ds
    
    call bp_registras

    mov al, modrm_baitas
    and al, 11000000b
    cmp al, 01000000b
    jne bp_neturi_8bit_poslinkio
    call spausdinti_registra_su_8_bitu_poslinkiu
    jmp bp_viskas

    bp_neturi_8bit_poslinkio:
    cmp al, 10000000b
    jne bp_neturi_16bit_poslinkio
    call spausdinti_registra_su_16_bitu_poslinkiu
    jmp bp_viskas

    bp_neturi_16bit_poslinkio:
    call spausdinti_viena_registra

    bp_viskas:
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

mod_bp endp

proc mod_bx
    push ax
    push bx
    push dx
    push bp
    push es
    push ds
    
    call bx_registras

    mov al, modrm_baitas
    and al, 11000000b
    cmp al, 01000000b
    jne bx_neturi_8bit_poslinkio
    call spausdinti_registra_su_8_bitu_poslinkiu
    jmp bx_viskas

    bx_neturi_8bit_poslinkio:
    cmp al, 10000000b
    jne bx_neturi_16bit_poslinkio
    call spausdinti_registra_su_16_bitu_poslinkiu
    jmp bx_viskas

    bx_neturi_16bit_poslinkio:
    call spausdinti_viena_registra

    bx_viskas:
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

mod_bx endp



; *** SIU PROCEDURU ESME YRA ISSISAUGOTI ATMINTYJE KOKIE REGISTRAI BUVO NAUDOJAMI KOMANDOJE ***


proc bx_registras
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov bx, registro_pavadinimas
    mov dx, BX_pavadinimas
    mov [bx], dx

    mov bx, registro_reiksme
    mov dx, BX_reiksme
    mov [bx], dx

 ; **** SITAS SEKANTIS BLOKAS LABAI SVARBUS ***
 ; kiekviena karta isivykdzius siai proc, as nukreipiu registro_pavadinimas i ANTRAJO REIGISTRO vardo ir reiksmiu adresus
 ; todel kai mums reikia spausdinti du registrus, pirma karta issaukus viena is registro proceduru
 ; registro_padavinimas == registro_pavadinimas1
 ; registro_reiksme     == registro_reiksme1
 ; bet iskart irasius reiksmes, mes registro_pavadinimas ir registro_reiksme nukreipiame i antrojo reg duomenis
 ; tai padarome su siomis eilutemis:

    lea di, registro_pavadinimas2
    mov registro_pavadinimas, di
    lea di, registro_reiksme2
    mov registro_reiksme, di

 ; po pirmo proceduros isivykdytmo pasikeicia registro_pavadinimas ir registro_reiksme adresai i siuos:
 ; registro_pavadinimas == registro_pavadinimas2
 ; registro_reiksme     == registro_reiksme2
 ; tai todel jei dabar issauksime sia procedura antra karta, mes pavadinimo ir reiksmiu reiksmes irasysime
 ; nebe i pirmam registrui skirtus buferius, o i antram
 ; trecio registro niekada nebus tai del to nebesukam galvos
 ; kiekvieno interupto pradzioje mes nukreipiame registro_pavadinimas ir registro_reiksme rodykles atgal
 ; i registro_pavadinimas1 ir registro_reiksme1 zr. 214 eilute
    
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
    ret

bx_registras endp

proc bp_registras
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov bx, registro_pavadinimas
    mov dx, [BP_pavadiniams]
    mov [bx], dx

    mov bx, registro_reiksme
    mov dx, [BP_reiksme]
    mov [bx], dx

    lea di, registro_pavadinimas2
    mov registro_pavadinimas, di
    lea di, registro_reiksme2
    mov registro_reiksme, di
    
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
    ret

bp_registras endp

proc si_registras
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov bx, registro_pavadinimas
    mov dx, [SI_pavadinimas]
    mov [bx], dx

    mov bx, registro_reiksme
    mov dx, [SI_reiksme]
    mov [bx], dx

    lea di, registro_pavadinimas2
    mov registro_pavadinimas, di
    lea di, registro_reiksme2
    mov registro_reiksme, di
    
    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
    ret

si_registras endp

proc di_registras
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov bx, registro_pavadinimas
    mov dx, [DI_pavadinimas]
    mov [bx], dx

    mov bx, registro_reiksme
    mov dx, [DI_reiksme]
    mov [bx], dx

    lea di, registro_pavadinimas2
    mov registro_pavadinimas, di
    lea di, registro_reiksme2
    mov registro_reiksme, di

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax
    ret

di_registras endp




; ***** SPAUSDINIMO PROCEDUROS *****



proc spausdinti_viena_registra
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov ah, 09h
    lea dx, ispejimo_pranesimas
    int 21h

    mov ax, segmentas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, dvitaskio_simbolis
    int 21h

    mov ax, poslinkis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, tarpo_simbolis
    int 21h

    xor ax, ax
    mov al, operacijos_kodas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2] ; +2 kad nespausdinti 00
    int 21h                           ; be +2 butu 008F, o su +2 gauname 8F

    lea dx, mnemonika
    int 21h
    lea dx, atidaromojo_skliaustelio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, uzdaromojo_skliaustelio_simbolis
    int 21h
    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

	mov bx, registro_reiksme1
	mov konvertuoti_sia_reiksme, bx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, atidaromojo_skliaustelio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, uzdaromojo_skliaustelio_simbolis
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

	mov bx, registro_reiksme1
    mov bx, [bx]
	mov konvertuoti_sia_reiksme, bx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, pirmo_steko_zodzio_pranesimas
    int 21h

    mov ax, pirmas_steko_zodis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, newline_simbolis
    int 21h

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

spausdinti_viena_registra endp

proc spausdinti_du_registrus
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov ah, 09h
    lea dx, ispejimo_pranesimas
    int 21h

    mov ax, segmentas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, dvitaskio_simbolis
    int 21h

    mov ax, poslinkis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, tarpo_simbolis
    int 21h

    xor ax, ax
    mov al, operacijos_kodas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h

    xor ax, ax
    mov al, modrm_baitas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h

    lea dx, mnemonika
    int 21h
    lea dx, atidaromojo_skliaustelio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, pliuso_simbolis
    int 21h
    lea dx, registro_pavadinimas2
    int 21h
    lea dx, uzdaromojo_skliaustelio_simbolis
    int 21h
    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

	mov bx, registro_reiksme1
	mov konvertuoti_sia_reiksme, bx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, registro_pavadinimas2
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

	mov bx, registro_reiksme2
	mov konvertuoti_sia_reiksme, bx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, atidaromojo_skliaustelio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, pliuso_simbolis
    int 21h
    lea dx, registro_pavadinimas2
    int 21h
    lea dx, uzdaromojo_skliaustelio_simbolis
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

    mov cx, registro_reiksme1
    mov dx, registro_reiksme2
    add cx, dx
	mov konvertuoti_sia_reiksme, cx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, pirmo_steko_zodzio_pranesimas
    int 21h

    mov ax, pirmas_steko_zodis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, newline_simbolis
    int 21h

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

spausdinti_du_registrus endp

proc spausdinti_registra_su_8_bitu_poslinkiu
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov ah, 09h
    lea dx, ispejimo_pranesimas
    int 21h

    mov ax, segmentas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, dvitaskio_simbolis
    int 21h

    mov ax, poslinkis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, tarpo_simbolis
    int 21h

    xor ax, ax
    mov al, operacijos_kodas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2] ; +2 kad nespausdinti 00
    int 21h                           ; be +2 butu 008F, o su +2 gauname 8F

    xor ax, ax
    mov al, modrm_baitas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h 

    xor ax, ax
    mov al, poslinkis_8bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h               

    lea dx, mnemonika
    int 21h
    lea dx, atidaromojo_skliaustelio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, pliuso_simbolis
    int 21h
    lea dx, [konvertuota_reiksme + 2]
    int 21h
    lea dx, uzdaromojo_skliaustelio_simbolis
    int 21h
    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

	mov bx, registro_reiksme1
	mov konvertuoti_sia_reiksme, bx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, atidaromojo_skliaustelio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, pliuso_simbolis
    int 21h

    xor ax, ax
    mov al, poslinkis_8bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h 

    lea dx, uzdaromojo_skliaustelio_simbolis
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

    xor ax, ax
    mov ah, poslinkis_8bit
	mov bx, registro_reiksme1
    add bx, ax
    mov bx, [bx]
	mov konvertuoti_sia_reiksme, bx
	call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, pirmo_steko_zodzio_pranesimas
    int 21h

    mov ax, pirmas_steko_zodis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, newline_simbolis
    int 21h

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

spausdinti_registra_su_8_bitu_poslinkiu endp

proc spausdinti_du_registrus_su_8_bitu_poslinkiu
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov ah, 09h
    lea dx, ispejimo_pranesimas
    int 21h

    mov ax, segmentas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, dvitaskio_simbolis
    int 21h

    mov ax, poslinkis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, tarpo_simbolis
    int 21h

    xor ax, ax
    mov al, operacijos_kodas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h

    xor ax, ax
    mov al, modrm_baitas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h

    xor ax, ax
    mov al, poslinkis_8bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h   

    lea dx, mnemonika
    int 21h
    lea dx, atidaromojo_skliaustelio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, pliuso_simbolis
    int 21h
    lea dx, registro_pavadinimas2
    int 21h
    lea dx, pliuso_simbolis
    int 21h

    xor ax, ax
    mov al, poslinkis_8bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h 

    lea dx, uzdaromojo_skliaustelio_simbolis
    int 21h
    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

	mov bx, registro_reiksme1
	mov konvertuoti_sia_reiksme, bx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, registro_pavadinimas2
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

	mov bx, registro_reiksme2
	mov konvertuoti_sia_reiksme, bx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, atidaromojo_skliaustelio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, pliuso_simbolis
    int 21h
    lea dx, registro_pavadinimas2
    int 21h
    lea dx, pliuso_simbolis
    int 21h

    xor ax, ax
    mov al, poslinkis_8bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h 

    lea dx, uzdaromojo_skliaustelio_simbolis
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

    xor bx, bx
    mov bl, poslinkis_8bit
    mov cx, registro_reiksme1
    mov dx, registro_reiksme2
    add cx, dx
    add cx, bx
	mov konvertuoti_sia_reiksme, cx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, pirmo_steko_zodzio_pranesimas
    int 21h

    mov ax, pirmas_steko_zodis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, newline_simbolis
    int 21h

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

spausdinti_du_registrus_su_8_bitu_poslinkiu endp

proc spausdinti_registra_su_16_bitu_poslinkiu
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov ah, 09h
    lea dx, ispejimo_pranesimas
    int 21h

    mov ax, segmentas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, dvitaskio_simbolis
    int 21h

    mov ax, poslinkis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, tarpo_simbolis
    int 21h

    xor ax, ax
    mov al, operacijos_kodas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2] ; +2 kad nespausdinti 00
    int 21h                           ; be +2 butu 008F, o su +2 gauname 8F

    xor ax, ax
    mov al, modrm_baitas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h 

    xor ax, ax
    mov al, poslinkis_16bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h      

    xor ax, ax
    mov al, poslinkis_8bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h             

    lea dx, mnemonika
    int 21h
    lea dx, atidaromojo_skliaustelio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, pliuso_simbolis
    int 21h

    xor ax, ax
    mov al, poslinkis_16bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h      

    xor ax, ax
    mov al, poslinkis_8bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h             

    lea dx, uzdaromojo_skliaustelio_simbolis
    int 21h
    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

	mov bx, registro_reiksme1
	mov konvertuoti_sia_reiksme, bx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, atidaromojo_skliaustelio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, pliuso_simbolis
    int 21h

    xor ax, ax
    mov al, poslinkis_16bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h      

    xor ax, ax
    mov al, poslinkis_8bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h             

    lea dx, uzdaromojo_skliaustelio_simbolis
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

    mov ah, poslinkis_8bit
    mov al, poslinkis_16bit
	mov bx, registro_reiksme1
    add bx, ax
    mov bx, [bx]
	mov konvertuoti_sia_reiksme, bx
	call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, pirmo_steko_zodzio_pranesimas
    int 21h

    mov ax, pirmas_steko_zodis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, newline_simbolis
    int 21h

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

spausdinti_registra_su_16_bitu_poslinkiu endp

proc spausdinti_du_registrus_su_16_bitu_poslinkiu
    push ax
    push bx
    push dx
    push bp
    push es
    push ds

    mov ah, 09h
    lea dx, ispejimo_pranesimas
    int 21h

    mov ax, segmentas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, dvitaskio_simbolis
    int 21h

    mov ax, poslinkis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, tarpo_simbolis
    int 21h

    xor ax, ax
    mov al, operacijos_kodas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h

    xor ax, ax
    mov al, modrm_baitas
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h

    xor ax, ax
    mov al, poslinkis_16bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h    

    xor ax, ax
    mov al, poslinkis_8bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h   

    lea dx, mnemonika
    int 21h
    lea dx, atidaromojo_skliaustelio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, pliuso_simbolis
    int 21h
    lea dx, registro_pavadinimas2
    int 21h
    lea dx, pliuso_simbolis
    int 21h

    xor ax, ax
    mov al, poslinkis_16bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h    

    xor ax, ax
    mov al, poslinkis_8bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h 

    lea dx, uzdaromojo_skliaustelio_simbolis
    int 21h
    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

	mov bx, registro_reiksme1
	mov konvertuoti_sia_reiksme, bx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, registro_pavadinimas2
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

	mov bx, registro_reiksme2
	mov konvertuoti_sia_reiksme, bx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, kabliataskio_simbolis
    int 21h
    lea dx, atidaromojo_skliaustelio_simbolis
    int 21h
    lea dx, registro_pavadinimas1
    int 21h
    lea dx, pliuso_simbolis
    int 21h
    lea dx, registro_pavadinimas2
    int 21h
    lea dx, pliuso_simbolis
    int 21h

    xor ax, ax
    mov al, poslinkis_16bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h    

    xor ax, ax
    mov al, poslinkis_8bit
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, [konvertuota_reiksme + 2]
    int 21h 

    lea dx, uzdaromojo_skliaustelio_simbolis
    int 21h
    lea dx, lygybes_simbolis
	int 21h    

    mov bl, poslinkis_16bit
    mov bh, poslinkis_8bit
    mov cx, registro_reiksme1
    mov dx, registro_reiksme2
    add cx, dx
    add cx, bx
	mov konvertuoti_sia_reiksme, cx
	call konvertuoti_i_ascii_simbolius
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, pirmo_steko_zodzio_pranesimas
    int 21h

    mov ax, pirmas_steko_zodis
    mov konvertuoti_sia_reiksme, ax
    call konvertuoti_i_ascii_simbolius
    mov ah, 09h
    lea dx, konvertuota_reiksme
    int 21h

    lea dx, newline_simbolis
    int 21h

    pop ds
    pop es
    pop bp
    pop dx
    pop bx
    pop ax

    ret

spausdinti_du_registrus_su_16_bitu_poslinkiu endp

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
