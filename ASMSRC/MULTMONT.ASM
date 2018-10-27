;----- MultMont.Asm - reports the type of display adpater(s) attached to the
;                     computer

;Copyright (c) 1990 Brian C. Giedt

;Calling syntax:
;
;   M% = MultMonitor%
;
;where a bit in M is set if that type of monitor is attached to the computer.
;
;NOTE: A value of zero means monochrome non-graphics monitor exists.
;
;    High Byte          Low Byte         Type       Resolution  Color?(Colors)
; MSB           LSB MSB           LSB
; 0 0 0 0 0 0 0 0   0 0 0 0 0 0 0 0
; ³ ³ ³ ³ ³ ³ ³ ³   ³ ³ ³ ³ ³ ³ ³ ÀÄ(1)Ä Hercules   (720x348)   Mono
; ³ ³ ³ ³ ³ ³ ³ ³   ³ ³ ³ ³ ³ ³ ÀÄÄÄ(2)Ä CGA        (320x200)   Color(4)
; ³ ³ ³ ³ ³ ³ ³ ³   ³ ³ ³ ³ ³ ÀÄÄÄÄÄ(4)Ä EGA        (640x350)   Mono
; ³ ³ ³ ³ ³ ³ ³ ³   ³ ³ ³ ³ ÀÄÄÄÄÄÄÄ(8)Ä EGA        (640x350)   Color(16)
; ³ ³ ³ ³ ³ ³ ³ ³   ³ ³ ³ ÀÄÄÄÄÄÄÄÄ(16)Ä VGA        (640x480)   Mono
; ³ ³ ³ ³ ³ ³ ³ ³   ³ ³ ÀÄÄÄÄÄÄÄÄÄÄ(32)Ä VGA        (640x480)   Color(16)
; ³ ³ ³ ³ ³ ³ ³ ³   ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄ(64)Ä MCGA       (320x200)   Mono(256)
; ³ ³ ³ ³ ³ ³ ³ ³   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄ(128)Ä MCGA       (320x200)   Color(256)
; ³ ³ ³ ³ ³ ³ ³ ³
; ³ ³ ³ ³ ³ ³ ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ(256)Ä EGA /CGA   (640x200)   Color(16)
; ³ ³ ³ ³ ³ ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ(512)Ä IBM 8514/A (640x480)   Color(256)
; ³ ³ ³ ³ ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ(1024)Ä
; ³ ³ ³ ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ(2048)Ä
; ³ ³ ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ(4096)Ä
; ³ ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ(8192)Ä
; ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ(16384)Ä
; ÀÄÄÄ(This not used because it would make the number negative in BASIC)
;


.Model Medium, Basic
.Data

    Public MultMonType

    MultMonType DW 0    ;holds the monitor type code

.Code

MultMonitor PROC

Include BC7Start.GW

    Push BP               ;save BP - older PC BIOS's trash BP during Int 10h

    Mov  AX,MultMonType   ;assume we've already checked the monitor type
    Or   AX,AX            ;have we been here before?
    Jz   CheckThem        ;no, start by assuming a color monitor
    Jmp  Exit             ;yes, don't bother checking again

CheckThem:
    Xor  AX,AX            ;set up ES to point to the BIOS in low memory
    Mov  ES,AX            ;

    Mov  CX,8000h         ;we're going to test this 32,768 times
    Mov  DX,3BAH          ;Hercules video status port
    In   AL,DX            ;if bit 7 ever changes, it's a Herc card
    And  AL,80h           ;isolate just the bit we care about
    Mov  AH,AL            ;save it in AH

HercLoop:
    In   AL,DX            ;now read register repeatedly
    And  AL,80h           ;isolate the bit again
    Cmp  AL,AH            ;has it changed?
    Jne  Hercules         ;yes, a Herc card must be installed
    Loop HercLoop         ;the bit hasn't changed yet, keep looping
    Jmp  Short Color      ;if we got this far, it's not a Hercules

Hercules:
    Or   MultMonType,1    ;indicate a Hercules adapter

Color:
    Mov  AX,1A00h         ;see if it's a VGA adapter
    Int  10h
    Cmp  AL,1Ah           ;was it a VGA?
    Jne  EGA              ;no, but maybe it's an EGA
    Xor  BH,BH            ;yes, clear BH so we can use all of BX
    Dec  BL               ;adjust for our monitor return code method
    Mov  AX,16            ;set bit 4 in AX in case it is a VGA
    Mov  DL,ES:[463h]     ;and get the mono/color byte in low memory

    Cmp  BL,6             ;if it really is a VGA, BL will now be 6 or 7
    Jne  TryColorVGA      ;it's not a mono VGA, so see if it's a color VGA
    Cmp  DL,0B4h          ;if this isn't B4h then it must be an 8514/A
    Je   PrepExit         ;it is a mono VGA, so we're all done
    Mov  AX,512           ;the only combination that reports a mono VGA AND
    Jmp  Short PrepExit   ;  shows a color monitor in low memory is an 8514/A
                          ;And since an 8514/A can't coexist with anything
                          ;other than a monochrome or hercules screen we can
                          ;say we're done finding things

TryColorVGA:
    Shl  AX,1             ;Sets bit 5 to report a VGA with a color monitor
    Cmp  BL,7             ;is it a color VGA?
    Jne  EGA              ;no, so see if it's an EGA
    Cmp  DL,0B4h          ;does low-memory show a mono monitor anyway?
    Jne  PrepExit         ;no, so we're correct in reporting a color VGA
    Shr  AX,1             ;yes, they used MODE MONO, so set bit 4 (VGA mono)
    Jmp  Short PrepExit   ;and get out

EGA:
    Mov  AH,12h           ;ask for EGA info to see if it's an EGA
    Mov  BL,10h
    Int  10h
    Cmp  BL,10h           ;BL will still hold 10h if it's not an EGA
    Je   CGA              ;not an EGA, so it must be a CGA or plain mono

    And  CL,00000110b     ;but wait - see if EGA is emulating a CGA
    Cmp  CL,0110b         ;well, is it?
    Je   ECGA             ;yes, let the code at label ECGA: handle it

    Mov  AX,8             ;assume EGA is connected to a color monitor
    Or   BH,BH            ;but is it really?
    Jz   PrepExit         ;yes, leave AX bit 3 set and get outa' here
    Shr  AX,1             ;no, show that it's a mono monitor with an EGA board
    Jmp  Short PrepExit   ;and go bye bye

ECGA:
    Mov AX,256            ;assume EGA w/color monitor
    Or  BH,BH             ;is the monitor color?
    Jz  PrepExit          ;yea boy - a Poor Man's EGA (640 x 200 x 16 colors)
                          ;  this can use QuickBasic's SCREEN 8
                          ;  if it's not then fall through to the CGA section

;TestCGA:
;    Or   MultMontype,AX

CGA:
    Mov  AL,ES:[463h]     ;see again if the current monitor is color or mono
    Cmp  AL,0B4h          ;is it mono?
    Mov  AX,0             ;assume it is
    Je   PrepExit         ;and it is

    Mov  AX,1A00h         ;see if it's a PS/2 using an MCGA monitor
    Int  10h              ;call the BIOS
    Or   AL,AL            ;is AL still 0 meaning it's not an MCGA?
    Mov  AX,2             ;indicate a CGA monitor in case it's not an MCGA
    Jz   PrepExit         ;"Or AL,AL" above says not MCGA, so it must be CGA

    Cmp  BL,0Bh           ;It is an MCGA.  Now is it monochrome?
    Mov  AX,64            ;assume it is and set bit 6
    Je   PrepExit         ;we were right
    Shl  AX,1             ;no, so it must be color - bump AX to set bit 7

PrepExit:
    Or   MultMontype,AX

Exit:
    Mov  AX,MultMonType   ;save the monitor type for the other video routines
    Pop  BP               ;retrieve BP

Include BC7End.GW
    Ret                   ;monitor type is returned in AX
MultMonitor Endp

            End
