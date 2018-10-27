;----- InitMous.Asm

;This routine not only returns if a mouse is installed, it also sets a
;"shared" variable so the other mouse routines can quickly determine if
;a mouse is present later.

;Copyright (c) 1989 Ethan Winer
;BC7 version

;Syntax - Call InitMouse(There%)      There% = 0 if a mouse driver is not
;                                     installed, -1 if it is

Public MouseThere

.Model Medium, BASIC
.Data

    MouseThere  DB 0      ;shares if mouse is installed with other routines

.Code

InitMouse Proc Uses ES SI, There:Ptr

    Mov  SI,There         ;get the address for There%
    Xor  AX,AX            ;service 0 tells us if a mouse is there
    Mov  ES,AX            ;but first see if the interrupt address is valid
    Cmp  Word Ptr ES:[(51*4)+2],0   ;is the Mouse segment zero?
    Jz   NoMouse          ;yes, so the mouse interrupt is not installed
    Int  51               ;no, call the Mouse interrupt

NoMouse:
    Mov  [SI],AX          ;assign There% = AX
    Mov  MouseThere,AL    ;remember it locally for other mouse routines too
    Ret

InitMouse Endp
End
