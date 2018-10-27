;----- HideCsr.Asm

;Copyright (c) 1988 Ethan Winer
;BC7 version

;syntax - CALL HideCursor

.Model Medium,BASIC
.Data

    Extrn MouseThere:Byte ;this byte is in InitMous.Asm

.Code

HideCursor      Proc
    Cmp  Byte Ptr SS:MouseThere,0
    Jz   NoMouse          ;no mouse interrupt installed, so bag out

    Mov  AX,2             ;service 2
    Int  51               ;call the Mouse driver

NoMouse:
    Ret                   ;return to BASIC

HideCursor      Endp
End
