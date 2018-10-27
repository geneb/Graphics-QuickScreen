;----- ShowCsr.Asm

;Copyright (c) 1989 Ethan Winer
;BC7 version

;Syntax - Call ShowCursor

.Model Medium,BASIC
.Data

    Extrn MouseThere:Byte ;this byte is in InitMous.Asm

.Code

ShowCursor Proc
    
    Cmp  Byte Ptr SS:MouseThere,0
    Je   NoMouse          ;no mouse installed, say goodnight Dick

    Mov  AX,1             ;service 1
    Int  51

NoMouse:
    Ret

ShowCursor      Endp
End
