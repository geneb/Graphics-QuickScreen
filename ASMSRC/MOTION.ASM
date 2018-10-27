;----- Motion.Asm

;Copyright (c) 1989 Ethan Winer
;BC7 version

;Syntax - Call Motion(Value%)     ;Value range: 0-255, 0 is most sensitive

.Model Medium,BASIC
.Data

    Extrn MouseThere:Byte ;this byte is in InitMous.Asm

.Code

Motion Proc Uses SI, Value:Ptr

    Cmp  MouseThere,0
    Je   NoMouse

    Mov  AX,15       ;service 15
    Mov  SI,Value
    Mov  CX,[SI]     ;set the horizontal sensitivity to Value%
    Mov  DX,[SI]     ;do the same for the vertical sensitivity
    Int  51

NoMouse:
    Ret

Motion Endp
End
