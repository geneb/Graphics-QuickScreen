;**** GMOVE4VE.ASM - Transfer contents of one VGA or EGA screen to an array
;                    in conventional memory

;Copyright 1990, Brian C. Giedt

;Syntax:
;
;    CALL GMove4VE (BYVAL Col1, BYVAL Line1, BYVAL Cols, BYVAL Rows, BYVAL DestSegment, BYVAL Direction)
;
;  If Direction = 0 then saves image to the array
;  If Direction <> 0 then restores the image from the array
;

.Model Medium, Basic
.Data

Include EXTERNAL.GW

.Code

Temp  DW ?

GMove4VE PROC XPos:Word, YPos:Word, Cols:Word, Rows:Word, DestZ:Word, Direction:Word

Include BC7Start.GW

         PUSH DS

         MOV  BX,Cols         ;Store away the number of Columns
         MOV  CX,Rows         ;And rows we are going to move into local memory
         PUSH CX

         MOV  AX,YPos         ;Calculate the beginning offset for the move
         MOV  DX,EGABytesPerLine ;Multiply it by the number of Bytes Per line
         MUL  DX
         ADD  AX,XPos         ;Add the X position to the offset
         DEC  AX
         MOV  SI,AX           ;Store the offset in SI and DX
         MOV  DI,AX
         MOV  DX,AX

         MOV  AX,EGABytesPerLine  ;Store a local copy of EGABytesPerLine
         MOV  Temp,AX

         MOV  AX,Direction
         CMP  AX,0
         JNZ  Reverse

         MOV  AX,DestZ        ;Segment of EGA destination page for storage
         MOV  ES,AX
         XOR  DI,DI           ;Store the 0 offset in DI
         MOV  AX,BX
         Shl  AX,1
         Shl  AX,1
         Shl  AX,1
         Stosw                ;Store the number of X Pixels
         Mov  AX,CX
         Stosw                ;Store the number of Y Pixels

         MOV  CX,EGAVideoSeg  ;Segment of EGA Page 0
         MOV  DS,CX           ;Now have modified our Data Segment

         POP  CX              ;Restore our Row counter

NextRow: PUSH CX              ;put row counter on stack
         MOV  CX,4
NextPlane:
         PUSH CX              ;put plane counter on stack
         Mov  DX,03CEh        ;This is the port for the EGA
         Mov  AX,0404h
         Sub  AH,CL
         OUT  DX,AX
         MOV  CX,BX
         Push SI
InnerLoop1:
         MOVSB
         Or   DI,DI
         JNz  DontIncSeg
         Mov  DI,ES
         Add  DI,1000h
         Mov  ES,DI
         Xor  DI,DI
DontIncSeg:
         Loop InnerLoop1
         Pop  SI
         POP  CX
         LOOP NextPlane

         ADD  SI,Temp
         POP  CX
         LOOP NextRow

         JMP  DoneMove

Reverse:
         XOR  SI,SI           ;Store the 0 offset in SI
         Add  SI,4
         MOV  CX,EGAVideoSeg  ;Segment of EGA Page 0
         MOV  ES,CX           ;Now have modified our Data Segment
         MOV  CX,DestZ        ;Segment of EGA destination page for storage
         MOV  DS,CX
         POP  CX              ;Restore our Row counter

NextRow2:
         PUSH CX              ;put row counter on stack
         MOV  CX,4
NextPlane2:
         Push CX              ;put plane counter on stack
         Mov  DX,03C4h        ;This is the port for the EGA
         Mov  AX,1002h
         Shr  AH,CL
         Out  DX,AX
         Mov  CX,BX
         Push DI
InnerLoop2:
         MOVSB
         Or   SI,SI
         JNz  DontIncSeg2
         Mov  SI,DS
         Add  SI,1000h
         Mov  DS,SI
         Xor  SI,SI
DontIncSeg2:
         Loop InnerLoop2
         Pop  DI
         POP  CX
         Loop NextPlane2

         Add  DI,Temp
         Pop  CX
         Loop NextRow2
         JMP  DoneMove

DoneMove:
         POP  DS

Include BC7End.GW
         RET
GMove4VE ENDP

END

