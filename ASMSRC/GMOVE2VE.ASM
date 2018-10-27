;**** GMOVE2VE.ASM - Transfer contents of one VGA or EGA screen to another
;                    selectively

;Copyright 1990, Brian C. Giedt

;Syntax:
;
;    CALL GMove2VE (BYVAL XPos, BYVAL YPos, BYVAL Cols, BYVAL Rows, BYVAL DestSegment, BYVAL Direct)
;
;  If Direct = 0 then saves image to the other screen
;  If Direct <> 0 then restores the image
;

.Model Medium, Basic
.Data

Include EXTERNAL.GW

.Code

GMove2VE PROC XPos:Word, YPos:Word, Cols:Word, Rows:Word, DestZ:Word, Direct:Word

Include BC7Start.GW

         PUSH DS
         MOV  DX,3CEh         ;Set to the EGA's Write Mode 1
         MOV  AX,0105h
         OUT  DX,AX

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

         MOV  AX,Direct
         CMP  AX,0
         JNZ  Reverse

         MOV  AX,EGABytesPerLine  ;Store a local copy of EGABytesPerLine

         XOR  DI,DI           ;Store the 0 offset in DI
         MOV  CX,EGAVideoSeg  ;Segment of EGA Page 0
         MOV  DS,CX           ;Now have modified our Data Segment
         MOV  CX,DestZ        ;Segment of EGA destination page for storage
         MOV  ES,CX

         POP  CX              ;Restore our Row counter

NextRow: PUSH CX
         MOV  CX,BX
REP      MOVSB
         ADD  DX,AX
         MOV  SI,DX
         POP  CX
         LOOP NextRow
         JMP  Short DoneMove

Reverse:
         MOV  AX,EGABytesPerLine  ;Store a local copy of EGABytesPerLine

         XOR  SI,SI           ;Store the 0 offset in SI
         MOV  CX,EGAVideoSeg  ;Segment of EGA Page 0
         MOV  ES,CX           ;Now have modified our Data Segment
         MOV  CX,DestZ        ;Segment of EGA destination page for storage
         MOV  DS,CX

         POP  CX              ;Restore our Row counter

NextRow2:PUSH CX
         MOV  CX,BX
REP      MOVSB
         ADD  DX,AX
         MOV  DI,DX
         POP  CX
         LOOP NextRow2

DoneMove:
         MOV  DX,3CEh                ;Reset to the EGA's Write Mode 0
         MOV  AX,0005h
         OUT  DX,AX
         POP  DS

Include BC7End.GW
         RET
GMove2VE ENDP

END

