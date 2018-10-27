;***** DISPPCXE.BAS - This routine loads a PCX from disk to the EGA/VGA video
;                     screen.  The routine OpenPCXFile must be called prior to
;                     calling this routine.

;Copyright (c) 1990 Brian C. Giedt

;Syntax:
;
;     CALL DispPCXVE (BYVAL Display%)
;
;Note: The routine OpenPCXFile must be called prior to calling this routine, as
;      it defines which file is to be displayed.  The displaying of a PCX file
;      was broken into two parts both to save code space for routines like
;      those in the routine OpenPCXFile which are screen independant.  Also
;      it is beneficial to be able to read the header information, like the
;      routine OpenPCXFile does, and then to display the PCX file to the
;      appropriate screen mode.

.Model Medium, Basic
.Data

Include EXTERNAL.GW
Include PCXEXTRN.GW

.Code

EXTRN PCX_ReadBlock:Proc

DispPCXVE PROC Display:Ptr

Include BC7Start.GW

      MOV  AX,0800h
      MOV  DX,Display
      MUL  DX
      ADD  AX,EGABaseVideoSeg
      MOV  VideoSeg,AX

      Mov  DX,03C4h    ;Set up the EGA or VGA registers
      Mov  AX,0F02h
      Out  DX,AX
      Mov  DX,03CEh
      Mov  AX,0001h    ;Enable No Bits
      Out  DX,AX
      Mov  AX,0002h    ;Not using Color Compare
      Out  DX,AX
      Mov  AX,0003h    ;No Data Rotate or Arithmetic functions
      Out  DX,AX
      Mov  AX,0005h    ;Use Write Mode 0
      Out  DX,AX
      Mov  AX,0FF08h   ;Set the Bit Mask Register to effect all bits
      Out  DX,AX
      Mov  AX,0F07h    ;Don't Care about all Colors
      Out  DX,AX

      Lea  DX,PCX_TheBuffer
      CALL PCX_ReadBlock

      Mov  BX,PCX_XMax
      Sub  BX,PCX_XMin
      Inc  BX
      Mov  PCX_CountBitsSave,BX
      Mov  PCX_CountBits,BX

      MOV  DI,PCX_StartAddress

      XOR  BX,BX
      MOV  PCX_Plane,1
      MOV  PCX_Y,0
      Lea  SI,PCX_TheBuffer
      MOV  ES,VideoSeg
      MOV  CL,PCX_NumPlanes
      MOV  AH,1
      SHL  AH,CL
      MOV  PCX_HighPlane,AH

      MOV  DX,03C4h        ;Send the instruction to the monitor
      MOV  AX,0102h
      OUT  DX,AX           ;AH already holds the current plane

DoAnother:
      Cmp  PCX_CountDown,0
      Jg   Continue
      ;Insert file stuff here
      Push DX
      Lea  DX,PCX_TheBuffer
      Mov  SI,DX
      Jl   SkipByteMove
      Mov  AL,PCX_TheBuffer[512]
      Mov  [SI],AL
      Inc  DX
SkipByteMove:
      Neg  PCX_CountDown
      Add  PCX_CountDown,512
      CALL PCX_ReadBlock
      Pop  DX
      Jnc  Continue
      Jmp  ExitSub
Continue:
      MOV  AL,[SI]
      Dec  PCX_CountDown
      CMP  AL,193
      JNB  NotSingleByte
      Jmp  SingleByte

NotSingleByte:
      SUB  AL,192
      MOV  CL,AL
      XOR  CH,CH
      INC  SI
      MOV  AL,[SI]
      Dec  PCX_CountDown

SendAnother:
      Cmp  PCX_CountBits,8
      Jge  DoSend1
      Inc  DI
      Cmp  PCX_CountBits,0
      Jle  SkipSend1
      Push CX
      Mov  CX,PCX_CountBits
      Dec  CL
      Mov  CH,128
      Or   CL,CL
      Jz   SkipShift1
MultiShift1:
      STC
      RCR  CH,1
      Dec  CL
      Jne  MultiShift1
SkipShift1:
      Dec  DI
      Push DX
      Push AX
      Mov  DX,03CEh
      Mov  AH,CH
      Mov  AL,08h
      Out  DX,AX
      Pop  AX
      XCHG ES:[DI],AL
      Push AX
      Mov  AX,0FF08h
      Out  DX,AX
      Pop  AX
      Pop  DX
      Inc  DI
      Pop  CX
      Sub  PCX_CountBits,8
      Jmp  Short SkipSend1
DoSend1:
      STOSB
      Sub  PCX_CountBits,8
SkipSend1:
      INC  BX
      CMP  BX,PCX_BytesPerLine
      JB   NoOverlap

      CMP  CX,1
      JE   NoOverlap
      Mov  BX,PCX_CountBitsSave
      Mov  PCX_CountBits,BX
      SUB  DI,PCX_BytesPerLine
      XOR  BX,BX
      SHL  AH,1
      CMP  AH,PCX_HighPlane
      JNE  NoLineChange

      MOV  AH,1
      ADD  DI,80
      INC  PCX_Y
NoLineChange:
      MOV  DX,03C4h        ;Send the instruction to the monitor
      MOV  AL,02
      OUT  DX,AX           ;AH already holds the current plane
      MOV  AL,[SI]
NoOverlap:
      LOOP SendAnother

      INC  SI
      JMP  Short CompareX

SingleByte:
      Cmp  PCX_CountBits,8
      Jge  DoSend2
      Inc  DI
      Cmp  PCX_CountBits,0
      Jle  SkipSend2
      Push CX
      Mov  CX,PCX_CountBits
      Dec  CL
      Mov  CH,128
      Or   CL,CL
      Jz   SkipShift2
MultiShift2:
      STC
      RCR  CH,1
      Dec  CL
      Jne  MultiShift2
SkipShift2:
      Dec  DI
      Push DX
      Push AX
      Mov  DX,03CEh
      Mov  AH,CH
      Mov  AL,08h
      Out  DX,AX
      Pop  AX
      XCHG ES:[DI],AL
      Push AX
      Mov  AX,0FF08h
      Out  DX,AX
      Pop  AX
      Pop  DX
      Inc  DI
      Pop  CX
      Sub  PCX_CountBits,8
      Jmp  Short SkipSend2
DoSend2:
      STOSB                 ;move the byte to video memory
      Sub  PCX_CountBits,8
SkipSend2:
      INC  SI
      INC  BX

CompareX:
      CMP  BX,PCX_BytesPerLine
      JNB  DontDoAnother
      Jmp  DoAnother

DontDoAnother:
      Mov  BX,PCX_CountBitsSave
      Mov  PCX_CountBits,BX
      SUB  DI,PCX_BytesPerLine
      SHL  AH,1
      MOV  DX,03C4h        ;Send the instruction to the monitor
      MOV  AL,02
      OUT  DX,AX           ;AH already holds the current plane
      CMP  AH,PCX_HighPlane
      JNE  DoMore
      MOV  AH,1
      Out  DX,AX
      INC  PCX_Y
      ADD  DI,80

      MOV  BX,PCX_YMax
      CMP  PCX_Y,BX
      JA   ExitSub
DoMore:
      XOR  BX,BX
      JMP  DoAnother

ExitSub:
      MOV  BX,PCX_FileHandle
      Mov  AH,3Eh           ;close file service
      Int  21h              ;do it

      MOV  DX,03CEh
      Mov  AX,0001h              ;reset EGA register 1
      Out  DX,AX
      Mov  AX,0003h              ;reset EGA register 3
      Out  DX,AX
      Mov  AX,0FF08h             ;reset EGA register 8
      Out  DX,AX

      MOV  DX,03C4h        ;Send the instruction to the monitor
      MOV  AX,0F02h
      OUT  DX,AX           ;AH already holds the current plane

Include BC7End.GW
      RET

DispPCXVE EndP

END
