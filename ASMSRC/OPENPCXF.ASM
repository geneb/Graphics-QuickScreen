;OpenPCXF.ASM - Two routines are contained in this file.  One opens a PCX file
;               and loads its header into appropriate variables and loads its
;               header and palette information into a user supplied array.
;               The other routine is used by the PCX routines to access the
;               disk for another block of information from the PCX file.

; Copyright(c) 1990, Brian C. Giedt

; Syntax:
;
;    IF NOT OPENPCXF(Filename$) THEN PRINT "ERROR OPENING .PCX FILE"
;
; Where Filename$ contains the name of a valid DOS file.

.Model Medium, Basic
.Data

Include External.GW

PCX_BitsPerPixel     DB  0
PCX_XMin             DW  0
PCX_YMin             DW  0
PCX_XMax             DW  639
PCX_YMax             DW  479
PCX_XSize            DW  640
PCX_YSize            DW  480

PCX_NumPlanes        DB  0
PCX_BytesPerLine     DW  0

PCX_HighPlane        DB  0
PCX_Plane            DB  0
PCX_Y                DW  0
PCX_ArraySeg         DW  0
PCX_DataSeg          DW  0
PCX_CountBits        DW  0
PCX_CountBitsSave    DW  0
PCX_StartAddress     DW  0
PCX_LocalFilename    DB  66 DUP(?)
PCX_FileHandle       DW  0
PCX_TheBuffer        DB  524 DUP(?)
PCX_CountDown        DW  514

                     ;Below is the data for the standard EGA and VGA palettes
PaletteVE            DB  0,  0,  0,  0,  0,  170,0,  170,0,  0,  170,170
                     DB  170,0,  0,  170,0,  170,170,85, 0,  170,170,170
                     DB  85, 85, 85, 0,  0,  255,0,  255,0,  0,  255,255
                     DB  255,0,  0,  255,0,  255,255,255,0,  255,255,255

PCX_AllDone          DB  0  ;Boolean variable
PCX_ByteAtOnLine     DW  0
PCX_BeginBuff        DW  0

Public PCX_XMin, PCX_YMin, PCX_XMax, PCX_YMax, PCX_XSize, PCX_YSize
Public PCX_NumPlanes, PCX_HighPlane, PCX_Plane, PCX_BytesPerLine
Public PCX_BitsPerPixel, PCX_Y, PCX_ArraySeg, PCX_DataSeg
Public PCX_CountBits, PCX_CountBitsSave, PCX_StartAddress
Public PCX_LocalFilename, PCX_FileHandle, PCX_TheBuffer, PCX_CountDown
Public PaletteVE, PCX_AllDone, PCX_ByteAtOnLine, PCX_BeginBuff

.Code

ifdef bc7
  EXTRN StringInfo:Proc
endif

OpenPCXFile Proc Filename:Ptr, Array:Ptr

Include BC7Start.GW

      Mov  SI,Filename                ;Get the filename
ifdef bc7
      Call StringInfo                 ;Get the string address
      Mov  ES,DX                      ;Place the address into ES:SI
      Mov  SI,AX
else
      Mov  CX,[SI]                    ;Place String Length in CX
      Mov  SI,[SI+2]                  ;Place the address of the string in SI
      Push DS                         ;Make the ES register point to our
      Pop  ES                         ; data segment
endif
      JCXZ ExitClose
      Lea  DI,PCX_LocalFilename
      Mov  DX,DI
MovesbFake:
      Mov  AL,ES:[SI]
      Mov  [DI],AL
      INC  DI
      INC  SI
      Loop MovesbFake
      MOV  Byte Ptr [DI],0            ;DOS Strings are terminated by a null

      Mov  AH,3Dh           ;open file service
      Mov  AL,0             ;open for reading only
      Int  21h              ;call DOS
      Jc   ExitClose

      Mov  BX,AX            ;save handle in BX
      Mov  PCX_FileHandle,BX
      Mov  AX,4200h         ;move file pointer to start of file
      Xor  CX,CX            ;file offset = 0
      Mov  DX,0             ;  plus 0 bytes
      Int  21h              ;do it
      Jc   ExitClose

      Lea  DX,PCX_TheBuffer
      Mov  CX,128           ;request 128 bytes read
      Mov  AH,3Fh           ;read file service #
      Int  21h              ;do it
      Jc   ExitClose

      Mov  PCX_CountDown,513    ;Setup for the first read

      Lea  SI,PCX_TheBuffer
      Add  SI,2                         ;Check for Encoding type
      Mov  CH,1
      Cmp  [SI],CH                   ;Only works with encoding is style 1
      Je   EncodingOK

ExitClose:
      Xor  AX,AX
      Jmp  ExitSub

EncodingOK:
      Push DS
      Pop  ES
      Inc  SI
      Lea  DI,PCX_BitsPerPixel
      Mov  CX,13
      Cld
      Rep  Movsb
      Add  SI,48+1
      Movsb
      Movsw

      Mov  SI,Array
ifdef bc7
      Call StringInfo                 ;Get the string address
      Mov  ES,DX                      ;Place the address into ES:DI
      Mov  DI,AX
else
      Mov  CX,[SI]                    ;Place String Length in CX
      Mov  DI,[SI+2]                  ;Place the address of the string in DI
      Push DS                         ;Make the ES register point to our
      Pop  ES                         ; data segment
endif
      Mov  DX,CX
      Cmp  CX,68+768
      Jl   ExitClose
      Mov  CX,68
      Lea  SI,PCX_TheBuffer
      Rep  Movsb
      Mov  AX,-1
      Mov  CL,8
      Cmp  PCX_BitsPerPixel,CL
      Jne  ExitSub

      Push ES                  ;Save the segment of Array$
      Push DI                  ;Save the offset cooresponding to element 69

      Lea  DX,PCX_TheBuffer    ;put DTA address into DS:DX for set DTA service
      Mov  AH,1Ah              ;set DTA service - creates space for file info
      Int  21h                 ;call DOS

      Lea  DX,PCX_LocalFileName;Find First needs search string address in DS:DX

      Mov  CL,00100111b        ;have search include normal files, hidden files,
                               ;  system files, and archive files
      Mov  AH,4Eh              ;find first matching file service
      Int  21h                 ;call DOS
      Jc   ExitWithPops        ;if Carry is set it wasn't there

      Lea  DI,PCX_TheBuffer    ;point DI to DTA for getting file size
      Mov  AX,[DI+26]          ;put low word of file size into AX
      Mov  DX,[DI+28]          ;put high word of file size into DX

      Sub  AX,768
      Sbb  DX,0

      Mov  CX,DX            ;high offset
      Mov  DX,AX            ;plus low offset
      Mov  AX,4200h         ;move file pointer to start of file
      Int  21h              ;do it
      Jc   ExitWithPops
      Jmp  Short GotFileSize

ExitWithPops:
      Pop  DI
      Pop  ES
      Xor  AX,AX
      Jmp  Short ExitSub

GotFileSize:
      Pop  DX               ;Get back the address of Array$ element 69 in AX:DX
      Pop  AX
      Push DS               ;Save the data segment so we can point DS to
      Mov  DS,AX            ; our string's segment
      Mov  CX,768           ;request 768 bytes read
      Mov  AH,3Fh           ;read file service #
      Int  21h              ;do it
      Pop  DS               ;return our Data segment
      Mov  AX,0
      Jc   ExitSub

      Mov  AX,4200h         ;move file pointer to start of file PCX data
      Xor  CX,CX            ;file offset = 0
      Mov  DX,128           ;  plus 128 bytes
      Int  21h              ;do it
      Mov  AX,-1            ;Assume everything's OK
      Jnc  ExitSub          ;Was it OK?  If so jump to the end
      Xor  AX,AX            ;No, there was an error so say so.
ExitSub:
      Mov  PCX_StartAddress,0
Include BC7End.GW
      Ret
OpenPCXFile Endp

PCX_ReadBlock Proc Far
      Push AX
      Push BX
      Push CX
      Mov  BX,PCX_FileHandle
      Mov  CX,PCX_CountDown     ;request 8920 bytes read
      Mov  AH,3Fh           ;read file service #
      Int  21h              ;do it
      Mov  PCX_CountDown,512
      Pop  CX
      Pop  BX
      Pop  AX
      RetF

PCX_ReadBlock Endp

PositionPCXVE Proc Line:Word,Col:Word

      Mov  AX,Line
      Mov  DX,EGABytesPerLine
      IMul DX
      Add  AX,Col
      Dec  AX
      Mov  PCX_StartAddress,AX

      Ret
PositionPCXVE Endp

End
