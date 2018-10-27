;**** GPrint0E.ASM

;Copyright 1990 Brian C. Giedt

;Syntax:
;
;    CALL GPrint0VE (BYVAL Row, BYVAL Column, Text$, BYVAL Colr)
;
;    Row, Column  Cooridinates of Character Locations Similar to
;      LOCATE Row,Column
;
;    Text$ any Basic String
;
;    Colr = BackgroundColor * 256 + ForegroundColor
;

.MODEL MEDIUM, BASIC

  EXTRN ScreenArray : Byte ;Stores the characters drawn to the screen with
                           ; GPrint0VE so that they can be determined at a
                           ; later date by the GetCharacter routine.

.DATA

Include EXTERNAL.GW

.CODE

ifdef bc7
  EXTRN StringInfo:Proc
endif

GPrint0VE PROC Row:Ptr, Column:Ptr, TextString:Ptr, Colr:Ptr

Include BC7Start.GW

ifdef bc7
       Push ES
endif

; Monitor Segment
       Mov  AX,EGAVideoSeg             ;Video Segment of the EGA and VGA
       Mov  VideoSeg,AX

; Calculate Video Offset
       Mov  AX,Row                     ;Get passed parameter Row into AX
       Dec  AX                         ;Decrement it for graphics offset use
       Mov  BX,Column                  ;Get passed parameter Column into BX
       Dec  BX                         ;Decrement it for graphics offset use
       Mov  CX,BX                      ;Put a duplicate in CX

       Mov  DX,EGABytesPerLine         ;Multiply Row by the Bytes per line
       Mul  DX
       Mov  ScreenOffset,Offset ScreenArray   ;Setup the ScreenArray offset
       Mov  DX,ScreenOffset
       Add  DX,SizeOfScrnArray
       Mov  ScreenArrayEnd,DX
       Add  ScreenOffset,AX
       Mov  DX,40h                     ;Ask for information from the video bios
       Mov  ES,DX                      ; about the characters
       Mov  DX,ES:[85h]                ;Get the number of rows from 40h:85h
       Mul  DX                         ;Multiply Row by lines per character

       Add  CX,AX                      ;Add Column position to the video offset
       Mov  VideoOffset,CX             ;Store Video Offset away
       Mov  DI,CX                      ;Store Video Offset into DI

       Mov  AX,ScreenOffset            ;Add Column position to screen array offset
       Add  AX,BX                      ;
       Mov  ScreenOffset,AX            ;Store it away for later use

; String information
       Mov  SI,TextString              ;Obtain String Length and Offset
ifdef bc7
       Pop  ES                         ;Get the BASIC ES register
       Call StringInfo                 ;Get the string address
       Mov  ES,DX                      ;Place the address into ES:SI
       Mov  SI,AX
else
       Mov  CX,[SI]                    ;Place String Length in CX
       Mov  SI,[SI+2]                  ;Place the address of the string in SI
       Push DS                         ;Make the ES register point to our
       Pop  ES                         ; data segment
endif
       Jcxz NearExit                   ;Exit if Null String
       Mov  DX,EGABytesPerLine         ;Check if string will cross side
       Sub  DX,BX                      ;boundary of the screen
       Mov  BX,0                       ;Assume no, and assume 0 length on
                                       ; next line
       Cmp  CX,DX                      ;Is CX < DX ?
       Jl   ThisIsAll                  ;Yes, so string is small enough
       Mov  BX,CX                      ;No, store total length in BX
       Sub  BX,DX                      ; Subtract Length on this line
       Mov  CX,DX                      ; Print characters til end of line
ThisIsAll:
       Mov  MoreString,BX              ;Store String Length away
       Mov  StringOffset,SI            ;Store Offset away for later use
       Mov  StringSegment,ES           ;Store Segment away as well
       Jmp  NextLine

NearExit:
       Jmp  Exit                 ;Used to allow jump above to be conditional

;****************************************************************************
;
; Below is the main loop for printing the characters that make up the string
;
;****************************************************************************

NextLine:
       Mov  BX,Colr              ;Get Colors for the string
                                 ;store it in BX (BL - foreground,
                                 ;                BH - background)
       Mov  SI,StringOffset      ;Restore StringOffset into SI
       Mov  ES,VideoSeg          ;Reload video segment into ES

NextChar:
       Push ES
       Push BX
       Mov  ES,StringSegment
       Mov  DL,ES:[SI]                 ;Get Character from string
       Mov  BX,Seg ScreenArray
       Mov  ES,BX
       Mov  BX,ScreenOffset            ;Place character into ScreenArray
       Cmp  BX,ScreenArrayEnd
       Jge  DontStoreChar
       Mov  Byte Ptr ES:[BX],DL        ;
DontStoreChar:
       Inc  ScreenOffset               ;Increment the screen offset for the
                                       ; next character
       Pop  BX
       Pop  ES

       Push CX                         ;Store away number of characters
       Push SI                         ;Store away the string offset
       Push DS                         ;Store away the local DATA Segment
       Push ES                         ;Store away the Video Segment

; obtain address of character
       Mov  AX,40h                     ;Ask for information from the video bios
       Mov  ES,AX                      ; about the characters
       Mov  CX,ES:[85h]                ;Get the number of rows from 40h:85h

       Xor  AX,AX                      ;Set ES to the
       Mov  ES,AX                      ; absolute zero segment

       Lds  SI,ES:[010Ch]              ;Set DS:[SI] to the start of character
                                       ; table

       Mov  AL,DL                      ;Set AL to the character
       Mul  CL                         ;Multiple by the number of pixel rows
       Add  SI,AX                      ;Add to create the address of character
                                       ; definition for desired character

       Mov  DX,03CEh                   ;Ask the EGA/VGA to use Write Mode 2
       Mov  AX,0205h
       Out  DX,AX
       Mov  DX,03C4h                   ;Make all EGA/VGA planes active
       Mov  AX,0F02h
       Out  DX,AX

       Mov  DX,03CEh                   ;Set DX to the EGA/VGA register
       Pop  ES                         ;Restore VideoSegment
DrawChar:
       Mov  AX,0FF08h                  ;Set the whole byte at
       Out  DX,AX                      ; ES:[DI] to the
       Mov  AL,ES:[DI]                 ; background color
       Mov  ES:[DI],BH                 ; in BH

       Mov  AL,08h                     ;Set the parts of the
       Mov  AH,DS:[SI]                 ; byte at ES:[DI] that
       Out  DX,AX                      ; correspond to the
       Mov  AL,ES:[DI]                 ; character to the
       Mov  ES:[DI],BL                 ; foreground color in BL

       Inc  SI                         ;Increment the Character pointer
       Add  DI,80                      ;Move the Video Offset to the next line
       Loop DrawChar                   ;Do the next segment of the character

       Pop  DS                         ;Restore the DATA Segment
       Mov  DI,VideoOffset             ;Increment the VideoOffset from
       Inc  DI                         ; the location where the last character
       Mov  VideoOffset,DI             ; started
       Pop  SI                         ;Restore the StringOffset
       Inc  SI                         ;Set offset to next char. in string
       Pop  CX                         ;Restore the number of chars left
       Loop NextChar                   ;Do another character

       Mov  StringOffset,SI            ;Store the String Offset away
       Mov  CX,MoreString              ;Any more string to print?
       Jcxz Exit                       ;No, so exit
       Mov  AX,EGABytesPerLine         ;Check if more than a lines worth left
       Mov  BX,0                       ;Assume there isn't
       Cmp  CX,AX                      ;Is CX < AX ?
       Jl   ThisIsAll2                 ;There isn't more than a lines worth
       Mov  BX,CX                      ;Store total length in BX
       Sub  BX,AX                      ;Take away one lines worth
       Mov  CX,AX                      ;Store One lines worth into CX
ThisIsAll2:
       Mov  MoreString,BX              ;Store String Length left away
       Mov  DX,40h                     ;Ask for information from the video bios
       Mov  ES,DX                      ; about the characters
       Mov  DX,ES:[85h]                ;Get the number of rows from 40h:85h
       Dec  DX                         ;Make it one less than the
       Mul  DX                         ; number of lines per character
       Add  DI,AX                      ;Set offset to beginning of next line
       Mov  VideoOffset,DI             ;Store the Video Offset away
       Jmp  NextLine                   ;Print the next line

Exit:  Mov  DX,03CEh                   ;Restore EGA/VGA to normal settings
       Mov  AX,0ff08h
       Out  DX,AX
       Mov  AX,0005h
       Out  DX,AX
       Mov  AX,0003h
       Out  DX,AX
       Mov  AX,0f07h
       Out  DX,AX
       Mov  AX,0001h
       Out  DX,AX

Include BC7End.GW
       Ret
GPrint0VE ENDP

END
