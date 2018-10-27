;Palette3.ASM - Routines for accessing the palette registers using three values

;Copyright(c) 1990, Brian C. Giedt

;Syntax:
;
;   CALL SetPalTripleVGA(BYVAL PalReg%, BYVAL Red%, BYVAL Green%, BYVAL Blue%)
;
; and
;
;   CALL GetPalTripleVGA(BYVAL PalReg%, Red%, Green%, Blue%)
;
;Where:
;   PalReg% is from 0 to the number of colors available.
;   Red%, Green%, Blue% are all between 0 and 63
;

.Model Medium, Basic

.Code

SetPalTripleVGA PROC PalReg:Word, RedClr:Byte, GreenClr:Byte, BlueClr:Byte

Include BC7Start.GW

     Mov  BX, PalReg       ;Get the Palette Register to Change
     Mov  DH, RedClr       ;Get the Red Palette Value
     Mov  CH, GreenClr     ;Get the Green Palette Value
     Mov  CL, BlueClr      ;Get the Blue Palette Value

     Mov  AX, 1010h        ;Function 10h, Sub-Function 10h sets the palette
                           ; using the three color values (256,000 combinations)
     Push BP               ;Save BP, some machines trash this value
     Int  10h              ;Let BIOS do the work
     Pop  BP               ;Restore our BP

Include BC7End.GW
     Ret
SetPalTripleVGA Endp

GetPalTripleVGA PROC PalReg:Word, RedClr:Ptr, GreenClr:Ptr, BlueClr:Ptr

Include BC7Start.GW

     Mov  BX,PalReg
     Mov  AX,1015h         ;Function 10h Sub-Function 15h gets the
                           ; three palette values
     Push BP
     Int  10h
     Pop  BP

     Xor  AH,AH            ;Clear the top half of AX for the returned values
     Mov  AL,DH            ;AX now holds the Red Palette Value
     Mov  SI,RedClr        ;Get Basic's address for the variable
     Mov  [SI],AX          ;Return the value
     Mov  AL,CH            ;AX now holds the Green Palette Value
     Mov  SI,GreenClr      ;Get Basic's address for the variable
     Mov  [SI],AX          ;Return the value
     Mov  AL,CL            ;AX now holds the Blue Palette Value
     Mov  SI,BlueClr       ;Get Basic's address for the variable
     Mov  [SI],AX          ;Return the value

Include BC7End.GW
     Ret
GetPalTripleVGA Endp

END
