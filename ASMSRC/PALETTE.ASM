;Palette.ASM - Routines for accessing the palette registers

;Copyright(c) 1990, Brian C. Giedt

;Syntax:
;
;    CALL SetPaletteEGA(BYVAL ColorRegister%, BYVAL Value%)
;
;    Value% = GetPaletteVGA%(BYVAL ColorRegister%)
;
;Where:
;
;   ColorRegister% is among the available colors, (usually 0 to 15)
;   Value% is from 0 to 63 and is set up with 2 bits for each of the RGB planes
;     in the following fashion 00RGBrgb (See DEMOPAL.BAS and HandlePCXPalette
;     for more on this)
;

.Model Medium, Basic

.Code

SetPaletteEGA PROC PalReg:Byte, Value:Byte

Include BC7Start.GW

     Mov  BL, PalReg       ;Get the Palette Register to Change
     Mov  BH, Value        ;Get the value to change it to

     Mov  AX,1000h         ;Function 10h, Sub-Function 00h sets the palette
                           ; register
     Push BP               ;Some machines trash BP so save it
     Int  10h              ;Let BIOS do the work
     Pop  BP               ;Restore BP

Include BC7End.GW
     Ret
SetPaletteEGA Endp


GetPaletteVGA PROC PalReg:Byte

Include BC7Start.GW

     Mov  BL,PalReg        ;Select the Palette register to look at
     Mov  AX,1007h         ;Function 10h, Sub-Function 7h reads the palette
     Push BP               ;Save BP, some machines trash it
     Int  10h              ;Let BIOS do the work for us
     Pop  BP               ;Restore our BP value

     Xor  AH,AH            ;Clear top half of AX
     Mov  AL,BH            ;Move the value into the low byte of AX

Include BC7End.GW
     Ret                   ;When declared as a function, this will return
                           ; return the palette value
GetPaletteVGA Endp

END
