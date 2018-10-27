;----- FSeek.Asm - positions the pointer for a file already opened for binary access

;Copyright (c) 1988 Ethan Winer
;Revised Jan 89 - J. R. Conrad
;BC7 Version J. Munro

;Syntax - CALL FSeek(Handle%, Location&)
;
;  Where Handle% is the number DOS originally assigned when the file was
;  opened, and Location& is a long integer holding the location to seek to.
;  Error are returned in DOS/WhichError.  Notice that in DOS, 0 is the first
;  file location, rather than 1 as in QuickBASIC.


.Model Medium, Basic
.Code
    Extrn Setup_CEH:Far, Restore_CEH:Far, Load_DOSErr:Far, DOS:Far

FSeek Proc Uses SI, Handle:Ptr, Location:Ptr

    Mov  DX,CS            ;install CEH
    Lea  BX,CS:Exit
    Call Setup_CEH

    Mov  SI,Handle        ;get the address for Handle%
    Mov  BX,[SI]          ;and put it into BX
    Mov  AX,6             ;assume user error
    Or   BX,BX            ;did they forget to assign Handle%?
    Jz   ErrExit          ;yes, and DOS won't show the error later!

    Mov  SI,Location      ;get the address for Location
    Mov  DX,[SI]          ;put the low portion into DX
    Mov  CX,[SI+02]       ;and put the high portion into CX

    Mov  AX,4200h         ;specify Move File Pointer service
    Call DOS              ;call DOS
    Jnc  Exit             ;no errors, continue

ErrExit:
    Call Load_DOSErr

Exit:
    Call Restore_CEH      ;restore QuickBASIC's critical error handler
    Ret                   ;return to BASIC

FSeek Endp
End
