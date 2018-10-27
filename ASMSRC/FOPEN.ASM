;----- FOpen.Asm - opens a file in preparation for binary file access from QuickBASIC

;Copyright (c) 1988 Ethan Winer
;Revised Jan 89 - J. R. Conrad

;Syntax - CALL FOpen(FileName$, Handle%)
;
;  Where FileName$ holds the file name to open, and Handle% returns with
;  the handle number that DOS assigns.  If the file cannot be opened,
;  Handle% will be set to -1.  Errors are returned in DOS/WhichError.


.Model Medium, Basic

.Data
    Extrn File_Name:Byte  ;this is in the file External.Asm

.Code
    Include Mixed.Inc
    Extrn Setup_CEH:Far, Restore_CEH:Far, Load_DOSErr:Far
    Extrn Load_File:Far, DOS:Far, Open_Err:Far

HProc FOpen, File:Ptr, Handle:Ptr

    Mov  DX,CS            ;install CEH
    Lea  BX,CS:CEH_Ret
    Call Setup_CEH

    Mov  SI,File          ;SI holds address of FileName$ descriptor
    Call Load_File        ;create an ASCIIZ string
    Jc   ErrExit          ;quit if an error was detected

    Mov  AH,3Dh           ;specify Open File service
    Mov  AL,2             ;specify Read/Write operation
    Call DOS              ;call DOS
    Jnc  Exit             ;if carry flag isn't set, file was opened okay
    Lea  DX,File_Name     ;else, restore the pointer to File_Name
    Call Open_Err         ;and process the error with Open_Err

ErrExit:
    Call Load_DOSErr      ;put up an error

CEH_Ret:
    Mov  AX,-1            ;clear the file handle variable

Exit:
    Mov  DI,Handle        ;put address of Handle% into DI
    Mov  [DI],AX          ;now assign Handle% = AX
    Call Restore_CEH
    HRet                  ;return to BASIC

HEndp
End
