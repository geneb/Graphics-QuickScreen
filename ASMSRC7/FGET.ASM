;----- FGet.Asm - gets bytes from a binary file

;Copyright (c) 1988 Ethan Winer
;Revised Jan 89 - J. R. Conrad

;Syntax - CALL FGet(Handle%, Dest$)

;  Where Handle% is the number DOS originally assigned when the file was
;  opened, Dest$ is the destination string that will receive the data.
;  The length of the string indicates how many bytes are to be read.
;  Errors are returned in DOS/WhichError.


.Model Medium, Basic
.Code
    Extrn Setup_CEH:Far, Restore_CEH:Far, Load_DOSErr:Far, DOS:Far
    Extrn StringInfo:Proc

FGet Proc Uses DS SI, Handle:Ptr, Dest:Ptr

    Mov  DX,CS            ;install CEH
    Lea  BX,CS:Exit
    Call Setup_CEH

    Mov  BX,Handle        ;get the address for Handle%
    Mov  BX,[BX]          ;and put it into BX
    Mov  SI,Dest          ;get the address for Dest$
    Call StringInfo       ;get address and length of string
    Mov  DS,DX            ;set DS to String Segment
    Mov  DX,AX            ;put the address of the string into DX
    Mov  AX,27            ;assume Dest$ is null
    Jcxz ErrExit          ;it IS null, get out now!
    Mov  AH,3Fh           ;specify Read service
    Call DOS              ;call DOS
    Jc   ErrExit          ;check for errors

    Cmp  AX,CX            ;check for end of file
    Je   Exit             ;did we get the full record?
    Mov  AX,23            ;if not, then "input past end"

ErrExit:
    Call Load_DOSErr

Exit:
    Call Restore_CEH      ;restore the QuickBASIC critical error handler
    Ret                   ;return to BASIC

FGet Endp
End

