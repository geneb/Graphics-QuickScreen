;KEYDOWN.ASM, reports if a key is currently being pressed

;Copyright (c) 1991 Ethan Winer

;Syntax:  DECLARE FUNCTION KeyDown% ()
;         CALL InstallKeyDown    'do this once at the start of your program
;         ...
;         IF KeyDown% THEN ...   'a key is now being pressed
;
;IMPORTANT NOTE: When running within QBX you must call DeinstallKeyDown
;manually before your program ends.  This is a bug in QBX only, and does
;not affect running within QB or compiled .EXE programs.


.Model Medium, Basic
    Extrn B_ONEXIT:Proc       ;This is a BASIC internal routine, and it
                              ;  unhooks KeyDown automatically when the
.Code                         ;  the program ends.

KeyDown Proc                  ;this is the actual KeyDown function

  Mov  AL,Byte Ptr CS:Status  ;get the current key status
  Cbw                         ;convert to a full word in AX
  Ret                         ;return to BASIC with function output in AX

KeyDown Endp



InstallKeyDown Proc           ;this installs the Interrupt 9 handler

  Cmp  CS:Old9Seg,0           ;have we already been installed?
  Jne  Exit                   ;yes, and don't do that again!

  Mov  AX,3509h               ;ask DOS for the existing Int 9 vector address
  Int  21h                    ;DOS returns the segment:address in ES:BX
  Mov  CS:Old9Adr,BX          ;save it locally
  Mov  CS:Old9Seg,ES
  
  Push DS                     ;save DS before we trash it
  Mov  AX,2509h               ;point keyboard Interrrupt 9 to our own handler
  Mov  DX,Offset NewInt9
  Push CS                     ;copy CS into DS
  Pop  DS
  Int  21h                    ;DOS does the dirty work
  Pop  DS                     ;restore DS before calling B_ONEXIT
  
  Push CS                     ;show B_ONEXIT where the deinstall routine is
  Mov  AX,Offset DeinstallKeyDown
  Push AX
  Call B_ONEXIT               ;register that routine with B_ONEXIT
  
Exit:
  Ret                         ;return to BASIC


;-- Control comes here each time a key is pressed or released.
NewInt9:
  Sti                         ;enable further interrupts
  Push AX                     ;save the only register we're using

  Mov  Byte Ptr CS:Status,0   ;assume the key is being released
  In   AL,60h                 ;get the scan code from the keyboard
  Test AL,80h                 ;is the key being released?
  Jnz  @F                     ;yes, so we assumed correctly
  Dec  Byte Ptr CS:Status     ;no, show that a key has just been pressed
  
@@:
  Pop  AX                     ;restore the saved register
  Jmp  DWord Ptr CS:Old9Adr   ;either way, continue on to BIOS interrupt 9

InstallKeyDown Endp



DeinstallKeyDown Proc         ;this is needed only within QBX (BASIC 7 PDS)

  Xor  AX,AX                  ;see if Int 9 still points here
  Mov  ES,AX
  Mov  AX,ES:[(9 * 4) + 2]    ;now AX holds the current Int 9 handler segment
  Mov  BX,CS                  ;and now BX holds our code segment
  Cmp  AX,BX                  ;did QBX already steal Interrupt 9 back again?
  Jne  @F                     ;yes, skip deinstallation!
  
  Push DS                     ;no, save DS for a moment
  Mov  AX,2509h               ;restore the original Interrupt 9 handler
  Mov  DX,CS:Old9Adr          ;from the segment and address saved earlier
  Mov  DS,CS:Old9Seg
  Int  21h                    ;DOS does the interrupt restoration for us
  Pop  DS                     ;restore DS
  Xor  AX,AX                  ;make a zero for smaller code below
  Mov  CS:Old9Seg,AX          ;clear the segment so it will run again in QB!
  Mov  CS:Status,AL           ;clear the status code for subsequent runs
@@:
  Ret                         ;and then return to BASIC's terminate routine

DeinstallKeyDown Endp

  Status    DB 0              ;this tracks if the key is down or not
  Even                        ;force word alignment for speed
  Old9Adr   DW 0              ;these remember the original Int 9 address
  Old9Seg   DW 0              ;they must be in the code segment because
                              ;  DS is undefined when NewInt9 gets control
End
