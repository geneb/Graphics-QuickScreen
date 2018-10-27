;----- Date2Num.Asm - converts a date in string form MMDDYYYY to an integer

;Copyright (c) 1989 Ethan Winer
;BC7 version
;Syntax - Dat$ = "MMDDYY" (or "MMDDYYYY" or MM/DD/YY or MM-DD-YYYY or . . .)
;         Number = Date2Num%(Dat$)
;
;where Number receives the number of days before/after 12/31/1979
;if Dat$ is illegal, Number instead receives -32768

.Model Medium,BASIC
.Data

    Extrn Year  :Word     ;these are in Extern2.Asm and allow sharing the
    Extrn Month :Byte     ;same "data" statements needed by both Num2Date
    Extrn Day   :Byte     ;and Date2Num
    Extrn Days  :Word
    Extrn LpYear:Byte
    Extrn Table :Word

.Code
    Extrn StringInfo:Proc

Date2Num Proc Uses ES SI, Date:Ptr

;locate Dat$, get the month and put it into local storage

    Mov  SI,Date          ;get the address for Dat$
    Call StringInfo
    Jcxz BadExit          ;bag out if it's a null string
    Mov  SI,AX            ;SI now points to the start of Dat$
    Mov  ES,DX
    Call Fetch            ;get a character
    Jcxz BadExit          ;no more string, and that's no good

    Mov  DL,10            ;multiply times 10
    Mul  DL
    Mov  AH,AL            ;save it in AH
    Call Fetch            ;get another digit
    Jcxz BadExit          ;as above
    Add  AL,AH            ;combine the two to a single month value
    Jz   BadExit          ;if it's zero that's no good
    Cmp  AL,12            ;is it more than 12?
    Ja   BadExit          ;yes, get out with an error
    Mov  Month,AL         ;no, save the month internally

;get the day and put it into local storage

    Call Fetch            ;as above
    Jcxz BadExit
    Mul  DL
    Mov  AH,AL
    Call Fetch
    Add  AL,AH
    Jz   BadExit
    Cmp  AL,31
    Ja   BadExit
    Mov  Day,AL

;get the year, add 1900 if needed, and put it into local storage

    Call Fetch
    Mul  DL
    Mov  AH,AL
    Call Fetch
    Add  AL,AH
    Xor  AH,AH
    Or   CX,CX            ;are there only two digits in the year?
    Jnz  FourDigits       ;no, keep parsing
    Mov  Year,AX          ;yes, we're all done, store it locally
    Jmp  Short DoneParse

BadExit:
    Mov  AX,-32768        ;show the date was illegal
    Jmp  Exit             ;and jump to the exit

FourDigits:
    Mov  BX,100           ;multiply first two year digits by 100
    Mul  BX
    Mov  BX,AX            ;save what we have so far in BX
    Call Fetch            ;get another digit
    
    Jcxz BadExit
    Mov  DL,10
    Mul  DL
    Mov  AH,AL            ;save what we have so far in AH
    Call Fetch
    
    Add  AL,AH
    Xor  AH,AH            ;clear AH so we can use all of AX
    Add  AX,BX            ;add the digits
    Cmp  AX,2065          ;is the date illegally high?
    Ja   BadExit          ;yes, show that
    
    Sub  AX,1900          ;adjust 1980 --> 80
    Or   AX,AX            ;is the year too low?
    Js   BadExit          ;yes, make an error
    Mov  Year,AX          ;no, store it internally

DoneParse:
    Mov  AX,Year          ;Days = (Year - 80) * 365
    Sub  AX,80
    Mov  CX,365
    IMul CX
    Mov  Days,AX

    Mov  AX,Year          ;IF Year => 80 THEN
    Cmp  AX,80            ;   AdDays = (Year - 77) \ 4
    Jb   Sub80            ;ELSE
    Sub  AX,77            ;   AdDays = (Year - 80) \ 4
    Jmp  Short Div4       ;END IF

Sub80:
    Sub  AX,80

Div4:
    Mov  CL,4
    IDiv CL               ;now AL holds AdDays
    Cbw                   ;convert this byte to a word
    Add  Days,AX          ;Days = Days + AdDays

    Mov  AX,Year          ;LpYear = Year MOD 4
    Xor  DX,DX
    Mov  CX,4
    Div  CX
    Mov  LpYear,DL

    Mov  AX,Year          ;IF Year MOD 100 = 0 THEN LpYear = 0
    Xor  DX,DX
    Mov  CX,100
    Div  CX
    Or   DL,DL
    Jnz  NotMOD100
    Xor  AL,AL
    Mov  LpYear,AL

NotMOD100:
    Mov  AX,Days          ;Days = Days + DyMth(Month) + Day
    Lea  SI,Table         ;point SI to the DyMth array table
    Mov  BL,Month         ;get the Month offset into BL
    Dec  BL               ;adjust 1-12 --> 0-11 for table offset
    Xor  BH,BH            ;clear BH so we can use all of BX
    Add  SI,BX            ;compute the offset into the table
    Add  SI,BX            ;twice because each table item occupies two bytes
    Add  AX,[SI]          ;add DyMth(Month)
    Mov  BL,Day
    Add  AX,BX            ;add the final "+ Day"
    Mov  Days,AX

    Mov  AL,LpYear        ;IF LpYear = 0 AND Month > 2 THEN Days = Days + 1
    Or   AL,AL
    Jnz  GoodExit
    Mov  AL,Month
    Cmp  AL,2
    Jbe  GoodExit
    Mov  AX,Days
    Inc  AX
    Mov  Days,AX

GoodExit:
    Mov  AX,Days          ;get the number of days into AX

Exit:
    Ret                   ;return to BASIC with AX holding the number of days

Date2Num Endp

Fetch Proc Near           ;fetches the next character

    Mov  AL,ES:[SI]       ;get the next character
    Inc  SI
    Dec  CX               ;consider that many less characters in the string
    Cmp  AL,"/"           ;is it a separator?
    Jnz  Skip1            ;no, continue on
    Mov  AL,ES:[SI]       ;yes, swallow it and get the next character
    Inc  SI
    Dec  CX               ;show that many fewer characters remaining
    Jmp  Short Skip2

Skip1:
    Cmp  AL,"-"           ;ignore this separator too
    Jnz  Skip2
    Mov  AL,ES:[SI]       ;throw the separator away and get next character
    Inc  SI
    Dec  CX

Skip2:
    Cmp  AL,"."           ;Europeans use this separator, ignore it as well
    Jnz  Skip3
    Mov  AL,ES:[SI]       ;throw it away and get next character
    Inc  SI
    Dec  CX

Skip3:
    Sub  AL,48            ;convert ASCII digit to Hex
    Ret

Fetch Endp


End
