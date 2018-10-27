DEFINT A-Z
DECLARE SUB BreakLines (Text$, Array$(), Element%, Wdth%)
DECLARE SUB BLPrint (LPt, X$, ErrCode)

DECLARE FUNCTION FUsing$ (Number$, Image$)
DECLARE FUNCTION EDate2Num% (Dat$)
DECLARE FUNCTION ENum2Date$ (Days)
DECLARE FUNCTION Date2Num% (Dat$)
DECLARE FUNCTION Num2Date$ (Days)

DECLARE FUNCTION MsgBox (Msg$(), Options)

'$INCLUDE: 'Common.GQS'
'$INCLUDE: 'FldInfo.bi'     

SUB BreakLines (Text$, Array$(), Element, Wdth)

    BrkTbl$ = " +-*/\)"
    I = Element
    P1 = 1
    P2 = Wdth
    Ln = LEN(Text$)

    DO UNTIL P1 > Ln OR I > UBOUND(Array$)

       DO UNTIL P2 >= Ln OR INSTR(BrkTbl$, MID$(Text$, P2, 1)) = 0
          P2 = P2 + 1
       LOOP

       DO UNTIL P2 = 1 OR INSTR(BrkTbl$, MID$(Text$, P2, 1))
          P2 = P2 - 1
       LOOP

       Array$(I, 1) = RTRIM$(MID$(Text$, P1, P2 - P1 + 1))

       I = I + 1
       P1 = P2 + 1
       P2 = P1 + Wdth - 1

    LOOP

END SUB

'******* Print the form definition
SUB PrintFRM (ScrName$, Form() AS FieldInfoG, Calc$(), Help$())


   REDIM FType$(23)
   FType$(1) = "String$"
   FType$(2) = "Proper Name"
   FType$(3) = "UCase$"
   FType$(4) = "Numeric$"
   FType$(5) = "Notes"
   FType$(6) = "Scroll$"
   FType$(7) = "Logical"
   FType$(8) = "Integer%"
   FType$(9) = "Long Int."
   FType$(10) = "Single!"
   FType$(11) = "Double#"
   FType$(12) = "$Money"
   FType$(13) = "US Date"
   FType$(14) = "Euro. Date"
   FType$(15) = "Phone"
   FType$(16) = "Zip Code"
   FType$(17) = "Social Sec"
   FType$(18) = "Relational"
   FType$(19) = "Choice Ar."
   FType$(20) = "Mouse"
   FType$(21) = "Button"
   FType$(22) = "HScrollbar"
   FType$(23) = "VScrollbar"

   REDIM Temp$(3, 1)

   PLine$ = SPACE$(79)

   Page = 0
   LCount = 0
   ErrCount = -1
   GOSUB PrintHeader

   Offset = 1

   FOR X = 1 TO Form(0).Fields
      FType = Form(X).FType

      LSET PLine$ = FUsing$(STR$(X), "###")     'the field sequence #
      MID$(PLine$, 6) = FUsing$(STR$(Offset), "#####")

      Offset = Offset + Form(X).StorLen         'accumulate the offset

      MID$(PLine$, 12) = Form(X).FldName        'Field Name


      IF FType = PButton OR FType = MouseFld THEN
        MID$(PLine$, 21) = FType$(Form(X).FType)
        Temp$ = LTRIM$(STR$(Form(X).Value))
        MID$(PLine$, 30 - LEN(Temp$) + 1) = Temp$
      ELSEIF FType = ScrollFld THEN
        MID$(PLine$, 21) = FType$(Form(X).FType)
        Temp$ = LTRIM$(STR$(Form(X).Decimals))
        MID$(PLine$, 30 - LEN(Temp$) + 1) = Temp$
      ELSE
        MID$(PLine$, 21) = FType$(Form(X).FType)  'the field type
      END IF

      IF FType = NotesFld THEN
        MID$(PLine$, 32) = FUsing$(STR$(Form(X).RCol - Form(X).LCol + 1), "###")
        MID$(PLine$, 35) = "x"
        MID$(PLine$, 36) = LTRIM$(STR$(Form(X).ScratchI - Form(X).Row + 1)) + "+"
      ELSE
        MID$(PLine$, 33) = FUsing$(STR$(Form(X).RCol - Form(X).LCol + 1), "###")
      END IF

      IF FType = Relational THEN
         FType = Form(X).ScratchI
      END IF
                                                '# of decimal places
      IF FType = SngFld OR FType = DblFld OR FType = MoneyFld THEN
         MID$(PLine$, 36) = "." + LTRIM$(STR$(Form(X).Decimals))
      END IF
                                                'length within record
      MID$(PLine$, 40) = FUsing$(STR$(Form(X).StorLen), "######")

                                                'the field row and column
      MID$(PLine$, 47) = FUsing$(STR$(Form(X).Row), "###") + ","
      MID$(PLine$, 51) = FUsing$(STR$(Form(X).LCol), "###")

      IF Form(X).FType = Relational THEN
         MID$(PLine$, 55) = Form(X).RelFile
         MID$(PLine$, 66) = FUsing$(STR$(Form(X).RelFld), "###")
      END IF

      IF Form(X).Indexed AND FType <> HScrollFld AND FType <> VScrollFld THEN                   'see if indexed or not
         MID$(PLine$, 72) = "Yes"
      ELSE

         IF FType <> HScrollFld AND FType <> VScrollFld THEN
           MID$(PLine$, 72) = "No"
         ELSE
           MID$(PLine$, 72) = "**"
         END IF

      END IF

      IF Form(X).Protected THEN                 'see if protected or not
         MID$(PLine$, 77) = "Yes"
      ELSE
         MID$(PLine$, 77) = "No"
      END IF

      FType = Form(X).FType
      IF FType = Relational THEN FType = Form(X).ScratchI
      SELECT CASE FType
         CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld, HScrollFld, VScrollFld
            Lo$ = LTRIM$(STR$(Form(X).LowRange))
            Hi$ = LTRIM$(STR$(Form(X).HiRange))
         CASE DateFld, EuroDateFld
            IF FType = DateFld THEN
               Lo$ = Num2Date$(INT(Form(X).LowRange))
               Hi$ = Num2Date$(INT(Form(X).HiRange))
            ELSE
               Lo$ = ENum2Date$(INT(Form(X).LowRange))
               Hi$ = ENum2Date$(INT(Form(X).HiRange))
            END IF
         CASE ELSE
            Lo$ = ""
            Hi$ = ""
      END SELECT


      IF LEN(Calc$(X)) OR LEN(Help$(X)) OR LEN(Lo$) THEN
         FieldDone = 0
      ELSE
         FieldDone = -1
      END IF

      GOSUB PrintIt

      IF LEN(Lo$) THEN
         IF FType = HScrollFld OR FType = VScrollFld THEN
           LSET PLine$ = "Range: " + Lo$ + " to " + Hi$
           MID$(PLine$, 26) = "Large change =" + STR$(Form(X).RelFld) + ", Small change =" + STR$(Form(X).Indexed)
         ELSE
           LSET PLine$ = "Range: " + Lo$ + " to " + Hi$
         END IF
         GOSUB PrintIt
      END IF

      IF LEN(Calc$(X)) THEN
         REDIM Temp$(10, 1)
         BreakLines Calc$(X), Temp$(), 1, 69
         LSET PLine$ = "FORMULA:"
         GOSUB PrintText
      END IF

      IF LEN(Help$(X)) THEN
         REDIM Temp$(10, 1)
         BreakLines Help$(X), Temp$(), 1, 69
         LSET PLine$ = "HELP:"
         GOSUB PrintText
      END IF

      IF LEN(Calc$(X)) OR LEN(Help$(X)) OR LEN(Lo$) THEN
         LSET PLine$ = "--- ------ -------- ---------- ------- ------ ------- -------- ------ ---- ----"
         FieldDone = -1
         GOSUB PrintIt
      END IF
      FieldDone = 0

   NEXT

   BLPrint GPDat(9), CHR$(12), ErrCount                'Send a Form feed

   EXIT SUB




PrintHeader:
   GOSUB PrintIt

   Page = Page + 1
   LSET PLine$ = ScrName$ + ":" + STR$(Form(0).Fields) + " fields, record length =" + STR$(Form(0).StorLen)
   MID$(PLine$, 72) = "PAGE:" + STR$(Page)
   GOSUB PrintIt

   LSET PLine$ = DATE$ + ", " + TIME$
   MID$(PLine$, 55) = STRING$(15, "-")
   GOSUB PrintIt

   LSET PLine$ = ""
   MID$(PLine$, 56) = "Related/Choice"
   GOSUB PrintIt

   LSET PLine$ = "Fld Offset   Name      Type    FldLen  RecLen Located   File   Field# Ndex Prot"
   GOSUB PrintIt

   LSET PLine$ = "=== ====== ======== ========== ======= ====== ======= ======== ====== ==== ===="
   GOSUB PrintIt

RETURN



PrintText:
   FOR N = 1 TO 10
      IF LEN(Temp$(N, 1)) THEN
         MID$(PLine$, 10) = Temp$(N, 1)
         GOSUB PrintIt
      END IF
      LSET PLine$ = ""
   NEXT
RETURN


PrintIt:

   IF INKEY$ <> "" THEN
      BLPrint GPDat(9), CHR$(12), ErrCount
      EXIT SUB
   END IF
   BLPrint GPDat(9), PLine$ + CHR$(13) + CHR$(10), ErrCount

   CountWas = 0
   DO WHILE ErrCount > -1

      REDIM Msg$(1)
      Msg$(0) = "Printer Error"
      Msg$(1) = "Printer Not Ready"
      ExitCode = MsgBox(Msg$(), 12)
      IF ExitCode <> 1 THEN EXIT SUB

      BLPrint GPDat(9), MID$(PLine$, ErrCount + CountWas + 1) + CHR$(13) + CHR$(10), ErrCount
      CountWas = CountWas + ErrCount
   LOOP

LCount = LCount + 1
IF LCount >= 56 AND FieldDone THEN
      LSET PLine$ = ""
      LCount = LCount - 66
      FOR N = LCount TO -1
          GOSUB PrintIt
      NEXT
      GOSUB PrintHeader
   END IF

RETURN

END SUB

