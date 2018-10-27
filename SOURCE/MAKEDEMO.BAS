DEFINT A-Z

'by Phil Cramer
'Copyright (c) 1992 Crescent Software

'$INCLUDE: 'FldInfo.Bi'

'---- BASIC subs
DECLARE SUB DemoType (FileName$, FRM%, SvLd%, Fs%)
DECLARE SUB FileThere (FileName$, Button%)
DECLARE SUB MakeFrmModule (SBName$, Fld() AS ANY, Calc$(), Help$())
DECLARE SUB InsertChars (Work$)

'---- ASM Subs
DECLARE SUB FCreate (FileName$)
DECLARE SUB FOpen (FileName$, Handle)
DECLARE SUB FPut (Handle, Work$)
DECLARE SUB FClose (Handle)
DECLARE SUB SortStr (BYVAL Address, NumEls, Dir)

'---- ASM Functions
DECLARE FUNCTION DOSError ()
DECLARE FUNCTION Exist% (FileSpec$)
DECLARE FUNCTION QPTrim$ (Work$)

'$INCLUDE: 'Common.GQS'

SUB InsertChars (Work$) STATIC

  Pointer = 1
  Strt = 0
  Nd = 0
  Old$ = CHR$(34)

  DO
    Pointer = INSTR(Pointer, Work$, Old$)

    IF Pointer THEN

      IF Pointer = 1 THEN
        New$ = "CHR$(34) + "
        Work$ = New$ + Work$
        Strt = -1
      ELSEIF Pointer = LEN(Work$) THEN
        New$ = " + CHR$(34)"
        Work$ = LEFT$(Work$, Pointer) + New$
        Nd = -1
      ELSE
        New$ = " + CHR$(34) + " + CHR$(34)
        Work$ = LEFT$(Work$, Pointer) + New$ + MID$(Work$, Pointer + 1)
      END IF

      Pointer = Pointer + LEN(New$) + 1

    END IF

  LOOP UNTIL Pointer = 0

  IF NOT Strt THEN Work$ = CHR$(34) + Work$
  IF NOT Nd THEN Work$ = Work$ + CHR$(34)

END SUB

SUB MakeDemo (FilName$, Fld() AS FieldInfoG, Calc$(), Help$()) STATIC

  'FileName$ should be base name w/no ext

  FileName$ = UCASE$(FilName$)
  DemoName$ = "DEMO" + LEFT$(FileName$, 4)

  DO
    OK = -1
    DemoType DemoName$, FRM, SvLd, Fs
    IF FRM = 2 THEN EXIT SUB
    DemoName$ = QPTrim$(UCASE$(DemoName$))
    IF NOT FRM AND DemoName$ = FileName$ THEN
      OK = 0
      BEEP
    END IF
  LOOP WHILE NOT OK

  FileThere DemoName$ + ".BAS", Button
  IF Button = 2 THEN EXIT SUB

  '---- Set up MAK file
  REDIM Mak$(1 TO 13)
  Mak$(1) = DemoName$ + ".BAS"
  Mak$(2) = "EDITFORM.BAS"
  Mak$(3) = "GDISPLAY.BAS"
  Mak$(4) = "NOCALC.BAS"
  Mak$(5) = "NOEVAL.BAS"
  Mak$(6) = "NONOTESG.BAS"
  Mak$(7) = "NOMULTG.BAS"
  Mak$(8) = "NOSCROLB.BAS"
  Mak$(9) = "NOSCROLL.BAS"
  IF FRM THEN
    Mak$(10) = "FRMFILE.BAS"
  ELSE
    Mak$(10) = FileName$ + ".BAS"
  END IF
  Mak$(12) = "GARRAYSZ.BAS"

  ScrollBars = 0
  ListBx$ = ""
  MouseFlds = 0
  FOR i = 1 TO UBOUND(Fld, 1)

    SELECT CASE Fld(i).FType
      CASE NotesFld
        Mak$(6) = "GQEDITS.BAS"
      CASE MultChAFld
        Mak$(7) = "LISTBOX.BAS"
        ScrollBars = -1
        ListBx$ = ListBx$ + LTRIM$(STR$(i)) + ", "
        Mak$(8) = "SCROLLB.BAS"
      CASE HScrollFld, VScrollFld
        ScrollBars = -1
        Mak$(8) = "SCROLLB.BAS"
      CASE ScrollFld
        Mak$(9) = "SCROLLIN.BAS"
      CASE MouseFld, PButton
        MouseFlds = -1
      CASE ELSE
    END SELECT

  NEXT

  '---- Any calculated fields?
  FOR i = 1 TO UBOUND(Calc$, 1)
    IF LEN(Calc$(i)) THEN
      Mak$(4) = "GQSCALC.BAS"
      IF Fld(i).FType > LogicalFld AND Fld(i).FType < DateFld THEN
        Mak$(5) = "EVALUATE.BAS"
        EXIT FOR
      END IF
    END IF
  NEXT

  SortStr BYVAL VARPTR(Mak$(2)), UBOUND(Mak$) - 1, 0

  CRLF$ = CHR$(13) + CHR$(10)
  FCreate DemoName$ + ".MAK"
  FOpen DemoName$ + ".MAK", Handle

  FOR i = 1 TO UBOUND(Mak$)
    FPut Handle, Mak$(i) + CRLF$
  NEXT

  FClose Handle
  ERASE Mak$

  '---- Create the Main module
  FCreate DemoName$ + ".BAS"
  FOpen DemoName$ + ".BAS", Handle

  '---- Generate Declare statements
  Partial = 0
  FPut Handle, "DEFINT A-Z" + CRLF$
  FPut Handle, CRLF$
  FPut Handle, "'$INCLUDE: 'FldInfo.bi'" + CRLF$
  FPut Handle, "'$INCLUDE: 'EditForm.BI'" + CRLF$
  FPut Handle, CRLF$

  FPut Handle, "'---- ASM subs" + CRLF$
  IF MouseFlds THEN FPut Handle, "DECLARE SUB DeInstallKeyDown()" + CRLF$
  FPut Handle, "DECLARE SUB InitMouse(There)" + CRLF$

  IF MouseFlds THEN FPut Handle, "DECLARE SUB InstallKeyDown()" + CRLF$

  FPut Handle, "DECLARE SUB ShowCursor ()" + CRLF$
  FPut Handle, "DECLARE SUB HideCursor ()" + CRLF$
  FPut Handle, "DECLARE SUB Motion (Sensitivity)" + CRLF$
  FPut Handle, CRLF$

  FPut Handle, "'---- ASM functions" + CRLF$
  IF FRM THEN
    FPut Handle, "DECLARE FUNCTION DOSError% ()" + CRLF$
    FPut Handle, "DECLARE FUNCTION WhichError% ()" + CRLF$
    FPut Handle, "DECLARE FUNCTION ErrorMsg$ (Er)" + CRLF$
  END IF
  FPut Handle, "DECLARE FUNCTION MultMonitor%" + CRLF$
  FPut Handle, CRLF$

  FPut Handle, "'---- BASIC subs" + CRLF$
  FPut Handle, "DECLARE SUB EditFormG (Form$(), Fld() AS FieldInfoG, Frm AS FormInfoG, Action)" + CRLF$

  IF FRM THEN
    FPut Handle, "DECLARE SUB GetFldDefG (FormName$, StartEl, Fld() AS ANY, Form$())" + CRLF$
    FPut Handle, "DECLARE FUNCTION NumFieldsG (FormName$)" + CRLF$
  ELSE
    FPut Handle, "DECLARE SUB " + FileName$ + " (Fld() AS FieldInfoG, Form$(), Start)" + CRLF$
  END IF
  FPut Handle, "DECLARE SUB ShowForm (FormName$, Fld() AS FieldInfoG, Row, Col, VPage, ErrCode)" + CRLF$
  FPut Handle, CRLF$

  FPut Handle, "'$INCLUDE: 'SetUp.BAS'" + CRLF$
  FPut Handle, CRLF$
  FPut Handle, CRLF$

  FPut Handle, "FormName$ = " + CHR$(34) + FileName$ + CHR$(34) + CRLF$
  FPut Handle, CRLF$
  IF FRM THEN

    FPut Handle, "NumFields = NumFieldsG(FormName$)" + CRLF$
    FPut Handle, CRLF$
    FPut Handle, "'---- Define mandatory arrays" + CRLF$
    FPut Handle, "DIM Frm AS FormInfoG                    'See 'EditForm.BI' for more info." + CRLF$
    FPut Handle, "REDIM Form$(NumFields, 2)               'Holds the data for the form" + CRLF$
    FPut Handle, "REDIM Fld(NumFields) AS FieldInfoG      'Holds information about the form" + CRLF$
    FPut Handle, "REDIM Choice$(0, 0)                     'Used to hold multiple choices" + CRLF$

    FPut Handle, CRLF$
    FPut Handle, "'---- Load the form definitions from disk" + CRLF$
    FPut Handle, "GetFldDefG FormName$, StartEl, Fld(), Form$()" + CRLF$

    FPut Handle, "IF DOSError THEN" + CRLF$
    FPut Handle, "  Print ErrorMsg$(WhichError)" + CRLF$
    FPut Handle, "  END" + CRLF$
    FPut Handle, "END IF" + CRLF$
    FPut Handle, CRLF$

  ELSE

    FPut Handle, "'---- Define mandatory arrays" + CRLF$
    FPut Handle, "DIM Frm AS FormInfoG                    'See 'EditForm.BI' for more info." + CRLF$
    FPut Handle, "REDIM Form$(0, 0)                       'Holds the data for the form" + CRLF$
    FPut Handle, "REDIM Fld(0) AS FieldInfoG              'Holds information about the form" + CRLF$
    FPut Handle, "REDIM Choice$(0, 0)                     'Used to hold multiple choices" + CRLF$
    
    FPut Handle, FileName$ + " Fld(), Form$(), Start           'Assign the form definition" + CRLF$
    FPut Handle, CRLF$

  END IF

  LL = LEN(ListBx$)
  IF LL THEN
    ListBx$ = LEFT$(ListBx$, LL - 2)
    FPut Handle, "REDIM Choice$(9, 0)" + CRLF$
    FPut Handle, "Choice$(0, 0) = " + CHR$(34) + ListBx$ + CHR$(34) + CRLF$
    FOR i = 1 TO 9
      FPut Handle, "Choice$(" + LTRIM$(STR$(i)) + ", 0) = " + CHR$(34) + "Choice" + STR$(i) + " " + CHR$(34) + CRLF$
    NEXT
    FPut Handle, CRLF$
  END IF

  Row$ = LTRIM$(STR$(Fld(0).Row))       'Holds partial screen's UL row
  Col$ = LTRIM$(STR$(Fld(0).LCol))      'Holds partial screen's UL column

  IF GPDat(31) = 5 THEN
    Mon$ = "an EGA or VGA monitor."
    VPage = 1
  ELSE
    Mon$ = "a VGA monitor."
    VPage = 0
  END IF

  FPut Handle, "ShowForm FormName$, Fld(), " + Row$ + ", " + Col$ + "," + STR$(VPage) + ", ErrorCode  'Display the form" + CRLF$
  FPut Handle, "SELECT CASE ErrorCode                       'Check for Errors" + CRLF$
  FPut Handle, "  CASE 1" + CRLF$
  FPut Handle, "    PRINT " + CHR$(34) + "Sorry, this form requires " + Mon$ + CHR$(34) + CRLF$
  FPut Handle, "    END" + CRLF$
  FPut Handle, "  CASE 2" + CRLF$
  FPut Handle, "    PRINT " + CHR$(34) + "An error occurred trying to load " + FileName$ + ".PCX" + CHR$(34) + CRLF$
  FPut Handle, "    END" + CRLF$
  FPut Handle, "END SELECT" + CRLF$
  FPut Handle, CRLF$

  IF GPDat(31) = 5 THEN FPut Handle, "PCOPY 1, 0" + CRLF$
  FPut Handle, CRLF$

  IF MouseFlds THEN FPut Handle, "InstallKeyDown                          'Install the KeyDown TSR" + CRLF$

  FPut Handle, "InitMouse There                         'Initialize the mouse" + CRLF$
  FPut Handle, "ShowCursor                              'Turn on the mouse cursor" + CRLF$
  FPut Handle, "Motion" + STR$(GPDat(4)) + "                                'Set the mouse sensitivity" + CRLF$
  FPut Handle, CRLF$

  FPut Handle, "Action = 1" + CRLF$
  FPut Handle, "'---- The following is the form editing loop" + CRLF$

  FPut Handle, "DO" + CRLF$
  FPut Handle, "  EditFormG Form$(), Fld(), Frm, Action" + CRLF$

  '---- Set up SELECT CASE statements for Push buttons, Mouse fields
  IF MouseFlds THEN
    FPut Handle, CRLF$
    FPut Handle, "    SELECT CASE Frm.KeyCode" + CRLF$
    FPut Handle, CRLF$

    FOR i = 1 TO UBOUND(Fld, 1)
      IF Fld(i).FType = MouseFld OR Fld(i).FType = PButton THEN
        FPut Handle, "      CASE " + LTRIM$(STR$(Fld(i).Value)) + "     'Key code for " + Fld(i).FldName + CRLF$
        FPut Handle, "        'Your key handling code goes here" + CRLF$
        FPut Handle, CRLF$
      END IF
    NEXT

    FPut Handle, "    END SELECT" + CRLF$
    FPut Handle, CRLF$
  END IF

  IF ScrollBars THEN
    IF NOT MouseFlds THEN FPut Handle, CRLF$
    FPut Handle, "    SELECT CASE Frm.FldNo" + CRLF$
    FPut Handle, CRLF$

    FOR i = 1 TO UBOUND(Fld, 1)
      IF Fld(i).FType = HScrollFld OR Fld(i).FType = VScrollFld THEN
        FPut Handle, "      CASE " + LTRIM$(STR$((i))) + "     'Field number for " + Fld(i).FldName + CRLF$
        FPut Handle, "        'Fld(" + LTRIM$(STR$(i)) + ").Value returns the scroll bar setting" + CRLF$
        FPut Handle, CRLF$
      END IF
    NEXT

    FPut Handle, "    END SELECT" + CRLF$
    FPut Handle, CRLF$

  END IF

  FPut Handle, "LOOP UNTIL Frm.KeyCode = 27" + CRLF$ + CRLF$
  IF MouseFlds THEN FPut Handle, "DeInstallKeyDown                        'UnInstall the KeyDown TSR" + CRLF$

  FClose Handle
  ERASE Mak$

  IF FRM = 0 THEN MakeFrmModule FileName$, Fld(), Calc$(), Help$()

END SUB

SUB MakeFrmModule (FilNam$, Fld() AS FieldInfoG, Calc$(), Help$())

  CRLF$ = CHR$(13) + CHR$(10)
  FileName$ = FilNam$ + ".BAS"

  Fd$ = "  Fd(El)"
  H$ = "  F$(El, 1)"
  C$ = "  F$(El, 2)"

  FCreate FileName$
  FOpen FileName$, Handle
  IF DOSError THEN
    BEEP
    EXIT SUB
  END IF

  Fld(0).Fields = 0
  Fld(0).StorLen = 0
  N = 1

  DO WHILE Fld(N).RCol <> 0 AND Fld(0).Fields < MaxFields
    Fld(0).StorLen = Fld(0).StorLen + Fld(N).StorLen
    Fld(0).Fields = Fld(0).Fields + 1
    N = N + 1
  LOOP

  FPut Handle, "DEFINT A-Z" + CRLF$
  FPut Handle, "DECLARE SUB Tokenize (Calc$, Fld() AS ANY)" + CRLF$
  FPut Handle, CRLF$
  FPut Handle, "'$INCLUDE: 'FldInfo.Bi'" + CRLF$
  FPut Handle, "'$INCLUDE: 'Common.Bi'" + CRLF$

  FPut Handle, CRLF$

  FPut Handle, "DEFINT A-Z" + CRLF$
  FPut Handle, "SUB " + FilNam$ + " (Fd() AS FieldInfoG, F$(), Start)" + CRLF$
  FPut Handle, CRLF$

  FPut Handle, "  REDIM Fd(" + LTRIM$(STR$(Fld(0).Fields)) + ") AS FieldInfoG     'Array to hold field definitions" + CRLF$
  FPut Handle, "  REDIM F$(" + LTRIM$(STR$(Fld(0).Fields)) + ", 2)               'Array$ to hold responses, help, formulas" + CRLF$

  FPut Handle, CRLF$
  FPut Handle, "  Fd(0)" + ".Fields = " + LTRIM$(STR$(Fld(0).Fields)) + CRLF$
  FPut Handle, "  Fd(0)" + ".StorLen = " + LTRIM$(STR$(Fld(0).StorLen)) + CRLF$
  FPut Handle, "  Fd(0)" + ".Indexed = " + LTRIM$(STR$(GPDat(71))) + CRLF$
  FPut Handle, "  Fd(0)" + ".Value = " + LTRIM$(STR$(GPDat(31))) + CRLF$
  FPut Handle, "  Fd(0)" + ".Row = " + LTRIM$(STR$(Fld(0).Row)) + CRLF$
  FPut Handle, "  Fd(0)" + ".LCol = " + LTRIM$(STR$(Fld(0).LCol)) + CRLF$

  FPut Handle, CRLF$
  FPut Handle, "  El = Start" + CRLF$
  FPut Handle, CRLF$

  FOR i = 1 TO Fld(0).Fields
    IF Fld(i).RCol THEN

      FPut Handle, "  El = El + 1" + CRLF$

      '---- Field name
      FPut Handle, Fd$ + ".FldName = " + CHR$(34) + QPTrim$(Fld(i).FldName) + CHR$(34) + CRLF$
      '---- Field type
      FPut Handle, Fd$ + ".FType = " + LTRIM$(STR$(Fld(i).FType)) + CRLF$
      '---- Field Row
      FPut Handle, Fd$ + ".Row = " + LTRIM$(STR$(Fld(i).Row)) + CRLF$
      '---- LCol
      FPut Handle, Fd$ + ".LCol = " + LTRIM$(STR$(Fld(i).LCol)) + CRLF$
      '---- RCol
      FPut Handle, Fd$ + ".RCol = " + LTRIM$(STR$(Fld(i).RCol)) + CRLF$
      '---- Protected
      FPut Handle, Fd$ + ".Protected = " + LTRIM$(STR$(Fld(i).Protected)) + CRLF$
      '---- Value
      FPut Handle, Fd$ + ".Value = " + LTRIM$(STR$(Fld(i).Value)) + CRLF$
      '---- Field Offset with Form$(0, 0)
      FPut Handle, Fd$ + ".Fields = " + LTRIM$(STR$(Fld(i).Fields)) + CRLF$

      IF Fld(i).FType < MouseFld AND Fld(i).FType <> NotesFld THEN
        FPut Handle, Fd$ + ".Indexed = " + LTRIM$(STR$(Fld(i).Indexed)) + CRLF$
      END IF

      '---- StorLen
      IF Fld(i).StorLen THEN FPut Handle, Fd$ + ".StorLen = " + LTRIM$(STR$(Fld(i).StorLen)) + CRLF$

      IF Fld(i).FType = RelFld THEN
        '---- RelFile
        FPut Handle, Fd$ + ".RelFile = " + CHR$(34) + Fld(i).RelFile + CHR$(34) + CRLF$
        '---- RelFld
        FPut Handle, Fd$ + ".RelFld = " + LTRIM$(STR$(Fld(i).RelFld)) + CRLF$
        '---- RelHandle
        FPut Handle, Fd$ + ".RelHandle = " + LTRIM$(STR$(Fld(i).RelHandle)) + CRLF$
        '---- RelType
        'FPut Handle, Fd$  + ".RelType = " + Fld(i).RelType + CRLF$
      END IF

      '---- Decimals
      IF Fld(i).Decimals THEN FPut Handle, Fd$ + ".Decimals = " + LTRIM$(STR$(Fld(i).Decimals)) + CRLF$

      '---- ScratchI
      IF Fld(i).ScratchI THEN FPut Handle, Fd$ + ".ScratchI = " + LTRIM$(STR$(Fld(i).ScratchI)) + CRLF$

      '---- Low/Hi Values
      IF Fld(i).LowRange THEN FPut Handle, Fd$ + ".LowRange = " + LTRIM$(STR$(Fld(i).LowRange)) + CRLF$
      IF Fld(i).HiRange THEN FPut Handle, Fd$ + ".HiRange = " + LTRIM$(STR$(Fld(i).HiRange)) + CRLF$

      '---- Yes/No Characters
      IF Fld(i).FType = LogicalFld THEN
        YN$ = MID$(Fld(i).RelFile, 1, 1) + " " + MID$(Fld(i).RelFile, 3, 1)
        FPut Handle, Fd$ + ".RelFile = " + CHR$(34) + YN$ + CHR$(34) + CRLF$
      END IF

      '---- Currency
      IF Fld(i).FType = MoneyFld THEN
        FPut Handle, Fd$ + ".ScratchS = " + CHR$(34) + Fld(i).ScratchS + CHR$(34) + CRLF$
      END IF

      SELECT CASE Fld(i).FType
        CASE HScrollFld, VScrollFld
          FPut Handle, Fd$ + ".RelFld = " + LTRIM$(STR$(Fld(i).RelFld)) + CRLF$
          FPut Handle, Fd$ + ".Indexed = " + LTRIM$(STR$(Fld(i).Indexed)) + CRLF$
          FPut Handle, Fd$ + ".RelHandle = " + LTRIM$(STR$(Fld(i).RelHandle)) + CRLF$
      END SELECT

      SELECT CASE Fld(i).FType
        CASE ScrollFld, MouseFld
        FPut Handle, Fd$ + ".RelFld = " + LTRIM$(STR$(Fld(i).RelFld)) + CRLF$
      END SELECT

      '---- Field formulas
      IF LEN(Calc$(i)) THEN
        Temp$ = QPTrim$(Calc$(i))
        InsertChars Temp$
        FPut Handle, C$ + " = " + Temp$ + CRLF$
        FPut Handle, "  Tokenize " + C$ + ", Fd()" + CRLF$
      END IF

      '---- Help messages
      IF LEN(Help$(i)) THEN
         Temp$ = QPTrim(Help$(i))
         InsertChars Temp$
         FPut Handle, H$ + " = " + Temp$ + CRLF$
      END IF

      FPut Handle, CRLF$

    END IF

  NEXT

  Temp$ = ""

  FPut Handle, "END SUB " + CRLF$
  FClose Handle

END SUB

