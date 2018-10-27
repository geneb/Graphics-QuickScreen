DEFINT A-Z

'by Phil Cramer
'Copyright (c) 1992 Crescent Software

'-----BASIC subs
DECLARE SUB BkGround (Row%, Rows%, Wide%, Dir%)
DECLARE SUB CheckMem (Chk, Amt&)
DECLARE SUB CursorXY (x%, y%, Button%, Ky%)
DECLARE SUB DrawCursor (x%, y%, CursorWidth%, CursorLength%, CursorIs%, CursorTime%)
DECLARE SUB GDialog (Choice%, DLog() AS ANY, TResponse$(), Menu$(), Style, Action%)
DECLARE SUB GEditor (Edit$, LeftCol%, Ky$, TxtPos%, Accept%, Button%, HotKeyTbl%())
DECLARE SUB GetFldType (TChoice, FldNo%, FldType%, BClr%, TxtClr%, HiClr%)
DECLARE SUB GQSMessage (A$(), Row, Dir)
DECLARE SUB LoadFiles (Spec$, FileMenu$(), Found)
DECLARE SUB MakeNewFldName (OldName$)
DECLARE SUB MakeUniqueFldName (FldNo%, Fld() AS ANY)
DECLARE SUB MouseBox (X1%, Y1%, lastx2%, lasty2%, LeaveIt%, Button%, Clr%)
DECLARE SUB PCBox (X1, Y1, X2, Y2, Outside)
DECLARE SUB StepBox (x, y, Wide, hite, Clr)
DECLARE SUB DeleteT (SEG Element AS ANY, ElSize, NumEls)
DECLARE SUB DeleteStr (BYVAL Address, NumEls)
DECLARE SUB InsertT (SEG Element AS ANY, ElSize, NumEls)
DECLARE SUB InsertStr (BYVAL Address, Ins$, NumEls)

'-----BASIC functions
DECLARE FUNCTION Eval# (N$, ErrCode)
DECLARE FUNCTION ES$ (D$)
DECLARE FUNCTION GetXORClr% (DesiredClr%, BGClr%)
DECLARE FUNCTION IsDup% (FldName$, Fld() AS ANY, FldNo)
DECLARE FUNCTION MsgBox (Text$(), Opts)

'-----.ASM subs
DECLARE SUB FClose (Handle)
DECLARE SUB Find2 (BYVAL Element, NumEls, Search$)
DECLARE SUB FOpen (Filename$, Handle)
DECLARE SUB FGet (Handle, Dest$)
DECLARE SUB FGetT (Handle, Dest AS ANY, NumBytes)
DECLARE SUB FSeek (Handle, Location&)
DECLARE SUB WaitUp ()

'Assembler functions
DECLARE FUNCTION CompareT (SEG First AS ANY, SEG Second AS ANY, NumBytes)
DECLARE FUNCTION Date2Num (D$)
DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION ErrorMsg$ (Er)
DECLARE FUNCTION GetDrive ()
DECLARE FUNCTION GetDir$ (Drive$)
DECLARE FUNCTION MinInt (A, B)
DECLARE FUNCTION MaxInt (A, B)
DECLARE FUNCTION Num2Date$ (N)
DECLARE FUNCTION QPTrim$ (A$)
DECLARE FUNCTION WhichError ()
DECLARE FUNCTION TrapInt (Num, Low, Hi)
DECLARE FUNCTION Null (Work$)
DECLARE FUNCTION PDQTimer& ()

'---- GW
DECLARE SUB LineBVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB LineBFVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB GetCursor (x%, y%, Button%)
DECLARE SUB ShowCursor ()
DECLARE SUB HideCursor ()
DECLARE SUB GPrint0VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GMove2VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)

DECLARE FUNCTION GetPointVE (BYVAL x%, BYVAL y%)

'$INCLUDE: 'FldInfo.bi'
'$INCLUDE: 'DLogType.Bi'
'$INCLUDE: 'Common.GQS'

'Choice titles for field type dialog box
Headings:
DATA String, Proper String, UCase String, Numeric Text, MultiLine Text, Scrolling Text
DATA Logical, Integer, Long Integer, Single Precision, Double Precision
DATA Currency, Date MM-DD-Y, Date DD-MM-Y, Phone Number, Zip Code, Social Security
DATA Relational..., Multiple Choice, Mouse Region, Push Button, Scroll Bar

FUNCTION ES$ (Dt$)

  Tmp$ = Dt$
  MID$(Tmp$, 1, 2) = MID$(Dt$, 4, 2)
  MID$(Tmp$, 4, 2) = MID$(Dt$, 1, 2)
  ES$ = Tmp$

END FUNCTION

SUB GetDefs (FldNo, NumFlds, Calc$(), Help$(), Fld() AS FieldInfoG) STATIC

Chk = 0
CheckMem Chk, 4000
IF Chk THEN EXIT SUB

IF GPDat(71) = 8 THEN Space8 = 2

TextClr = 0 + 7 * 256
BxClr = 15 + 24 * 256

FLen$ = SPACE$(2)                          'Space to hold field length display
FHt$ = SPACE$(2)                           'Space to hold field height display
Row$ = SPACE$(2)
Col$ = SPACE$(2)
FldNo$ = SPACE$(3)

UpperB = UBOUND(Fld, 1)

CursorTiming = 6                           'Make cursor blink 3 times a second
MinLen = 8
FWidth = 8                                 'Default starting width in pixels
BHite = 7

DIM Test AS FieldInfoG
DIM Blank AS FieldInfoG
DIM TempFld AS FieldInfoG

REDIM Name$(1 TO 23)

Name$(1) = "STRING"
Name$(2) = "PROPSTR"
Name$(3) = "UCASE"
Name$(4) = "NUMERIC"
Name$(5) = "MULTI"
Name$(6) = "SCROLL"

Name$(7) = "LOGIC"
Name$(8) = "INTEGER"
Name$(9) = "LONGINT"
Name$(10) = "SINGLE"
Name$(11) = "DOUBLE"

Name$(12) = "MONEY"
Name$(13) = "USDATE"
Name$(14) = "EDATE"
Name$(15) = "PHONE"
Name$(16) = "ZIP"
Name$(17) = "SOSEC"

Name$(18) = "RELATE"
Name$(19) = "LIST"
Name$(20) = "MOUSE"
Name$(21) = "BUTTON"
Name$(22) = "HSCROLL"
Name$(23) = "VSCROLL"

Lower = -1                   'Draw it first in the lower half of the screen

Over:
DO

'***************************************************************************
'*                1.Get Starting Row and Column for Fields                 *
'***************************************************************************
EnRow = 0

FldNo = MinInt(FldNo, Fld(0).Fields + 1)
IF FldNo > MaxFields THEN
  FldNo = MaxFields
  BEEP
  REDIM Msg$(1)
  Msg$(0) = "End of Form"
  Msg$(1) = "Last available field has been used!"
  Button = MsgBox(Msg$(), 21)
END IF

GOSUB LocateCsr

RSET Row$ = QPTrim$(STR$(CurRow \ GPDat(71) + 1))
RSET Col$ = QPTrim$(STR$(CurCol \ 8 + 1))
RSET FldNo$ = QPTrim$(STR$(FldNo))

REDIM A$(5)
A$(1) = "  Move the cursor to the beginning of Field# " + FldNo$
A$(2) = "  Then press [Enter]. Press [F10] when finished  "
A$(3) = ""
A$(4) = "  [Ins]-Insert Field, [Del]-Delete Field  Row:"' + Row$
A$(5) = "  (-)-Previous Field, (+)-Next Field      Col:"' + Col$

IF FRow > GPDat(85) \ 2 THEN
  FLRow = 4
  Lower = 0
ELSE
  FLRow = GPDat(85) - 8
  Lower = -1
END IF

GQSMessage A$(), FLRow, 0

BoxCol = 61 * 8 - 2                        'Draw a box around the field number
HideCursor
PCBox BoxCol, FLRow * GPDat(71) - GPDat(71) - 1, BoxCol + 27, FLRow * GPDat(71), 0
ShowCursor

DO
                                           'Draw and blink the cursor
  CALL DrawCursor(CurCol, CurRow, 8, GPDat(71) - 1, CursorIs, CursorTiming)

  Ky$ = INKEY$
  SELECT CASE LEN(Ky$)
    CASE 0
      Ky = 0
    CASE 1
      Ky = ASC(Ky$)                        'Normal keys
    CASE 2
      Ky = -ASC(RIGHT$(Ky$, 1))            'Extended keys
    CASE ELSE
  END SELECT

  CALL GetCursor(c, r, Button)
  IF Button = 1 THEN
    Pressed = -1
    IF c > BoxCol AND c < BoxCol + 27 AND r > FLRow * GPDat(71) - GPDat(71) - 1 AND r < FLRow * GPDat(71) - 1 THEN
      Ky = 9
    ELSE
      GOSUB MoveCursor
      CurCol = (c \ 8) * 8
      CurRow = (r \ GPDat(71)) * GPDat(71)
    END IF
  ELSEIF Button >= 2 THEN
    Ky = 27
  END IF

  IF Button = 0 AND Pressed THEN
    Pressed = 0
    t& = PDQTimer&
    IF t& - LastT& < 6 THEN Ky = 13        'DoubleClick
    LastT& = PDQTimer&
  END IF

  IF Ky <> 0 THEN
  GOSUB MoveCursor
  SELECT CASE Ky

    CASE 27, -68                           'F10  Done, Escape
      HideCursor                           'Restore the screen
      GOSUB MoveCursor
      GQSMessage A$(), FLRow, -1
      ShowCursor
      EXIT SUB                             'Outta here

    CASE 9                                 'Tab Select new field
      GOSUB MoveCursor
      REDIM HKT(0)
      TxtPos = 1
      Accept = 11
      LOCATE FLRow, 62
      RSET FldNo$ = QPTrim$(STR$(FldNo))
      GEditor FldNo$, 62, Ky1$, TxtPos, Accept, Button, HKT()
      WaitUp
      TFldNo = VAL(FldNo$)

      SELECT CASE TFldNo
        CASE IS = NumFlds
          RSET FldNo$ = STR$(NumFlds)

        CASE IS >= NumFlds + 1
          TFldNo = NumFlds + 1
          RSET FldNo$ = STR$(TFldNo)
          Ky1$ = ""

        CASE IS < 1
          TFldNo = 1
          RSET FldNo$ = "1"
          Ky1$ = ""

        CASE ELSE
          RSET FldNo$ = STR$(TFldNo)
      END SELECT

      FldNo = TFldNo
      LSET A$(1) = "  Move the cursor to the beginning of Field# " + FldNo$

      RSET FldNo$ = QPTrim$(STR$(FldNo))
      GPrint0VE FLRow, 62, FldNo$, TextClr

      GOSUB LocateCsr

    CASE -72     'Up  arrow
      CurRow = CurRow - GPDat(71)

    CASE -56     'Shift/Up  arrow
      CurRow = CurRow - GPDat(71) * 5

    CASE -80      'Down
      CurRow = CurRow + GPDat(71)

    CASE -50      'Shift/Down
      CurRow = CurRow + GPDat(71) * 5

    CASE -77      'Right
      CurCol = CurCol + 8

    CASE -54      'Shift/Right
      CurCol = CurCol + 8 * 10

    CASE -75      'Left
      CurCol = CurCol - 8

    CASE -52      'Shift/Left
      CurCol = CurCol - 8 * 10

    CASE -53      'Shift/Center
      CurCol = 40 * 8
      CurRow = (GPDat(85) \ 2) * GPDat(71)

    CASE -79      'End
      CurCol = 79 * 8

    CASE -81      'PgDn
      CurRow = GPDat(85) * GPDat(71) - GPDat(71)

    CASE -73      'PgUp
      CurRow = 0

    CASE -71      'Home
      CurCol = 0

    CASE -117
      FldNo = MaxInt(1, Fld(0).Fields)
      GOSUB ShowFldNo

    CASE -119
      FldNo = 1
      GOSUB ShowFldNo

    CASE 45       '"-"  Go back a Field
      FldNo = FldNo - 1
      IF FldNo < 1 THEN FldNo = 1
      GOSUB ShowFldNo

    CASE 43       '"+"  Go ahead a Field
      FldNo = FldNo + 1
      IF FldNo > NumFlds + 1 THEN FldNo = NumFlds + 1
      IF FldNo > MaxFields THEN FldNo = MaxFields
      GOSUB ShowFldNo

    CASE -82      'Insert field

      IF FldNo < NumFlds THEN
        SOUND 500, 1
        SOUND 1000, 1

        IF Fld(0).Fields < MaxFields THEN
          FOR N = MaxFields TO FldNo + 1 STEP -1
            Fld(N) = Fld(N - 1)
            Calc$(N) = Calc$(N - 1)
            Help$(N) = Help$(N - 1)
          NEXT N
          Fld(FldNo) = Blank

          GPDat(84) = 1

          Fld(0).Fields = Fld(0).Fields + 1
          NumFlds = NumFlds + 1
        ELSE
          BEEP
          GQSMessage A$(), FLRow, -1       'Erase
          REDIM Msg$(1)
          Msg$(1) = "Too many fields!"
          Button = MsgBox(Msg$(), 11)
          GQSMessage A$(), FLRow, 0        'Restore
        END IF
      END IF

    CASE -83       'Delete Field

      IF FldNo <= NumFlds THEN
        GOSUB MoveCursor
        SOUND 1000, 1
        SOUND 500, 1

        IF GPDat(25) THEN                  'clear deleted field
          HideCursor
          SELECT CASE Fld(FldNo).FType
            CASE NotesFld
              LineBFVE Fld(FldNo).LCol * 8 - 8, Fld(FldNo).Row * GPDat(71) - GPDat(71), Fld(FldNo).RCol * 8 - 1, Fld(FldNo).ScratchI * GPDat(71) - 1, GPDat(57)
            CASE IS > MultChAFld
              LineBFVE Fld(FldNo).LCol, Fld(FldNo).Row, Fld(FldNo).RCol, Fld(FldNo).ScratchI, GPDat(57)
            CASE ELSE
              LineBFVE Fld(FldNo).LCol * 8 - 8, Fld(FldNo).Row * GPDat(71) - GPDat(71), Fld(FldNo).RCol * 8 - 1, Fld(FldNo).Row * GPDat(71) - 1, GPDat(57)
          END SELECT
          ShowCursor
        END IF

        FOR N = FldNo TO MaxFields - 1
          Fld(N) = Fld(N + 1)
          Calc$(N) = Calc$(N + 1)
          Help$(N) = Help$(N + 1)
        NEXT N
        Fld(MaxFields) = Blank
        Fld(0).Fields = MaxInt(Fld(0).Fields - 1, 0)
        FldNo = MinInt(FldNo, Fld(0).Fields)
        FldNo = MaxInt(FldNo, 1)

        RSET FldNo$ = QPTrim$(STR$(FldNo))
        HideCursor
        GPrint0VE FLRow, 62, FldNo$, TextClr
        ShowCursor

        GOSUB LocateCsr
        GPDat(84) = 1

        NumFlds = NumFlds - 1

      END IF

    CASE 13       'Enter                   'Accepted a starting location
      GOSUB MoveCursor
      BegCol = CurCol \ 8 + 1              'Save the row
      BegRow = CurRow \ GPDat(71) + 1      ' and the starting column
      GQSMessage A$(), FLRow, -1
      TEndCol = Fld(FldNo).RCol

      EXIT DO

    CASE ELSE

  END SELECT
  IF CurRow < 0 THEN CurRow = 0
  IF CurRow > GPDat(85) * GPDat(71) - GPDat(71) THEN CurRow = GPDat(85) * GPDat(71) - GPDat(71)
  IF CurCol > GPDat(43) - 8 THEN CurCol = GPDat(43) - 8
  IF CurCol < 0 THEN CurCol = 0

  END IF

  IF CurRow < GPDat(44) \ 2 AND NOT Lower THEN  'If
    GOSUB MoveCursor
    GQSMessage A$(), FLRow, -1             'Erase the old message
    FLRow = GPDat(85) - 8
    GQSMessage A$(), FLRow, 0              'Draw the new message
    HideCursor
    PCBox BoxCol, FLRow * GPDat(71) - GPDat(71) - 1, BoxCol + 27, FLRow * GPDat(71), 0
    ShowCursor
    Lower = -1
  ELSEIF CurRow > GPDat(44) \ 2 AND Lower THEN
    GOSUB MoveCursor
    GQSMessage A$(), FLRow, -1             'Draw the message
    FLRow = 4
    GQSMessage A$(), FLRow, 0              'Draw the message
    HideCursor
    PCBox BoxCol, FLRow * GPDat(71) - GPDat(71) - 1, BoxCol + 27, FLRow * GPDat(71), 0
    ShowCursor
    Lower = 0
  END IF

  IF CurRow <> LastRow OR CurCol <> LastCol THEN
    RSET Row$ = LTRIM$(STR$(CurRow \ GPDat(71) + 1))
    RSET Col$ = LTRIM$(STR$(CurCol \ 8 + 1))
    HideCursor
    GPrint0VE FLRow + 3, 63, Row$, TextClr
    GPrint0VE FLRow + 4, 63, Col$, TextClr
    ShowCursor
  END IF
  LastRow = CurRow
  LastCol = CurCol

LOOP


'***************************************************************************
'*                     2.Set up Field Type Dialog box                      *
'***************************************************************************

REDIM DLog(30) AS Dialog
REDIM Menu$(0)

'---- Set up Dialog Box

DLog(0).Heading = "Select Field Type"

FOR i = 1 TO 2
  DLog(i).DType = 6                        'Push Button
  DLog(i).Row = 2 * GPDat(71) - GPDat(71) \ 2 - Space8 - 1
  DLog(i).NumRows = GPDat(80)
  DLog(i).TextLen = 66
NEXT

DLog(1).Col = 214
DLog(1).Heading = "OK"

DLog(2).Col = 294
DLog(2).Heading = "Cancel"
DLog(2).Group = 1000

THead$ = "Current Field#: " + LTRIM$(STR$(FldNo))
DLog(3).DType = 5
LSET DLog(3).Heading = THead$
DLog(3).Row = GPDat(71)
DLog(3).Col = 4

OS = 1
Col = 4

RESTORE Headings
FOR i = 4 TO 25
  READ DLog(i).Heading
  DLog(i).DType = 2
  DLog(i).Group = 1
  DLog(i).Col = Col
  DLog(i).Row = i + OS
  IF i = 9 THEN OS = 3
  IF i = 14 THEN
    Col = 28
    OS = -10
  END IF
  IF i = 20 THEN OS = -8
NEXT

DLog(4).HotKey = 1
DLog(7).HotKey = 10
DLog(9).HotKey = 2
DLog(15).HotKey = 1
DLog(16).HotKey = 11
DLog(17).HotKey = 6
DLog(23).HotKey = 4

DLog(19).TextLen = 8

DLog(26).DType = 7
DLog(26).Heading = "Text Fields"
DLog(26).Col = 1
DLog(26).Row = 4
DLog(26).TextLen = 22
DLog(26).NumRows = 6

DLog(27).DType = 7
DLog(27).Heading = "Numeric"
DLog(27).Col = 1
DLog(27).Row = 12
DLog(27).TextLen = 22
DLog(27).NumRows = 5

DLog(28).DType = 7
DLog(28).Heading = "Formatted"
DLog(28).Col = 25
DLog(28).Row = 4
DLog(28).TextLen = 21
DLog(28).NumRows = 6

DLog(29).DType = 7
DLog(29).Heading = "Special"
DLog(29).Col = 25
DLog(29).Row = 12
DLog(29).TextLen = 21
DLog(29).NumRows = 5

DLog(30).DType = 7
DLog(30).Col = 1
DLog(30).Row = 1
DLog(30).TextLen = 22
DLog(30).NumRows = 1

REDIM Response$(30)
Response$(3) = SPACE$(3)

RespEl = Fld(FldNo).FType + 3              'Find which field to check
IF RespEl = 3 THEN RespEl = 4              'Make sure to start at least at 1
IF RespEl = 26 THEN RespEl = 25

Response$(RespEl) = "O"                    'Check the field type
RSET Response$(3) = STR$(FldNo)            'Put the field number in the buffer
LResponse$ = Response$(3)                  'Keep a copy for later
Choice = 1

CALL GDialog(Choice, DLog(), Response$(), Menu$(), Style, 0)

IF Choice = 2 OR Choice = 27 THEN
  EXIT SUB
END IF

'***************************************************************************
'*                         3.Handle Field selection                        *
'***************************************************************************

LastFieldType = Fld(FldNo).FType
IF LastFieldType = VScrollFld THEN LastFieldType = HScrollFld

FOR i = 4 TO 25
  IF Response$(i) = "O" THEN EXIT FOR
NEXT
FldType = i - 3
MainType = FldType


'---- Clear the field definition if it was changed to a new field type
IF LastFieldType > 0 AND FldType <> LastFieldType THEN
  Fld(FldNo) = Blank
  Help$(FldNo) = ""
  Calc$(FldNo) = ""
  NewFld = -1
END IF


StartLen = 8

SELECT CASE FldType

  CASE 1 TO 18

    SELECT CASE FldType

      CASE LogicalFld                      'Logical, 1 character
        MaxLen = 8
        MinLen = 8
        TEndCol = BegCol

      CASE DateFld, EuroDateFld, ZipFld    'Date/Zip field, 10 characters
        StartLen = 80
        MinLen = StartLen
        MaxLen = StartLen
        TEndCol = BegCol + 9

      CASE SoSecFld                        'Social Security field, 9 characters
        StartLen = 88
        MinLen = StartLen
        MaxLen = StartLen
        TEndCol = BegCol + 10

      CASE PhoneFld                        'Phone field, 14 characters
        StartLen = 112
        MinLen = StartLen
        MaxLen = StartLen
        TEndCol = BegCol + 13

      CASE Relational                      'Relational field
        MinLen = 8
        MaxLen = (81 - Fld(FldNo).LCol) * 8

        '***************** Get Relational File ******************
GetRelFile:
        REDIM RDLog(4) AS Dialog
        REDIM RlResp$(4)
        REDIM RelMenu$(0)

        RelExt$ = ".FRM"
        RDLog(3).Heading = "Select the file for relation: <"
        RDLog(0).Heading = "Files"

        Drive$ = CHR$(GetDrive)
        Path$ = GetDir$(Drive$)

        IF Path$ = "\" THEN
          BSlash$ = ""
        ELSE
          BSlash$ = "\"
        END IF
        Spec$ = Drive$ + ":" + Path$ + BSlash$ + "*.FRM"
        CALL LoadFiles(Spec$, RelMenu$(), Found)

        IF NOT Found THEN
          REDIM Msg$(2)
          Msg$(0) = "Warning"
          Msg$(1) = "There are no .FRM files in the"
          Msg$(2) = "current directory to relate to!"
          Butn = MsgBox(Msg$(), 11)
          GOTO Over
        ELSE
          NumEls = UBOUND(RelMenu$, 1)

          Find2 BYVAL VARPTR(RelMenu$(0)), NumEls, Fld(FldNo).RelFile

          IF NumEls = -1 THEN NumEls = 0
          RDLog(3).MenuChoice = NumEls

          GOSUB Relational
          IF RelChoice = 2 OR RelChoice = 27 THEN
            EXIT SUB
          ELSE
            El = VAL(RlResp$(3))

            IF El = 0 THEN
              REDIM Msg$(1)
              Msg$(0) = "Warning"
              Msg$(1) = "You must select a file to relate to!"
              Butn = MsgBox(Msg$(), 11)
              GOTO GetRelFile
            END IF

            RelFile$ = RelMenu$(El)

            RelPer = INSTR(RelFile$, ".")
            RelName$ = LEFT$(RelFile$, LEN(RelFile$) - 4)
            FOpen Drive$ + ":" + Path$ + BSlash$ + RelFile$, Handle
            CALL FGetT(Handle, TempFld, FrmWdth)
            NumRelFlds = TempFld.Fields

            REDIM RelMenu$(NumRelFlds)
            REDIM RelType(NumRelFlds)

            FOR i = 1 TO NumRelFlds
              CALL FGetT(Handle, TempFld, FrmWdth)
              RelMenu$(i) = TempFld.FldName
              IF TempFld.FType = Relational THEN
                RelType(i) = TempFld.ScratchI
              ELSE
                RelType(i) = TempFld.FType
              END IF
            NEXT
            IF LEN(RelMenu$(1)) = 0 THEN RelMenu$(1) = SPACE$(8)
            FClose Handle

          END IF

          '***************** Get Relational field ******************

          RDLog(3).Heading = "Select the field for relation:<"
          RDLog(0).Heading = "Fields"
          RDLog(3).MenuChoice = Fld(FldNo).RelFld
        
          GOSUB Relational
          IF RelChoice = 2 OR RelChoice = 27 THEN
           ' GOSUB Cancel
            ERASE RelMenu$, RelType
            EXIT SUB
          ELSE

            RelFld = VAL(RlResp$(3))
            RelType = RelType(RelFld)
            RelFld$ = QPTrim$(RelMenu$(RelFld))

            ERASE RelMenu$, RelType

          END IF
        END IF
      CASE ELSE                            'String Fields, any length
        MinLen = 8
        MaxLen = (81 - Fld(FldNo).LCol) * 8
    END SELECT

    IF FldType = Relational THEN
      FldType = RelType
    END IF
    GOSUB EndCol
    GOSUB GetDefs

  CASE MouseFld, PButton, HScrollFld, VScrollFld      'Mouse box fields
    SELECT CASE FldType
      CASE MouseFld
        t$ = "Mouse Field"
      CASE PButton
        t$ = "Button Field"
      CASE HScrollFld, VScrollFld
        t$ = "Scrollbar"
    END SELECT

    REDIM A$(2)
    A$(1) = "Using the Mouse or Cursor keys, draw"
    A$(2) = "a box surrounding the " + t$
    GQSMessage A$(), 20, 0                 'Draw the message

    IF Fld(FldNo).RCol > 0 THEN            'If already defined
      HideCursor
      LineBFVE Fld(FldNo).LCol, Fld(FldNo).Row, Fld(FldNo).RCol, Fld(FldNo).ScratchI, 6159
      ShowCursor
      FldType = Fld(FldNo).FType
      MainType = FldType
    END IF

    CALL CursorXY(lasttx, lastty, Button, Ky) 'Display until the cursor moves

    DO
      CALL CursorXY(tx, ty, Button, Ky)
    LOOP UNTIL tx <> lasttx OR ty <> lastty OR Ky = 13

    BegRow = Fld(FldNo).Row
    BegCol = Fld(FldNo).LCol
    EnRow = Fld(FldNo).ScratchI
    EnCol = Fld(FldNo).RCol

    IF Fld(FldNo).RCol THEN                'If already defined
      HideCursor
      LineBFVE Fld(FldNo).LCol, Fld(FldNo).Row, Fld(FldNo).RCol, Fld(FldNo).ScratchI, 6159
      ShowCursor
    END IF

    GQSMessage A$(), 20, -1                'Clear the message

    IF Ky <> 13 THEN
      GPDat(7) = 50                        'Prevent F10 restore
                                           'Get the Field size
      CALL MouseBox(X1, Y1, X2, Y2, 0, Button, 14 + 24 * 256)
      GPDat(7) = 0
      IF Button = 2 AND Fld(FldNo).RCol = 0 THEN
        WaitUp
        GOTO Over
      END IF
      IF Button = 1 THEN
        BegRow = Y1
        BegCol = X1
        EnRow = Y2
        EnCol = X2
        IF FldType = HScrollFld AND X2 - X1 < Y2 - Y1 THEN
          FldType = VScrollFld
          MainType = VScrollFld
        END IF
      END IF
    END IF

    GOSUB GetDefs

  CASE ELSE
    MinLen = 8
    MaxLen = (81 - Fld(FldNo).LCol) * 8

    GOSUB EndCol
    GOSUB GetDefs

END SELECT

FldOS = 1

FOR i = 1 TO NumFlds
  IF i = 1 THEN
    Fld(1).Fields = 1
  ELSE
    Fld(i).Fields = Fld(i - 1).Fields + Fld(i - 1).StorLen
  END IF
NEXT

FldType = 0
IF NOT CompareT(Test, Fld(FldNo), FrmWdth) THEN ' OR OldCalc$ <> Calc$(FldNo) OR OldHelp$ <> Help$ THEN
  GPDat(84) = -1
END IF

FldNo = FldNo + 1

OldCalc$ = ""
OldHelp$ = ""

LOOP     'To start over


       '************** END OF Field Definition Loop *************
       '*********************************************************


'******************************** Gosubs ************************************

EndCol:
'****************************************************************************
'*                       Get the field length                               *
'****************************************************************************

StCol = BegCol * 8 - 8                     'Convert starting col to pixels

FCols = TEndCol - BegCol + 1               'Determine field width

IF NewFld THEN
  FCols = 0
  NewFld = 0
END IF

IF FCols < 1 THEN FCols = 1
FWidth = FCols * 8

'---- If already assigned and a fixed field length
IF FldType > MoneyFld AND FldType < Relational THEN
  LasCol = StCol + StartLen - 1
  RSET FLen$ = QPTrim$(STR$(StartLen \ 8))
ELSE
  RSET FLen$ = QPTrim$(STR$(FCols))
  LasCol = StCol + FCols * 8 - 1
END IF

NdCol = LasCol
IF FWidth < 8 THEN FWidth = MinLen

REDIM A$(2)
A$(1) = "Now locate the cursor at the end of Field#:" + STR$(FldNo)
A$(2) = "Length of Field:"' + FLen$

GQSMessage A$(), FLRow, 0

UprLine = BegRow * GPDat(71) - GPDat(71)
IF FldType = NotesFld AND Fld(FldNo).ScratchI > 0 THEN
  LwrLine = Fld(FldNo).ScratchI * GPDat(71) - 1
ELSE
  LwrLine = UprLine + GPDat(71) - 1
END IF

LasLine = LwrLine
HideCursor

IF FldType = NotesFld THEN
  NRows = MaxInt(Fld(FldNo).ScratchI - Fld(FldNo).Row + 1, 1)
  RSET FHt$ = LTRIM$(STR$(NRows))
  CALL GPrint0VE(FLRow + 1, 47, "Number of rows:", TextClr)
  CALL GPrint0VE(FLRow + 1, 63, QPTrim$(FHt$), TextClr)
  CALL PCBox(62 * 8 - 9, (FLRow + 1) * GPDat(71) - GPDat(71) - 1, 62 * 8 + 9, (FLRow + 1) * GPDat(71), 0)
END IF

BxClr = GetXORClr(15, GetPointVE(StCol, UprLine))
LineBVE StCol, UprLine, LasCol, LwrLine, BxClr

BHite = 5

IF FldNo > 9 THEN PCOS = 1
CALL PCBox((35 - PCOS) * 8 - 9, (FLRow + 1) * GPDat(71) - GPDat(71) - 1, (35 - PCOS) * 8 + 9, (FLRow + 1) * GPDat(71), 0)
RSET FLen$ = LTRIM$(STR$(ABS(FWidth \ 8)))
CALL GPrint0VE(FLRow + 1, 35 - PCOS, FLen$, TextClr)

ShowCursor

DO
  K$ = INKEY$

  GetCursor c, r, Button
  IF Button = 1 THEN
    Pressed = -1
    K$ = "z"
    FWidth = ((c + 8) \ 8) * 8 - StCol
    IF FldType = NotesFld THEN LwrLine = ((r + GPDat(71) \ 2) \ GPDat(71)) * GPDat(71) - 1
  ELSEIF Button >= 2 THEN
    K$ = CHR$(27)
  END IF

  IF Button = 0 AND Pressed THEN
    Pressed = 0
    t& = PDQTimer&
    IF t& - LastT& < 8 THEN K$ = CHR$(13)  'DoubleClick
    LastT& = PDQTimer&
  END IF

  IF LEN(K$) THEN
    SELECT CASE LEN(K$)
      CASE 0
        K = 0
      CASE 1
        K = ASC(K$)
      CASE 2
        K = -ASC(RIGHT$(K$, 1))
      CASE ELSE
    END SELECT

    SELECT CASE K
      CASE -77 'Right
        FWidth = FWidth + 8

      CASE -75 'Left
        FWidth = FWidth - 8

      CASE -72 'Up
        IF FldType = NotesFld THEN LwrLine = LwrLine - GPDat(71)

      CASE -80 'Down
        IF FldType = NotesFld THEN LwrLine = LwrLine + GPDat(71)
        
      CASE 13
        HideCursor
        LineBVE StCol, UprLine, NdCol, LwrLine, BxClr
        ShowCursor

        GQSMessage A$(), FLRow, -1
        EnCol = BegCol + (FWidth \ 8) - 1
        EnRow = (LwrLine + 1) \ GPDat(71)
        First = 0
        Lower = -1
        EXIT DO

      CASE 27, -68                         'Done
        HideCursor
        LineBVE StCol, UprLine, NdCol, LwrLine, BxClr
        ShowCursor
        GQSMessage A$(), FLRow, -1
        WaitUp
        GOTO Over
      CASE ELSE
    END SELECT

    IF FWidth < MinLen THEN FWidth = MinLen
    IF FWidth > MaxLen THEN FWidth = MaxLen

    NdCol = StCol + FWidth - 1
    
    IF NdCol <> LasCol OR LwrLine <> LasLine THEN
      IF FldType = NotesFld THEN
        IF LwrLine > GPDat(44) - 1 THEN LwrLine = GPDat(44) - 1
        IF LwrLine < UprLine + GPDat(71) - 1 THEN LwrLine = UprLine + GPDat(71) - 1
        NEditRows = ((LwrLine - UprLine) + 1) \ GPDat(71)
        RSET FHt$ = QPTrim$(STR$(NEditRows))
      END IF

      HideCursor
      LineBVE StCol, UprLine, LasCol, LasLine, BxClr
      LineBVE StCol, UprLine, NdCol, LwrLine, BxClr
      ShowCursor
      LasCol = NdCol
      LasLine = LwrLine
      RSET FLen$ = LTRIM$(STR$(ABS(FWidth \ 8)))
      CALL GPrint0VE(FLRow + 1, 35 - PCOS, FLen$, TextClr)
      IF FldType = NotesFld THEN CALL GPrint0VE(FLRow + 1, 62, FHt$, TextClr)

    END IF
  END IF
LOOP

RETURN


'***************************************************************************
'*                    Set up Field Definition Dialog Boxs                  *
'***************************************************************************

GetDefs:

REDIM DLog(16) AS Dialog
REDIM Resp$(16)
REDIM Menu$(0)

NumFlds = MaxInt(FldNo, NumFlds)
Fld(0).Fields = NumFlds

IF Null(Fld(FldNo).FldName) THEN
  Fld(FldNo).FldName = Name$(MainType)
  MakeUniqueFldName FldNo, Fld()
END IF

Resp$(3) = QPTrim$(Fld(FldNo).FldName)

IF Fld(FldNo).LCol THEN                    'If we've been here before
  SELECT CASE FldType
    CASE StrFld TO MultChAFld, PButton
      Resp$(4) = QPTrim$(STR$(Fld(FldNo).Value))     'Text Color

    CASE MouseFld
      Resp$(4) = QPTrim$(STR$(Fld(FldNo).Value))     'Return KeyCode
      Resp$(5) = QPTrim$(STR$(Fld(FldNo).Decimals))  'Tab Color
      Resp$(6) = LTRIM$(STR$(Fld(FldNo).RelFld))     'HiLite Color
      IF Fld(FldNo).LowRange <> 0# THEN Resp$(8) = "X" 'Toggle

    CASE HScrollFld, VScrollFld
      Resp$(4) = QPTrim$(STR$(Fld(FldNo).Indexed))   'Small change
      Resp$(5) = QPTrim$(STR$(Fld(FldNo).RelFld))    'Large Change

  END SELECT
ELSE          'Have'nt been here yet
  SELECT CASE FldType
    CASE StrFld TO MultChAFld
      Resp$(4) = "15"

    CASE MouseFld
      Resp$(4) = "255"
      Resp$(5) = "0"
      Resp$(6) = "0"

    CASE PButton
      Resp$(4) = "255"

    CASE HScrollFld, VScrollFld
      Resp$(4) = "1"
      Resp$(5) = "10"
  END SELECT
END IF


SELECT CASE FldType
  CASE SngFld, DblFld, MoneyFld
    IF Fld(FldNo).Decimals = -1 THEN
      Resp$(9) = "X"                       'Formatted
      Resp$(5) = "0"
    ELSE
      Resp$(5) = LTRIM$(STR$(Fld(FldNo).Decimals))
    END IF

  CASE IntFld, LongIntFld
    IF Fld(FldNo).Decimals THEN Resp$(9) = "X"

  CASE LogicalFld
    Resp$(10) = MID$(Fld(FldNo).RelFile, 1, 1)
    Resp$(11) = MID$(Fld(FldNo).RelFile, 3, 1)

  CASE ScrollFld
    IF Fld(FldNo).StorLen = 0 THEN Fld(FldNo).StorLen = EnCol - BegCol + 1
    Resp$(5) = LTRIM$(STR$(Fld(FldNo).StorLen))
    Resp$(6) = LTRIM$(STR$(Fld(FldNo).Decimals))
END SELECT

IF FldType = MoneyFld THEN
  IF NOT Null(Fld(FldNo).ScratchS) THEN
    Resp$(6) = Fld(FldNo).ScratchS
  ELSE
    Resp$(6) = "$"
  END IF
END IF

IF Fld(FldNo).Protected THEN Resp$(7) = "X"'vProtected field
IF Fld(FldNo).Indexed THEN Resp$(8) = "X"  'Indexed field

Resp$(14) = Calc$(FldNo)
Resp$(15) = Help$(FldNo)

'---- Set Defaults for dialog box
OKRow = 2 * GPDat(71) - GPDat(71) \ 2 + 4  'PButton row
OKCol = 50 * 8 - 2                         '" col
CancRow = OKRow
CancCol = OKCol + 80

TRow = 2                                   'Text color row
DLog(4).TextLen = 2                        'Text clr field len
TxtClr$ = "Text Color:<11"

DecRow = 4                                 'Dec places row
Dec$ = "Dec Places:<11"                    'Dec places heading

Ndx$ = "Indexed"

Curr$ = "Currency:  <10"                   'Currency type heading
CurrLen = 8                                'Currency type field len

Fmt$ = "No Formatting"
DLog(5).TextLen = 2                        'Dec places field len

ChkRow = 2                                 'Protected row
ChkRowOS = 0

RangeLen = 22                              'Hi/Lo range len

'Note All Fields get: 2 Push Buttons, Help Field, Heading, Field Name

DLog(4).DType = 3                          'Text color = String$ fld

DLog(7).DType = 1                          'Protected Field (All fields)
DLog(7).Col = 30           'All but MouseFld, PButton, H/VScrollbar MultCh?

DLog(8).DType = 1          'Indexed Field (Not Logic,Notes,Mouse,PB,H/VScrollBar)
DLog(8).Col = 30           'Always 30

'---- Set field ranges
SELECT CASE FldType
  CASE IntFld TO EuroDateFld, HScrollFld, VScrollFld
    DLog(12).DType = 3                     'LoRange
    DLog(12).Col = 2
    DLog(13).DType = 3                     'HiRange
    DLog(13).Col = 38

    IF Fld(FldNo).LowRange = Fld(FldNo).HiRange THEN

      SELECT CASE FldType
        CASE IntFld
          Fld(FldNo).LowRange = -32768
          Fld(FldNo).HiRange = 32767

        CASE LongIntFld
          Fld(FldNo).LowRange = -2147483647
          Fld(FldNo).HiRange = 2147483648#

        CASE SngFld
          Fld(FldNo).LowRange = -3.402823E+38
          Fld(FldNo).HiRange = 3.402823E+38

        CASE DblFld, MoneyFld
          Fld(FldNo).LowRange = -1.79769313486231D+308
          Fld(FldNo).HiRange = 1.79769313486231D+308

        CASE HScrollFld, VScrollFld
          Fld(FldNo).LowRange = 0
          Fld(FldNo).HiRange = 1000

        CASE DateFld, EuroDateFld
          Fld(FldNo).LowRange = -29219
          Fld(FldNo).HiRange = 31368
      END SELECT

    END IF

    SELECT CASE FldType
      CASE IntFld
        RangeLen = 6
      CASE LongIntFld
        RangeLen = 11
      CASE HScrollFld, VScrollFld
        RangeLen = 6
    END SELECT

    IF FldType = DateFld THEN
      Resp$(12) = Num2Date$(CINT(Fld(FldNo).LowRange))
      Resp$(13) = Num2Date$(CINT(Fld(FldNo).HiRange))
      RangeLen = 10
    ELSEIF FldType = EuroDateFld THEN
      Resp$(12) = ES$(Num2Date$(CINT(Fld(FldNo).LowRange)))
      Resp$(13) = ES$(Num2Date$(CINT(Fld(FldNo).HiRange)))
      RangeLen = 10
    ELSE
      Resp$(12) = QPTrim$(STR$(Fld(FldNo).LowRange))
      Resp$(13) = QPTrim$(STR$(Fld(FldNo).HiRange))
    END IF

  END SELECT

SELECT CASE FldType                        'Who gets formula field
  CASE NotesFld, LogicalFld, MultChAFld TO VScrollFld
    DLog(14).DType = 0
    DLog(15).DType = 0
  CASE ELSE
    DLog(14).DType = 8                     'Formula Field
    DLog(15).DType = 8                     'Formula row 2
END SELECT

SELECT CASE FldType

  CASE StrFld, PropStrFld, UCaseStrFld, NumericStrFld, ScrollFld 'String, Proper string, Numeric string Field
    HelpRow = 9

    SELECT CASE FldType
      CASE StrFld
        DLog(0).Heading = "String"
      CASE PropStrFld
        DLog(0).Heading = "Proper String"
      CASE UCaseStrFld
        DLog(0).Heading = "UCase String"
      CASE NumericStrFld
        DLog(0).Heading = "Numeric String"
      CASE ScrollFld
        DLog(0).Heading = "Scrolling Text"

        Dec$ = "Max Length:<11"
        DLog(5).DType = 3                  'Maximum length (Dec)
        DLog(5).TextLen = 3

        Curr$ = "Filter 1-5:<10"
        DLog(6).DType = 3                  'Filter ($$ type)
        DLog(6).Col = 2

        CurrLen = 1
        HelpRow = 11

    END SELECT
    FormulaRow = HelpRow - 3

  CASE NotesFld               'Multi-Line
    DLog(0).Heading = "Multi-Line"
    DLog(8).DType = 0                      'Cannot Index  Multi-line
    HelpRow = 5

  CASE LogicalFld             'Logical
    DLog(0).Heading = "Logical"
    DLog(10).DType = 3                     'True Character
    DLog(10).Col = 18
    DLog(11).DType = 3                     'False Character
    DLog(11).Col = 36
    DLog(8).DType = 0                      'Cannot Index Y/N
    HelpRow = 6

  CASE IntFld, LongIntFld                  'Integer, Long Integer
    IF FldType = IntFld THEN
      DLog(0).Heading = "Integer"
    ELSE
      DLog(0).Heading = "Long Integer"
    END IF
    
    DLog(9).DType = 1                      'No Formatting
    DLog(9).Col = 30
    RangeRow = 6
    HelpRow = 12

  CASE SngFld, DblFld                      'Single/Double Precision
    IF Fld(FldNo).Decimals > -1 THEN
       Resp$(5) = LTRIM$(STR$(Fld(FldNo).Decimals))  'Decimals field
    END IF

    IF FldType = SngFld THEN                'Determine Heading
      DLog(0).Heading = "Single Precision"
    ELSE
      DLog(0).Heading = "Double Precision"
    END IF

    DLog(5).DType = 3                       'Decimal places
    DLog(9).DType = 1                       'No Formatting
    DLog(9).Col = 30
    RangeRow = 6
    HelpRow = 12

  CASE MoneyFld                             'Currency Field
    DLog(0).Heading = "Currency"
    DLog(5).DType = 3                       'Decimal places
    DLog(6).DType = 3                       'Currency Type
    DLog(6).Col = 2
    RangeRow = 7
    HelpRow = 13

  CASE DateFld, EuroDateFld                 'Date
    IF FldType = DateFld THEN
      DLog(0).Heading = "Date MM-DD-Y"
    ELSE
      DLog(0).Heading = "Date DD-MM-Y"
    END IF

    RangeRow = 5
    HelpRow = 11

  CASE PhoneFld, ZipFld, SoSecFld           'Phone, ZipCode, Social Security
    SELECT CASE FldType
      CASE PhoneFld
        DLog(0).Heading = "Phone"
      CASE ZipFld
        DLog(0).Heading = "Zip Code"
      CASE SoSecFld
        DLog(0).Heading = "Social Security"
    END SELECT

    HelpRow = 9

  CASE Relational                           'Relational
    DLog(0).Heading = "Relational"
    DLog(16).DType = 5                      'No Formatting
    DLog(16).Col = 2
    DLog(16).Row = GPDat(71) * 4
    DLog(16).Heading = "Related to field `" + RelFld$ + "' in `" + QPTrim$(RelFile$) + "'"
    RangeRow = 7
    HelpRow = 11

  CASE MultChAFld                           'Multiple Choice
    DLog(0).Heading = "Multiple Choice"
    DLog(8).DType = 1                       'Indexed
    DLog(8).Col = 30
    HelpRow = 5

  CASE MouseFld, PButton
    IF FldType = PButton THEN
      DLog(0).Heading = "Push Button"
      HelpRow = 5
      TxtClr$ = "Key Code:  <11"
      DLog(4).TextLen = 4                   'Text clr field len
      DLog(8).DType = 0

    ELSE
      DLog(0).Heading = "Mouse Region"

      TxtClr$ = "Key Code:  <11"
      DLog(4).TextLen = 4                   'Key Code field length

      Dec$ = "Tab Color: <11"
      DLog(5).DType = 3                     'Decimals stores Tab Clr

      DLog(6).DType = 3                     'RelFlds stores HiLite clr
      DLog(6).Col = 2
      Curr$ = "HiLite Clr:<11"
      CurrLen = 2

      Fmt$ = "HiLite when activated"
      HelpRow = 7
      DLog(8).DType = 1
      DLog(8).Col = 25
      Ndx$ = "Toggle"

    END IF

    DLog(7).Col = 25                        'Protected element

    TRow = 2

  CASE HScrollFld, VScrollFld
    IF FldType = HScrollFld THEN
      DLog(0).Heading = "Horizontal Scroll Bar"
    ELSE
      DLog(0).Heading = "Vertical Scroll Bar"
    END IF

    TxtClr$ = "Small Change: <11"
    DLog(4).TextLen = 5


    Dec$ = "Large Change: <11"
    DLog(5).DType = 3                       'Large Change/Decimal places
    DLog(5).TextLen = 5

    IF QPTrim$(Resp$(4)) = "0" THEN Resp$(4) = "1"
    IF QPTrim$(Resp$(5)) = "0" THEN Resp$(5) = "10"

    DLog(7).Col = 25
    DLog(8).DType = 0                       'Cannot Index ScrollBars

    TRow = 2

    RangeRow = 6
    HelpRow = 8'12

END SELECT


'---- Set up Dialog Box

DLog(1).DType = 6                           'Push Button
DLog(1).Row = OKRow - 2 * Space8
DLog(1).Col = OKCol
DLog(1).Heading = "OK"
DLog(1).NumRows = GPDat(80)
DLog(1).TextLen = 66

DLog(2).DType = 6                           'Push Button
DLog(2).Row = CancRow - 2 * Space8
DLog(2).Col = CancCol'60 * 8 - 2
DLog(2).Heading = "Cancel"
DLog(2).NumRows = GPDat(80)
DLog(2).TextLen = 66
DLog(2).Group = 1000

DLog(3).DType = 3                           'Text entry
DLog(3).Row = TRow
DLog(3).Col = 2
DLog(3).Heading = "Field Name:<10"
DLog(3).TextLen = 8
DLog(3).HotKey = 6

DLog(4).Row = 3
DLog(4).Col = 2
DLog(4).Heading = TxtClr$


'---This field is for # decimal places for Sng, Dbl and $$ fields, Max len for
'   scrolling fields, MouseFld Hilite color, ScrollBar Large Change value
DLog(5).Row = DecRow
DLog(5).Col = 2
DLog(5).Heading = Dec$

'This field is for the Currency type, or the Filter for a scrolling field
DLog(6).Row = 5
DLog(6).Heading = Curr$
DLog(6).TextLen = CurrLen
DLog(6).HotKey = 2

DLog(7).Row = ChkRow                       'Protected fields
DLog(7).Heading = "Protected"

DLog(8).Row = ChkRow + 1 + ChkRowOS
DLog(8).Heading = Ndx$

'This field is for all numeric fields and for MouseFld "HiLight on Selection"
DLog(9).Row = ChkRow + 2
DLog(9).Heading = Fmt$
DLog(9).HotKey = 12

'Fields 9 and 10 are for logicl fields only
DLog(10).Row = 4
DLog(10).Heading = "True Char<10"
DLog(10).TextLen = 1

DLog(11).Row = 4
DLog(11).Heading = "False Char<10"
DLog(11).TextLen = 1

DLog(12).Row = RangeRow
DLog(12).Col = 2
DLog(12).Heading = "LoRange:<11"
DLog(12).TextLen = RangeLen

DLog(13).Row = RangeRow
DLog(13).Col = 38
DLog(13).Heading = "HiRange:<11"
DLog(13).TextLen = RangeLen
DLog(13).HotKey = 2

IF FldType < MouseFld THEN
  DLog(14).Row = HelpRow - 4
  DLog(14).Col = 2
  DLog(14).Heading = "Formula:<"
  DLog(14).NumRows = 3
  DLog(14).HotKey = 3
  DLog(14).TextLen = 58
END IF

DLog(15).Row = HelpRow
DLog(15).Col = 2
DLog(15).Heading = "Help:   <"
DLog(15).NumRows = 3
DLog(15).DType = 8
DLog(15).TextLen = 58

DLog(0).Heading = RTRIM$(DLog(0).Heading) + "   Field#" + (STR$(FldNo))

Choice = 1

DO
  CALL GDialog(Choice, DLog(), Resp$(), Menu$(), Style, 0)

  IF Choice = 1 THEN
    IF IsDup(UCASE$(Resp$(3)), Fld(), FldNo) THEN
      BEEP
      REDIM Msg$(1)
      Msg$(0) = "Input Error"
      Msg$(1) = "Field name `" + Resp$(3) + "' is already in use."
      Button = MsgBox(Msg$(), 11)
    ELSE
      GPDat(84) = 1   'Set field defs changed flag
      EXIT DO
    END IF
  ELSE
    EXIT DO
  END IF
LOOP

IF Choice = 2 OR Choice = 27 THEN GOTO Over

Test = Fld(FldNo)
Fld(FldNo) = Blank
OldCalc$ = Calc$(FldNo)
OldHelp$ = Help$(FldNo)

Fld(FldNo).FType = MainType

'---- Special case for Scrolling fields
IF FldType = ScrollFld THEN
  Fld(FldNo).Decimals = VAL(Resp$(6))     'Filter selection
  Fld(FldNo).ScratchI = 1                 'Starting character in field
END IF

'---- Set the storage lengths
SELECT CASE FldType
  CASE IntFld, DateFld, EuroDateFld, HScrollFld, VScrollFld
    Fld(FldNo).StorLen = 2
  CASE NotesFld, LongIntFld, SngFld
    Fld(FldNo).StorLen = 4
  CASE DblFld, MoneyFld
    Fld(FldNo).StorLen = 8
  CASE MouseFld, PButton
    Fld(FldNo).StorLen = 0
  CASE ScrollFld
    Fld(FldNo).StorLen = MaxInt(VAL(Resp$(5)), Fld(FldNo).RCol - Fld(FldNo).LCol + 1) 'MaxLen
  CASE ELSE
    Fld(FldNo).StorLen = FWidth \ 8
END SELECT

SELECT CASE FldType
  CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld, HScrollFld, VScrollFld
    Fld(FldNo).LowRange = Eval#(Resp$(12), ErrCode)
    IF ErrCode THEN Fld(FldNo).LowRange = -1.79769313486231D+308
    Fld(FldNo).HiRange = Eval#(Resp$(13), ErrCode)
    IF ErrCode THEN Fld(FldNo).HiRange = 1.79769313486231D+308
  CASE ELSE
END SELECT

SELECT CASE FldType
  CASE IntFld, HScrollFld, VScrollFld      'IntFld, ScrollBar
    IF Fld(FldNo).LowRange < -32768 THEN Fld(FldNo).LowRange = -32768
    IF Fld(FldNo).HiRange > 32768 THEN Fld(FldNo).HiRange = 32767
  CASE DateFld                             'DateFld
    Fld(FldNo).LowRange = MaxInt(Date2Num(Resp$(12)), -29219)
    Fld(FldNo).HiRange = MinInt(Date2Num(Resp$(13)), 31368)
  CASE EuroDateFld                         'EuroDateFld
    Fld(FldNo).LowRange = MaxInt(Date2Num(ES$(Resp$(12))), -29219)
    Fld(FldNo).HiRange = MinInt(Date2Num(ES$(Resp$(13))), 31368)
  CASE LongIntFld                          'LongIntFld
    IF Fld(FldNo).LowRange < -2147483648# THEN Fld(FldNo).LowRange = -2147483648#
    IF Fld(FldNo).HiRange > 2147483647# THEN Fld(FldNo).HiRange = 2147483647#
  CASE SngFld                              'SngFld
    IF Fld(FldNo).LowRange < -3.402823E+38 THEN Fld(FldNo).LowRange = -3.402823E+38
    IF Fld(FldNo).HiRange > 3.402823E+38 THEN Fld(FldNo).HiRange = 3.402823E+38
END SELECT

'---- Set Field Location
Fld(FldNo).Row = BegRow
Fld(FldNo).LCol = BegCol
Fld(FldNo).RCol = EnCol                    'Mouse fields or notes
IF FldType > MultChAFld OR FldType = NotesFld THEN Fld(FldNo).ScratchI = EnRow

Fld(FldNo).FldName = UCASE$(QPTrim$(Resp$(3)))

'---- Set relational
IF MainType = Relational THEN
  Fld(FldNo).RelFile = RelFile$
  Fld(FldNo).RelFld = RelFld
  Fld(FldNo).ScratchI = RelType
  Fld(FldNo).RelHandle = Test.RelHandle
END IF

'---- Set text color if it is a text field
IF FldType < MouseFld THEN
  Fld(FldNo).Value = TrapInt(VAL(Resp$(4)), 0, 15)
ELSEIF FldType = MouseFld OR FldType = PButton THEN
  Fld(FldNo).Value = TrapInt(VAL(Resp$(4)), -255, 255)
ELSE
  Fld(FldNo).Value = VAL(Resp$(4))
END IF

Fld(FldNo).Protected = Resp$(7) = "X"
Fld(FldNo).Indexed = Resp$(8) = "X"

IF FldType = HScrollFld OR FldType = VScrollFld THEN                                'If its a ScrollBar
  Fld(FldNo).Value = Fld(FldNo).LowRange   'Pointer starting value
  Fld(FldNo).Indexed = VAL(Resp$(4))       'Small change
  Fld(FldNo).RelFld = VAL(Resp$(5))        'Large change
END IF

IF FldType = HScrollFld THEN                      'H ScrollBar
  Fld(FldNo).RelHandle = Fld(FldNo).LCol + 16     'Start of H pointer
END IF

IF FldType = VScrollFld THEN                      'V ScrollBar
  Fld(FldNo).RelHandle = Fld(FldNo).Row + 16      'Start of V pointer
END IF

IF FldType = SngFld OR FldType = DblFld OR FldType = MoneyFld THEN
  IF Resp$(9) = "X" THEN
    Fld(FldNo).Decimals = -1
  ELSE
    Fld(FldNo).Decimals = VAL(Resp$(5))
  END IF
ELSEIF FldType = MouseFld THEN             'Tab Clr, Filter
  Fld(FldNo).Decimals = VAL(Resp$(5))
END IF

IF FldType = LongIntFld OR FldType = IntFld THEN  'Formatting?
  Fld(FldNo).Decimals = Resp$(9) = "X"
END IF

IF FldType = LogicalFld THEN               'Logical Field
  MID$(Fld(FldNo).RelFile, 1, 1) = Resp$(10)
  MID$(Fld(FldNo).RelFile, 3, 1) = Resp$(11)
END IF

IF FldType = MoneyFld THEN                 'Currency type
  Fld(FldNo).ScratchS = Resp$(6)
END IF

IF FldType = MouseFld THEN
  Fld(FldNo).RelFld = VAL(Resp$(6))        'HiLite Color
  Fld(FldNo).LowRange = Resp$(8) = "X"
  IF Fld(FldNo).LowRange THEN Fld(FldNo).StorLen = 1
END IF

Calc$(FldNo) = ""
IF NOT Null(Resp$(14)) THEN
  Calc$(FldNo) = LEFT$(QPTrim$(Resp$(14)), 348)
  IF ((FldType > LogicalFld) AND (FldType < PhoneFld)) OR FldType > PButton THEN Calc$(FldNo) = UCASE$(Calc$(FldNo))
END IF

Help$(FldNo) = ""
IF NOT Null(Resp$(15)) THEN Help$(FldNo) = LEFT$(QPTrim$(Resp$(15)), 348)

Chk = 0
CheckMem Chk, 4000&
IF Chk THEN EXIT SUB

RETURN

MoveCursor:
  IF CursorIs THEN
    CALL DrawCursor(CurCol, CurRow, 8, GPDat(71) - 1, CursorIs, -1)
  END IF
RETURN
  
LocateCsr:
  IF Fld(FldNo).RCol THEN                  'If we've been here before
    IF Fld(FldNo).FType > MultChAFld THEN
      CurRow = Fld(FldNo).Row              ' use last mouse coords
      CurCol = Fld(FldNo).LCol
      FRow = CurRow \ GPDat(71) + 1
    ELSE
      CurRow = Fld(FldNo).Row * GPDat(71) - GPDat(71)  ' use last coords
      CurCol = Fld(FldNo).LCol * 8 - 8
      FRow = Fld(FldNo).Row
    END IF
  ELSE
    IF Fld(FldNo - 1).FType > MultChAFld THEN
      CurRow = Fld(FldNo - 1).Row          'If its a new field, locate
      CurCol = Fld(FldNo - 1).RCol - 8     ' cursor at end of last mouse field
      FRow = CurRow \ GPDat(71) + 1
    ELSE
      IF FldNo = 1 THEN
        CurRow = 0
        CurCol = 0
      ELSE
        CurRow = Fld(FldNo - 1).Row * GPDat(71) - GPDat(71) 'If its a new field, locate
        CurCol = Fld(FldNo - 1).RCol * 8 - 8     ' cursor at end of last field
      END IF
      FRow = Fld(FldNo).Row

    END IF
  END IF

RETURN

Relational:
  RDLog(1).DType = 6
  RDLog(1).Heading = "OK"
  RDLog(1).Row = 5 * GPDat(71) - Space8 * 3
  RDLog(1).Col = 23 * 8 - 2
  RDLog(1).NumRows = GPDat(80)'2 * GPDat(71) - GPDat(71) \ 2
  RDLog(1).TextLen = 66

  RDLog(2).DType = 6
  RDLog(2).Heading = "Cancel"
  RDLog(2).Row = 7 * GPDat(71)
  RDLog(2).Col = 23 * 8 - 2
  RDLog(2).NumRows = GPDat(80)'2 * GPDat(71) - GPDat(71) \ 2
  RDLog(2).TextLen = 66'76
  RDLog(2).Group = 1000


  RDLog(3).DType = 4                       'Menu
  RDLog(3).Row = 2
  RDLog(3).Col = 2
  RDLog(3).NumRows = 7

  RelChoice = 1

  CALL GDialog(RelChoice, RDLog(), RlResp$(), RelMenu$(), RStyle, 0)
RETURN

ShowFldNo:
  FldType = Fld(FldNo).FType
  GOSUB LocateCsr
  RSET FldNo$ = QPTrim$(STR$(FldNo))
  A$(1) = "  Move the cursor to the beginning of Field #" + FldNo$
  GPrint0VE FLRow, 62, FldNo$, TextClr
RETURN

END SUB

FUNCTION GetXORClr (DesiredClr, BGClr)

  IF BGClr = 15 THEN BGClr = 0
  GetXORClr = DesiredClr XOR BGClr + 6144

END FUNCTION

SUB GQSMessage (A$(), Row, Dir) STATIC

UprBound = UBOUND(A$)
LStr = 0
FOR i = 1 TO UprBound
  t = LEN(A$(i))
  IF t > LStr THEN LStr = t
NEXT

HStr = LStr \ 2
X1 = 312 - HStr * 8
Y1 = Row * GPDat(71) - GPDat(71) - 4
X2 = X1 + LStr * 8 + 16
Y2 = Y1 + UprBound * GPDat(71) + 6

Address = &HAA00
IF GPDat(31) = 5 THEN Address = &HA800

HideCursor
IF Dir THEN
  GMove2VE 41 - HStr - 2, Y1 - 12, LStr + 4, Y2 - Y1 + 20, Address, -1
  ShowCursor                                          '^ 3 extra
  EXIT SUB
END IF

GMove2VE 41 - HStr - 2, Y1 - 12, LStr + 4, Y2 - Y1 + 20, Address, 0

LINE (X1 - 4, Y1 - 12)-(X2 + 4, Y2 + 4), 7, BF
PCBox X1 - 4, Y1 - 12, X2 + 4, Y2 + 4, -1
PCBox X1, Y1, X2, Y2, 0

FOR i = 1 TO UprBound
  IF LEN(A$(i)) THEN GPrint0VE Row + i - 1, 41 - HStr, A$(i), 0 + 7 * 256
NEXT
ShowCursor

END SUB

FUNCTION IsDup (FldName$, Fld() AS FieldInfoG, FldNo) STATIC

  IsDup = 0
  FOR i = 1 TO Fld(0).Fields
    IF FldName$ = Fld(i).FldName THEN
      IF i <> FldNo THEN
        IsDup = -1
        EXIT FOR
      END IF
    END IF
  NEXT

END FUNCTION

SUB MakeNewFldName (OldName$)

  '----- Find the end of the field's name
  FOR Nd = 1 TO 8
    Temp = ASC(MID$(OldName$, Nd, 1))
    IF Temp = 32 OR Temp = 0 THEN EXIT FOR
  NEXT

  '----- Look backwards for non-number
  FOR i = Nd - 1 TO 1 STEP -1
    Temp = ASC(MID$(OldName$, i, 1))
    IF Temp < 48 OR Temp > 57 THEN EXIT FOR
  NEXT
  i = i + 1

  '----- Find their field number if any
  Temp = VAL(MID$(OldName$, i))
  Temp = Temp + 1
  IF Temp = 1 THEN Temp = 2

  '----- Put the new number in the name
  Num$ = LTRIM$(STR$(Temp))
  i = MinInt(i, 8 - LEN(Num$) + 1)

  MID$(OldName$, i) = Num$

END SUB

SUB MakeUniqueFldName (FldNo, Fld() AS FieldInfoG)

  DO
    FOR i = 1 TO Fld(0).Fields
      IF i <> FldNo AND Fld(FldNo).FldName = Fld(i).FldName THEN
        MakeNewFldName Fld(FldNo).FldName
        EXIT FOR
      END IF
    NEXT
  LOOP UNTIL i > Fld(0).Fields

END SUB

