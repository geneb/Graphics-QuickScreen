DEFINT A-Z

'---- Graphic dialog box subprogram

'by Phil Cramer
'Copyright (c) 1992 Crescent Software

'----Basic subs
DECLARE SUB DrawCursor (x, y, CursorWidth, CursorLength, CursorIs, CursorTime)
DECLARE SUB GDialog (Choice%, DLog() AS ANY, Response$(), Menu$(), Style, Action%)
DECLARE SUB GEditor (Edit$, LeftCol%, Ky$, TxtPos%, Accept%, Button%, HotKeyTbl())
DECLARE SUB Icons (x%, y%, Icon)
DECLARE SUB NCBox (x%, y%, BWidth%, BHite%, Mask, Clr%)
DECLARE SUB PCBox (X1, y1, X2, y2, InOut)
DECLARE SUB QEdit (array$(), x$, Action%, Ed AS ANY)
DECLARE SUB SPushButton (x%, y%, BWidth%, BHite%, Text$, UpDn%, HotKey%)
DECLARE SUB ListBox (Item$(), Choice%, MaxLen%, BoxBot%, Ky$, HotKeyTbl(), Action%)
DECLARE SUB WaitUpKey ()

'----ASM subs
DECLARE SUB MGetState (x$)
DECLARE SUB MSetState (x$)
DECLARE SUB WaitUp ()
DECLARE SUB Pause (Ticks)
DECLARE SUB ProperName (Work$)
DECLARE SUB StuffBuf (A$)
DECLARE SUB HideCursor ()
DECLARE SUB ShowCursor ()
DECLARE SUB GetCursor (x, y, Button)
DECLARE SUB GMove2VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GPrint0VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint1VE (BYVAL Line1%, BYVAL Col1%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint2VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint3VE (BYVAL Line1%, BYVAL Col1%, Text$, BYVAL TextColor%)
DECLARE SUB DrawPointVEOpts (BYVAL x%, BYVAL y%, BYVAL PointColor%)
DECLARE SUB Mouse (AX%, BX%, CX%, DX%)


'----BASIC functions
DECLARE FUNCTION MsgBox% (Text$(), Opts%)
DECLARE FUNCTION GArraySize& (X1, y1, X2, y2)

'----ASM functions
DECLARE FUNCTION AltKey% ()
DECLARE FUNCTION ErrorMsg$ (E)
DECLARE FUNCTION FindLast% (BYVAL Address, Size)
DECLARE FUNCTION INSTRTBL2 (Start, Source$, Table$)
DECLARE FUNCTION KeyDown% ()
DECLARE FUNCTION LongestStr (array$())
DECLARE FUNCTION MinInt% (Val1, Val2)
DECLARE FUNCTION MBuffSize% ()
DECLARE FUNCTION QPTrim$ (A$)
DECLARE FUNCTION PDQTimer& ()
DECLARE FUNCTION WhichError% ()


DECLARE SUB GMove2VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GPrint0VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint1VE (BYVAL Line1%, BYVAL Col1%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint3VE (BYVAL Line1%, BYVAL Col1%, Text$, BYVAL TextColor%)

'$INCLUDE: 'Common.GQS'
'$INCLUDE: 'DLogType.Bi'
'$INCLUDE: 'QEditype.Bi'

TYPE MCoords
  X1 AS INTEGER
  y1 AS INTEGER
  X2 AS INTEGER
  y2 AS INTEGER
END TYPE

CONST BGClr = 7
CONST FGClr = 0
CONST HKClr = 15 + 7 * 256      'used for 60 and 43 line mode only

CONST AltHotKey$ = "QWERTYUIOP    ASDFGHJKL     ZXCVBNM"

CONST IntOnly$ = " 1234567890"
CONST IntComma$ = " 1234567890,+-"
CONST SDPrecision$ = " 1234567890.-+^eEeD"

'---- Dialog Field type Constants
CONST CheckBox = 1
CONST OptButton = 2
CONST TextIn = 3
CONST DMenu = 4
CONST Caption = 5
CONST PButton = 6
CONST Frame = 7
CONST MultLine = 8

Caps1:
DATA Ok, Cancel, Re-Try

Caps2:
DATA Ok, No, Cancel

SUB DOSMsg

  REDIM Msg$(1)
  Msg$(0) = "Error"
  Msg$(1) = ErrorMsg$(WhichError)
  IF LEN(Msg$(1)) MOD 2 THEN Msg$(1) = Msg$(1) + CHR$(255)
  BPress = MsgBox(Msg$(), 21)

END SUB

SUB GDialog (Choice, DLog() AS Dialog, Response$(), Menu$(), Style, Action) STATIC


IF Action < 2 THEN
  '---- These variables need to be initialized each time due to STATIC sub
  MenuRows = 0
  FirstRow = 0
  LastRow = 0
  BWidth = 0
  PB = 0
  MChoice = 0
  WasCheckFld = 0
  LC = 1
  LastMChoice = 1
  LastLen = 0
  TextClr = FGClr + BGClr * 256
  DotBoxOS = 0
  CheckBoxOS = 1
  MouseKey$ = CHR$(0) + CHR$(3)
  REDIM Work$(0)
  DIM Ed AS EditInfo

  SELECT CASE GPDat(71)
    CASE 14
      OS = 8                 'Screen 9 Option Button circle location
      OS1 = 1                'Hotkey underline offset
      ORadius = 7            'Option button radius
      BxHite = 10
      BxWide = 10

    CASE 16
      OS = 8                 'Screen 12 Option Button circle location
      OS1 = 1
      ORadius = 6
      BxHite = 10
      BxWide = 10

    CASE 8
      OS = 5                 '43/50 Option Button circle location
      OS1 = 3
      ORadius = 4
      BxHite = 8
      BxWide = 8
      BxV = 4
      DotBoxOS = 1
      CheckBoxOS = 0

  END SELECT

  ScrnRows = GPDat(85)
  halfhite = GPDat(71) \ 2

  UpperBound = UBOUND(DLog, 1)

  REDIM TResponse$(UpperBound)
  REDIM HotKeyTbl(UpperBound)              'Array to hold HotKey values

  GOSUB CopyResp

  FOR i = 1 TO UpperBound
    SELECT CASE DLog(i).DType
      CASE CheckBox, OptButton, TextIn, Frame
	ThisRow = DLog(i).Row + DLog(i).NumRows

      CASE MultLine
	ThisRow = DLog(i).Row + DLog(i).NumRows - 1

      CASE Caption
	ThisRow = DLog(i).Row \ GPDat(71) + 1

      CASE PButton
	ThisRow = (DLog(i).Row + DLog(i).NumRows - GPDat(71) \ 8) \ GPDat(71) - 1

      CASE DMenu
	MenuRows = DLog(i).NumRows + DLog(i).Row - 1

    END SELECT

    IF ThisRow < FirstRow THEN FirstRow = ThisRow
    IF ThisRow > LastRow THEN LastRow = ThisRow

    SELECT CASE DLog(i).DType

      CASE CheckBox, OptButton, MultLine, Caption
	IF DLog(i).Col - 1 + LEN(QPTrim$(DLog(i).Heading)) + DLog(i).TextLen + 2 > BWidth THEN
	  BWidth = DLog(i).Col - 1 + LEN(QPTrim$(DLog(i).Heading)) + DLog(i).TextLen + 2
	END IF

      CASE TextIn
	IF DLog(i).Col - 1 + LEN(QPTrim$(DLog(i).Heading)) + DLog(i).TextLen > BWidth THEN
	  BWidth = DLog(i).Col - 1 + LEN(QPTrim$(DLog(i).Heading)) + DLog(i).TextLen
	END IF

      CASE Frame
	IF DLog(i).Col + DLog(i).TextLen + 1 > BWidth THEN BWidth = DLog(i).Col + DLog(i).TextLen + 1
 
      CASE PButton
	IF (DLog(i).Col + DLog(i).TextLen) \ 8 + 1 > BWidth THEN BWidth = (DLog(i).Col + DLog(i).TextLen) \ 8 + 1
 
      CASE DMenu
	IF DLog(i).Col + LEN(Menu$(1)) + 2 > BWidth THEN BWidth = DLog(i).Col + LEN(Menu$(1)) + 2

    END SELECT

    IF DLog(i).DType <> Caption AND DLog(i).DType <> Frame THEN HotKeyTbl(i) = ASC(UCASE$(MID$(DLog(i).Heading, DLog(i).HotKey + 1, 1)))

  NEXT

  NumRows = LastRow - FirstRow
  IF MenuRows > NumRows THEN NumRows = MenuRows

  ULCol = 42 - BWidth \ 2
  ULRow = ScrnRows \ 2 - NumRows \ 2

  X1 = ULCol * 8 - 16
  X2 = X1 + BWidth * 8 + 8
  y1 = ULRow * GPDat(71) - GPDat(71) - halfhite
  y2 = y1 + NumRows * GPDat(71) + GPDat(71) + 4

  HideCursor

  IF GPDat(71) = 8 THEN TopOS = 4
  GMove2VE ULCol - 2, y1 - GPDat(71) - TopOS, BWidth + 4, y2 - y1 + 2 * GPDat(71) + 2 * TopOS, &HAA00, 0

  LINE (X2, y1 - GPDat(71) + 8)-(X2 + 10, y2 + 10), 0, BF 'V Shadow
  LINE (X1 + 4, y2)-(X2 + 10, y2 + 10), 0, BF             'H Shadow

  LINE (X1 - 4, y1 - GPDat(71) - TopOS)-(X2 + 4, y2 + 4), BGClr, BF 'Dialog BackGround
  PCBox X1 - 4, y1 - GPDat(71) - TopOS, X2 + 4, y2 + 4, -1
  PCBox X1, y1 + 2 - TopOS, X2, y2, 0

  PCBox X1 + 3, y1 + 4 - TopOS, X2 - 3, y2 - 3, 0

  GPrint3VE y1 - GPDat(71) + 2 - TopOS, 41 - (LEN(RTRIM$(DLog(0).Heading)) \ 2) + 1, DLog(0).Heading, TextClr

  CALL Icons(X1, y1, Style)

  StartRow = ULRow - 1
  StartCol = ULCol - 1

  REDIM Mouse1(UpperBound) AS MCoords    'Array of Mouse Coords
  GOSUB DispText                         'Display the Text$()
  GOSUB DispResp                         'Display the Response$()
  FOR i = 1 TO UpperBound
    IF (DLog(i).DType > 0 AND DLog(i).DType < 5) OR DLog(i).DType = 8 THEN
      EXIT FOR
    END IF
  NEXT
  IF i > UpperBound THEN i = UpperBound
  IF DLog(i).DType = 2 THEN GOSUB FindIt
  CALL StuffBuf(CHR$(129))               'Fall through loop the first time
 ShowCursor
END IF

IF Action = 1 THEN Action = 3
IF Action = 2 THEN
  GOSUB CopyResp
  GOSUB DispResp
  Action = 3
  i = Exiti
  Lasti = i
  CALL StuffBuf(CHR$(129))
END IF

IF Action = 5 THEN
  HideCursor
  GMove2VE ULCol - 2, y1 - GPDat(71) - TopOS, BWidth + 4, y2 - y1 + 2 * GPDat(71) + 2 * TopOS, &HAA00, -1
  ShowCursor
  EXIT SUB
END IF


'---- Look for keystrokes and mouse clicks
DO
  Ky$ = UCASE$(INKEY$)

  SELECT CASE LEN(Ky$)
    CASE 0
      Ky = 0
    CASE 1
      Ky = ASC(Ky$)
    CASE 2
      Ky = -ASC(RIGHT$(Ky$, 1))
  END SELECT

  SELECT CASE Ky

    CASE 9                                 'Tab
      LastGroup = DLog(i).Group
      GOSUB FindIt
    CASE -15                               'Shift Tab, go backwards
      LastGroup = DLog(i).Group
      GOSUB FindItB

    CASE 32                                'Spacebar
      IF DLog(i).DType = 1 THEN            'If its a check box
	GOSUB CheckIt                      'Check or un-Check it
      ELSE
	GOSUB WhichButton                  'Check to see if Cancel was enabled
	MouseFlg = -1
	i = Choice
      END IF

    CASE 27                                'Escape
      Choice = Ky' -1
      WaitUp
      CALL WaitUpKey
      FOR i = 1 TO UpperBound
	Response$(i) = TResponse$(i)
      NEXT
      HideCursor
      GMove2VE ULCol - 2, y1 - GPDat(71) - TopOS, BWidth + 4, y2 - y1 + 2 * GPDat(71) + 2 * TopOS, &HAA00, -1
      ShowCursor
      Action = 4
      EXIT SUB

    CASE 13                                'Enter
      GOSUB WhichButton                    'Check to see if Cancel was enabled
      MouseFlg = -1
      Exiti = i
      i = Choice

    CASE -80, -77                          'down, right
      IF DLog(i).DType = 2 THEN            'If its an Option Button
	ThisGroup = DLog(i).Group          'Save the group number
	OClr = BGClr                       'Erase
	GOSUB OMark                        ' last mark
	i = i + 1
	IF i > UpperBound THEN i = 1       'Make sure we stay in bounds
	IF DLog(i).Group <> ThisGroup THEN 'If we're out of this group,
	  FOR i = 1 TO UpperBound          'find where it starts
	    IF DLog(i).Group = ThisGroup THEN EXIT FOR
	  NEXT
	  IF i > UpperBound THEN i = UpperBound
	END IF
      ELSE
	Ky = 0
      END IF

    CASE -72, -75                          'Up, Left

      IF DLog(i).DType = OptButton THEN    'If its an Option Button
	ThisGroup = DLog(i).Group
	OClr = BGClr
	GOSUB OMark
	i = i - 1
	IF i < 1 THEN i = UpperBound
	  IF DLog(i).Group <> ThisGroup THEN
	    FOR i = UpperBound TO 1 STEP -1
	      IF DLog(i).Group = ThisGroup THEN EXIT FOR
	    NEXT
	  IF i < 1 THEN i = 1
	END IF
      ELSE
	Ky = 0
      END IF

    CASE 65 TO 90, -50 TO -16              'Look for HotKeys...
      GOSUB HKSearch

    CASE 129

    CASE ELSE
      Ky = 0

  END SELECT

  GetCursor x, y, Button        '************* Handle mouse clicks
  IF Button > 1 THEN
    WaitUp
    CALL StuffBuf(CHR$(27))
  ELSEIF Button = 1 THEN
    El = i
    FOR i = 1 TO UpperBound
      IF x >= Mouse1(i).X1 AND x <= Mouse1(i).X2 AND y >= Mouse1(i).y1 AND y <= Mouse1(i).y2 THEN
	MouseFlg = -1
	EXIT FOR
      END IF
    NEXT
    IF i = UpperBound + 1 THEN
      i = El
    END IF
  END IF

  IF MouseFlg OR Ky <> 0 THEN    '************** Handle the selection

    IF WasCheckFld AND i <> Lasti THEN
      A = LastChkBox
      DotClr = BGClr
      GOSUB DotBox                         'Erase last DotBox
      IF GPDat(71) = 8 THEN
	GPrint1VE StartRow + DLog(A).Row, StartCol + DLog(A).Col + 2, RTRIM$(DLog(A).Heading), TextClr
	GPrint1VE StartRow + DLog(A).Row, StartCol + DLog(A).Col + 2 + DLog(A).HotKey, MID$(DLog(A).Heading, DLog(A).HotKey + 1, 1), HKClr
	IF A - 1 > -1 THEN
	  IF DLog(A - 1).DType > 0 AND DLog(A - 1).DType < 3 THEN
	    IF DLog(A).Row - 1 = DLog(A - 1).Row THEN
	      GPrint1VE StartRow + DLog(A - 1).Row, StartCol + DLog(A - 1).Col + 2, RTRIM$(DLog(A - 1).Heading), TextClr
	      GPrint1VE StartRow + DLog(A - 1).Row, StartCol + DLog(A - 1).Col + 2 + DLog(A - 1).HotKey, MID$(DLog(A - 1).Heading, DLog(A - 1).HotKey + 1, 1), HKClr
	    END IF
	  END IF
	END IF

      END IF
    END IF

    IF PB > 0 THEN                         'Erase Box around Button
      BClr = BGClr
      CALL NCBox(StartCol * 8 - 9 + DLog(PB).Col, (StartRow - 1) * GPDat(71) - 1 + DLog(PB).Row, DLog(PB).TextLen + 1, DLog(PB).NumRows + 1, -1, BClr)
      CALL NCBox(BtnHCenter - ((LEN(RTRIM$(DLog(PB).Heading)) * 8) \ 2) - 3, (StartRow - 1) * GPDat(71) + DLog(PB).Row + DLog(PB).NumRows \ 2 - GPDat(71) \ 2, LEN(RTRIM$(DLog(PB).Heading)) * 8 + 6, GPDat(71), 21845, BClr)
      PB = 0
    END IF
	    'Choice
    IF i <> 1 THEN 'Choice AND Default > 0 THEN     'If we're off the default
      IF DLog(i).DType <> 6 THEN                    'Draw box
	CALL NCBox(StartCol * 8 - 9 + DLog(1).Col, (StartRow - 1) * GPDat(71) - 1 + DLog(1).Row, DLog(1).TextLen + 1, DLog(1).NumRows + 1, -1, 0)
      ELSEIF DLog(i).DType = 6 THEN   'Erase Box
	CALL NCBox(StartCol * 8 - 9 + DLog(Choice).Col, (StartRow - 1) * GPDat(71) - 1 + DLog(Choice).Row, DLog(Choice).TextLen + 1, DLog(Choice).NumRows + 1, -1, BGClr)
      END IF
      Choice = 1                           'Set the default
    END IF

    SELECT CASE DLog(i).DType
      CASE Caption, Frame                  'Plain string
	GOSUB FindIt
	CALL StuffBuf(CHR$(129))           'Dummy char to fall through loop
      CASE CheckBox                        'CheckBox
	IF MouseFlg THEN
	  HideCursor
	  LINE (Mouse1(i).X1 + 1, Mouse1(i).y1 + 2 + CheckBoxOS)-STEP(BxWide - 2, BxHite - 2), 0, B
	  ShowCursor
	END IF

	A = i
	DotClr = 0
	GOSUB DotBox                       'Draw dotted box around the heading
	LastChkBox = i
	WaitUp

	IF MouseFlg THEN
	  IF Response$(i) = "X" THEN
	    Response$(i) = " "
	    XClr = 7
	  ELSE
	    Response$(i) = "X"
	    XClr = 0
	  END IF
	  HideCursor
	  LINE (Mouse1(i).X1 + 1, Mouse1(i).y1 + 2 + CheckBoxOS)-STEP(BxHite - 2, BxHite - 2), BGClr, B
	  ShowCursor
	  GOSUB XMark
	END IF
	WasCheckFld = -1

      CASE OptButton                       'Option Button
	A = i
	DotClr = 0
	GOSUB DotBox                       'Draw dotted box around the heading
	LastChkBox = i

	IF MouseFlg THEN
	  HideCursor
	  CIRCLE ((StartCol + DLog(i).Col) * 8 - 8, (StartRow + DLog(i).Row) * GPDat(71) - OS), ORadius - 1, 0
	  ShowCursor
	END IF
	Group = DLog(i).Group
	FOR T = 1 TO UpperBound            'Find the last one Checked
	  IF Response$(T) = "O" AND DLog(T).Group = Group THEN
	    Response$(T) = " "
	    Response$(i) = "O"
	    EXIT FOR
	  END IF
	NEXT

	WaitUp
	HideCursor
	CIRCLE ((StartCol + DLog(i).Col) * 8 - 8, (StartRow + DLog(i).Row) * GPDat(71) - OS), ORadius - 1, BGClr
	ShowCursor
	Tempi = i                          'Save i just for a sec
	i = T
	IF MouseFlg THEN
	  OClr = BGClr
	  GOSUB OMark
	END IF
	i = Tempi                          'Now copy it back
	OClr = 0
	GOSUB OMark
	WasCheckFld = -1

      CASE TextIn                          'Text Entry
	WasCheckFld = 0

	Nd = INSTR(DLog(i).Heading, "<")
	Tnd$ = RTRIM$(RIGHT$(DLog(i).Heading, 50 - Nd))
	Accept = VAL(Tnd$)
	TxtPos = LEN(QPTrim$(Response$(i)))
	LOCATE StartRow + DLog(i).Row, StartCol + DLog(i).Col + LEN(RTRIM$(DLog(i).Heading)) - 1 + (Accept >= 10)
	GEditor Response$(i), StartCol + DLog(i).Col + LEN(RTRIM$(DLog(i).Heading)) - 1 + (Accept >= 10), Ky1$, TxtPos, Accept, Button, HotKeyTbl()
	TResponse$(i) = Response$(i)

	IF Ky1$ < CHR$(255) THEN
	  CALL StuffBuf(Ky1$)
	END IF

      CASE DMenu                           'Menu
	WasCheckFld = 0
	MAction = 6
	MChoice = DLog(i).MenuChoice
	LastMChoice = 0

	LOCATE StartRow + DLog(i).Row, StartCol + DLog(i).Col

	DO
	  ListBox Menu$(), MChoice, LEN(Menu$(1)), DLog(i).NumRows, Ky2$, HotKeyTbl(), MAction
	
	  IF DLog(i).Group THEN
	    IF MChoice <> LastMChoice OR Ky2$ = CHR$(13) THEN
	      IF MChoice THEN
		LastMChoice = MChoice
		Nd = INSTR(DLog(DLog(i).Group).Heading, "<")
		Txt$ = Menu$(MChoice)

		HideCursor
		LINE ((StartCol + DLog(DLog(i).Group).Col + Nd) * 8 - 9, (StartRow + DLog(DLog(i).Group).Row - 1) * GPDat(71))-STEP(DLog(DLog(i).Group).TextLen * 8, GPDat(71) - 2), 7, BF
		GPrint3VE (StartRow + DLog(DLog(i).Group).Row - 1) * GPDat(71), StartCol + DLog(DLog(i).Group).Col + Nd, Txt$, TextClr
		ShowCursor
		LSET Response$(DLog(i).Group) = Txt$
	      END IF
	    END IF
	  END IF
	LOOP UNTIL MAction = 4

	DLog(i).MenuChoice = MChoice

	CALL StuffBuf(Ky2$)
	Response$(i) = STR$(ABS(MChoice))


      CASE PButton                         'Push Button
	BClr = 0
	PB = i
	CALL NCBox(StartCol * 8 - 9 + DLog(PB).Col, (StartRow - 1) * GPDat(71) - 1 + DLog(PB).Row, DLog(PB).TextLen + 1, DLog(PB).NumRows + 1, -1, BClr)
	Choice = i

	BtnHCenter = StartCol * 8 - 8 + DLog(PB).Col + DLog(PB).TextLen \ 2
	CALL NCBox(BtnHCenter - ((LEN(RTRIM$(DLog(PB).Heading)) * 8) \ 2) - 3, (StartRow - 1) * GPDat(71) + DLog(PB).Row + DLog(PB).NumRows \ 2 - GPDat(71) \ 2, LEN(RTRIM$(DLog(PB).Heading)) * 8 + 6, GPDat(71), 21845, 8)'BClr)

	IF MouseFlg THEN
	  MouseFlg = 0
	  WasCheckFld = 0
	  HideCursor
	  SPushButton (StartCol) * 8 - 8 + DLog(i).Col, (StartRow - 1) * GPDat(71) + DLog(i).Row, DLog(i).TextLen, DLog(i).NumRows, RTRIM$(DLog(i).Heading$), 0, HotKey
	  ShowCursor

	  WaitUp
	  IF Ky > 0 THEN
	    DO
	      WHILE INKEY$ <> "": WEND
	      x = KeyDown%
	    LOOP UNTIL x = 0
	  END IF
					   'Let it up
	  HideCursor
	  SPushButton (StartCol) * 8 - 8 + DLog(i).Col, (StartRow - 1) * GPDat(71) + DLog(i).Row, DLog(i).TextLen, DLog(i).NumRows, RTRIM$(DLog(i).Heading$), 1, DLog(i).HotKey
	  Pause 4

	  IF Action = 0 OR Action = 5 THEN
	    GMove2VE ULCol - 2, y1 - GPDat(71) - TopOS, BWidth + 4, y2 - y1 + 2 * GPDat(71) + 2 * TopOS, &HAA00, -1
	    ShowCursor
	    GOSUB WhichButton
	    EXIT SUB
	  END IF
	  ShowCursor
	  Action = 4
	END IF

      CASE MultLine                        'Multi-Line text
	Nd = INSTR(DLog(i).Heading, "<")
	EndMarker = VAL(RIGHT$(DLog(i).Heading, 50 - Nd))
	WasCheckFld = 0
	GOSUB SetUpEdit
	QAction = 1

	ElNo = i

	x$ = ""
	DO
	  GetCursor Qx, Qy, Button
	  QEdit Work$(), x$, QAction, Ed
	  IF LEN(x$) = 2 THEN
	    Ky = -ASC(RIGHT$(x$, 1))
	    IF Ky < -15 AND Ky > -51 THEN
	      GOSUB HKSearch
	      IF Found AND i <> Tempi THEN EXIT DO
	    END IF
	  END IF

	  IF Button = 1 THEN
	    IF Qx < Mouse1(i).X1 OR Qx > Mouse1(i).X2 OR Qy < Mouse1(i).y1 OR Qy > Mouse1(i).y2 THEN
	      QAction = 4
	    END IF
	  END IF
	  IF Button = 2 OR x$ = CHR$(27) THEN
	    QAction = 4
	    x$ = CHR$(27)
	  END IF

	LOOP UNTIL QAction = 4 OR x$ = CHR$(9) OR x$ = CHR$(0) + CHR$(15)
	TT$ = x$
	QEdit Work$(), x$, 5, Ed
	Response$(ElNo) = ""

	Temp = UBOUND(Work$)               'in the form array
	Cnt = FindLast%(VARPTR(Work$(Temp)), Temp)
	DrawText$ = ""
	IF EndMarker THEN DrawText$ = ""  '(return line identifier)

	FOR N = 1 TO Cnt
	  Response$(ElNo) = Response$(ElNo) + Work$(N) + " " + DrawText$
	NEXT

	TResponse$(ElNo) = Response$(ElNo)

	REDIM Work$(0)
	CALL StuffBuf(TT$)
	IF Found GOTO QEHK

      CASE ELSE

    END SELECT
  END IF

  MouseFlg = 0
  Found = 0
  Lasti = i

QEHK:

  IF Action > 1 THEN EXIT SUB

LOOP


		'****************** Gosubs *******************

CopyResp:
  FOR i = 1 TO UpperBound                  'Make a copy of the Response$ array
    TResponse$(i) = Response$(i)           ' in case of Esc or right mouse click
  NEXT                                     ' Work on the temporary array
RETURN

FindIt:
  DO
    i = i + 1
    IF i > UpperBound THEN i = 1
  LOOP UNTIL DLog(i).DType < Caption AND DLog(i).DType > 0 OR DLog(i).DType = PButton OR DLog(i).DType = MultLine
  IF DLog(i).DType = OptButton THEN        'If its an Option button
    IF LastGroup = DLog(i).Group THEN      ' and we're still in the same group
      DO                                   ' find the next field

	i = i + 1
	 IF i > UpperBound THEN i = 1
      LOOP UNTIL Response$(i) = "O" OR (DLog(i).DType <> OptButton AND DLog(i).DType <> Frame)
    ELSE                                   'If we're in a new Option group,
      i = i - 1                            ' step back one
      DO                                   ' and find the Dot
	i = i + 1
	IF i > UpperBound THEN i = 1
      LOOP UNTIL Response$(i) = "O"
    END IF
  END IF
RETURN

FindItB:
  DO
    i = i - 1
    IF i < 1 THEN i = UpperBound
  LOOP UNTIL DLog(i).DType < Caption AND DLog(i).DType > 0 OR DLog(i).DType = PButton OR DLog(i).DType = MultLine

  IF DLog(i).DType = OptButton THEN
    IF LastGroup = DLog(i).Group THEN
      DO
	i = i - 1
	IF i < 1 THEN i = UpperBound
      LOOP UNTIL Response$(i) = "O" OR (DLog(i).DType <> OptButton AND DLog(i).DType <> Frame)
    ELSE
      i = i + 1
      DO
	i = i - 1
	IF i < 1 THEN i = UpperBound
      LOOP UNTIL Response$(i) = "O"
    END IF
  END IF
RETURN

WhichButton:
  IF DLog(i).Group = 1000 THEN
    Choice = i
    FOR v = 1 TO UpperBound
      Response$(v) = TResponse$(v)
    NEXT
  END IF
RETURN

CheckIt:
  IF Response$(i) = "X" THEN
    Response$(i) = " "
    XClr = 7
  ELSE
    Response$(i) = "X"
    XClr = 0
  END IF
  GOSUB XMark
RETURN

XMark:
  HideCursor
  LINE (Mouse1(i).X1 + 1, Mouse1(i).y1 + 2 + CheckBoxOS)-STEP(BxWide - 2, BxHite - 2), XClr
  LINE (Mouse1(i).X1 + 1, Mouse1(i).y1 + BxHite + CheckBoxOS)-STEP(BxWide - 2, -BxHite + 2), XClr
  ShowCursor
RETURN

OMark:
  HideCursor
  CIRCLE ((StartCol + DLog(i).Col) * 8 - 8, (StartRow + DLog(i).Row) * GPDat(71) - OS), ORadius \ 2, OClr
  PAINT ((StartCol + DLog(i).Col) * 8 - 8, (StartRow + DLog(i).Row) * GPDat(71) - OS), OClr, OClr
  ShowCursor
RETURN

DotBox:
  HideCursor
  LINE ((StartCol + DLog(A).Col + 1) * 8 - 4, (StartRow + DLog(A).Row) * GPDat(71) - GPDat(71) - DotBoxOS)-STEP(LEN(RTRIM$(DLog(A).Heading)) * 8 + 8, 0), DotClr, , 21845
  LINE ((StartCol + DLog(A).Col + 1) * 8 - 4, (StartRow + DLog(A).Row) * GPDat(71) - DotBoxOS)-STEP(LEN(RTRIM$(DLog(A).Heading)) * 8 + 8, 0), DotClr, , 21845
  LINE ((StartCol + DLog(A).Col + 1) * 8 - 4, (StartRow + DLog(A).Row) * GPDat(71) - DotBoxOS)-STEP(0, -GPDat(71)), DotClr, , 21845 '13107'-772
  LINE ((StartCol + DLog(A).Col + 1) * 8 - 4 + LEN(RTRIM$(DLog(A).Heading)) * 8 + 8, (StartRow + DLog(A).Row) * GPDat(71) - DotBoxOS)-STEP(0, -GPDat(71)), DotClr, , 21845
  ShowCursor
RETURN

DispText:
FOR i = 1 TO UpperBound                    'Display text, save Mouse Coords
  SELECT CASE DLog(i).DType
    CASE CheckBox                          'Check Box
      GPrint1VE StartRow + DLog(i).Row, StartCol + DLog(i).Col + 2, RTRIM$(DLog(i).Heading), TextClr
      Mouse1(i).X1 = (StartCol + DLog(i).Col) * 8 - 13
      Mouse1(i).X2 = Mouse1(i).X1 + ((LEN(RTRIM$(DLog(i).Heading)) + 3) * 8) + 8
      Mouse1(i).y1 = (StartRow + DLog(i).Row) * GPDat(71) - GPDat(71) + (GPDat(71) = 8)
      Mouse1(i).y2 = Mouse1(i).y1 + GPDat(71)

      LINE (Mouse1(i).X1, Mouse1(i).y1 + 1 + CheckBoxOS)-STEP(BxWide, BxHite), 0, B

      IF GPDat(71) = 8 THEN
	GPrint1VE StartRow + DLog(i).Row, StartCol + DLog(i).Col + 2 + DLog(i).HotKey, MID$(DLog(i).Heading, DLog(i).HotKey + 1, 1), 15
      ELSE
	LINE ((StartCol + DLog(i).Col + DLog(i).HotKey + 1) * 8, (StartRow + DLog(i).Row) * GPDat(71) - 3 + OS1)-STEP(6, 0), TextClr
      END IF

    CASE OptButton                         'Option Button
      GPrint1VE StartRow + DLog(i).Row, StartCol + DLog(i).Col + 2, RTRIM$(DLog(i).Heading), TextClr
      Mouse1(i).X1 = (StartCol + DLog(i).Col) * 8 - 14
      Mouse1(i).X2 = Mouse1(i).X1 + ((LEN(RTRIM$(DLog(i).Heading)) + 3) * 8)
      Mouse1(i).y1 = (StartRow + DLog(i).Row) * GPDat(71) - GPDat(71)'+ 4
      Mouse1(i).y2 = Mouse1(i).y1 + GPDat(71)
      IF Response$(i) = "O" THEN GOSUB OMark
      CIRCLE ((StartCol + DLog(i).Col) * 8 - 8, (StartRow + DLog(i).Row) * GPDat(71) - OS), ORadius, 0

      IF GPDat(71) = 8 THEN
	GPrint1VE StartRow + DLog(i).Row, StartCol + DLog(i).Col + 2 + DLog(i).HotKey, MID$(DLog(i).Heading, DLog(i).HotKey + 1, 1), 15
      ELSE
	LINE ((StartCol + DLog(i).Col + DLog(i).HotKey + 1) * 8, (StartRow + DLog(i).Row) * GPDat(71) - 3 + OS1)-STEP(6, 0), TextClr
      END IF


    CASE TextIn                            'Text Entry Field
      Nd = INSTR(DLog(i).Heading, "<")
      GPrint1VE StartRow + DLog(i).Row, StartCol + DLog(i).Col, LEFT$(DLog(i).Heading, Nd - 1), TextClr
      Mouse1(i).X1 = (StartCol + DLog(i).Col + Nd) * 8 - 10
      Mouse1(i).X2 = Mouse1(i).X1 + DLog(i).TextLen * 8 + 3
      Mouse1(i).y1 = (StartRow + DLog(i).Row) * GPDat(71) - GPDat(71) - 1
      Mouse1(i).y2 = Mouse1(i).y1 + GPDat(71)
      IF DLog(i).Group > -1 THEN  'dont draw a box if its part of a group
	PCBox Mouse1(i).X1, Mouse1(i).y1, Mouse1(i).X2, Mouse1(i).y2 + (DLog(i).NumRows * GPDat(71)), 0
	IF GPDat(71) = 8 THEN
	  GPrint1VE StartRow + DLog(i).Row, StartCol + DLog(i).Col + DLog(i).HotKey, MID$(DLog(i).Heading, DLog(i).HotKey + 1, 1), 15
	ELSE
	  LINE ((StartCol + DLog(i).Col + DLog(i).HotKey - 1) * 8, (StartRow + DLog(i).Row) * GPDat(71) - 3 + OS1)-STEP(6, 0), TextClr
	END IF
      END IF

    CASE DMenu                             'Menu
      Nd = INSTR(DLog(i).Heading, "<")
      GPrint3VE (StartRow + DLog(i).Row - 1) * GPDat(71) - GPDat(71) - 3, StartCol + DLog(i).Col - 1, LEFT$(DLog(i).Heading, Nd - 1), TextClr
      IF GPDat(71) = 8 THEN
	GPrint3VE (StartRow + DLog(i).Row - 1) * GPDat(71) - GPDat(71) - 3, StartCol + DLog(i).Col + DLog(i).HotKey - 1, MID$(DLog(i).Heading, DLog(i).HotKey + 1, 1), 15
      ELSE
	LINE ((StartCol + DLog(i).Col + DLog(i).HotKey - 2) * 8, (StartRow + DLog(i).Row - 1) * GPDat(71) - 6 + OS1)-STEP(6, 0), 0
      END IF

   CASE PButton                            'Push Button

      SPushButton (StartCol) * 8 - 8 + DLog(i).Col, (StartRow - 1) * GPDat(71) + DLog(i).Row, DLog(i).TextLen, DLog(i).NumRows, RTRIM$(DLog(i).Heading$), 1, DLog(i).HotKey
      Mouse1(i).X1 = (StartCol) * 8 - 8 + DLog(i).Col
      Mouse1(i).X2 = Mouse1(i).X1 + DLog(i).TextLen
      Mouse1(i).y1 = (StartRow) * GPDat(71) - GPDat(71) + DLog(i).Row
      Mouse1(i).y2 = Mouse1(i).y1 + DLog(i).NumRows

    CASE Frame                             'Heading Box                                                                                                                                                                                               '-2
      PCBox (StartCol + DLog(i).Col) * 8 - 3, (StartRow + DLog(i).Row) * GPDat(71) - GPDat(71) \ 2, (StartCol + DLog(i).Col) * 8 - 3 + DLog(i).TextLen * 8 - 2, ((StartRow + DLog(i).Row) * GPDat(71) - GPDat(71) \ 2 + (DLog(i).NumRows + 1) * GPDat(71) _
 - 3) - (GPDat(71) = 8), 0
      IF LEN(QPTrim$(DLog(i).Heading)) THEN
	GPrint2VE (StartRow + DLog(i).Row) * GPDat(71) - GPDat(71) - 1, StartCol + DLog(i).Col + 1, " " + RTRIM$(DLog(i).Heading) + " ", TextClr
      END IF

    CASE MultLine

      Nd = INSTR(DLog(i).Heading, "<")
      GPrint1VE StartRow + DLog(i).Row, StartCol + DLog(i).Col, LEFT$(DLog(i).Heading, Nd - 1), TextClr
      Mouse1(i).X1 = (StartCol + DLog(i).Col + Nd) * 8 - 10
      Mouse1(i).X2 = Mouse1(i).X1 + DLog(i).TextLen * 8 + 3
      Mouse1(i).y1 = (StartRow + DLog(i).Row) * GPDat(71) - GPDat(71) - 1
      Mouse1(i).y2 = Mouse1(i).y1 + DLog(i).NumRows * GPDat(71) + 1
      IF GPDat(71) = 8 THEN
	GPrint1VE StartRow + DLog(i).Row, StartCol + DLog(i).Col + DLog(i).HotKey, MID$(DLog(i).Heading, DLog(i).HotKey + 1, 1), 15
      ELSE
	LINE ((StartCol + DLog(i).Col + DLog(i).HotKey - 1) * 8, (StartRow + DLog(i).Row) * GPDat(71) - 3 + OS1)-STEP(6, 0), TextClr
      END IF
      PCBox Mouse1(i).X1, Mouse1(i).y1, Mouse1(i).X2, Mouse1(i).y2, 0

      GOSUB SetUpEdit

      QEdit Work$(), x$, 1, Ed

    CASE ELSE
  END SELECT

NEXT
RETURN

DispResp:
HideCursor
FOR i = 1 TO UpperBound                    'Display text, save Mouse Coords
  SELECT CASE DLog(i).DType
    CASE CheckBox                          'Check Box
      XClr = 7
      IF Response$(i) = "X" THEN XClr = 0
      GOSUB XMark

    CASE OptButton                         'Option Button
      OClr = 7
      IF Response$(i) = "O" THEN OClr = 0
      GOSUB OMark

    CASE TextIn                            'Text Entry Field
      Nd = INSTR(DLog(i).Heading, "<")
      Ln = LEN(Response$(i))
      IF Ln > DLog(i).TextLen THEN
	Response$(i) = LEFT$(Response$(i), DLog(i).TextLen)
      ELSEIF Ln <= DLog(i).TextLen THEN
	Response$(i) = Response$(i) + SPACE$(DLog(i).TextLen - LEN(Response$(i)))
      END IF
      IF Action THEN LINE ((StartCol + DLog(i).Col + Nd) * 8 - 9, (StartRow + DLog(i).Row - 1) * GPDat(71))-STEP(DLog(i).TextLen * 8, GPDat(71) - 2), 7, BF
      GPrint3VE (StartRow + DLog(i).Row - 1) * GPDat(71), StartCol + DLog(i).Col + Nd, Response$(i), TextClr

    CASE DMenu                             'Menu
      MAction = -3
      IF Action = 2 THEN LINE (LastMenux1, LastMenuy1)-(LastMenux2, LastMenuy2), BGClr, BF
      LOCATE StartRow + DLog(i).Row, StartCol + DLog(i).Col
      MChoice = DLog(i).MenuChoice
      MenuRows = DLog(i).NumRows
      ListBox Menu$(), MChoice, LEN(Menu$(1)), MenuRows, Ky$, HotKeyTbl(), MAction
      MenuBound = UBOUND(Menu$)

      IF MenuBound > MenuRows THEN
	ScrlWidth = 27
      ELSE
	ScrlWidth = 8
      END IF

      Mouse1(i).X1 = (StartCol + DLog(i).Col - 2) * 8 - 1
      Mouse1(i).X2 = Mouse1(i).X1 + LEN(Menu$(1)) * 8 + ScrlWidth
      Mouse1(i).y1 = (StartRow + DLog(i).Row) * GPDat(71) - GPDat(71) - 1
      Mouse1(i).y2 = Mouse1(i).y1 + MinInt(MenuRows, MenuBound) * GPDat(71) + 1

      LastMenux1 = Mouse1(i).X1
      LastMenuy1 = Mouse1(i).y1
      LastMenux2 = Mouse1(i).X2
      LastMenuy2 = Mouse1(i).y2

    CASE Caption                           'Plain String
      Blank$ = QPTrim$(DLog(i).Heading)
      IF LEN(Blank$) THEN
	IF Action > 1 THEN
	  GPrint2VE StartRow * GPDat(71) + DLog(i).Row, StartCol + DLog(i).Col, SPACE$(LastLen), TextClr
	END IF
	GPrint2VE StartRow * GPDat(71) + DLog(i).Row, StartCol + DLog(i).Col, RTRIM$(DLog(i).Heading), TextClr
	LastLen = LEN(RTRIM$(DLog(i).Heading))
      END IF

    CASE PButton, Frame                    'Push Button, Heading Box

    CASE ELSE
  END SELECT

NEXT
XClr = 0
OClr = 0
ShowCursor
RETURN

SetUpEdit:
  Nd = INSTR(DLog(i).Heading, "<")
  Ed.Rows = DLog(i).NumRows
  Ed.Wide = DLog(i).TextLen
  Ed.Wrap = Ed.Wide - 2
  Ed.HTab = 8
  Ed.AColor = 7 * 256
  Ed.LC = 0
  Ed.LSCol = 0
  Ed.CurCol = 0'StartCol + DLog(i).Col + LEN(RTRIM$(DLog(i).Heading)) - 1
  Ed.CurLine = 1
  IF DLog(i).MenuChoice = 0 THEN DLog(i).MenuChoice = 6
  REDIM Work$((DLog(i).MenuChoice))
  Work$(1) = Response$(i)
  LOCATE StartRow + DLog(i).Row, StartCol + DLog(i).Col + Nd
RETURN


HKSearch:
  IF Ky < -15 THEN                         'Alt + some key
    Ty = ABS(Ky) - 15                      'Find offset in AltHotKey$
    Ky = ASC(MID$(AltHotKey$, Ty, 1))      'Assign Ky that ASCII value
  END IF

  Tempi = i
  LC = i + 1
  Search = LC
  DO
    IF Search > UpperBound THEN Search = 1
    IF Ky = HotKeyTbl(Search) THEN
      i = Search
      Found = -1
      MouseFlg = -1
      EXIT DO                              ' to press the button
    END IF
    Search = Search + 1
  LOOP UNTIL Search = LC

  Search = Search + 1
  IF Search > UpperBound THEN Search = 1
  LC = Search

  '---- Allow hot key to activate itself if pb
  IF NOT Found OR i = Tempi AND DLog(i).DType <> 6 THEN
    Ky = 0
    IF DLog(i).DType > 2 THEN Ky = 255
    MouseFlg = 0
  END IF

RETURN



END SUB

SUB GEditor (Edit$, LeftCol, Ky$, TxtPos, Accept, Button, HotKeyTbl()) STATIC

    FirstKey = Button = 0

    Temp$ = Edit$
    HKUpper = UBOUND(HotKeyTbl, 1)
    Style = 0
    IF Accept > 9 THEN
      Accept = Accept - 10
      Style = -1
    END IF

    HiLiteClr = 0          'Edit background color
    GTextClr = 15          'Edit text color
    CursorTiming = 4   'A value of 9 will make the cursor blink once a second
		       'CursorTiming is measured in 1/18ths of a second per blink

    PixelsPerCol = 8                            'Assume 8 pixels horizontaly

    CursorRow = (CSRLIN - 1) * GPDat(71)        'Find vertical pixel position

    TxtPos = POS(0) - LeftCol + TxtPos + 1        'Get the cursor's location to
    IF TxtPos < 1 THEN TxtPos = 1               '  see where to begin editing
    IF TxtPos > LEN(Edit$) THEN TxtPos = LEN(Edit$)

    HideCursor
   ' LINE ((LeftCol - 1) * 8 - 1, CSRLIN * GPDat(71) - GPDat(71))-STEP(LEN(Edit$) * 8 + 1, GPDat(71) - 2), HiLiteClr, BF
    GOSUB PrintTheString
    CALL ShowCursor
    mx1 = POS(0) * 8 - 8
    mx2 = mx1 + LEN(Edit$) * 8
    my1 = CSRLIN * GPDat(71) - GPDat(71)
    my2 = my1 + GPDat(71)
    WaitUp

    LastPos = 0

    '----- Main loop for handling key presses
    DO
       IF LastPos <> TxtPos THEN
	   GOSUB MoveCursor
	   LastPos = TxtPos
       END IF

       AtCol = LeftCol + TxtPos - 1

       'Blink the cursor
       CALL DrawCursor((AtCol - 1) * 8, CursorRow, 1 - 7 * Inserting, GPDat(71) - 2, CursorIs, CursorTiming)

       Ky$ = INKEY$

	 CALL GetCursor(x, y, Button)
	 IF Button > 1 THEN
	   WaitUp
	   Ky$ = CHR$(27)
	 END IF

	 IF Button = 1 THEN
	   IF x > mx1 AND x < mx2 AND y > my1 AND y < my2 THEN
	     TxtPos = x \ 8 - LeftCol + 2
	     FirstKey = 0
	   ELSE
	     Temp$ = ""
	     Button = 1001
	     EXIT DO
	   END IF
	 END IF

       SELECT CASE LEN(Ky$)                     'Make a key code from Ky$
	  CASE 0
	     KeyCode = 0
	  CASE 1
	     KeyCode = ASC(Ky$)                 'Single character key
	  CASE 2
	     KeyCode = -ASC(RIGHT$(Ky$, 1))     'Extended keys are negative
	  CASE ELSE
       END SELECT

       '----- Branch according to the key pressed
       SELECT CASE KeyCode
	  CASE 0

	  '----- Backspace
	  CASE 8
	     TxtPos = TxtPos - 1                'Back up the text pointer
	     IF TxtPos > 0 THEN                 'Still within the field?
		IF Inserting THEN               'Truncate the string
		   MID$(Edit$, TxtPos) = MID$(Edit$, TxtPos + 1) + " "
		ELSE                            'Blank the letter
		   MID$(Edit$, TxtPos) = " "
		END IF
		GOSUB PrintTheString
	     END IF
	     FirstKey = 0

	  '----- Enter or Escape
	  CASE 13, 27, 9', -15
	     GOSUB MoveCursor
	     Temp$ = ""
	     EXIT DO                            'Bail out

	  '----- Up
	  CASE -72, -15                             'Make like Shift Tab
	    GOSUB MoveCursor
	    Ky$ = CHR$(0) + CHR$(15)
	    Temp$ = ""
	    FirstKey = 0
	    EXIT DO

	  '----- Down
	  CASE -80                              'Make like Tab
	    GOSUB MoveCursor
	    Ky$ = CHR$(9)
	    Temp$ = ""
	    FirstKey = 0
	    EXIT DO

	  '----- Letter keys
	  CASE 32 TO 254
	     Found = 0
	     SELECT CASE Accept
	       CASE 1        'Double and Single precision
		 IF INSTR(SDPrecision$, Ky$) THEN Found = -1
	       CASE 2        'Integers Only
		 IF INSTR(IntOnly$, Ky$) THEN Found = -1
	       CASE 3        'Integers + ","
		 IF INSTR(IntComma$, Ky$) THEN Found = -1
	       CASE 5        'All UpperCase
		 Ky$ = UCASE$(Ky$)
		 Found = -1
	       CASE ELSE
		 Found = -1
	     END SELECT

	     IF FirstKey AND Found THEN
	       FirstKey = 0
	       TxtPos = 1
	       LastPos = 0
	       Edit$ = SPACE$(LEN(Edit$))
	       GOSUB PrintTheString
	     END IF

	     IF Found THEN GOSUB OK

	  '----- Left arrow
	  CASE -75
	     TxtPos = TxtPos - 1                'Decrement the text pointer
	     FirstKey = 0

	  '----- Right arrow
	  CASE -77
	     TxtPos = TxtPos + 1                'Increment the text pointer
	     FirstKey = 0

	  '----- Home
	  CASE -71
	     TxtPos = 1                         'Move text pointer to 1
	     FirstKey = 0

	  '---- Control C
	  CASE 3
	     Edit$ = SPACE$(LEN(Edit$))
	     TxtPos = 1
	     GOSUB PrintTheString
	     FirstKey = 0

	  '---- Control R
	  CASE 18
	     Edit$ = Temp$
	     GOSUB PrintTheString
	     FirstKey = 0

	  '----- End
	  CASE -79
	     FOR N = LEN(Edit$) TO 1 STEP -1    'Look backwards for non-blank
		IF MID$(Edit$, N, 1) <> " " THEN EXIT FOR
	     NEXT
	     TxtPos = N + 1                     'Set pointer to last char +1
	     IF TxtPos > LEN(Edit$) THEN TxtPos = LEN(Edit$)
	     FirstKey = 0

	  '----- Insert key
	  CASE -82
	     GOSUB MoveCursor
	     Inserting = NOT Inserting          'Toggle the Inserting flag

	  '----- Delete
	  CASE -83                              'Truncate the text
	     IF FirstKey THEN
	       FirstKey = 0
	       Edit$ = SPACE$(LEN(Edit$))
	       TxtPos = 1
	     ELSE
	       MID$(Edit$, TxtPos) = MID$(Edit$, TxtPos + 1) + " "
	     END IF

	     GOSUB PrintTheString
	     LastPos = 0


	  CASE -50 TO -16                          'Look for HotKeys...
						   ' Negative values = Alt + Key
	    Ty = ABS(KeyCode) - 15                  'Find offset in AltHotKey$
	    KeyCode = ASC(MID$(AltHotKey$, Ty, 1))  'Assign Ky that ASCII value
	    Search = LC
	    DO
	      IF KeyCode = HotKeyTbl(Search) THEN
		Ky$ = CHR$(KeyCode)
		HotKey = -1
		EXIT DO
	      END IF
	      Search = Search + 1
	      IF Search > HKUpper THEN Search = 1
	    LOOP UNTIL Search = LC
	    LC = LC + 1
	    IF LC > HKUpper THEN LC = 1
	    IF HotKey THEN EXIT DO
	  CASE ELSE                             'All other keys,

       END SELECT

    IF TxtPos < 1 THEN TxtPos = 1

    IF Style THEN            'Stay in field at end of string
      IF TxtPos > LEN(Edit$) THEN TxtPos = LEN(Edit$)
    ELSE                     'Exit field at end of string
      IF TxtPos > LEN(Edit$) THEN
	Ky$ = CHR$(9)
	EXIT DO
      END IF
    END IF

    LOOP

  HiLiteClr = BGClr
  GTextClr = FGClr
  GOSUB PrintTheString

EXIT SUB

MoveCursor:
    IF CursorIs THEN
      CALL DrawCursor((AtCol - 1) * 8, CursorRow, 1 - 7 * Inserting, GPDat(71) - 2, CursorIs, -1)
    END IF
    RETURN

PrintTheString:
    GOSUB MoveCursor
    HideCursor
    LINE ((LeftCol - 1) * 8 - 1, CSRLIN * GPDat(71) - GPDat(71))-STEP(LEN(Edit$) * 8 + 1, GPDat(71) - 2), HiLiteClr, BF
    ''LINE ((LeftCol - 1 + TxtPos) * 8 - 1, CSRLIN * GPDat(71) - GPDat(71))-STEP(8, GPDat(71) - 2), HiLiteClr, BF
    GPrint3VE CSRLIN * GPDat(71) - GPDat(71), LeftCol, Edit$, GTextClr' + 0 * 256'GPDat (74)
    CALL ShowCursor

    RETURN

OK:
    IF Inserting THEN                  'Expand the text string
      MID$(Edit$, TxtPos) = Ky$ + MID$(Edit$, TxtPos)
    ELSE
      MID$(Edit$, TxtPos) = Ky$       'Put the new letter in string
    END IF
    GOSUB PrintTheString
    TxtPos = TxtPos + 1                'Increment the text pointer

RETURN

END SUB

SUB Icons (X1, y1, Icon)

  x = X1
  y = y1

  IF Icon < 4 THEN
    IF GPDat(71) = 8 THEN
      ICONOs = 4
    END IF

    x = x + 28 - ICONOs
    y = y + 28 - ICONOs
  END IF

  SELECT CASE GPDat(31)
    CASE 5         'EGA
      Ht = 11
      Top = 9
      PerCtr = 7
      Bot = 8
      QCtr = y - 4
      Aspect! = .6
    CASE 8         'VGA
      Ht = 16
      Top = 12
      PerCtr = 10
      Bot = 12
      QCtr = y - 5
      Aspect! = .9
  END SELECT


  SELECT CASE Icon

    CASE 1                                '"!" Icon
      CIRCLE (x, y), 16, 0
      PAINT (x, y), 14, 0
      LINE (x - 4, y - Top)-STEP(8, Ht), 0, BF
      CIRCLE (x, y + PerCtr), 4, 0
      PAINT (x, y + PerCtr), 0, 0

    CASE 2
      CIRCLE (x, y), 16, 0                  '"i" Icon
      PAINT (x, y), 15, 0
      LINE (x - 4, y - 4 - 2 * (GPDat(31) = 5))-STEP(8, Ht), 1, BF
      LINE (x - 6, y - 4 - 2 * (GPDat(31) = 5))-STEP(2, 2), 1, BF
      LINE (x - 6, y + Bot - (GPDat(31) = 5))-STEP(12, -2), 1, BF

      CIRCLE (x, y - PerCtr), 4, 1
      PAINT (x, y - PerCtr), 1, 1

    CASE 3                                '"?" Icon
      CIRCLE (x, y), 16, 0
      PAINT (x, y), 2, 0

      CIRCLE (x, QCtr), 9, 15, , , Aspect!
      CIRCLE (x, QCtr), 3, 15, , , Aspect!
      PAINT (x, QCtr - 4), 15, 15

      IF GPDat(31) = 8 THEN

	LINE (x - 10, y + 4)-STEP(6, -6), 2, BF
	LINE (x - 3, y - 1)-STEP(6, 5), 15, BF
	LINE (x, QCtr + 2)-STEP(-5, 5), 2
	LINE (x - 3, QCtr + 3)-STEP(3, 0), 2
	LINE (x - 8, QCtr + 4)-STEP(6, 0), 2
	LINE (x - 3, QCtr + 2)-STEP(1, 0), 2
	DrawPointVEOpts x - 9, QCtr + 2, 2
	CIRCLE (x, y + PerCtr), 4, 15
	PAINT (x, y + PerCtr), 15, 15

      ELSE
	LINE (x - 3, y - 1)-STEP(6, 3), 15, BF
	LINE (x, QCtr + 1)-STEP(-5, 5), 2
	LINE (x - 8, QCtr + 2)-STEP(8, 0), 2
	LINE (x - 8, QCtr + 3)-STEP(6, 0), 2
	LINE (x - 8, QCtr + 4)-STEP(4, 0), 2
	LINE (x - 2, QCtr + 1)-STEP(1, 0), 2
	CIRCLE (x, y + PerCtr), 4, 15
	PAINT (x, y + PerCtr), 15, 15
      END IF

    CASE 4      'Dotted line types
       x = x + 62
       LINE (x, y + 2 * GPDat(71))-STEP(114, 0), 0, , -1
       LINE (x, y + 3 * GPDat(71))-STEP(114, 0), 0, , -30584
       LINE (x, y + 4 * GPDat(71))-STEP(114, 0), 0, , -3856
       LINE (x, y + 5 * GPDat(71))-STEP(114, 0), 0, , -897
       x = x + 166
       LINE (x, y + 2 * GPDat(71))-STEP(114, 0), 0, , -28680
       LINE (x, y + 3 * GPDat(71))-STEP(114, 0), 0, , -27652
       LINE (x, y + 4 * GPDat(71))-STEP(114, 0), 0, , -772

    CASE ELSE

  END SELECT

END SUB

FUNCTION MsgBox (Text$(), Opts)    'Opts = ??
'                         ^^^
'            Button text ÄÙ³ÀÄ Number of push buttons 1-3
'                          ÀÄÄ Icon

   IF GPDat(71) = 8 THEN Spc8 = 8
   NumButtons = Opts MOD 10
   IF Opts > 9 THEN
     Icon = (Opts MOD 100) \ 10
     OS = 6
     OS1 = 6
   END IF
   Captions = Opts \ 100

   IF Captions THEN
     RESTORE Caps2
   ELSE
     RESTORE Caps1
   END IF

   Upper = UBOUND(Text$, 1)

   SELECT CASE NumButtons

     CASE 1
       Min = 10
       GOSUB BoxWidth
       Col = LocX - 26
     CASE 2
       Min = 22
       GOSUB BoxWidth
       Col = LocX - 74
     CASE 3
       Min = 30
       GOSUB BoxWidth
       Col = LocX - 122
     CASE ELSE

   END SELECT

   Bound = NumButtons + Upper + 1
   REDIM DLog(Bound) AS Dialog
   REDIM Menu$(0)
   REDIM Resp$(Bound)

   DLog(0).Heading = Text$(0)

   FOR i = 1 TO NumButtons
     DLog(i).DType = 6
     READ DLog(i).Heading
     DLog(i).Row = (Upper + 3) * GPDat(71) - GPDat(71) \ 2
     DLog(i).TextLen = 66
     DLog(i).NumRows = GPDat(80) + 1
     DLog(i).Col = Col
     Col = Col + 96
   NEXT

   Row = GPDat(71) \ 2
   FOR i = NumButtons + 1 TO Upper + NumButtons
     DLog(i).DType = 5
     DLog(i).Heading = Text$(i - NumButtons)
     DLog(i).Row = Row
     IF Icon AND Row > 44 THEN OS = 0
     DLog(i).Col = 3 + OS
     Row = Row + GPDat(71)
   NEXT
   
   DLog(Bound).Row = (Upper + 2) * GPDat(71) + Spc8
   DLog(Bound).DType = 5
   DLog(Bound).Heading = ""
   DLog(Bound).Col = x - 5 + OS1

   Choice = 1

   CALL GDialog(Choice, DLog(), Resp$(), Menu$(), Icon, 0)
   MsgBox = Choice               'Return which button was pressed

  EXIT FUNCTION


BoxWidth:

  x = LongestStr(Text$())
  IF x < Min THEN                   'If text is shorter than button width
    x = Min + 2                     ' make 2 greater than button width
  END IF

  x = x + 10
  DLogULC = 42 - x \ 2
  LeftOfCenter = DLogULC + x \ 2 - 1
  LocX = (LeftOfCenter - DLogULC + 1) * 8 + 8

RETURN

END FUNCTION

