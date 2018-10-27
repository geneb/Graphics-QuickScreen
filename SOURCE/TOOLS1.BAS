DECLARE SUB EngravedBox (X1%, Y1%, X2%, Y2%)
DEFINT A-Z

'by Phil Cramer
'Copyright (c) 1992 Crescent Software

'----BASIC subs
DECLARE SUB Box (X1, Y1, X2, Y2, Clr)
DECLARE SUB ClosedCurve (xy%(), NP%, NS%)
DECLARE SUB ColorEdit (x, y, BWidth, BHite, Clr)
DECLARE SUB CrossHair (x, y)
DECLARE SUB CursorXY (x%, y%, Button%, Ky%)
DECLARE SUB DefCursor (Cursor%)
DECLARE SUB DOSMsg ()
DECLARE SUB DownLeft (X1%, Y1%, X2%, Y2%)
DECLARE SUB DownRight (X1%, Y1%, X2%, Y2%)
DECLARE SUB DrawCursor (x%, y%, CursorWidth%, CursorLength%, CursorIs%, CursorTime%)
DECLARE SUB DrawPolar (x%, y%, Started%)
DECLARE SUB HVScrollBar (X1%, Y1%, X2%, Y2%, HV%, HiWhite%, White%, Gray%, SlideClr%)
DECLARE SUB Flood (Tile$)
DECLARE SUB FreeMouse ()
DECLARE SUB GDialog (Choice%, DLog() AS ANY, TResponse$(), Menu$(), Style, Action%)
DECLARE SUB GetSides (Angle!, Hypotenuse!, Adjacent!, Opposite!)
DECLARE SUB GetXORBox (X1, Y1, X2, Y2, Corner, A, B, Paste, PasteBuf(), ANDArray())
DECLARE SUB Grid (X1, y, X2, Y2, xspace, yspace, XHiSpace, YHiSpace, Clr, Clr1)
DECLARE SUB HScrollBar (Button%, x%, y%, Ky$, ScrollBar AS ANY, Clean%)
DECLARE SUB MouseArc ()
DECLARE SUB MouseEllipse (x%, y%, Clr%)
DECLARE SUB MousePolygon (x%, y%, Clr%)
DECLARE SUB MouseCircle (Radius, PaintIt, LeaveIt, Clr1, Clr2)
DECLARE SUB MouseLine (Clr1)
DECLARE SUB MouseBox (X1, Y1, LastX2, LastY2, LeaveIt, Button, Clr)
DECLARE SUB MouseXY ()
DECLARE SUB ParabPt (xy%(), TP%, T#, x%, y%)
DECLARE SUB PaintBrush (xStep, yStep)
DECLARE SUB PaintLine ()
DECLARE SUB PaletteEdit (x, y, BWidth, BHite, Done)
DECLARE SUB PCBox (X1, Y1, X2, Y2, Outside)
DECLARE SUB PCBoxes (bx1%, by1%, bx2%, by2%)
DECLARE SUB PixelEdit (x, y, X1, Y1, boxsize, Grid1, ExitCode)
DECLARE SUB Polygon (XCenter%, YCenter, NumSides%, Radius%, StartAngle%, Sides2Disp%, Clr%)
DECLARE SUB PrintText ()
DECLARE SUB PushButton (X1%, Y1%, X2%, Y2%, Size%, White%, Gray%, HiWhite%)
DECLARE SUB OpenCurve (xy%(), NP%, NS%)
DECLARE SUB RoundBox (x, y, X1, Y1, Clr)
DECLARE SUB SetPBColors (Fld AS ANY, HV%)
DECLARE SUB SPushButton (x%, y%, BWidth%, BHite%, Text$, UpDn%, HotKey%)
DECLARE SUB SRScreen (SaveRest)
DECLARE SUB StepBox (x, y, wide, Hite, Clr)
DECLARE SUB UpLeft (X1%, Y1%, X2%, Y2%)
DECLARE SUB UpRight (X1%, Y1%, X2%, Y2%)
DECLARE SUB WaitUp ()
DECLARE SUB WaitUpKey ()

'----BASIC functions
DECLARE FUNCTION GArraySize& (X1, Y1, X2, Y2)
DECLARE FUNCTION GetFactor# (A, c, c#, d#)
DECLARE FUNCTION GQSPath$ ()
DECLARE FUNCTION MsgBox (A$(), Opts)

'----ASM subs
DECLARE SUB ButtonPress (Butn, Stat, Pressed, Bx, By)
DECLARE SUB DrawPointVEOpts (BYVAL x%, BYVAL y%, BYVAL PointColor%)
DECLARE SUB CircleVE (BYVAL XCenter%, BYVAL YCenter%, BYVAL Radius%, BYVAL CircleColor%, BYVAL XAspect%, BYVAL YAspect%)
DECLARE SUB ClearVE ()
DECLARE SUB FPutA (Handle, SEG Element AS ANY, NumBytes&)
DECLARE SUB FClose (Handle)
DECLARE SUB FGetA (Handle, SEG Element AS ANY, NumBytes&)
DECLARE SUB FOpen (FileName$, Handle)
DECLARE SUB GetCursor (x%, y%, Button%)
DECLARE SUB GetPalTripleVGA (BYVAL PalReg%, Red%, Green%, Blue%)
DECLARE SUB GMove1VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestCol%, BYVAL DestLine%)
DECLARE SUB GMove2VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GMove4VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GPrint2VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint3VE (BYVAL Line1%, BYVAL Col1%, Text$, BYVAL TextColor%)
DECLARE SUB HideCursor ()
DECLARE SUB LineVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB LineBVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB LineBFVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB Mouse (A, B, c, d)
DECLARE SUB SetGWPages (BYVAL Source%, BYVAL Dest%)
DECLARE SUB SetPalTripleVGA (BYVAL PalReg%, BYVAL Red%, BYVAL Green%, BYVAL Blue%)
DECLARE SUB SetPaletteEGA (BYVAL PalReg%, BYVAL MValue%)
DECLARE SUB ShowCursor ()

'----ASM functions
DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION ErrorMsg$ (Er)
DECLARE FUNCTION Exist% (FileName$)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION GetPointVE% (BYVAL x%, BYVAL y%)
DECLARE FUNCTION KeyDown% ()
DECLARE FUNCTION MaxInt% (A, B)
DECLARE FUNCTION PDQTimer& ()
DECLARE FUNCTION ShiftKey% ()
DECLARE FUNCTION QPTrim$ (A$)
DECLARE FUNCTION WhichError ()

'$INCLUDE: 'Common.GQS'
'$INCLUDE: 'DLogType.Bi'
'$INCLUDE: 'ScrollB.Bi'

TYPE ScreenObject
  EType AS INTEGER
  ex1 AS INTEGER                'Co-ords for entity field
  ey1 AS INTEGER
  ex2 AS INTEGER
  ey2 AS INTEGER
  Clr AS INTEGER
  Status AS INTEGER
END TYPE

CONST pi! = 3.141
CONST ValidAngle$ = "1234567890"
CONST ValidRadius$ = "123456789"

VGA:
DATA R,G,B

SUB DrawPolar (x, y, Started)

Clr1 = 15 + 24 * 256
startx = x
starty = y
              'Set up FirstGDialog Box
REDIM DLog(4) AS Dialog
REDIM Response$(4)
REDIM Menu$(0)

DO
CALL FreeMouse
ShowCursor

DLog(0).Heading = "Enter Polar Coordinates"

IF GPDat(71) = 8 THEN MdOS = 8
DLog(1).DType = 6                            'Push Button
DLog(1).Row = 3 * GPDat(71) - GPDat(71) \ 2 + 2
DLog(1).Col = 5 * 8 - 2
DLog(1).Heading = "OK"
DLog(1).NumRows = GPDat(80)
DLog(1).TextLen = 66

DLog(2).DType = 6                            'Push Button
DLog(2).Row = 3 * GPDat(71) - GPDat(71) \ 2 + 2
DLog(2).Col = 18 * 8 - 2
DLog(2).Heading = "Cancel"
DLog(2).NumRows = GPDat(80)
DLog(2).TextLen = 66
DLog(2).Group = 1000

DLog(3).DType = 3                            'Text entry
DLog(3).Row = 1
DLog(3).Col = 3
DLog(3).Heading = "Length, Angle: <3"
DLog(3).TextLen = 8

DLog(4).DType = 5
DLog(4).Row = 2 * GPDat(71) + MdOS
DLog(4).Heading = ""

Choice = 1
CALL GDialog(Choice, DLog(), Response$(), Menu$(), Style, 0)

IF Choice = 2 OR Choice = 27 THEN
  HideCursor
  CrossHair startx, starty
  EXIT SUB
END IF

  HideCursor

  SRScreen 0                 'Save the screen

  Comma = INSTR(Response$(3), ",")        'Find the Comma

    IF Choice = 2 OR Choice = 27 OR Comma < 2 THEN     'Mouse button 2 or escape, bail out
      GOSUB CleanUp
      IF Comma < 2 THEN BEEP
      EXIT SUB
    END IF

    Hypotenuse! = VAL(QPTrim$(LEFT$(Response$(3), Comma - 1)))
    IF Hypotenuse! <= 0 THEN Hypotenuse! = 1
    IF Hypotenuse! > 900 THEN Hypotenuse! = 900

    Angle! = VAL(QPTrim$(RIGHT$(Response$(3), LEN(Response$(3)) - Comma)))
    Angle! = Angle! MOD 365

    Neg! = 1
    IF Angle! > 90 AND Angle! < 270 THEN Neg! = -1
    IF Angle! < -90 AND Angle! > -270 THEN Neg! = -1

    IF Angle! = 0 THEN Angle! = .0001            'Make sure we don't divide by 0
    Divisor! = (360 / Angle!) / 2                'Convert Angle to Radians
    Radians! = pi! / Divisor!

    Opposite = Hypotenuse! * SIN(Radians!) * -1  'Calculate x and y offsets
    OP! = Opposite
    Adjacent = SQR(Hypotenuse! ^ 2 - OP! ^ 2) * Neg!
 
    Aspect! = GPDat(49) / 1000
    X2 = startx + Adjacent
    Y2 = (starty + Opposite)

    SideOS = (starty - Y2) - ((starty - Y2) * Aspect!)
    Y2 = Y2 + SideOS

    IF Y2 > GPDat(44) OR Y2 < 0 OR X2 < 0 OR X2 > GPDat(43) THEN
      GOSUB CleanUp
      BEEP
      EXIT SUB
    END IF


    CrossHair startx, starty          'Delete last crosshair
    CALL LineVE(startx, starty, X2, Y2, Clr1)
    CrossHair X2, Y2

    DO
      Ky$ = INKEY$                    'Look for keystrokes
      GetCursor tx, ty, Button
      IF Button AND 1 THEN Ky$ = CHR$(13)
      IF Button AND 2 THEN
        WaitUp
        Ky$ = CHR$(27)
      END IF

      IF LEN(Ky$) THEN

        SELECT CASE ASC(Ky$)
       
          CASE 13                     'Enter, print the line
            CrossHair X2, Y2          'Erase GCursor
            CALL LineVE(startx, starty, X2, Y2, GPDat(82))
            CrossHair X2, Y2          'Restore GCursor
            Mouse 7, 0, X2, X2
            Mouse 8, 0, Y2, Y2
            Started = -1
            GPDat(83) = -1            'Set drawing changed flag

          CASE 27                     'Escape, delete the line
            CrossHair X2, Y2          'Erase GCursor
            CrossHair startx, starty         'Erase GCursor
            CALL LineVE(startx, starty, startx + Adjacent, starty + Opposite, Clr1)
            GOSUB CleanUp
            EXIT SUB
     
          CASE ELSE
     
        END SELECT
      END IF
 
    LOOP UNTIL LEN(Ky$)
 
    startx = X2                       'Save coords for next time
    starty = Y2
LOOP

CleanUp:
  CrossHair startx, starty          'Erase GCursor
  Mouse 7, 0, startx, startx        'Trap the mouse
  Mouse 8, 0, starty, starty
  x = startx
  y = starty
RETURN

END SUB

SUB Flood (Tile$)

SnapStatus = GPDat(17)
GPDat(17) = 0

CALL DefCursor(1)

DO
  DO
    CALL CursorXY(x, y, Button, Ky)
  LOOP UNTIL Button > 0 OR Ky = 13
 
  IF Button = 2 THEN
    CALL DefCursor(2)
    CALL ShowCursor
    GPDat(17) = SnapStatus
    EXIT SUB
  END IF

  IF Button = 1 OR Ky = 13 THEN
    HideCursor
    OldClr = GetPointVE(x, y)
    Searchx = x
    DO
      Searchx = Searchx + 1
      SearchClr = GetPointVE(Searchx, y)
    LOOP UNTIL SearchClr <> OldClr OR Searchx > GPDat(43)
    IF Searchx > GPDat(43) THEN SearchClr = GPDat(82)
    IF Tile$ = "" THEN
      PAINT (x, y), GPDat(82), SearchClr
    ELSE
      PAINT (x, y), Tile$, SearchClr
    END IF

    ShowCursor
    GPDat(83) = -1
    WaitUp
  END IF
  WaitUpKey

LOOP

END SUB

DEFSNG A-Z
SUB GetSides (Angle!, Hypotenuse!, Adjacent!, Opposite!)

  Neg = 1
  IF Angle! = 0 THEN Angle! = .0001
  IF Angle! > 90 AND Angle! < 270 THEN Neg = -1
  IF Angle! < -90 AND Angle! > -270 THEN Neg = -1
  IF Angle! > 450 AND Angle! < 630 THEN Neg = -1

  Divisor! = (360 / Angle!) / 2
  Radians! = pi / Divisor!
  Opposite! = (Hypotenuse! * SIN(Radians!)) * -1

  Adjacent! = (SQR(Hypotenuse! ^ 2 - Opposite! ^ 2)) * Neg
  Opposite! = Opposite! * (GPDat%(69) / 1000)

END SUB

DEFINT A-Z
SUB GetXORBox (X1, Y1, LastX2, LastY2, Corner, xOffset, yOffset, Paste, Array(), Array1()) STATIC
'                                      ^ Which corner was picked last


ButtonPress 1, Status, Pressed1, Bx, By
ButtonPress 2, Status, Pressed2, Bx, By

IF Paste < 2 THEN
  IF Paste = -1 THEN
    HideCursor
    Button = 0
    IF GPDat(67) = 1 THEN
      X2 = 0
      Y2 = 0

      CALL Mouse(7, 0, Array(0), Array(0))  'Horizontal
      CALL Mouse(8, 0, Array(1), Array(1))  'Vertical range
      FreeMouse
      GPDat(86) = -1

      xmax = GPDat(43) - 1
      ymax = GPDat(44) - 1
      xmin = Array(0)
      ymin = Array(1)

      xOS = -Array(0)
      yOS = -Array(1)

    END IF
    GOTO PasteBlk
  END IF

  LpFlag = 0
  CALL Mouse(8, 0, 0, GPDat(44) - 1)     'Vertical range
  CALL Mouse(7, 0, 0, GPDat(43) - 1)     'Horizontal

Skip:

  HideCursor
  xLen = ABS(LastX2 - X1) + xOffset
  yLen = ABS(LastY2 - Y1) + yOffset
  xOS = 0
  yOS = 0

  SELECT CASE Corner       'See which corner was last picked
 
    CASE 0 'LR
      xOS = -xLen
      yOS = -yLen
      CALL Mouse(8, 0, yLen, GPDat(44) - 1)   'Limit Vertical range
      CALL Mouse(7, 0, xLen, GPDat(43) - 1)   'Limit Horizontal range
      xmax = GPDat(43) - 1
      ymax = GPDat(44) - 1
      xmin = xLen
      ymin = yLen

    CASE 1 'LL
      yOS = -yLen
      xOS = xOffset
      CALL Mouse(8, 0, yLen, GPDat(44) - 1)   'Limit Vertical range
      CALL Mouse(7, 0, 0, GPDat(43) - 1 - ABS(X1 - LastX2))  'Limit Horizontal range
      xmax = GPDat(43) - 1 - xLen
      ymax = GPDat(44) - 1
      xmin = 0
      ymin = yLen
 
    CASE 2 'UR
      xOS = -xLen
      yOS = yOffset
      CALL Mouse(7, 0, xLen, GPDat(43) - 1)   'Limit Horizontal range
      CALL Mouse(8, 0, 0, GPDat(44) - 1 - ABS(Y1 - LastY2))  'Limit Vertical range
      xmax = GPDat(43) - 1
      ymax = GPDat(44) - 1 - yLen
      xmin = xLen
      ymin = 0
 
    CASE 3 'UL
      xOS = xOffset
      yOS = yOffset

      CALL Mouse(8, 0, 0, GPDat(44) - 1 - ABS(Y1 - LastY2))  'Limit Vertical range
      CALL Mouse(7, 0, 0, GPDat(43) - 1 - ABS(X1 - LastX2))  'Limit Horizontal range
      xmax = GPDat(43) - 1 - xLen
      ymax = GPDat(44) - 1 - yLen
      xmin = 0
      ymin = 0
 
    CASE ELSE

  END SELECT

  BB& = (GArraySize&(X1, Y1, LastX2, LastY2) \ 2)  'See how big an array we need

  IF BB& > FRE(-1) \ 2 OR BB& > 32000 THEN          '64k Max
    BEEP                                           'Too big, bag out
    CALL ShowCursor
    CALL FreeMouse
    EXIT SUB
  END IF

  IF LastX2 > GPDat(43) - 1 THEN LastX2 = GPDat(43) - 1
  IF LastY2 > GPDat(44) - 1 THEN LastY2 = GPDat(44) - 1
  IF X1 < 0 THEN X1 = 0
  IF Y1 < 0 THEN Y1 = 0

  REDIM Array(BB&)                                'OK to dim the array
  GET (X1, Y1)-(LastX2, LastY2), Array            'Get the image
  GPDat(86) = -1                                   'Paste Buffer avail
  GPDat(67) = 2
  GPDat(70) = 0
  x = X1
  y = Y1

  ShowCursor
  WaitUp

END IF

MultCopy:

DO
  CursorXY X2, Y2, Button, Ky

  IF X2 > xmax THEN X2 = xmax
  IF X2 < xmin THEN X2 = xmin
  IF Y2 > ymax THEN Y2 = ymax
  IF Y2 < ymin THEN Y2 = ymin
  
  X2 = X2 + xOS
  Y2 = Y2 + yOS


  IF Ky = -68 THEN
    HideCursor
    PUT (X2, Y2), Array, XOR
    ShowCursor
    x = X2
    y = Y2
  END IF
 

  SELECT CASE Ky
    CASE 72, 104                    '"H, h", center the block horizontally
      X2 = GPDat(43) \ 2 - xLen \ 2
      Mouse 7, 0, X2 - xOS, X2 - xOS
      Mouse 7, 0, 0, GPDat(43) - 1
    CASE 86, 118                    '"V, v", center the block vertically
      Y2 = GPDat(44) \ 2 - yLen \ 2
      Mouse 8, 0, Y2 - yOS, Y2 - yOS
      Mouse 8, 0, 0, GPDat(44) - 1
    CASE ELSE
  END SELECT
 
  IF X2 <> x OR Y2 <> y THEN
    HideCursor
    IF GPDat(20) THEN               'GPDat(25) = -1 means make a copy, leave original
      IF LpFlag = 0 THEN            'If its the first time through
        PUT (X1, Y1), Array, PSET  'Leave a copy
      ELSE
        PUT (x, y), Array, XOR     'Clear last image
      END IF
    ELSE
      IF LpFlag = 0 THEN
        LINE (X1, Y1)-(LastX2, LastY2), GPDat(57), BF
      ELSE
        PUT (x, y), Array, XOR       'Clear last image
      END IF
    END IF
PasteBlk:

    '---- Additional test while copying or moving fields
    IF X2 < 0 THEN X2 = 0
    IF X2 > 639 - Array(0) + 1 THEN X2 = 639 - Array(0) + 1
    IF Y2 < 0 THEN Y2 = 0
    IF Y2 > GPDat(44) - Array(1) THEN Y2 = GPDat(44) - Array(1)

    PUT (X2, Y2), Array, XOR       'Draw new image
    ShowCursor
    LpFlag = 1
  END IF

  ButtonPress 1, Status, Pressed1, Bx, By
  IF Pressed1 THEN Button = 1
  ButtonPress 2, Status, Pressed2, Bx, By
  IF Pressed2 THEN Button = 2

  x = X2
  y = Y2

LOOP UNTIL Button > 0 OR Ky = 13

IF Button = 2 THEN
  HideCursor
  PUT (x, y), Array, XOR       'Clear the image
  IF NOT GPDat(20) THEN PUT (X1, Y1), Array, PSET    'put back the old one
  ShowCursor
  CALL FreeMouse
  IF Paste > 0 THEN Paste = 5
  EXIT SUB
END IF

HideCursor
IF WinImage THEN
  PUT (x, y), Array1, AND
  PUT (x, y), Array, XOR
ELSE
  IF NOT GPDat(66) THEN          'If XOR off
    PUT (x, y), Array, PSET
  END IF
END IF
ShowCursor


WaitUp
WaitUpKey                     'Copy
IF GPDat(20) THEN
  HideCursor
  PUT (x, y), Array, XOR      'Clear last image
  ShowCursor
  IF Paste > 0 THEN
    Paste = 2
    X1 = x
    Y1 = y

    EXIT SUB
  ELSE
    GOTO MultCopy
  END IF
END IF
CALL FreeMouse
GPDat(83) = -1                 'Set drawing changed flag

X1 = x
Y1 = y

END SUB

SUB GPalette (ULx, ULy, BWidth, BHite, DChoice, Clr, PasteBuf(), ANDArray())

STATIC PBkt(), Zoom(), Brush(), ChgClr(), Scrib(), Loaded

IF NOT Loaded THEN

  ICPath$ = IconPath$
  IF RIGHT$(ICPath$, 1) <> "\" THEN ICPath$ = ICPath$ + "\"

  IF NOT Exist%(ICPath$ + "Bucket.GMP") THEN
    ICPath$ = GQSPath$
  END IF

  E = 0
  FOpen ICPath$ + "Bucket.GMP", Handle1
  IF DOSError THEN E = E + 1
  FOpen ICPath$ + "PBrush.GMP", Handle2
  IF DOSError THEN E = E + 1
  FOpen ICPath$ + "Zoom.GMP", Handle3
  IF DOSError THEN E = E + 1
  FOpen ICPath$ + "ClrWheel.GMP", Handle4
  IF DOSError THEN E = E + 1
  FOpen ICPath$ + "Scribble.GMP", Handle5
  IF DOSError THEN E = E + 1

  IF E THEN
    REDIM Msg$(2)
    Msg$(0) = "File Access Error"
    Msg$(1) = "Cannot find the drawing palette's .GMP files"
    Msg$(2) = "Change the path settings in SetPaths dialog box."
    BPress = MsgBox(Msg$(), 21)
    ERASE Msg$
  END IF

  IF E = 0 THEN
    DIM PBkt(194)
    DIM Zoom(194)
    DIM Brush(194)
    DIM ChgClr(194)
    DIM Scrib(110)

    FGetA Handle1, SEG PBkt(0), 388
    FClose Handle1

    FGetA Handle2, SEG Brush(0), 388
    FClose Handle2

    FGetA Handle3, SEG Zoom(0), 388
    FClose Handle3

    FGetA Handle4, SEG ChgClr(0), 388
    FClose Handle4

    FGetA Handle5, SEG Scrib(0), 220
    FClose Handle5

    Loaded = -1
  END IF

END IF

SnapStatus = GPDat(17)
GPDat(17) = 0

BYCenter = BHite \ 2
BxCenter = BWidth \ 2
PalHite = 5 * BHite
PalWidth = 8 * BWidth
PalCols = PalWidth \ 8
 
WaitUp
HideCursor
REDIM Area(0 TO 32) AS ScreenObject
CALL GMove2VE(ULx \ 8 - 1, ULy - 12, PalCols + 5, PalHite + 26, &HAA00, 0)

LINE (ULx, ULy + 4)-STEP(PalWidth + 12, PalHite + 8), 0, BF
LINE (ULx - 6, ULy - 5)-STEP(PalWidth + 12, PalHite + 11), 7, BF

CALL PCBox(ULx - 6, ULy - 5, ULx + PalWidth + 6, ULy + PalHite + 7, -1)

FOR py1 = 0 TO BHite STEP BHite
  FOR px1 = 0 TO 7 * BWidth STEP BWidth                       'Draw the Palette
    LINE (ULx + px1, ULy + 1 + py1)-STEP(BWidth - 1, BHite - 2), Count, BF
    Area(Count).ex1 = ULx + px1
    Area(Count).ex2 = ULx + px1 + BWidth
    Area(Count).ey1 = ULy + py1
    Area(Count).ey2 = ULy + py1 + BHite
    Count = Count + 1
  NEXT
NEXT
 
FOR py1 = BHite TO BHite + BHite \ 2 + 30 STEP BHite + BHite \ 2
  FOR px1 = 0 TO 7 * BWidth STEP BWidth                       'Drawing choices
    Area(Count).ex1 = ULx + px1
    Area(Count).ex2 = ULx + px1 + BWidth - 1
    Area(Count).ey1 = ULy + BHite + py1
    Area(Count).ey2 = Area(Count).ey1 + ((2 * BHite - BHite \ 2) - 1)
    Count = Count + 1
    Cnt = Count - 16
    ON Cnt GOSUB MLine1, MBox1, RBox1, MBoxF1, MArc, MCircle1, MPolyGon, PaintLine1, PaintBrush1, Flood1, PxlEdit1, ReClr1, MCopy1, MText1, MButton1
  NEXT
NEXT
 
FOR i = BWidth TO 7 * BWidth STEP BWidth
  LINE (ULx + i, ULy)-STEP(0, PalHite), 0
NEXT

LINE (ULx, ULy + BHite)-STEP(PalWidth, BHite), 0, B
LINE (ULx, ULy + 3 * BHite + BHite \ 2)-STEP(PalWidth, 0), 0

CALL PCBox(ULx - 1, ULy - 1, ULx + PalWidth + 2, ULy + PalHite + 2 + (GPDat(31) = 5), 0)
LineBVE ULx, ULy, ULx + PalWidth, ULy + PalHite, 0

  LINE (ULx + PalWidth - 1, ULy + PalHite - 1)-STEP(-BWidth + 2, -BHite - BHite \ 2 + 2), GPDat(82), BF

  ShowCursor
  LastClr = GPDat(82)
  DO
    CALL CursorXY(X1, Y1, Button, Ky)
     
      IF GPDat(82) <> LastClr THEN
        LastClr = GPDat(82)
        LINE (ULx + PalWidth - 1, ULy + PalHite - 1)-STEP(-BWidth + 2, -BHite - BHite \ 2 + 2), GPDat(82), BF
      END IF

      IF Button = 1 OR Ky = 13 THEN
        FOR i = 0 TO 32
          IF X1 >= Area(i).ex1 AND X1 <= Area(i).ex2 AND Y1 >= Area(i).ey1 AND Y1 <= Area(i).ey2 THEN
            IF i < 16 THEN            'If we've clicked on a color
              GPDat(82) = i
              HideCursor
              LINE (ULx + PalWidth - 1, ULy + PalHite - 1)-STEP(-BWidth + 2, -BHite - BHite \ 2 + 2), i, BF
              IF GPDat(5) THEN LINE (GPDat(15) + 5, GPDat(16) + 5)-(GPDat(15) + 45, GPDat(16) + GPDat(71) + 4), GPDat(82), BF
              ShowCursor
              WaitUpKey
              WaitUp
              EXIT FOR
            ELSE

              HideCursor
              LineBFVE Area(i).ex1 + 1, Area(i).ey1 + 1, Area(i).ex2, Area(i).ey2, 15 + 6144
              ShowCursor
              WaitUp

              HideCursor
              CALL GMove2VE(ULx \ 8 - 1, ULy - 12, PalCols + 5, PalHite + 26, &HAA00, -1)
              ShowCursor

              SRScreen 0                 'Save the screen
              GPDat(7) = i               'Current drawing choice
              GPDat(17) = SnapStatus

              SELECT CASE i              'Branch according to "Choice"
              
                CASE 16                  'Line
                  CALL MouseLine(14 + (24 * 256))
              
                CASE 17                  'Box
                  CALL MouseBox(X1, Y1, X2, Y2, 1, Button, 14 + 24 * 256)

                CASE 18                  'Radius Box
                  CALL MouseBox(X1, Y1, X2, Y2, 4, Button, 14 + 24 * 256)

                CASE 19                  'Filled Box
                    CALL MouseBox(X1, Y1, X2, Y2, 2, Button, 14 + 24 * 256)

                CASE 20                  'Draw arc
                  CALL MouseArc

                CASE 21                  'Circle
                  CALL MouseEllipse(X1, Y1, 10 + 6144)
              
                CASE 22                  'Polygon
                  SRScreen 2             'Un-save the screen
                  CALL MousePolygon(X1, Y1, 14 + 6144)

                CASE 23
                  CALL PaintLine
              
                CASE 24
                  CALL PaintBrush(16, 16)
                   
                CASE 25
                  CALL Flood("")

                CASE 26                  'Pixel Editor
                  DO
                    SRScreen 2             'Un-Save the screen
                    boxsize = 8
                    Clr = 14 + 24 * 256
                    Grid1 = -1
                    CALL MouseBox(X1, Y1, X2, Y2, LeaveIt, Button, Clr)
                    IF Button > 1 THEN EXIT DO
                    CALL PixelEdit(X1, Y1, X2, Y2, boxsize, Grid1, ExitCode)
                  LOOP UNTIL ExitCode

                CASE 27   'Re-Color
                  CALL ColorEdit(ULx, ULy, BWidth, BHite, Clr)
              
                CASE 28
                  Clr = 14 + 24 * 256
                  LeaveIt = 0
                  CALL MouseBox(X1, Y1, X2, Y2, LeaveIt, Button, Clr)
                  IF Button <> 2 THEN
                    WinImage = 0
                    REDIM ANDArray(0)
                    GPDat(7) = 35
                    CALL GetXORBox(X1, Y1, X2, Y2, LeaveIt, 0, 0, 0, PasteBuf(), ANDArray())
                  END IF

                CASE 29
                  SRScreen 2
                  CALL PrintText
              
                CASE 30
                  CALL MouseBox(X1, Y1, X2, Y2, 7, Button, 15 + 24 * 256)
              
                CASE ELSE
              
              END SELECT

              SRScreen 2                 'Clear the saved screen
            
              EXIT SUB

            END IF
          END IF
        NEXT
      END IF

    LOOP UNTIL Button = 2

  GPDat(17) = SnapStatus
  WaitUp

  HideCursor
  CALL GMove2VE(ULx \ 8 - 1, ULy - 12, PalCols + 5, PalHite + 26, &HAA00, -1)
  ShowCursor

  EXIT SUB


'---- Draw palette icons

MLine1:
  LINE (ULx + px1 + BxCenter - 5, ULy + BHite + 3 + py1)-STEP(10, BHite), 0
RETURN

MBox1:
  LINE (ULx + px1 + BxCenter - 5, ULy + BHite + 3 + py1)-STEP(10, BHite), 0, B
RETURN

MBoxF1:
  LINE (ULx + px1 + BxCenter - 5, ULy + BHite + 3 + py1)-STEP(10, BHite), 0, BF
RETURN

RBox1:
  TGP = GPDat(3)
  GPDat(3) = 3
  CALL RoundBox(ULx + px1 + BxCenter - 5, ULy + BHite + 3 + py1, ULx + px1 + BxCenter + 5, ULy + BHite + 3 + BHite + py1, 0)
  GPDat(3) = TGP
RETURN

MArc:
  CIRCLE (ULx + px1 + BxCenter, ULy + BHite + 13 + py1), 5, 0, .5, 2.2
  CIRCLE (ULx + px1 + BxCenter, ULy + BHite + 13 + py1), 5, 0, pi!, 2 * pi!
  CIRCLE (ULx + px1 + BxCenter, ULy + BHite + 13 + py1), 8, 0, 0, 1.2
  CIRCLE (ULx + px1 + BxCenter, ULy + BHite + 13 + py1), 8, 0, 1.7, 5.3
  CIRCLE (ULx + px1 + BxCenter, ULy + BHite + 13 + py1), 11, 0, .78, 6.28
RETURN

MCircle1:
  CIRCLE (ULx + px1 + BxCenter, ULy + BHite + 13 + py1), 11 + (GPDat(31) <> 5), 0
RETURN

MPolyGon:
  TDat = GPDat(68)
  GPDat(68) = -1
  TDat1 = GPDat(69)
  IF GPDat(31) = 5 THEN
    GPDat(69) = 835
  ELSE
    GPDat(69) = 1050
  END IF
  CALL Polygon(ULx + px1 + BxCenter, ULy + BHite + 15 + py1, 3, 10, 90, 3, 0)
  GPDat(68) = TDat
  GPDat(69) = TDat1
RETURN

PaintBrush1:
  IF Loaded THEN PUT (ULx + px1 + 3, ULy + BHite + 2 + py1), Brush, PSET
RETURN

MButton1:
  CALL PushButton(ULx + px1 + BxCenter - 10, ULy + BHite + 3 + py1, ULx + px1 + BxCenter - 10 + 19, ULy + BHite + 3 + py1 + BHite + 3, 2, 7, 8, 15)
RETURN

PaintLine1:
  IF Loaded THEN PUT (ULx + px1 + 9, ULy + BHite + 4 + py1), Scrib, PSET
RETURN

Flood1:
  IF Loaded THEN PUT (ULx + px1 + 4, ULy + BHite + 2 + py1), PBkt, PSET
RETURN

ReClr1:
  IF Loaded THEN PUT (ULx + px1 + 5, ULy + BHite + 2 + py1), ChgClr, PSET
RETURN

MCopy1:
  LINE (ULx + px1 + BWidth \ 4, ULy + BHite + 3 + py1)-STEP(BWidth \ 2, BHite + 3), 11, BF
  LINE (ULx + px1 + BWidth \ 4, ULy + BHite + 3 + py1)-STEP(BWidth \ 2, BHite + 3), 0, B
  LINE (ULx + px1 + BWidth \ 4, ULy + BHite + 8 + py1)-STEP(BWidth \ 4 + 2, BHite - 8), 0, BF
  LINE (ULx + px1 + BWidth \ 4 - 2, ULy + BHite + 6 + py1)-STEP(BWidth \ 4 + 2, BHite - 8), 11, BF
  LINE (ULx + px1 + BWidth \ 4 - 2, ULy + BHite + 6 + py1)-STEP(BWidth \ 4 + 2, BHite - 8), 0, B
RETURN

PxlEdit1:
  IF Loaded THEN PUT (ULx + px1 + 7, ULy + BHite + 2 + py1), Zoom, PSET
RETURN

MText1:
  LINE (ULx + px1 + BxCenter - 1, ULy + BHite + 4 + py1)-STEP(3, BHite), 0, BF
  LINE (ULx + px1 + BxCenter - 7, ULy + BHite + 4 + py1)-STEP(15, 3), 0, BF
  LINE (ULx + px1 + BxCenter - 7, ULy + BHite + 8 + py1)-STEP(2, 0), 0
  LINE (ULx + px1 + BxCenter + 6, ULy + BHite + 8 + py1)-STEP(2, 0), 0
  LINE (ULx + px1 + BxCenter - 3, ULy + 2 * BHite + py1 + 5)-STEP(7, -2), 0, BF
RETURN

END SUB

SUB HFlip

  MouseBox X1, Y1, X2, Y2, 0, Button, 14 + 24 * 256
  IF Button = 2 THEN EXIT SUB

  IF X1 < 0 THEN X1 = 0
  IF Y1 < 0 THEN Y1 = 0
  IF X2 > GPDat(43) - 1 THEN X2 = GPDat(43) - 1
  IF Y2 > GPDat(44) - 1 THEN Y2 = GPDat(44) - 1

  T = GArraySize&(X1, Y1, X1, Y2) \ 2

  REDIM L(T)
  REDIM R(T)
  HideCursor
  FOR i = 0 TO (X2 - X1) \ 2
    GET (X1 + i, Y1)-(X1 + i, Y2), L
    GET (X2 - i, Y1)-(X2 - i, Y2), R
    PUT (X1 + i, Y1), R, PSET
    PUT (X2 - i, Y1), L, PSET
  NEXT
  ShowCursor
  GPDat(83) = -1                 'Set drawing changed flag
END SUB

SUB MouseArc

XORClr = 6159

CClockWise = -1
HideCursor                               'Turn off the cursor
IF GPDat(31) = 5 THEN YAsp = 23948

GetCursor LastX, LastY, Button           'Get mouse coords
RestartArc:
CrossHair LastX, LastY                   'Draw the GCursor

DO
  DO
    CALL CursorXY(X1, Y1, Button, Ky)        'Look for keystrokes, mouse clicks

    IF Ky = 65 OR Ky = 97 THEN           'A, a  "A"rc
      CrossHair LastX, LastY
      GOSUB Toggle
      CrossHair X1, Y1
    ELSEIF Ky = -68 THEN
      CrossHair X1, Y1
      LastX = X1
      LastY = Y1
      GOTO RestartArc
    END IF

    IF LastX <> X1 OR LastY <> Y1 THEN       'Draw new cursor if it has moved
      CrossHair LastX, LastY
      CrossHair X1, Y1
      LastX = X1
      LastY = Y1
    END IF
 
  LOOP UNTIL Button > 0 OR Ky = 13         'Wait for mouseclick or spacebar
  WaitUp
  WaitUpKey

  IF Button = 2 OR Ky = 27 THEN
    CrossHair X1, Y1
    ShowCursor
    GPDat(22) = XORClr
    EXIT SUB
  END IF

  LastX2 = X1
  LastY2 = Y1

  CALL LineVE(X1, Y1, LastX2, LastY2, XORClr)      'erase last line

  DO
    CALL CursorXY(X2, Y2, Button, Ky)        'Look for keystrokes, mouse clicks

    IF Ky = -68 THEN
      CrossHair X2, Y2
      LastX = X2
      LastY = Y2
      GOTO RestartArc
    ELSEIF Ky = 65 OR Ky = 97 THEN           'A, a  "A"rc
      CrossHair LastX2, LastY2
      GOSUB Toggle
      CrossHair X2, Y2
    END IF

    IF LastX2 <> X2 OR LastY2 <> Y2 THEN      'Draw new cursor if it has moved
      CrossHair LastX2, LastY2
      CALL LineVE(X1, Y1, LastX2, LastY2, XORClr)
      CALL LineVE(X1, Y1, X2, Y2, XORClr)
      CrossHair X2, Y2
      LastX2 = X2
      LastY2 = Y2
    END IF
  LOOP UNTIL Button > 0 OR Ky = 13
  WaitUp
  WaitUpKey
  IF Button = 2 THEN
    CrossHair X2, Y2
    CALL LineVE(X1, Y1, X2, Y2, XORClr)
    WaitUp
    GOTO RestartArc
  END IF

  Aspct! = 1000 / GPDat(49)

  A! = ABS(X2 - X1)                   'adjacent side
  IF A! = 0 THEN A! = .0005
  B! = ABS(Y2 - Y1) * Aspct!                 'opposite side
  IF B! = 0 THEN B! = .0005
  Radius = SQR((A! ^ 2) + (B! ^ 2))
  ArcStart! = ATN(B! / A!)

  IF X2 >= X1 AND Y2 <= Y1 THEN        'upper right quarter

  ELSEIF X2 >= X1 AND Y2 >= Y1 THEN    'lower right quarter
    ArcStart! = 1.5705 - ArcStart! + 4.7115
  ELSEIF X2 <= X1 AND Y2 >= Y1 THEN    'lower left quarter
    ArcStart! = ArcStart! + 3.141
  ELSE                               'upper left quarter
    ArcStart! = 1.5705 - ArcStart! + 1.5705
  END IF

  lastx3 = X2
  lasty3 = Y2

  CircleVE X1, Y1, Radius, XORClr - 7, 0, YAsp

  GOSUB XMark

  DO
    CALL CursorXY(x3, y3, Button, Ky)        'Look for keystrokes, mouse clicks

    IF Ky = -68 THEN
      CrossHair x3, y3
      GOTO RestartArc
    ELSEIF Ky = 65 OR Ky = 97 THEN           'A, a  "A"rc
      CrossHair lastx3, lasty3
      GOSUB Toggle
      CrossHair x3, y3
    END IF
  
    IF lastx3 <> x3 OR lasty3 <> y3 THEN      'Draw new cursor if it has moved
      CrossHair lastx3, lasty3
      CALL LineVE(X1, Y1, lastx3, lasty3, XORClr)
      CALL LineVE(X1, Y1, x3, y3, XORClr)
      CrossHair x3, y3
      lastx3 = x3
      lasty3 = y3
    END IF

  LOOP UNTIL Button > 0 OR Ky = 13

  GOSUB XMark
  CircleVE X1, Y1, Radius, XORClr - 7, 0, YAsp

  IF Button = 2 THEN
    CrossHair x3, y3
    CALL LineVE(X1, Y1, x3, y3, XORClr)
    WaitUp
    GOTO RestartArc
  END IF

  A! = ABS(x3 - X1)                   'adjacent side
  IF A! = 0 THEN A! = .0005
  B! = ABS(y3 - Y1) * Aspct!                  'opposite side
  IF B! = 0 THEN B! = .0005
  ArcEnd! = ATN(B! / A!)

  IF x3 >= X1 AND y3 <= Y1 THEN        'upper right quarter

  ELSEIF x3 >= X1 AND y3 >= Y1 THEN    'lower right quarter
    ArcEnd! = 1.5705 - ArcEnd! + 4.7115
  ELSEIF x3 <= X1 AND y3 >= Y1 THEN    'lower left quarter
    ArcEnd! = ArcEnd! + 3.141
  ELSE                               'upper left quarter
    ArcEnd! = 1.5705 - ArcEnd! + 1.5705
  END IF

  IF NOT CClockWise THEN SWAP ArcStart!, ArcEnd!

  LastX = x3
  LastY = y3

  CrossHair x3, y3
  CALL LineVE(X1, Y1, x3, y3, XORClr)      'erase last line
  CIRCLE (X1, Y1), Radius, GPDat(82), ArcStart!, ArcEnd!
  CrossHair x3, y3
  GPDat(83) = -1                 'Set drawing changed flag

  WaitUp
  WaitUpKey

  Lastx1 = x3
  Lasty1 = y3

LOOP UNTIL Button = 2

EXIT SUB

Toggle:
  CClockWise = NOT CClockWise
  IF CClockWise THEN
    GPDat(22) = XORClr
    SOUND 1000, 1
    SOUND 500, 1
  ELSE
    GPDat(22) = 14 + 24 * 256

    SOUND 1000, 1
  END IF
RETURN

XMark:
  LineVE X2 - 4, Y2 - 4, X2 + 3, Y2 + 3, XORClr
  LineVE X2 + 3, Y2 - 4, X2 - 4, Y2 + 3, XORClr
RETURN

END SUB

SUB MouseBox (X1, Y1, LastX2, LastY2, LeaveIt, Button, Clr)

WaitUp
HideCursor
GetCursor lastcx, lastcy, Button

RestartBox:
CrossHair lastcx, lastcy

DO                                     'Wait for mouse click
  Corner = 0
  DO
    CALL CursorXY(X1, Y1, Button, Ky)
    IF Ky = -68 THEN
      IF GPDat(7) <> 50 THEN
        CrossHair X1, Y1
        lastcx = X1
        lastcy = Y1
        GOTO RestartBox
      END IF
    END IF
    IF lastcx <> X1 OR lastcy <> Y1 THEN
      CrossHair lastcx, lastcy
      CrossHair X1, Y1
      lastcx = X1
      lastcy = Y1
    END IF
  LOOP UNTIL Button > 0 OR Ky = 13
 
  IF Button = 2 THEN
    CrossHair X1, Y1
    ShowCursor
    EXIT SUB
  END IF

  Lastx1 = X1                          'Start Box coordinates
  Lasty1 = Y1
  LastX2 = X1
  LastY2 = Y1

  WaitUp
  WaitUpKey
  DO                                   'Get end co-ordinates
    Button = 0
    CALL CursorXY(X2, Y2, Button, Ky)

    IF Ky = -68 THEN  'GPDat(7) = 52 means screen not saved
      IF GPDat(7) <> 50 THEN
        CrossHair X2, Y2
        lastcx = X2
        lastcy = Y2
        GOTO RestartBox
      END IF
    END IF

    IF LastX2 <> X2 OR LastY2 <> Y2 THEN
   
      'Erase last box
      CrossHair LastX2, LastY2
      LineBVE X1, Y1, LastX2, LastY2, Clr
     
      'Draw new box
      LineBVE X1, Y1, X2, Y2, Clr
      CrossHair X2, Y2
     
      LastX2 = X2
      LastY2 = Y2

    END IF

  LOOP UNTIL Button > 0 OR Ky = 13
 
  'Erase last box
  CrossHair X2, Y2
  LineBVE X1, Y1, X2, Y2, Clr
 
  IF Button = 1 OR Ky = 13 THEN
    GPDat(83) = -1                 'Set drawing changed flag
    IF LastX2 < X1 THEN    'Make sure coordinates are positive
      SWAP X1, LastX2
      Corner = Corner + 1  'use 2nd coordinate
    END IF
   
    IF LastY2 < Y1 THEN
      SWAP Y1, LastY2
      Corner = Corner + 2
    END IF

    ClipLine = LeaveIt = 2 OR LeaveIt = 8 OR LeaveIt = 9

    IF (LeaveIt = 2 OR LeaveIt = 9 OR LeaveIt = 10) AND (GPDat(1) = 8 AND GPDat(6) = GPDat(71) AND GPDat(17) = -1) THEN
      LastX2 = LastX2 - 1
      IF LastY2 < (GPDat(44) - 1) THEN LastY2 = LastY2 - 1
    END IF

    SELECT CASE LeaveIt
      CASE 1
        LINE (X1, Y1)-(LastX2, Y1), GPDat(82), , GPDat(68)
        LINE (X1, LastY2)-(LastX2, LastY2), GPDat(82), , GPDat(68)
        LINE (X1, Y1)-(X1, LastY2), GPDat(82), , GPDat(68)
        LINE (LastX2, Y1)-(LastX2, LastY2), GPDat(82), , GPDat(68)

      CASE 2
        LINE (X1, Y1)-(LastX2, LastY2), GPDat(82), BF

      CASE 4
        CALL RoundBox(X1, Y1, LastX2, LastY2, GPDat(82))

      CASE 5
        CALL RoundBox(X1, Y1, LastX2, LastY2, GPDat(82))

      CASE 6
        LineBVE X1, Y1, LastX2, LastY2, GPDat(82)

      CASE 7
        GOSUB SetClr
        CALL PushButton(X1, Y1, LastX2, LastY2, 2, ButtonClr, Gray, HiClr)

      CASE 8
        GOSUB SetClr
        HV = 0
        IF LastX2 - X1 >= LastY2 - Y1 THEN 'Horiz
          HV = 0
        ELSE
          HV = -1
        END IF
        HVScrollBar X1, Y1, LastX2, LastY2, HV, HiClr, ButtonClr, Gray, ButtonClr

      CASE 9, 10           '3D Box, Frame
        IF LeaveIt = 9 THEN LINE (X1, Y1)-(LastX2, LastY2), GPDat(82), BF
        PCBoxes X1 - 1 + Border, Y1 - 1 + Border, LastX2 + 1 - Border, LastY2 + 1 - Border
        IF Border THEN
          BorderWidth = MaxInt(BevelWidth, 4) \ 4
          FOR i = 1 TO BorderWidth
            LINE (X1 - i, Y1 - i)-(LastX2 + i, LastY2 + i), 0, B
          NEXT
        END IF

      CASE 11
        EngravedBox X1, Y1, LastX2, LastY2


      CASE ELSE
        WaitUp
        ShowCursor
        LeaveIt = Corner
        EXIT SUB

    END SELECT
 END IF

  WaitUp
  WaitUpKey
  CrossHair X2, Y2
  lastcx = X2
  lastcy = Y2
LOOP

SetClr:
  IF GPDat(82) = 8 OR GPDat(82) = 0 THEN GPDat(82) = 7
    IF GPDat(82) < 8 THEN
      ButtonClr = GPDat(82)
      HiClr = GPDat(82) + 8
    ELSE
      ButtonClr = GPDat(82) - 8
      HiClr = GPDat(82)
    END IF
    Gray = 8
RETURN

END SUB

SUB MouseEllipse (x, y, Clr)

Clr = 15 + 24 * 256
HideCursor
GetCursor LastX, LastY, Button

RestartCircle:
CrossHair LastX, LastY
WaitUp                            'IF GPDat(31) = 5 THEN YAsp = 23790

DO
  Started = 0
  LastRadius = 0
  DO                                          'Wait for button press
    CALL CursorXY(x, y, Button, Ky)
    IF Ky = -68 THEN
      CrossHair x, y
      LastX = x
      LastY = y
      GOTO RestartCircle
    END IF
    IF LastX <> x OR LastY <> y THEN
      CrossHair LastX, LastY
      CrossHair x, y
      LastX = x
      LastY = y
    END IF
   
  LOOP UNTIL Button > 0 OR Ky = 13
  WaitUp
  WaitUpKey

  IF Button = 2 THEN
    IF Started THEN CALL CircleVE(XCenter, YCenter, Radius, Clr, 0, YAsp)
    CrossHair x, y
    ShowCursor
    EXIT SUB
  END IF
  XCenter = x
  YCenter = y
  CALL CircleVE(XCenter, YCenter, LastRadius, Clr, 0, YAsp)
  Started = -1

  DO
    CALL CursorXY(x, y, Button, Ky)

    IF Ky = -68 THEN
      CrossHair x, y
      LastX = x
      LastY = y
      GOTO RestartCircle
    END IF

    Radius = SQR(((ABS(XCenter - x + 1)) ^ 2) + ((ABS(YCenter - y + 1)) ^ 2))

    IF Radius <> LastRadius AND NOT Button THEN
      CrossHair LastX, LastY
      CALL CircleVE(XCenter, YCenter, LastRadius, Clr, 0, YAsp)
      CALL CircleVE(XCenter, YCenter, Radius, Clr, 0, YAsp)
      CrossHair x, y
      LastX = x
      LastY = y
      LastRadius = Radius
    END IF

  LOOP UNTIL Button > 0 OR Ky = 13
  WaitUpKey
  CrossHair LastX, LastY

  IF Button = 2 THEN
    CALL CircleVE(XCenter, YCenter, Radius, Clr, 0, YAsp)
    WaitUp
  ELSE
    GPDat(83) = -1                 'Set drawing changed flag
 
    LastXAspect = 0
    LastYAspect = 0
    YAspect = 0

    Rise = ABS(YCenter - y)
    IF Rise < 1 THEN Rise = 1
    RRun = ABS(XCenter - x)
    IF RRun < 1 THEN RRun = 1
     
    GPDat(7) = 25

    ShowCursor
    DO
      CALL CursorXY(x, y, Button, Ky)
     

      IF Ky = -68 THEN
        HideCursor
        LastX = x
        LastY = y
        GPDat(7) = 21               'Reset current drawing choice
        GOTO RestartCircle
      END IF

      XAspect& = (32760& * ABS(XCenter - x)) \ RRun
      IF XAspect& > 32766& THEN
        XAspect = 32766&
      ELSE
        XAspect = XAspect&
      END IF
 
      YAspect& = (32760& * ABS(YCenter - y)) \ Rise
      IF YAspect& > 32766& THEN
        YAspect = 32766&
      ELSE
        YAspect = YAspect&
      END IF
    
      IF ABS(YCenter - y) > ABS(XCenter - x) THEN
        XAspect = 0
        YAspect = YAspect - YAspectOS
      ELSE
        YAspect = 0 + YAsp
      END IF

      IF XAspect <> LastXAspect OR LastYAspect <> YAspect THEN
        HideCursor
        CALL CircleVE(XCenter, YCenter, Radius, Clr, LastXAspect, LastYAspect)
        CALL CircleVE(XCenter, YCenter, Radius, Clr, XAspect, YAspect)
        ShowCursor
        LastX = x
        LastY = y
        LastXAspect = XAspect
        LastYAspect = YAspect

      END IF
    LOOP UNTIL Button = 0
    GPDat(7) = 21
    HideCursor
    CALL CircleVE(XCenter, YCenter, Radius, Clr, XAspect, YAspect)

    IF XAspect THEN
      CIRCLE (XCenter, YCenter), Radius, GPDat(82), , , 32767 / XAspect
    ELSE
      CIRCLE (XCenter, YCenter), Radius, GPDat(82), , , YAspect / 32767
    END IF

  END IF
 
  GetCursor LastX, LastY, Button
  CrossHair LastX, LastY                'Turn cursor back on

LOOP

END SUB

SUB MouseLine (Clr1)
'                 ^XOR
HideCursor                               'Turn off the cursor
GetCursor LastX, LastY, Button           'Get mouse coords

Restart:
Started = 0
CrossHair LastX, LastY                   'Draw the GCursor

DO
  CALL CursorXY(x, y, Button, Ky)        'Look for keystrokes, mouse clicks

    IF Ky = -68 THEN
      CrossHair x, y
      LastX = x
      LastY = y
      GOTO Restart
    END IF

    IF LastX <> x OR LastY <> y THEN       'Draw new cursor if it has moved
      CrossHair LastX, LastY
      CrossHair x, y
      LastX = x
      LastY = y
    END IF

LOOP UNTIL Button > 0 OR Ky = 13         'Wait for mouseclick or Enter

IF Button = 2 THEN                       'Right click,
  CrossHair x, y                         ' delete GCursor
  ShowCursor                             'Turn on Cursor
  EXIT SUB                               'Bag out
END IF

startx = x
starty = y
X1 = startx
Y1 = starty


CrossHair x, y
CALL LineVE(x, y, X1, Y1, Clr1)
CrossHair x, y

DO
  WaitUp
  WaitUpKey
  DO
    CALL CursorXY(x, y, Button, Ky)     'Look for mouse click or keypress

    IF Ky = -68 THEN
      CrossHair x, y
      LastX = x
      LastY = y
      GOTO Restart
    END IF

    IF x <> X1 OR y <> Y1 THEN          'If we've moved,
      CrossHair X1, Y1                  ' turn off GCursor
      CALL LineVE(startx, starty, x, y, Clr1)   'delete old line
      CALL LineVE(startx, starty, X1, Y1, Clr1) 'draw new line
      CrossHair x, y                            'draw new GCursor
      X1 = x                                    'update last coords
      Y1 = y
    END IF
   
    IF Ky = 80 OR Ky = 112 THEN                 'If "Pp" pressed
      CrossHair x, y                            ' erase last GCursor
      CALL LineVE(startx, starty, X1, Y1, Clr1) ' clear last line

      IF Started THEN
        CALL DrawPointVEOpts(startx, starty, GPDat(82))    ' ""
      END IF

      Mouse 7, 0, startx, startx                ' trap the mouse at last coords
      Mouse 8, 0, starty, starty                ' ""
      CrossHair startx, starty                  ' draw the new GCursor
      MouseXY                                   ' display new coords if on

      CALL DrawPolar(startx, starty, PStarted)  'Go into polar mode
      IF NOT PStarted THEN
        PStarted = 0
        CALL DrawPointVEOpts(startx, starty, Clr1)
      END IF

      CrossHair startx, starty
      X1 = startx
      Y1 = starty
      CALL FreeMouse
      Ky = 0
    END IF

  LOOP UNTIL Button > 0 OR Ky = 13

  IF Button = 2 THEN
    CrossHair x, y
    CALL LineVE(startx, starty, X1, Y1, Clr1)
    IF Started THEN CALL DrawPointVEOpts(startx, starty, GPDat(82))
    WaitUp
    GOTO Restart
  ELSE
    Started = -1
    CrossHair x, y
    CALL LineVE(startx, starty, X1, Y1, Clr1)
    'CALL DrawPointVEOpts(x1, y1, Clr1)
    LINE (startx, starty)-(X1, Y1), GPDat(82), , GPDat(68)
    CrossHair x, y
    GPDat(83) = -1                 'Set drawing changed flag
 END IF

    startx = X1
    starty = Y1

LOOP

END SUB

SUB MousePolygon (x, y, Clr)

'---- 'Save these values between calls
STATIC NumSides AS INTEGER, StartAngle AS INTEGER
                         ', Sides2Disp AS INTEGER

REDIM DLog(7) AS Dialog
REDIM Response$(7)
REDIM Menu$(0)

Response$(3) = SPACE$(2)
Response$(4) = SPACE$(3)
Response$(5) = SPACE$(2)            'Aspect Ratio
Response$(6) = SPACE$(4)

T$ = LTRIM$(STR$(GPDat(69) / 1000))
IF T$ = "1" THEN T$ = "1.000"
Response$(6) = T$

IF NumSides = 0 THEN NumSides = 3

LSET Response$(3) = LTRIM$(STR$(NumSides))
LSET Response$(4) = LTRIM$(STR$(StartAngle))
LSET Response$(5) = LTRIM$(STR$(Sides2Disp))

IF GPDat(71) = 8 THEN MdOS = 3
DLog(0).Heading = "Draw Polygon"

DLog(1).DType = 6                            'Push Button
DLog(1).Row = 1 * GPDat(71) - GPDat(71) \ 2 + 10 - 2 * MdOS
DLog(1).Col = 27 * 8 - 2
DLog(1).Heading = "OK"
DLog(1).NumRows = GPDat(80)
DLog(1).TextLen = 66

DLog(2).DType = 6                            'Push Button
DLog(2).Row = 3 * GPDat(71) - GPDat(71) \ 2 + 7 + MdOS
DLog(2).Col = 27 * 8 - 2
DLog(2).Heading = "Cancel"
DLog(2).NumRows = GPDat(80)
DLog(2).TextLen = 66
DLog(2).Group = 1000

DLog(3).DType = 3                            'Text entry
DLog(3).Row = 1
DLog(3).Col = 2
DLog(3).Heading = "Number of Sides: <12"
DLog(3).TextLen = 2

DLog(4).DType = 3                            'Text entry
DLog(4).Row = 2
DLog(4).Col = 2
DLog(4).Heading = "Starting Angle:  <13" '12   7/6/94
DLog(4).TextLen = 3
DLog(4).HotKey = 9

DLog(5).DType = 3                              'Text entry
DLog(5).Row = 4
DLog(5).Col = 2
DLog(5).Heading = "Sides to display:<11"
DLog(5).TextLen = 2
DLog(5).HotKey = 9

DLog(6).DType = 3                              'Text entry
DLog(6).Row = 5
DLog(6).Col = 2
DLog(6).Heading = "x:y Ratio:       <11"
DLog(6).TextLen = 4
DLog(6).HotKey = 4

DLog(7).DType = 5
DLog(7).Heading = ""
DLog(7).Row = 4 * GPDat(71)
DLog(7).Col = 37

Choice = 1
CALL GDialog(Choice, DLog(), Response$(), Menu$(), Style, 0)

IF Choice = 2 OR Choice = 27 THEN
  EXIT SUB
END IF

SRScreen 0                 'Save the screen

Test# = VAL(Response$(6))
IF Test# > 32 THEN Test# = 32
GPDat(69) = Test# * 1000

NumSides = VAL(RTRIM$(Response$(3)))
IF NumSides = 0 THEN NumSides = 3

StartAngle = VAL(RTRIM$(Response$(4))) MOD 365

Sides2Disp = VAL(Response$(5))
IF Sides2Disp = 0 THEN Sides2Disp = NumSides

HideCursor
GetCursor LastX, LastY, Button

RestartPoly:
CrossHair LastX, LastY

DO
  LastRadius = 1
  DO                                          'Wait for button press
    CALL CursorXY(x, y, Button, Ky)

    IF Ky = -68 THEN
      CrossHair x, y
      LastX = x
      LastY = y
      GOTO RestartPoly
    END IF

    IF LastX <> x OR LastY <> y THEN
      CrossHair LastX, LastY
      CrossHair x, y
      LastX = x
      LastY = y
    END IF
 
  LOOP UNTIL Button > 0 OR Ky = 13

  IF Button = 2 THEN
    CrossHair x, y
    ShowCursor
    EXIT SUB
  END IF
  XCenter = x         '    ((x + GPDat(2)) \ GPDat(1)) * GPDat(1)  'Get snap spacing
  YCenter = y         '((y + GPDat(10)) \ GPDat(6)) * GPDat(6)

  WaitUp
  WaitUpKey

  HideCursor
  CALL Polygon(XCenter, YCenter, NumSides, LastRadius, StartAngle, Sides2Disp, Clr)
  ShowCursor
 
  DO
    CALL CursorXY(x, y, Button, Ky)

    IF Ky = -68 THEN
      CrossHair x, y
      LastX = x
      LastY = y
      GOTO RestartPoly
    END IF
   
    Radius = SQR(((ABS(XCenter - x + 1)) ^ 2) + ((ABS(YCenter - y + 1)) ^ 2))

    IF Radius <> LastRadius AND NOT Button THEN
     
      CrossHair LastX, LastY
      CALL Polygon(XCenter, YCenter, NumSides, LastRadius, StartAngle, Sides2Disp, Clr)
      CALL Polygon(XCenter, YCenter, NumSides, Radius, StartAngle, Sides2Disp, Clr)
      CrossHair x, y
      LastX = x
      LastY = y

      LastRadius = Radius
    END IF

  LOOP UNTIL Button > 0 OR Ky = 13
  'Erase it
  CALL Polygon(XCenter, YCenter, NumSides, Radius, StartAngle, Sides2Disp, Clr)
 
  IF Button = 1 OR Ky = 13 THEN
    CrossHair x, y
    CALL Polygon(XCenter, YCenter, NumSides, Radius, StartAngle, Sides2Disp, GPDat(82))
    CrossHair x, y
    GPDat(83) = -1                 'Set drawing changed flag
  END IF

  WaitUp
  WaitUpKey

LOOP

END SUB

SUB MouseXY STATIC

  IF NOT GPDat(5) THEN EXIT SUB
  
  CALL GetCursor(x, y, Button)

  IF GPDat(19) THEN                'Row, Col
    IF GPDat(21) THEN              'Snap on
      x = (x + 4) \ 8
      y = (y + GPDat(71) \ 2) \ GPDat(71)
      OS = 0
    ELSE                           'Snap off
      x = x \ 8 + 1
      y = y \ GPDat(71) + 1
    END IF
  ELSE                             'x, y
    IF GPDat(17) THEN      'If Snap is on, convert to snap coordinates
      x = ((x + GPDat(2)) \ GPDat(1)) * GPDat(1)     'GPDat(1) = XSnap Spacing
      y = ((y + GPDat(10)) \ GPDat(6)) * GPDat(6)    'GPDat(6) = YSnap Spacing
      OS = 1
    END IF
  END IF
 
 ' IF GPDat(21) THEN        'Relative
    IF Button = 1 THEN
      LastxPos = x
      LastyPos = y
    END IF
  IF GPDat(21) THEN        'Relative
    x = ABS(LastxPos - x) + OS
    y = ABS(LastyPos - y) + OS
  END IF

  IF LastX <> x OR LastY <> y THEN

    IF GPDat(19) THEN
      LSET XCoord$ = "c:" + LTRIM$(STR$(x))
      LSET YCoord$ = "r:" + LTRIM$(STR$(y))
    ELSE
      LSET XCoord$ = "x:" + LTRIM$(STR$(x))
      LSET YCoord$ = "y:" + LTRIM$(STR$(y))
    END IF

    IF GPDat(17) THEN
      XCoord$ = UCASE$(XCoord$)
      YCoord$ = UCASE$(YCoord$)
    END IF
    
    CALL GPrint2VE(GPDat(16) + 5, GPDat(15) \ 8 + 8, XCoord$, 7 * 256)
    CALL GPrint2VE(GPDat(16) + 5, GPDat(15) \ 8 + 15, YCoord$, 7 * 256)
    LastX = x
    LastY = y
  END IF

END SUB

SUB PaintLine

LineSpacing = 2
HideCursor
GetCursor Lastx1, Lasty1, Button
CrossHair Lastx1, Lasty1

DO
Kbd = 0
DO
  CALL CursorXY(X1, Y1, Button, Ky)
  IF Lastx1 <> X1 OR Lasty1 <> Y1 THEN
    CrossHair Lastx1, Lasty1
    CrossHair X1, Y1
    Lastx1 = X1
    Lasty1 = Y1
  END IF

LOOP UNTIL Button > 0 OR Ky = 13           'Wait 'til we press a button
IF Ky = 13 THEN Kbd = -1
WaitUpKey

IF Button = 2 THEN                         'Bail out if they hit button 2
  CrossHair X1, Y1
   ShowCursor
  EXIT SUB
END IF

CrossHair Lastx1, Lasty1
CALL LineVE(X1, Y1, X1, Y1, GPDat(82))           'Start the line
CrossHair X1, Y1

DO
  CALL CursorXY(X2, Y2, Button, Ky)
    IF Button > 0 THEN Kbd = 0
    IF X1 <> X2 OR Y1 <> Y2 THEN           'If we've moved,
      CrossHair X1, Y1
      CALL LineVE(X1, Y1, X2, Y2, GPDat(82))        ' draw a new line segment
      CrossHair X2, Y2
      X1 = X2                              'update starting coordinates
      Y1 = Y2
    END IF
LOOP UNTIL (Button = 0 AND NOT Kbd) OR Ky = 13 OR Ky = 27'keep drawing until we hit a button

WaitUpKey
CrossHair X1, Y1
GPDat(83) = -1                 'Set drawing changed flag

CrossHair X1, Y1
Lastx1 = X1
Lasty1 = Y1

LOOP

END SUB

SUB PaletteEdit (x, y, BWidth, BHite, Done)

Done = -1
PalHite = 4 * BHite
PalWidth = 4 * BWidth

IF GPDat(31) = 5 THEN
  TxtHeight = 14
  BoxHite = PalHite + 24 + 2 * TxtHeight
ELSE
  TxtHeight = 16
  BoxHite = PalHite + 24 + 4 * TxtHeight
END IF

tx = x
ty = y
bx1 = x - 12
by1 = y - 8
bx2 = x + PalWidth + 12 + 88
by2 = y + BoxHite - 8'           PalHite + 18 + 4 * TxtHeight

BoxWidth = PalWidth + 12 + 88 + 12

WaitUp

HideCursor
CALL GMove2VE((x - 8) \ 8 - 1, y - 8, (PalWidth + 24 + 80) \ 8 + 5, BoxHite + 10, &HAA00, 0)

'Box & Box Outline
LINE (x - 12, y - 8)-STEP(PalWidth + 24 + 88, BoxHite), 7, BF
CALL PCBox(x - 12, y - 8, x + PalWidth + 12 + 88, y + BoxHite - 8, -1)

LINE (x + 4, y + BoxHite - 7)-STEP(PalWidth + 8 + 88, 8), 0, BF
LINE (x + BoxWidth - 11, y + 1)-STEP(8, BoxHite), 0, BF

'Inner Box
CALL PCBox(x - 8, y - 4, x + PalWidth + 8 + 88, y + BoxHite - 2 * TxtHeight + 5, 0)

IF GPDat(71) = 8 THEN HtOS = 3
IF GPDat(31) = 5 THEN
  CALL GPrint3VE(y + BoxHite - TxtHeight - 8 + HtOS, x \ 8 + 1, "EGA            palette editor", 0)
  REDIM Area(0 TO 17) AS ScreenObject
  UB = 17
  DIM ScrollBar(16 TO 16) AS ScrollBar
  REDIM Colr(1)
  Sp = 24
ELSE
  CALL GPrint3VE(y + PalHite + TxtHeight * 4 + HtOS, x \ 8 + 1, "VGA            palette editor", 0)
  REDIM Area(0 TO 19) AS ScreenObject
  UB = 19
  DIM ScrollBar(16 TO 18) AS ScrollBar
  REDIM Colr(3)
  Sp = 14
END IF


FOR Y1 = 0 TO PalHite - BHite STEP BHite        'Draw the Palette
  FOR X1 = 0 TO PalWidth - BWidth STEP BWidth
    LINE (X1 + x, Y1 + y)-STEP(BWidth, BHite), PClr, BF
    Area(PClr).ex1 = X1 + x
    Area(PClr).ex2 = Area(PClr).ex1 + BWidth
    Area(PClr).ey1 = y + Y1
    Area(PClr).ey2 = Area(PClr).ey1 + BHite
    PClr = PClr + 1
  NEXT
NEXT

LINE (x + PalWidth + 8, y)-STEP(80, 4 * BHite), GPDat(82), BF
LineBVE x + PalWidth + 8, y, x + PalWidth + 88, y + 4 * BHite, 0
CALL PCBox(x + PalWidth + 7, y - 1, x + PalWidth + 90, y + 4 * BHite + 2 + (GPDat(31) = 5), 0)


FOR X1 = BWidth TO PalWidth - BWidth STEP BWidth  'Vertical lines
  LINE (x + X1, y)-STEP(0, PalHite), 0
NEXT
LINE (x + X1, y)-STEP(0, PalHite), 15             'Hilite

FOR Y1 = BHite TO PalHite - BHite STEP BHite      'Horizontal lines
  LINE (x, y + Y1)-STEP(PalWidth, 0), 0
NEXT

CALL PCBox(x - 1, y - 1, x + PalWidth + 2, y + PalHite + 2 + (GPDat(31) = 5), 0)
LineBVE x, y, x + PalWidth, y + PalHite, 0

IF GPDat(31) = 5 THEN
  BRows = 0
ELSE
  BRows = 2
END IF

LINE (x, y + PalHite + 8)-STEP(56, (BRows + 1) * TxtHeight), 15, BF
CALL PCBox(x - 1, y + PalHite + 7, x + PalWidth + 90, y + PalHite + 8 + (BRows + 1) * TxtHeight + 2 + (GPDat(31) = 5), 0)

RESTORE VGA

FOR Y1 = 0 TO BRows * TxtHeight STEP TxtHeight

  LINE (x, y + Y1 + PalHite + 8)-STEP(2 * BWidth, TxtHeight), 0, B
  LINE (x + Sp, y + Y1 + PalHite + 8)-STEP(0, TxtHeight), 0, B

  IF GPDat(31) = 5 THEN
    CALL GPrint3VE(y + Y1 + PalHite + 9 + HtOS, x \ 8 + 1, STR$(GPDat(82)), 0)
  ELSE
    READ A$
    CALL GPrint3VE(y + Y1 + PalHite + 9 + HtOS, x \ 8 + 2, A$, 0)
  END IF

          'Save Mouse coords
  Area(PClr).ex1 = x + 56
  Area(PClr).ex2 = Area(PClr).ex1 + 168
  Area(PClr).ey1 = y + Y1 + PalHite + 8
  Area(PClr).ey2 = Area(PClr).ey1 + TxtHeight

  ScrollBar(PClr).X1 = Area(PClr).ex1
  ScrollBar(PClr).X2 = Area(PClr).ex2
  ScrollBar(PClr).Y1 = Area(PClr).ey1
  ScrollBar(PClr).Y2 = Area(PClr).ey2
  ScrollBar(PClr).Hi = 63
  ScrollBar(PClr).Lo = 0
  ScrollBar(PClr).PointerPos = ScrollBar(PClr).X1 + 16
  ScrollBar(PClr).LargeChange = 8
  ScrollBar(PClr).SmallChange = 1
  ScrollBar(PClr).Factor = GetFactor#(ScrollBar(PClr).X1, ScrollBar(PClr).X2, ScrollBar(PClr).Lo, ScrollBar(PClr).Hi)
  ScrollBar(PClr).Action = 1
  HVScrollBar ScrollBar(PClr).X1, ScrollBar(PClr).Y1, ScrollBar(PClr).X2, ScrollBar(PClr).Y2, 0, 15, 7, 8, 7
  SetPBColors ScrollBar(PClr), 0
  PClr = PClr + 1
NEXT

Area(UB).ex1 = bx1
Area(UB).ey1 = by2 - TxtHeight
Area(UB).ex2 = bx2
Area(UB).ey2 = by2

IF GPDat(31) = 5 THEN
  Colr(1) = ASC(MID$(EGAPal$, GPDat(82) + 1, 1))
  CALL GPrint3VE(ty + PalHite + 9 + HtOS, tx \ 8 + 4, STR$(Colr(1)), 0)
  ScrollBar(16).Value = Colr(1)
  CALL HScrollBar(Button, x, y, Ky$, ScrollBar(16), Clean)
  LastClr = GPDat(82)
ELSE
  CALL GetPalTripleVGA(GPDat(82), Colr(1), Colr(2), Colr(3))
  CALL GPrint3VE(ty + PalHite + 9 + HtOS, tx \ 8 + 4, STR$(Colr(1)), 0)
  CALL GPrint3VE(ty + PalHite + 9 + TxtHeight + HtOS, tx \ 8 + 4, STR$(Colr(2)), 0)
  CALL GPrint3VE(ty + PalHite + 9 + TxtHeight * 2 + HtOS, tx \ 8 + 4, STR$(Colr(3)), 0)

  ScrollBar(16).Value = Colr(1)
  CALL HScrollBar(Button, x, y, Ky$, ScrollBar(16), Clean)
  ScrollBar(16).Blink = -1

  ScrollBar(17).Value = Colr(2)
  CALL HScrollBar(Button, x, y, Ky$, ScrollBar(17), Clean)
  ScrollBar(17).Blink = -1

  ScrollBar(18).Value = Colr(3)
  CALL HScrollBar(Button, x, y, Ky$, ScrollBar(18), Clean)
  ScrollBar(18).Blink = -1
END IF

ShowCursor
ScrlBr = 16
ClrComp = 1

DO
  GetCursor x, y, Button
  Ky$ = INKEY$
  SELECT CASE LEN(Ky$)
    CASE 1
      Ky = ASC(Ky$)
    CASE 2
      Ky = -ASC(RIGHT$(Ky$, 1))
    CASE ELSE
      Ky = 0
  END SELECT

  SELECT CASE Ky
    CASE 9
      CALL HScrollBar(Button, x, y, Ky$, ScrollBar(ScrlBr), -1)
      ScrlBr = ScrlBr + 1
      IF ScrlBr > UB - 1 THEN ScrlBr = 16
      ClrComp = ScrlBr - 15
    CASE -15
      CALL HScrollBar(Button, x, y, Ky$, ScrollBar(ScrlBr), -1)
      ScrlBr = ScrlBr - 1
      IF ScrlBr < 16 THEN ScrlBr = UB - 1
      ClrComp = ScrlBr - 15
    CASE 48 TO 57
      GPDat(82) = Ky - 48
    CASE 41
      GPDat(82) = 10
    CASE 33
      GPDat(82) = 11
    CASE 64
      GPDat(82) = 12
    CASE 35
      GPDat(82) = 13
    CASE 36
      GPDat(82) = 14
    CASE 37
      GPDat(82) = 15

  END SELECT

  IF GPDat(82) <> LastClr THEN
    LastClr = GPDat(82)
    Ky = 1000
    NewClr = -1
  END IF

  IF (Button = 1 OR Ky = 1000) AND NOT Scrolling THEN

    FOR i = 0 TO UB
      IF NewClr OR (x > Area(i).ex1 AND x < Area(i).ex2 AND y > Area(i).ey1 AND y < Area(i).ey2) THEN
          
        IF i < 16 THEN
          NewClr = 0
          CALL HScrollBar(Button, x, y, Ky$, ScrollBar(ScrlBr), -1)
          Scrolling = 0
          IF Button THEN GPDat(82) = i
          HideCursor
          LINE (tx + PalWidth + 9, ty + 1)-STEP(78, PalHite - 2), GPDat(82), BF
          IF Ky = 1000 THEN i = GPDat(82)
          IF GPDat(31) = 5 THEN
            LINE (tx + 4, ty + PalHite + 10)-STEP(16, TxtHeight - 4), 15, BF
            GPrint3VE ty + Y1 + PalHite - 5 + HtOS, tx \ 8 + 1, STR$(GPDat(82)), 0
            Colr(1) = ASC(MID$(EGAPal$, i + 1, 1))
            LINE (tx + 28, ty + PalHite + 10)-STEP(26, TxtHeight - 4), 15, BF
            Button = 0
            ScrollBar(16).Value = Colr(1)
            CALL HScrollBar(Button, x, y, Ky$, ScrollBar(16), Clean)
            CALL HScrollBar(Button, x, y, Ky$, ScrollBar(16), -1)
          ELSE
            CALL GetPalTripleVGA(GPDat(82), Colr(1), Colr(2), Colr(3))
            LINE (tx + 24, ty + PalHite + 10)-STEP(24, TxtHeight - 4), 15, BF
            Button = 0
            FOR N = 16 TO 18
              ScrollBar(N).Value = Colr(N - 15)
              CALL HScrollBar(Button, x, y, Ky$, ScrollBar(N), Clean)
              CALL HScrollBar(Button, x, y, Ky$, ScrollBar(N), -1)
            NEXT
            LastValue = ScrollBar(ScrlBr).Value


          END IF
          CALL GPrint3VE(ty + PalHite + 9 + HtOS, tx \ 8 + 4, STR$(Colr(1)), 0)
          IF GPDat(31) > 5 THEN
            LINE (tx + 24, ty + PalHite + 10 + TxtHeight)-STEP(30, TxtHeight - 4), 15, BF
            CALL GPrint3VE(ty + PalHite + 9 + TxtHeight + HtOS, tx \ 8 + 4, STR$(Colr(2)), 0)
            LINE (tx + 24, ty + PalHite + 10 + TxtHeight * 2)-STEP(24, TxtHeight - 4), 15, BF
            CALL GPrint3VE(ty + PalHite + 9 + TxtHeight * 2 + HtOS, tx \ 8 + 4, STR$(Colr(3)), 0)
          END IF
          LastClr = GPDat(82)

          ShowCursor
          WaitUp
        ELSEIF i = UB THEN     'Move the whole thing
          CALL HScrollBar(Button, x, y, Ky$, ScrollBar(ScrlBr), -1)
          Scrolling = 0
          IF Ky = 13 THEN EXIT FOR
          BoxClr = 8 + 24 * 256
          HideCursor
          CALL StepBox(bx1, by1, BoxWidth, BoxHite, BoxClr)
          ShowCursor
          xOS = ((x - bx1) \ 8) * 8
          yOS = y - by1
          X1 = bx1
          Y1 = by1
          Mouse 7, 0, xOS, GPDat(43) - BoxWidth + xOS  'Trap the mouse
          Mouse 8, 0, yOS, GPDat(44) - BoxHite + yOS
           
          DO
            CALL GetCursor(x, y, Button)
            x = ((x + 4) \ 8) * 8
            x = x - xOS
            y = y - yOS
            IF x <> X1 OR y <> Y1 THEN
              HideCursor
              CALL StepBox(X1, Y1, BoxWidth, BoxHite, BoxClr)
              CALL StepBox(x, y, BoxWidth, BoxHite, BoxClr)
              ShowCursor
              X1 = x
              Y1 = y
            END IF
          LOOP UNTIL Button = 0

          FreeMouse
          HideCursor
          CALL StepBox(x, y, BoxWidth, BoxHite, BoxClr)
          CALL GMove2VE((tx - 8) \ 8 - 1, ty - 8, (PalWidth + 24 + 80) \ 8 + 5, BoxHite + 10, &HAA00, -1)
          ShowCursor
          Done = 0
          x = x + 12
          y = y + 8
          'GPDat(17) = SnapStatus
          EXIT SUB
         
        ELSEIF i > 15 THEN
          CALL HScrollBar(Button, x, y, Ky$, ScrollBar(ScrlBr), -1)
          ScrlBr = i
          Scrolling = -1
          ClrComp = i - 15
          GPDat(83) = -1                 'Set drawing changed flag
        END IF
        EXIT FOR
      END IF
    NEXT
  END IF
  
  CALL HScrollBar(Button, x, y, Ky$, ScrollBar(ScrlBr), Clean)
  Ky$ = ""
  IF Button = 0 THEN Scrolling = 0
  Colr(ClrComp) = ScrollBar(ScrlBr).Value
  IF LastValue <> ScrollBar(ScrlBr).Value THEN
    LastValue = ScrollBar(ScrlBr).Value
    GOSUB SetPal
  END IF

LOOP UNTIL Button = 2 OR Ky = 27

'GPDat(17) = SnapStatus
HideCursor
CALL GMove2VE((tx - 8) \ 8 - 1, ty - 8, (PalWidth + 24 + 80) \ 8 + 5, BoxHite + 10, &HAA00, -1)
ShowCursor

x = tx
y = ty

EXIT SUB

SetPal:
  IF GPDat(31) = 5 THEN
    LastClr = GPDat(82)
    CALL SetPaletteEGA(GPDat(82), Colr(1))
    MID$(EGAPal$, GPDat(82) + 1, 1) = CHR$(Colr(1))
    HideCursor
    LINE (tx + 28, ty + PalHite + 10)-STEP(26, TxtHeight - 4), 15, BF
    CALL GPrint3VE(ty + PalHite + 9 + HtOS, tx \ 8 + 4, STR$(Colr(1)), 0)
    ShowCursor
  ELSE
    CALL SetPalTripleVGA(GPDat(82), Colr(1), Colr(2), Colr(3))
    HideCursor
    SELECT CASE ClrComp
      CASE 1
    LINE (tx + 24, ty + PalHite + 10)-STEP(24, TxtHeight - 4), 15, BF
    CALL GPrint3VE(ty + PalHite + 9 + HtOS, tx \ 8 + 4, STR$(Colr(1)), 0)
      CASE 2
    LINE (tx + 24, ty + PalHite + 10 + TxtHeight)-STEP(24, TxtHeight - 4), 15, BF
    CALL GPrint3VE(ty + PalHite + 9 + TxtHeight + HtOS, tx \ 8 + 4, STR$(Colr(2)), 0)
      CASE 3
    LINE (tx + 24, ty + PalHite + 10 + TxtHeight * 2)-STEP(24, TxtHeight - 4), 15, BF
    CALL GPrint3VE(ty + PalHite + 9 + TxtHeight * 2 + HtOS, tx \ 8 + 4, STR$(Colr(3)), 0)
    END SELECT

    ShowCursor
  END IF

RETURN

END SUB

DEFSNG A-Z
SUB Polygon (XCenter%, YCenter%, NumSides%, Radius%, StartAngle%, Sides2Disp%, DrawClr%)

IF NumSides% < 1 THEN NumSides% = 1
Angle! = 360 / NumSides%
Hypotenuse! = Radius%

NAngle! = StartAngle%

CALL GetSides(NAngle!, Hypotenuse!, Adjacentx1!, Oppositey1!)

startx1% = XCenter% + Adjacentx1!
starty1% = YCenter% + Oppositey1!

FOR i% = 1 TO Sides2Disp%    'NumSides%
  NAngle! = NAngle! + Angle!

  CALL GetSides(NAngle!, Hypotenuse!, Adjacent!, Opposite!)
  X2% = Adjacent! + XCenter%
  Y2% = Opposite! + YCenter%

  IF DrawClr% < 17 THEN
    LINE (startx1%, starty1%)-(X2%, Y2%), DrawClr%, , GPDat%(68)
  ELSE
    CALL LineVE(startx1%, starty1%, X2%, Y2%, DrawClr%)
  END IF

  startx1% = X2%
  starty1% = Y2%

NEXT

END SUB

DEFINT A-Z
'Subroutine to print text any where on the graphics screen
SUB PrintText

STATIC Row, Col

GPDat(19) = -1                        'Turn on text coords
CursorTiming = 6
'DO                                   'Wait for a mouse click
'  CALL CursorXY(x1, y1, Button, Ky)
'LOOP UNTIL Button > 0 OR Ky = 32

'IF Button = 2 THEN                   'Right click, so bag out
'  EXIT SUB
'END IF

'Col = x \ 8                          'Starting column
'Row = y \ GPDat(71) + 1              'Starting row

IF Col = 0 THEN Col = 1               'Starting column
IF Row = 0 THEN Row = 1               'Starting row


GOSUB SaveRow
DO
  CALL DrawCursor(Col * 8 - 8, Row * GPDat(71) - GPDat(71), 8, GPDat(71) - 1, CursorIs, CursorTiming)
 
  Ky$ = INKEY$                        'Look for keystrokes
  CALL GetCursor(tx, ty, Button)      'Look for mouse presses
  CALL MouseXY
 
  IF Button = 2 THEN                  'Right click, same as Enter
    Ky$ = CHR$(13)
    DO                                'Wait 'til they lift their finger
      CALL GetCursor(G, h, Button)
    LOOP UNTIL Button = 0
  END IF
  IF Button = 1 THEN                 'Left click, find row and column
    GOSUB MoveCursor
    CALL MouseXY
    Col = tx \ 8 + 1
    Row = ty \ GPDat(71) + 1
    GOSUB SaveRow
  END IF
 
  SELECT CASE LEN(Ky$)
    CASE 0
      Ky = 0                     'No key pressed
    CASE 1
      Ky = ASC(Ky$)              'Normal keys
    CASE 2
      Ky = -ASC(RIGHT$(Ky$, 1))  'Extended keys
    CASE ELSE
  END SELECT

  SELECT CASE Ky
    CASE 8  'Backspace
      GOSUB MoveCursor           'Turn cursor off
      Col = Col - 1
      IF Col < 1 THEN Col = 1
      GOSUB RestBG
                  
    CASE 9                       'Tab
      GOSUB MoveCursor
      Col = Col + 8

    CASE -71                     'Home
      GOSUB MoveCursor
      Col = 1
   
    CASE -72                     'Up
      GOSUB MoveCursor
      Row = Row - 1
      IF Row < 1 THEN Row = 1
      GOSUB SaveRow
   
    CASE -73                     'PgUp
      GOSUB MoveCursor
      Row = 1
      GOSUB SaveRow
   
    CASE -75                     'Left
      GOSUB MoveCursor
      Col = Col - 1
  
    CASE -77                     'Right
      GOSUB MoveCursor
      Col = Col + 1
   
    CASE -115                    'Control Left
      GOSUB MoveCursor
      Col = Col - 20
 
    CASE -116                    'Control Right
      GOSUB MoveCursor
      Col = Col + 20
   
    CASE -79                     'End
      GOSUB MoveCursor
      Col = 80
    
    CASE -80                     'Down
      GOSUB MoveCursor
      Row = Row + 1
      IF Row > GPDat(85) THEN Row = 1
      GOSUB SaveRow
     
    CASE -81                     'PgDown
      GOSUB MoveCursor
      Row = GPDat(85)
      GOSUB SaveRow
     
    CASE 13, 27                  'Enter, Escape
      GOSUB MoveCursor
      EXIT SUB
   
    CASE 32
      GOSUB MoveCursor
      GOSUB RestBG
      Col = Col + 1
      IF Col > 80 THEN Col = 80


    CASE IS > 32 AND Ky < 256    'All other normal keys
      GOSUB MoveCursor
      HideCursor
      CALL GPrint3VE(Row * GPDat(71) - GPDat(71), Col, Ky$, GPDat(82))
      ShowCursor
      Col = Col + 1
      GPDat(83) = -1                 'Set drawing changed flag
   
    CASE ELSE
 
  END SELECT
  IF Col < 1 THEN Col = 1
  IF Col > 80 THEN Col = 80
 
  GOSUB XYCoord                          'Print Row and Col if Status is on

LOOP

SaveRow:                                 'Save the current edit row
  HideCursor
  CALL SetGWPages(0, 1)
  CALL GMove1VE(1, Row * GPDat(71) - GPDat(71), 80, GPDat(71), 1, 96)
  CALL SetGWPages(0, 0)
  ShowCursor
RETURN

RestBG:
  HideCursor
  CALL SetGWPages(1, 0)           'Restore background behind the character
  CALL GMove1VE(Col, 96, 1, GPDat(71), Col, Row * GPDat(71) - GPDat(71))
  CALL SetGWPages(0, 0)
  ShowCursor
RETURN

MoveCursor:
  IF CursorIs THEN             'If cursor is on, turn it off before moving
    CALL DrawCursor(Col * 8 - 8, Row * GPDat(71) - GPDat(71), 8, GPDat(71) - 1, CursorIs, -1)
  END IF
RETURN

XYCoord:
  IF LastCol <> Col OR LastRow <> Row THEN
    IF GPDat(5) THEN             'If Status is on, Display the row and column
      LSET XCoord$ = "c:" + LTRIM$(STR$(Col))
      LSET YCoord$ = "r:" + LTRIM$(STR$(Row))
      CALL GPrint2VE(GPDat(16) + 5, GPDat(15) \ 8 + 8, XCoord$, 7 * 256)
      CALL GPrint2VE(GPDat(16) + 5, GPDat(15) \ 8 + 15, YCoord$, 7 * 256)
    END IF
    LastCol = Col
    LastRow = Row
  END IF
RETURN

END SUB

SUB SelTile (LeftCol, TopLine, PasteBuf())

'TPal.Til = random tile file
'TilePal.GM4 = tile palette image

'SnapStatus = GPDat(17)
'GPDat(17) = 0


TPath$ = TilePath$
IF RIGHT$(TPath$, 1) <> "\" THEN TPath$ = TPath$ + "\"

IF NOT Exist%(TempPath$ + "TILEPAL.GM4") THEN
  TempPath$ = GQSPath$
END IF

TFName$ = TPath$ + "TilePal.GM4"

NumBytes& = FileSize&(TFName$)
IF DOSError THEN
  BEEP
  DOSMsg
  EXIT SUB
END IF
DO

SnapStatus = GPDat(17)
GPDat(17) = 0


REDIM PasteBuf(NumBytes& \ 2 - 2)
WinImage = 0
FOpen TFName$, Handle

GPDat(86) = -1
'GPDat(67) = -1
GPDat(70) = -1

FGetA Handle, SEG PasteBuf(0), NumBytes&
FClose (Handle)

IF LeftCol < 1 THEN LeftCol = 1
IF LeftCol > 24 THEN LeftCol = 24

X1 = LeftCol * 8 - 8
Y1 = TopLine

HideCursor
GMove2VE LeftCol, TopLine, 57, 176, &HAA00, 0
GMove4VE LeftCol, TopLine, 55, 167, VARSEG(PasteBuf(0)), -1

CALL ClearVE

LINE (LeftCol * 8, TopLine + 166)-(LeftCol * 8 + 436, TopLine + 172), 0, BF
LINE (LeftCol * 8 + 432, TopLine + 8)-(LeftCol * 8 + 436, TopLine + 168), 0, BF

ShowCursor
WaitUp

DO
  CALL CursorXY(x, y, Button, Ky)
  Row = 0
  Col = 0
  IF Button = 2 THEN
    HideCursor
    GMove2VE LeftCol, TopLine, 57, 176, &HAA00, -1
    ShowCursor
    GPDat(17) = SnapStatus
    EXIT SUB
  END IF

  IF Button = 1 OR Ky = 13 THEN

    IF Button = 1 AND y > TopLine + 150 AND y < TopLine + 166 THEN
      IF x > LeftCol * 8 AND x < (LeftCol + 54) * 8 THEN

        X1 = LeftCol * 8 - 8
        Y1 = TopLine
        xOS = x - X1
        yOS = y - TopLine

        HideCursor
        LineBVE X1, Y1, X1 + 440, Y1 + 167, 6152
        ShowCursor
        Mouse 7, 0, xOS + 8, GPDat(43) - 449 + xOS'Trap the mouse
        Mouse 8, 0, yOS + 2, GPDat(44) - 175 + yOS

        DO
          CALL GetCursor(x, y, Button)
          x = ((x + 4) \ 8) * 8
          x = x - xOS
          y = y - yOS

          IF x <> X1 OR y <> Y1 THEN
          HideCursor
          LineBVE X1, Y1, X1 + 440, Y1 + 167, 6152
          LineBVE x, y, x + 440, y + 167, 6152
          ShowCursor
          X1 = x
          Y1 = y
          END IF
        LOOP UNTIL Button = 0

          FreeMouse
          HideCursor
          LineBVE X1, Y1, X1 + 440, Y1 + 167, 6152
          GMove2VE LeftCol, TopLine, 57, 176, &HAA00, -1

          LeftCol = x \ 8 + 1
          TopLine = y

          GMove2VE LeftCol, TopLine, 57, 176, &HAA00, 0
          GMove4VE LeftCol, TopLine, 55, 167, VARSEG(PasteBuf(0)), -1


          LINE (LeftCol * 8, TopLine + 166)-(LeftCol * 8 + 436, TopLine + 172), 0, BF
          LINE (LeftCol * 8 + 432, TopLine + 8)-(LeftCol * 8 + 436, TopLine + 168), 0, BF

          ShowCursor

        END IF
     ELSE

    FOR Py = TopLine + 8 TO TopLine + 143 STEP 20
      IF y > Py AND y < Py + 15 THEN
            
        FOR Px = LeftCol * 8 + 4 TO LeftCol * 8 + 408 STEP 20'           108 TO 524 STEP 20             '112,528
          Col = Col + 1
          IF x > Px AND x < Px + 15 THEN
            Tile = Row * 21 + Col

            HideCursor
            LineBVE Px - 2, Py - 2, Px + 18, Py + 18, 15
            ShowCursor
            WaitUp
            WaitUpKey
            HideCursor
            GMove2VE LeftCol, TopLine, 57, 176, &HAA00, -1
            ShowCursor
            GPDat(7) = 33

            EXIT DO
          END IF
       NEXT
      END IF
      Row = Row + 1
    NEXT
  END IF
  END IF

LOOP

Tile$ = SPACE$(32)
SeekOS& = Tile * 32 - 32
CALL FOpen(TPath$ + BSlash$ + "TPal.Til", Handle)
CALL FSeek(Handle, SeekOS&)
CALL FGet(Handle, Tile$)
CALL FClose(Handle)
IF DOSError THEN
  DOSMsg
  EXIT SUB
END IF
SRScreen 0                 'Save the screen
GPDat(17) = SnapStatus
CALL Flood(Tile$)
SRScreen 2                 'Save the screen
LOOP
END SUB

SUB SmoothLine (OC)

GPDat(7) = 37                            'Current drawing choice
HideCursor                               'Turn off the cursor
Clr1 = 6151                              '7 + 24 * 256
NumPoints = 0
REDIM xy(100, 1)

GetCursor LastX, LastY, Button           'Get mouse coords

RestartSM:
CrossHair LastX, LastY                   'Draw the GCursor

DO
  CALL CursorXY(x, y, Button, Ky)        'Look for keystrokes, mouse clicks

  IF Ky = -68 THEN
    CrossHair x, y
    LastX = x
    LastY = y
    GOTO RestartSM
  END IF

  IF LastX <> x OR LastY <> y THEN       'Draw new cursor if it has moved
    CrossHair LastX, LastY
    CrossHair x, y
    LastX = x
    LastY = y
  END IF
                                            '3
  IF (Ky = 13 OR Button = 1) AND NumPoints > 1 THEN
    CrossHair x, y
    WaitUp
    IF NumPoints > 2 THEN
      NS = NumPoints * 10
      FOR i = 1 TO NumPoints - 2
        CALL DrawPointVEOpts(xy(i + 1, 0), xy(i + 1, 1), Clr1)
        CALL LineVE(xy(i, 0), xy(i, 1), xy(i + 1, 0), xy(i + 1, 1), Clr1)
      NEXT
      CALL LineVE(xy(i, 0), xy(i, 1), xy(i + 1, 0), xy(i + 1, 1), Clr1)

      IF OC THEN
        CALL OpenCurve(xy(), NumPoints, NS)
      ELSE
        CALL ClosedCurve(xy(), NumPoints, NS)
      END IF

    ELSE
      CALL LineVE(xy(1, 0), xy(1, 1), xy(2, 0), xy(2, 1), Clr1)

      END IF
      NumPoints = 1
      Button = 3
    END IF

LOOP UNTIL Button > 0 OR Ky = 13          'Wait for mouseclick or spacebar

IF Button = 3 THEN GOTO RestartSM'1


IF Button = 2 THEN                       'Right click,
  CrossHair x, y                         ' delete GCursor
  IF NumPoints > 1 THEN
    FOR i = 1 TO NumPoints - 2
      CALL DrawPointVEOpts(xy(i + 1, 0), xy(i + 1, 1), Clr1)
      CALL LineVE(xy(i, 0), xy(i, 1), xy(i + 1, 0), xy(i + 1, 1), Clr1)
    NEXT
    CALL LineVE(xy(i, 0), xy(i, 1), xy(i + 1, 0), xy(i + 1, 1), Clr1)

    'NumPoints = 0
  END IF

  WaitUp
  WaitUpKey
  IF NumPoints = 0 THEN
    REDIM xy(0, 0)
    ShowCursor                             'Turn on Cursor
    EXIT SUB                               'Bag out
  ELSE
    NumPoints = 0
    GOTO RestartSM
  END IF

END IF

startx = x
starty = y
X1 = startx
Y1 = starty

NumPoints = 1
xy(NumPoints, 0) = x
xy(NumPoints, 1) = y

'HideCursor                         'Start line
CrossHair X1, Y1                            'draw new GCursor
CALL LineVE(x, y, X1, Y1, Clr1)
CrossHair X1, Y1                            'draw new GCursor

'ShowCursor

DO
  WaitUp
  WaitUpKey
  DO
    CALL CursorXY(x, y, Button, Ky)     'Look for mouse click or keypress

    IF Ky = -68 THEN
      CrossHair x, y
      LastX = x
      LastY = y
      NumPoints = 0
      GOTO RestartSM
    END IF

    IF x <> X1 OR y <> Y1 THEN          'If we've moved,
      CrossHair X1, Y1                  ' turn off GCursor
      CALL LineVE(startx, starty, x, y, Clr1)   'delete old line
      CALL LineVE(startx, starty, X1, Y1, Clr1) 'draw new line
      CrossHair x, y                            'draw new GCursor
      X1 = x                                    'update last coords
      Y1 = y
    END IF

  LOOP UNTIL Button > 0 OR Ky = 13

      CrossHair X1, Y1                            'draw new GCursor
  CALL LineVE(startx, starty, X1, Y1, Clr1)
      CrossHair X1, Y1                            'draw new GCursor




  IF NumPoints = 100 THEN
    BEEP
    Button = 2
  END IF

  IF Button = 2 THEN
    CrossHair x, y
    IF NumPoints > 1 THEN CALL DrawPointVEOpts(startx, starty, Clr1)
    WaitUp
    LastX = x
    LastY = y
    GOTO RestartSM'1
  ELSEIF Button = 1 OR Ky = 13 THEN
    NumPoints = NumPoints + 1
    xy(NumPoints, 0) = x
    xy(NumPoints, 1) = y
 '   Started = -1
    CrossHair x, y
    CALL LineVE(startx, starty, X1, Y1, Clr1)
    CrossHair x, y
    GPDat(83) = -1                 'Set drawing changed flag
  END IF
    startx = X1
    starty = Y1
'  WaitUpKey
LOOP

END SUB

SUB VFlip
  MouseBox X1, Y1, X2, Y2, 0, Button, 14 + 24 * 256
  IF Button = 2 THEN EXIT SUB

  IF X1 < 0 THEN X1 = 0
  IF Y1 < 0 THEN Y1 = 0
  IF X2 > GPDat(43) - 1 THEN X2 = GPDat(43) - 1
  IF Y2 > GPDat(44) - 1 THEN Y2 = GPDat(44) - 1

  T = GArraySize&(X1, Y1, X2, Y1) \ 2
  REDIM Top(T)
  REDIM Bot(T)

  HideCursor
  FOR i = 0 TO (Y2 - Y1) \ 2
    GET (X1, Y1 + i)-(X2, Y1 + i), Top
    GET (X1, Y2 - i)-(X2, Y2 - i), Bot
    PUT (X1, Y1 + i), Bot, PSET
    PUT (X1, Y2 - i), Top, PSET
  NEXT
  ShowCursor
  GPDat(83) = -1                 'Set drawing changed flag

END SUB

SUB WaitUpKey

  DO
    WHILE INKEY$ <> "": WEND
    x = KeyDown%
  LOOP UNTIL x = 0


END SUB

