DEFINT A-Z

'by Phil Cramer
'Copyright (c) 1992 Crescent Software

'---- BASIC subs
DECLARE SUB Box (X1%, Y1%, X2%, Y2%, Clr%)
DECLARE SUB CheckMem (X, Amt&)
DECLARE SUB CrossHair (X, Y)
DECLARE SUB CursorXY (X%, Y%, Button%, Ky%)
DECLARE SUB DefCursor (Cursor%)
DECLARE SUB DownLeft (X1, Y1, X2, Y2)
DECLARE SUB DownRight (X1, Y1, X2, Y2)
DECLARE SUB FreeMouse ()
DECLARE SUB GetSides (Angle!, Hypotenuse!, Adjacent!, Opposite!)
DECLARE SUB GetXORBox (X1, Y1, X2, Y2, Corner, A, B, Paste, PasteBuf(), AndArray())
DECLARE SUB MakeUniqueFldName (FldNo%, Form() AS ANY)
DECLARE SUB MouseBox (X1, Y1, lastx2, lasty2, LeaveIt, Button, Clr)
DECLARE SUB PCBox (X1%, Y1%, X2%, Y2%, Outside%)
DECLARE SUB PixelEdit (X, Y, X1, Y1, boxsize, Grid1, ExitCode)
DECLARE SUB Polygon (XCenter%, YCenter%, NumSides%, Radius%, StartAngle%, Sides2Disp%, DrawClr%)
DECLARE SUB ReColor (X1, Y1, X2, Y2, OldClr, NewClr)
DECLARE SUB SRScreen (SaveRest)
DECLARE SUB UpLeft (X1, Y1, X2, Y2)
DECLARE SUB UpRight (X1, Y1, X2, Y2)
DECLARE SUB WaitUpKey ()

'---- BASIC functions
DECLARE FUNCTION GArraySize& (X1, Y1, X2, Y2)
DECLARE FUNCTION MsgBox (A$(), Opts)

'---- ASM subs
DECLARE SUB DrawPointVE (BYVAL X%, BYVAL Y%, BYVAL PointColor%)
DECLARE SUB GetCursor (X, Y, Button)
DECLARE SUB GMove2VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GPrint3VE (BYVAL Line1%, BYVAL Col1%, Text$, BYVAL TextColor%)
DECLARE SUB HideCursor ()
DECLARE SUB LineBVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB LineVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB PaintBits (BYVAL Col1%, BYVAL Line1%, BYVAL Mask%, BYVAL OldColor%, BYVAL NewColor%)
DECLARE SUB PaintByte (BYVAL Col1%, BYVAL Line1%, BYVAL OldColor%, BYVAL NewColor%)
DECLARE SUB ShowCursor ()
DECLARE SUB StuffBuf (Char$)
DECLARE SUB WaitUp ()

'---- ASM functions
DECLARE FUNCTION Bin2Num% (Bin$)
DECLARE FUNCTION GetPointVE% (BYVAL X%, BYVAL Y%)
DECLARE FUNCTION MaxInt% (A, B)
DECLARE FUNCTION MinInt% (A, B)

TYPE EditMouse
  X1 AS INTEGER
  X2 AS INTEGER
  Y1 AS INTEGER
  Y2 AS INTEGER
END TYPE

TYPE Coord
  Mx1 AS INTEGER
  Mx2 AS INTEGER
  My1 AS INTEGER
  My2 AS INTEGER
END TYPE

TYPE ScreenObject
  EType AS INTEGER
  EX1 AS INTEGER                'Co-ords for entity field
  EY1 AS INTEGER
  EX2 AS INTEGER
  EY2 AS INTEGER
  Clr AS INTEGER
  Status AS INTEGER
END TYPE

'$INCLUDE: 'Common.GQS'
'$INCLUDE: 'FldInfo.bi'

CONST pi! = 3.141

SUB ColorEdit (X, Y, BWidth, BHite, Clr)

FX = X
FY = Y
BYCenter = BHite \ 2
BxCenter = BWidth \ 2
PalHite = 2 * BHite
PalWidth = 8 * BWidth
TxtHeight = GPDat(71)

IF GPDat(71) = 8 THEN HtOS = 1'4

REDIM Area(0 TO 15) AS ScreenObject
REDIM PColor(2)
DO
  SnapStatus = GPDat(17)
  GOSUB DrawPalette
  GPDat(17) = 0        'Turn snap off

  DO
    CALL CursorXY(X1, Y1, Button, Ky)

    IF Button = 1 OR Ky = 13 THEN
      FOR i = 0 TO 15
        IF X1 > Area(i).EX1 AND X1 < Area(i).EX2 AND Y1 > Area(i).EY1 AND Y1 < Area(i).EY2 THEN
          Cnt = Cnt + 1
          HideCursor
          LINE (X + 8 * BWidth - 1, Y + PalHite + 7)-STEP(-2 * BWidth + 2, TxtHeight - 2), i, BF
          ShowCursor
          PColor(Cnt) = i
          WaitUp
          WaitUpKey

          Msg$ = "Now pick the new color      "
          HideCursor
          LINE (X + 2, Y + PalHite + 7)-STEP(197, TxtHeight - 2), 7, BF
          CALL GPrint3VE(Y + PalHite + 6 + HtOS, X \ 8 + 2, Msg$, 0)
          ShowCursor
          EXIT FOR
        END IF
      NEXT
    ELSEIF Button = 2 THEN
      WaitUp
      HideCursor
      CALL GMove2VE(X \ 8 - 1, Y - 10, BWidth + 5, PalHite + TxtHeight + 30, &HAA00, -1)
      ShowCursor
      'TxtHeight = SnapStatus
      GPDat(17) = SnapStatus
      EXIT SUB
    END IF

  LOOP UNTIL Cnt = 2

  HideCursor
  CALL GMove2VE(X \ 8 - 1, Y - 10, BWidth + 5, PalHite + TxtHeight + 30, &HAA00, -1)
  ShowCursor
  GPDat(17) = SnapStatus
  MBClr = 14 + 24 * 256
  TempClr = GPDat(82)
  GPDat(82) = 15 + 24 * 256

  SRScreen 0

  DO
    LeaveIt = 0
    CALL MouseBox(X1, Y1, X2, Y2, LeaveIt, Button, MBClr)
    WaitUp
    IF Button = 2 THEN EXIT DO

    CALL ReColor(X1, Y1, X2, Y2, PColor(1), PColor(2))
    GPDat(83) = -1                                       'Set drawing changed flag
  LOOP

  GPDat(82) = TempClr

LOOP


DrawPalette:

  SRScreen 2
  X = FX
  Y = FY
  Cnt = 0

  HideCursor
  CALL GMove2VE(X \ 8 - 1, Y - 10, BWidth + 5, PalHite + TxtHeight + 30, &HAA00, 0)

  LINE (X, Y + 4)-STEP(PalWidth + 12, PalHite + TxtHeight + 12), 0, BF 'Painted BGround
  LINE (X - 6, Y - 6)-STEP(PalWidth + 12, PalHite + TxtHeight + 16), 7, BF'Painted BGround

  CALL PCBox(X - 6, Y - 6, X + PalWidth + 6, Y + PalHite + TxtHeight + 11, -1)

  '---- Paint Color
  LINE (X + 8 * BWidth - 1, Y + PalHite + 7)-STEP(-2 * BWidth + 2, TxtHeight - 2), GPDat(82), BF
  CALL PCBox(X + PalWidth - 2 * BWidth, Y + PalHite + 6, X + 8 * BWidth + 1, Y + PalHite + TxtHeight + 6, 0)

  FOR Y1 = 0 TO BHite STEP BHite
    FOR X1 = 0 TO PalWidth - BWidth STEP BWidth        'Draw the Palette
      LINE (X + X1, Y + Y1)-STEP(BWidth, BHite), Count, BF
      Area(Count).EX1 = X + X1
      Area(Count).EX2 = Area(Count).EX1 + BWidth
      Area(Count).EY1 = Y + Y1
      Area(Count).EY2 = Area(Count).EY1 + BHite
      Count = Count + 1
    NEXT
  NEXT

  FOR X1 = X TO X + PalWidth - BWidth STEP BWidth      'Draw the Palette
    LINE (X1, Y)-STEP(0, PalHite), 0
  NEXT

  LineVE X, Y + BHite, X + PalWidth, Y + BHite, 0
  CALL PCBox(X - 1, Y - 1, X + PalWidth + 2, Y + PalHite + 2 + (GPDat(31) = 5), 0)
  LineBVE X, Y, X + PalWidth, Y + PalHite, 0

  Count = 0

  Msg$ = "Pick the color to change"
  CALL GPrint3VE(Y + PalHite + 6 + HtOS, X \ 8 + 2, Msg$, 0)
  CALL PCBox(X, Y + PalHite + 6, X + 200, Y + PalHite + 6 + TxtHeight, 0)
  ShowCursor

RETURN


END SUB

SUB DownLeft (X1, Y1, X2, Y2)

  'Assume x1 > x2 and y1 < y2  Moving down and to the left
  BSize = GPDat(8) * 2
  HBSize = GPDat(8)

  W = (X1 - X2) + 1                 'Get width of triangle
  H = (Y2 - Y1) + 1                 '    height

  IF H > W THEN
    Ratio! = H / W
    Rat = Ratio!
    Y = Y1                          'Start at y1
    Wide = 0
    Ht = MaxInt(Rat, BSize)
    FOR X = X1 TO X2 STEP -1
      LINE (X + HBSize, Y - HBSize)-STEP(-BSize, Ht), GPDat(82), BF
      Wide = Wide + 1
      NewHt = Wide * Ratio!
      Y = Y1 + NewHt
    NEXT
  ELSE
    Ratio! = W / H
    Rat = Ratio!
    Hite = 0
    X = X1
    Wd = MaxInt(Rat, BSize)
    FOR Y = Y1 TO Y2
      LINE (X + HBSize, Y + HBSize)-STEP(-Wd, -BSize), GPDat(82), BF
      Hite = Hite + 1
      NewWidth = Hite * Ratio!
      X = X1 - NewWidth
    NEXT
  END IF
  GPDat(83) = -1              'Drawing changed flag

END SUB

SUB DownRight (X1, Y1, X2, Y2)

  'Assume x1 < x2 and y1 < y2  Moving up and to the right

  BSize = GPDat(8) * 2
  HBSize = GPDat(8)

  W = (X2 - X1) + 1                 'Get width of triangle
  H = (Y2 - Y1) + 1                 '    height

  IF H > W THEN
    Ratio! = H / W
    Rat = Ratio!
    Y = Y1                             'Start at y1
    Wide = 0
    Ht = MaxInt(Rat, BSize)
    FOR X = X1 TO X2
      LINE (X - HBSize, Y - HBSize)-STEP(BSize, Ht), GPDat(82), BF
      Wide = Wide + 1
      NewHt = Wide * Ratio!
      Y = Y1 + NewHt
    NEXT
  ELSE
    Ratio! = W / H
    Rat = Ratio!
    Hite = 0
    X = X1
    Wd = MaxInt(Rat, BSize)
    FOR Y = Y1 TO Y2
      LINE (X - HBSize, Y + HBSize)-STEP(Wd, -BSize), GPDat(82), BF
      Hite = Hite + 1
      NewWidth = Hite * Ratio!
      X = X1 + NewWidth
    NEXT
  END IF
  GPDat(83) = -1              'Drawing changed flag

END SUB

SUB EngravedBox (X1, Y1, X2, Y2)

   '---- Draw hilite portion
   FOR i = 0 TO BevelWidth - 1
     LINE (X1 - i, Y1 - i)-(X2 + i + BevelWidth, Y2 + i + BevelWidth), BevelHi, B
   NEXT

   '---- Shade the sides
   LINE (X1 - 2 * BevelWidth + 1, Y1 - BevelWidth)-(X2 + 2 * BevelWidth - 1, Y1 - 2 * BevelWidth + 1), BevelLo, BF'Top
   LINE (X1 - 2 * BevelWidth + 1, Y1 - BevelWidth + 1)-(X1 - BevelWidth, Y2 + 2 * BevelWidth - 1), BevelLo, BF'Left
   LINE (X2, Y1 + 1)-(X2 + BevelWidth - 1, Y2 + BevelWidth - 1), BevelLo, BF'Right
   LINE (X1 + 1, Y2)-(X2, Y2 + BevelWidth - 1), BevelLo, BF'Bottom

END SUB

SUB MoveField (Fld() AS FieldInfoG, Copy, PasteBuf(), AndArray(), Calc$(), Help$()) STATIC

  CM = GPDat(20)
  XR = GPDat(66)
  GPDat(66) = 0
  TxtFld = 0
  N = 1
  CurFld = 0
  IF Copy THEN
    GPDat(20) = -1
    Paste = 1
  ELSE
    GPDat(20) = 0
    Paste = 0
  END IF
  SnapStatus = GPDat(17)

  Clr = 14 + 24 * 256
  Corner = 0
  CALL MouseBox(X1, Y1, X2, Y2, Corner, Button, Clr)

  IF Button = 1 THEN

    Pressed = -1

    '---- See what fields they grabbed
    LC = X1 \ 8 + 1
    RC = X2 \ 8 + 1
    TRow = Y1 \ GPDat(71) + 1
    BRow = Y2 \ GPDat(71)

    lastx = X1
    lasty = Y1

    REDIM Move(1 TO Fld(0).Fields)
    FOR i = 1 TO Fld(0).Fields

      SELECT CASE Fld(i).FType
        CASE NotesFld
          IF Fld(i).LCol >= LC AND Fld(i).RCol <= RC AND Fld(i).Row >= TRow AND Fld(i).ScratchI <= BRow THEN
            Move(N) = i
            N = N + 1
            TxtFld = -1
          END IF

        CASE StrFld TO MultChAFld

          IF Fld(i).LCol >= LC AND Fld(i).RCol <= RC AND Fld(i).Row >= TRow AND Fld(i).Row <= BRow THEN
            Move(N) = i
            N = N + 1
            TxtFld = -1
          END IF

        CASE ELSE
          IF Fld(i).LCol >= X1 AND Fld(i).RCol <= X2 AND Fld(i).Row >= Y1 AND Fld(i).ScratchI <= Y2 THEN
            Move(N) = i
            N = N + 1
          END IF
      END SELECT

    NEXT

    IF TxtFld THEN
      GPDat(17) = -1      'turn snap on
      GPDat(7) = 50       'disable snap on/off
      A = GPDat(1)        'remember last snap
      B = GPDat(2)
      C = GPDat(6)
      D = GPDat(10)
      GPDat(1) = 8        'set text snap coords
      GPDat(2) = 4
      GPDat(6) = GPDat(71)
      GPDat(10) = GPDat(71) \ 2

      Mx1 = ((X1 + GPDat(2)) \ GPDat(1)) * GPDat(1)    'GPDat(1) = XSnap Spacing
      My1 = ((Y1 + GPDat(10)) \ GPDat(6)) * GPDat(6)   'GPDat(6) = YSnap Spacing
      Mx2 = ((X2 + GPDat(2)) \ GPDat(1)) * GPDat(1)    'GPDat(1) = XSnap Spacing
      My2 = ((Y2 + GPDat(10)) \ GPDat(6)) * GPDat(6)   'GPDat(6) = YSnap Spacing

      SELECT CASE Corner
        CASE 0 'LR
          xOffset = Mx2 - X2
          yOffset = My2 - Y2

        CASE 1 'LL
          xOffset = X1 - Mx1
          yOffset = My2 - Y2

        CASE 2 'UR
          xOffset = Mx2 - X2
          yOffset = Y1 - My1

        CASE 3 'UL
          xOffset = X1 - Mx1
          yOffset = Y1 - My1

      END SELECT

    END IF

    GPDat(7) = 35
    WinImage = 0

    DO

      CALL GetXORBox(X1, Y1, X2, Y2, Corner, xOffset, yOffset, Paste, PasteBuf(), AndArray())

      IF Paste = 5 THEN EXIT DO      'Finished making copies

      xMove = lastx - X1
      yMove = lasty - Y1

      IF TxtFld THEN
        Cols = xMove \ 8
        Rows = yMove \ GPDat(71)
      END IF

      X& = FRE("")

      FOR i = 1 TO N - 1

        IF CurFld + 1 > MaxFields THEN
          BEEP
          StuffBuf CHR$(27)

          CALL GetXORBox(X1, Y1, X2, Y2, Corner, xOffset, yOffset, Paste, PasteBuf(), AndArray())
          FreeMouse
          REDIM Msg$(1)
          Msg$(0) = "End of Form"
          Msg$(1) = "Last available field has been used!"
          Button = MsgBox(Msg$(), 21)
          EXIT SUB
        END IF

        IF Copy THEN
          CurFld = MinInt%(Fld(0).Fields + 1, MaxFields)
        ELSE
          CurFld = Move(i)
        END IF

        IF Copy THEN
          IF FRE("") < 4000 THEN
            Chk = 0
            CheckMem Chk, 3000
            IF Chk THEN EXIT SUB
          END IF
          Fld(CurFld) = Fld(Move(i))
          Calc$(CurFld) = Calc$(Move(i))
          Help$(CurFld) = Help$(Move(i))
        END IF

        SELECT CASE Fld(Move(i)).FType
          CASE NotesFld
            XVal = Cols
            YVal = Rows
            Fld(CurFld).ScratchI = Fld(Move(i)).ScratchI - YVal

          CASE StrFld TO MultChAFld
            XVal = Cols
            YVal = Rows

          CASE ELSE
            XVal = xMove
            YVal = yMove
            Fld(CurFld).ScratchI = Fld(Move(i)).ScratchI - YVal
            IF Fld(CurFld).FType = HScrollFld THEN Fld(CurFld).RelHandle = Fld(Move(i)).RelHandle - XVal
            IF Fld(CurFld).FType = VScrollFld THEN Fld(CurFld).RelHandle = Fld(Move(i)).RelHandle - YVal

        END SELECT

        MakeUniqueFldName CurFld, Fld()

        Fld(CurFld).Row = Fld(Move(i)).Row - YVal
        Fld(CurFld).LCol = Fld(Move(i)).LCol - XVal
        Fld(CurFld).RCol = Fld(Move(i)).RCol - XVal

        IF Copy THEN
          Fld(0).Fields = MinInt%(Fld(0).Fields + 1, MaxFields)
          Fld(0).StorLen = Fld(0).StorLen + Fld(CurFld).StorLen
        END IF

      NEXT

    LOOP WHILE GPDat(20)

  END IF

  REDIM PasteBuf(0)
  
  GPDat(86) = 0
  GPDat(20) = CM
  GPDat(66) = XR
  GPDat(17) = SnapStatus

  IF TxtFld THEN
    GPDat(1) = A
    GPDat(2) = B
    GPDat(6) = C
    GPDat(10) = D
    GPDat(7) = 0
  END IF

END SUB

SUB PaintBrush (X, Y)
HideCursor

LastCursor = GPDat(18)
GPDat(18) = 4
GetCursor lastx, lasty, Button

BrushSize = GPDat(8) * 2
HalfBrush = GPDat(8)

DO
CrossHair lastx, lasty
Kbd = 0
DO
  CALL CursorXY(X, Y, Button, Ky)        'Look for keystrokes, mouse clicks
  GOSUB Checkxy

  '  IF Ky = -68 THEN
  '    CrossHair x1, y1
  '    lastcx = x1
  '    lastcy = y1
  '    GOTO RestartBox
  '  END IF

  IF lastx <> X OR lasty <> Y THEN       'Draw new cursor if it has moved
    CrossHair lastx, lasty
    CrossHair X, Y
    lastx = X
    lasty = Y
  END IF
LOOP UNTIL Button > 0 OR Ky = 13         'Wait for mouseclick or spacebar

WaitUpKey

IF Ky = 13 THEN Kbd = -1

IF Button = 2 OR Ky = 27 THEN
  CrossHair X, Y
  GPDat(18) = LastCursor
  ShowCursor
  EXIT SUB

END IF

lastx = X
lasty = Y

DO

  CALL CursorXY(X, Y, Button, Ky)        'Look for keystrokes, mouse clicks
  GOSUB Checkxy

  IF Button > 0 THEN Kbd = 0
  IF lastx <> X OR lasty <> Y THEN       'Draw new cursor if it has moved
    CrossHair lastx, lasty

    IF X >= lastx THEN
      IF Y <= lasty THEN   'UpRight
        CALL UpRight(lastx, lasty, X, Y)
      ELSE
        CALL DownRight(lastx, lasty, X, Y)
      END IF
    ELSEIF X <= lastx THEN
      IF Y <= lasty THEN
        CALL UpLeft(lastx, lasty, X, Y)
      ELSE
        CALL DownLeft(lastx, lasty, X, Y)
      END IF
    END IF

    lastx = X
    lasty = Y
    CrossHair X, Y
  END IF

  LINE (X - GPDat(8), Y - GPDat(8))-STEP(GPDat(8) * 2, GPDat(8) * 2), GPDat(82), BF

LOOP UNTIL (Button = 0 AND NOT Kbd) OR Ky = 13 OR Ky = 27
WaitUpKey

GPDat(83) = -1                 'Set drawing changed flag
CrossHair X, Y                 'Turn cursor off
LOOP

GPDat(18) = LastCursor
ShowCursor

Checkxy:

  IF X < GPDat(8) THEN X = GPDat(8)
  IF X > (GPDat(43) - 1) - GPDat(8) THEN X = (GPDat(43) - 1) - GPDat(8)
  IF Y < GPDat(8) THEN Y = GPDat(8)
  IF Y > (GPDat(44) - 1) - GPDat(8) THEN Y = (GPDat(44) - 1) - GPDat(8)
RETURN




END SUB

SUB PCBoxes (bx1, by1, bx2, by2)

  '---- Draws a "3D" frame

  X1 = bx1
  Y1 = by1
  X2 = bx2
  Y2 = by2

'  SELECT CASE GPDat(57)      'GPDat 57 = Background color
'    CASE 0
'      HiClr = 7
'      ShadowClr = 8
'    CASE 1 TO 7
'      HiClr = GPDat(57) + 8
'      ShadowClr = 8
'    CASE 8
'      HiClr = 7
'      ShadowClr = 0
'    CASE 9 TO 14
'      HiClr = 15
'      ShadowClr = GPDat(57) - 8
'    CASE 15
'      HiClr = 15
'      ShadowClr = 7
'  END SELECT
'
  HiClr = BevelHi
  LoClr = BevelLo
  IF Drop THEN SWAP HiClr, LoClr

  FOR i = 1 TO BevelWidth
    LINE (X1, Y1)-(X2, Y1), HiClr
    LINE (X1, Y1)-(X1, Y2), HiClr
    LINE (X2, Y1)-(X2, Y2), LoClr
    LINE (X1, Y2)-(X2, Y2), LoClr
    X1 = X1 - 1
    X2 = X2 + 1
    Y1 = Y1 - 1
    Y2 = Y2 + 1
  NEXT

END SUB

SUB PixelEdit (X, Y, X1, Y1, boxsize, Grid1, ExitCode)

IF X = X1 OR Y = Y1 THEN
  BEEP
  ExitCode = 0
  EXIT SUB
END IF

SnapOn = GPDat(17)
IF SnapOn THEN GPDat(17) = 0        'Turn Snap off

DefCursor 1

EClr = GPDat(82)

wx1 = X - 1      'coords of the selected window
wx2 = X1 + 1
wy1 = Y - 1
wy2 = Y1 + 1


GridClr = 8
Count = 1
NumXBoxes = X1 - X
NumYBoxes = Y1 - Y
Boxx& = NumXBoxes
Boxy& = NumYBoxes


NumBoxes& = Boxx& * Boxy& + 16

gridhite = boxsize * NumYBoxes
gridwidth = boxsize * NumXBoxes

IF NumBoxes& > 3000 THEN
  BEEP
  DefCursor 2
  ExitCode = 0
  EXIT SUB
END IF

ExitCode = -1                             'GPDat(11) = xScreen center
                                          'GPDat(12) = yScreen center
IF gridwidth > gridhite THEN              'If its wider than it is high,
  IF Y < GPDat(12) THEN                   ' and its above vertical center
    yCorner = Y + Y1 - Y + GPDat(71)      ' place grid just below window
  ELSE                                    'If its below vertical center
    yCorner = Y - gridhite - GPDat(71)    ' place it just above the window
  END IF
  xcorner = GPDat(11) - gridwidth \ 2     'Center the grid horizontally
  IF xcorner < 1 THEN xcorner = 1
ELSE                                      'Must be higher than wide
  IF X < GPDat(11) THEN                   ' If its left of horizontal center
    xcorner = X1 + GPDat(71)              ' place grid just to the right
  ELSE                                    'If its to the right of horiz center
    xcorner = X - gridwidth - GPDat(71)   ' place grid just to the left
  END IF
  yCorner = GPDat(12) - gridhite \ 2      'Center the grid vertically
  IF yCorner < 1 THEN yCorner = 1

END IF

                                          'Make sure we only save valid coords
SaveWidth = gridwidth \ 8 + 2             'See how many text cols wide to save
savehite = gridhite + 1                   'See how many lines to save

                                          'Save only to column 80
IF SaveWidth + xcorner \ 8 > 80 THEN SaveWidth = 80 - xcorner \ 8 + 1
IF savehite + yCorner > GPDat(44) THEN savehite = GPDat(44) - yCorner + 1

IF yCorner < 0 THEN
  saveyCorner = 0
ELSE
  saveyCorner = yCorner
END IF




HideCursor
CALL GMove2VE(xcorner \ 8, saveyCorner, SaveWidth, savehite, &HAA00, 0)
REDIM PxlArray(GArraySize&(wx1 + 1, wy1 + 1, wx2 - 1, wy2 - 1) \ 2)
GET (wx1 + 1, wy1 + 1)-(wx2 - 1, wy2 - 1), PxlArray'Save zoomed image

LineBVE wx1, wy1, wx2, wy2, 15 + 24 * 256

REDIM MCoords(NumXBoxes + NumYBoxes + 1) AS Coord

FOR mousex = xcorner TO xcorner + gridwidth - 1 STEP boxsize
  MCount = MCount + 1
  MCoords(MCount).Mx1 = mousex
  MCoords(MCount).Mx2 = mousex + boxsize - 1
NEXT

FOR mousey = yCorner TO yCorner + gridhite - 1 STEP boxsize
  MCount = MCount + 1
  MCoords(MCount).My1 = mousey
  MCoords(MCount).My2 = mousey + boxsize - 1
NEXT

ReDraw:
IF GPDat(13) THEN                              'Draw the grid

  LINE (xcorner, yCorner)-STEP(gridwidth, gridhite), GridClr, B

  FOR i = boxsize + yCorner TO gridhite - 1 + yCorner STEP boxsize   'Horiz
    LINE (xcorner, i)-STEP(gridwidth, 0), GridClr
  NEXT

  FOR i = boxsize + xcorner TO gridwidth + xcorner STEP boxsize    'Vert
    LINE (i, yCorner)-STEP(0, gridhite), GridClr
  NEXT
  N = 1

END IF

FOR Y1 = 0 TO NumYBoxes - 1       'Save mouse coordinates and draw pixels
  FOR X1 = 0 TO NumXBoxes - 1
    PxlClr = GetPointVE(X + X1, Y1 + Y)
    LINE (X1 * boxsize + xcorner + N, Y1 * boxsize + yCorner + N)-STEP(boxsize - 2 * N, boxsize - 2 * N), PxlClr, BF
    Count = Count + 1
  NEXT
NEXT

CALL ShowCursor

DO                                 'Edit the pixel grid
  CALL CursorXY(GCx, GCy, Button, Ky)

  IF Ky = -68 THEN
    HideCursor
    PUT (wx1 + 1, wy1 + 1), PxlArray, PSET
    GOTO ReDraw
  END IF

  IF Button = 1 OR Ky = 13 THEN
    FOR MouseCol = 1 TO NumXBoxes
      IF GCx > MCoords(MouseCol).Mx1 - 1 AND GCx < MCoords(MouseCol).Mx2 + 1 THEN
        FOR MouseRow = NumXBoxes + 1 TO NumXBoxes + NumYBoxes
          IF GCy > MCoords(MouseRow).My1 - 1 AND GCy < MCoords(MouseRow).My2 + 1 THEN
            HideCursor
            LINE (MCoords(MouseCol).Mx1 + N, MCoords(MouseRow).My1 + N)-STEP((boxsize - 1) - 1 * N, (boxsize - 1) - 1 * N), EClr, BF
            CALL DrawPointVE(X + MouseCol - 1, Y + MouseRow - NumXBoxes - 1, EClr)
            ShowCursor
            GPDat(83) = -1                  'Set drawing changed flag
            GOTO Outahere                   'found it, exit this mess
          END IF
        NEXT
      END IF
    NEXT
  END IF

Outahere:
   SELECT CASE Ky             'Look for color keys

     CASE 48 TO 57
       EClr = Ky - 48
     CASE 41                  'Shift 0
       EClr = 10
     CASE 33                  'Shift 1
       EClr = 11
     CASE 64                  'Shift 2
       EClr = 12
     CASE 35                  'Shift 3
       EClr = 13
     CASE 36                  'Shift 4
       EClr = 14
     CASE 37                  'Shift 5
       EClr = 15
     CASE ELSE

   END SELECT

LOOP UNTIL Button = 2 OR Ky = 27

HideCursor
LineBVE wx1, wy1, wx2, wy2, 15 + 24 * 256
CALL GMove2VE(xcorner \ 8, saveyCorner, SaveWidth, savehite, &HAA00, -1)
CALL ShowCursor
IF SnapOn THEN GPDat(17) = -1            'Turn snap back on
ERASE PxlArray

DefCursor 2

END SUB

SUB PutTile (X1, Y1, X2, Y2, Array())

  Wide = X2 - X1 + 1
  Hi = Y2 - Y1 + 1

  IWidth = Array(0)
  IHeight = Array(1)

  IF IHeight > Hi OR IWidth > Wide THEN
    BEEP
    EXIT SUB
  END IF

  HCopies = Wide \ IWidth
  VCopies = Hi \ IHeight

  VRemainder = Hi - (VCopies * IHeight)
  HRemainder = Wide - (HCopies * IWidth)

  X3 = X1
  Y3 = Y1
  FOR X = 1 TO HCopies
    FOR Y = 1 TO VCopies
      PUT (X3, Y3), Array, PSET
      Y3 = Y3 + IHeight
    NEXT

    IF VRemainder THEN
      Array(1) = VRemainder
      PUT (X3, Y3), Array, PSET
      Array(1) = IHeight
    END IF

    Y3 = Y1
    X3 = X3 + IWidth
  NEXT

  IF HRemainder THEN
    REDIM Array2(GArraySize&(X1, Y1, X1 + HRemainder - 1, Y1 + IHeight - 1) \ 2)
    GET (X1, Y1)-(X1 + HRemainder - 1, Y1 + IHeight - 1), Array2
    Y3 = Y1
    X3 = X1 + HCopies * IWidth

    FOR Y = 1 TO VCopies
      PUT (X3, Y3), Array2, PSET
      Y3 = Y3 + IHeight
    NEXT
  END IF


  IF HRemainder * VRemainder THEN
    Array2(1) = VRemainder
    PUT (X3, Y3), Array2, PSET
  END IF

END SUB

SUB ReColor (X1, Y1, X2, Y2, OldClr, NewClr)

  Mask$ = "11111111"
  LBin$ = "00000000"                         'Strings to hold byte mask
  RBin$ = "00000000"

  Cols = (X2 \ 8 + 1) - (X1 \ 8 + 1) + 1
  LCol = (X1 + 8) \ 8 + 1                    'Determine start byte column
  RCol = X2 \ 8                              'Determine right end byte column

  LeftOver = (LCol * 8 - 8) - X1             'Determine left bits
  IF LeftOver THEN MID$(LBin$, 8 - LeftOver + 1, MinInt%(LeftOver, (X2 - X1) + 1)) = Mask$

  IF Cols > 1 THEN
    RiteOver = X2 - (RCol * 8) + 1           'Determine right bits
    IF RiteOver THEN MID$(RBin$, 1, RiteOver) = Mask$
  END IF

  LMask = Bin2Num(LBin$)
  RMask = Bin2Num(RBin$)                     'Convert binary$ to decimal mask

  HideCursor

  SELECT CASE Cols                           'Paint the box

    CASE 1
      IF X2 - X1 = 7 THEN
        FOR Y = Y1 TO Y2
          CALL PaintByte(LCol - 1, Y, OldClr, NewClr)
        NEXT
      ELSE           'less than 8 pixels
        FOR Y = Y1 TO Y2
          PaintBits LCol - 1, Y, LMask, OldClr, NewClr
        NEXT
      END IF

     CASE 2
       FOR Y = Y1 TO Y2
         PaintBits LCol - 1, Y, LMask, OldClr, NewClr
         PaintBits RCol + 1, Y, RMask, OldClr, NewClr
       NEXT

    CASE ELSE
      IF RCol < LCol THEN RCol = LCol
      FOR Y = Y1 TO Y2
        IF LeftOver THEN PaintBits LCol - 1, Y, LMask, OldClr, NewClr
        FOR Col = LCol TO RCol
          CALL PaintByte(Col, Y, OldClr, NewClr)
        NEXT
        IF RiteOver THEN PaintBits RCol + 1, Y, RMask, OldClr, NewClr
      NEXT

  END SELECT

  ShowCursor

  Mask$ = ""
  LBin$ = ""
  RBin$ = ""

END SUB

SUB UpLeft (X1, Y1, X2, Y2)

  'Assume x1 > x2 and y1 > y2  Moving up and to the left

  BSize = GPDat(8) * 2
  HBSize = GPDat(8)

  W = (X1 - X2) + 1                 'Get width of triangle
  H = (Y1 - Y2) + 1                 '    height

  IF H > W THEN
    Ratio! = H / W
    Rat = Ratio!
    Y = Y1                          'Start at y1
    Wide = 0
    Ht = MaxInt(Rat, BSize)
    FOR X = X1 TO X2 STEP -1
      LINE (X - HBSize, Y + HBSize)-STEP(BSize, -Ht), GPDat(82), BF
      Wide = Wide + 1
      NewHt = Wide * Ratio!
      Y = Y1 - NewHt
    NEXT
  ELSE
    Ratio! = W / H
    Rat = Ratio!
    Hite = H
    X = X1
    Wd = MaxInt(Rat, BSize)
    FOR Y = Y1 TO Y2 STEP -1
      LINE (X + HBSize, Y + HBSize)-STEP(-Wd, -BSize), GPDat(82), BF
      Hite = Hite - 1
      NewWidth = Hite * Ratio!
      X = X2 + NewWidth
    NEXT
  END IF
  GPDat(83) = -1              'Drawing changed flag

END SUB

SUB UpRight (X1, Y1, X2, Y2)

  'Assume x1 < x2 and y1 > y2  Moving up and to the right

  BSize = GPDat(8) * 2
  HBSize = GPDat(8)

  W = (X2 - X1) + 1                 'Get width of triangle
  H = (Y1 - Y2) + 1                 '    height

  IF H > W THEN
    Ratio! = H / W
    Rat = Ratio!
    Y = Y1                          'Start at W
    Wide = W
    Ht = MaxInt(Rat, BSize)
    FOR X = X1 TO X2
      LINE (X - HBSize, Y + HBSize)-STEP(BSize, -Ht), GPDat(82), BF
      Wide = Wide - 1
      NewHt = Wide * Ratio!
      Y = Y2 + NewHt - 1
    NEXT
  ELSE
    Ratio! = W / H
    Rat = Ratio!
    Hite = H
    X = X1
    Wd = MaxInt(Rat, BSize)
    FOR Y = Y1 TO Y2 STEP -1
      LINE (X - HBSize, Y + HBSize)-STEP(Wd, -BSize), GPDat(82), BF
      Hite = Hite - 1
      NewWidth = Hite * Ratio!
      X = X2 - NewWidth
    NEXT
  END IF
  GPDat(83) = -1              'Drawing changed flag

END SUB

