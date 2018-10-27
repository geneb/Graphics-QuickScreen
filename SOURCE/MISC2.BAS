DEFINT A-Z
'by Phil Cramer
'Copyright (c) 1992 Crescent Software

'-----BASIC subs
DECLARE SUB CrossHair (x%, y%)
DECLARE SUB CursorXY (x%, y%, Button%, Ky%)
DECLARE SUB DotStepBox (x1, y1, x2, y2, Mask, Clr)
DECLARE SUB FreeMouse ()
DECLARE SUB Grid (x1, y, x2, y2, xspace, yspace, XHiSpace, YHiSpace, Clr, Clr1)
DECLARE SUB MouseXY ()
DECLARE SUB ParabPt (XY(), TP, T#, x, y)
DECLARE SUB PCBox (x1, y1, x2, y2, Outside)
DECLARE SUB RoundBox (x, y, x1, y1, Clr)
DECLARE SUB SRScreen (SaveRest%)
DECLARE SUB StatBox (A)
DECLARE SUB StepBox (x, y, Wide, Hite, Clr)

'----Basic functions
DECLARE FUNCTION Eval# (E$, ErrCode)
DECLARE FUNCTION GArraySize& (x1, y1, x2, y2)
DECLARE FUNCTION GQSPath$ ()
DECLARE FUNCTION MsgBox (A$(), Opt)
DECLARE FUNCTION NoExtension$ (F$)

'----ASM subs
DECLARE SUB ButtonPress (Button, Stat, Cnt, x, y)
DECLARE SUB ClearVE
DECLARE SUB DrawPointVEOpts (BYVAL x%, BYVAL y%, BYVAL PointColor%)
DECLARE SUB HideCursor ()
DECLARE SUB GetCursor (x%, y%, Button%)
DECLARE SUB GMove2VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GMove4VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GPrint2VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint3VE (BYVAL Line1%, BYVAL Col1%, Text$, BYVAL TextColor%)
DECLARE SUB GrafCursor (x, y, Arrow$)
DECLARE SUB InitMouse (There)
DECLARE SUB LineVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB LineBVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB LineBFVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB Mouse (ax, bx, cx, dx)
DECLARE SUB ShowCursor ()

'---- ASM functions
DECLARE FUNCTION ExeName$ ()
DECLARE FUNCTION PDQTimer& ()
DECLARE FUNCTION QInstrB2% (Start, Source$, Search$)


DIM SHARED LastTime

'$INCLUDE: 'Common.GQS'

CONST Pi! = 3.141

SUB CheckMem (F, Amt&)

  'F = 1 checks far memory
  'F = 0 checks string space

  IF F THEN
    F = 0
    IF FRE(-1) < Amt& THEN F = -1
  ELSE
    IF FRE("") < Amt& THEN F = -1
  END IF

  IF F THEN
    REDIM Msg$(1)
    Msg$(0) = "WARNING!"
    Msg$(1) = "Out of Memory"
    Btn = MsgBox(Msg$(), 11)
    ERASE Msg$
  END IF

END SUB

SUB ClosedCurve (XY(), NP, NS) STATIC

    '******* Closed curve
    REDIM x(1 TO 2), y(1 TO 2)

    PSET (XY(1, 0), XY(1, 1)), GPDat(82)        'Set first point of the
                                                ' open curve
    MT = NP' - 1
    TS# = MT / NS                               'Compute T parameter increment

    XY(0, 0) = XY(NP, 0)
    XY(0, 1) = XY(NP, 1)                        'Load additional
                                                ' points to close
    XY(NP + 1, 0) = XY(1, 0)                    ' the curve
    XY(NP + 1, 1) = XY(1, 1)

    XY(NP + 2, 0) = XY(2, 0)
    XY(NP + 2, 1) = XY(2, 1)

    FOR IC = 1 TO NS
        T# = TS# * IC + 1
        IF T# > NP + 1 THEN T# = NP + 1
        TK = INT(T#)

        ParabPt XY(), TK - 1, T#, x(1), y(1)    'First parabola point

        ParabPt XY(), TK, T#, x(2), y(2)        'Second parabola point

        LT# = T# - TK                           'Using equation (3)
        RT# = INT(T# + 1) - T#

        IX = INT(x(2) * LT# + x(1) * RT# + .5)  'Blend Parabola points
        IY = INT(y(2) * LT# + y(1) * RT# + .5)  'Using equation (3)

        LINE -(IX, IY), GPDat(82)

    NEXT IC

END SUB

SUB CrossHair (x, y)

SELECT CASE GPDat(18)                                'Cursor Type

  CASE 1                                             '+ Crosshair
    LineVE x, y - 16, x, y, GPDat(22)
    LineVE x, y + 16, x, y, GPDat(22)
    x1 = x - 16
    IF x1 < 0 THEN x1 = 0
    x2 = x + 16
    IF x2 > GPDat(43) - 1 THEN x2 = GPDat(43) - 1
    LineVE x1, y, x, y, GPDat(22)
    LineVE x, y, x2, y, GPDat(22)

  CASE 2                                             'Full Crosshair
    LineVE 0, y, GPDat(43), y, GPDat(22)
    LineVE x, 0, x, GPDat(44), GPDat(22)

  CASE 3                                             'X Crosshair
    IF x > 7 THEN
      LineVE x - 8, y - 8, x, y, GPDat(22)
      LineVE x - 8, y + 8, x, y, GPDat(22)
    END IF

    IF x < GPDat(43) - 8 THEN
      LineVE x + 8, y - 8, x, y, GPDat(22)
      LineVE x + 8, y + 8, x, y, GPDat(22)
    END IF

  CASE 4
    x1 = x - GPDat(8) - 1
    y1 = y - GPDat(8) - 1
    x2 = x + GPDat(8) + 1
    y2 = y + GPDat(8) + 1

    IF x1 > -1 THEN LineVE x1, y1, x1, y2, GPDat(22)
    IF x2 < GPDat(43) THEN LineVE x2, y1, x2, y2, GPDat(22)

    IF x1 < 0 THEN x1 = 0
    IF x2 > GPDat(43) - 1 THEN x2 = GPDat(43) - 1
    LineVE x1, y1, x2, y1, GPDat(22)
    LineVE x1, y2, x2, y2, GPDat(22)

  CASE ELSE

END SELECT

END SUB

SUB CursorXY (x, y, Button, Ky) STATIC

  IF GPDat(17) THEN            'GPDat(17)  = Snap on/off variable (-1 = on)
    XSnap = GPDat(1)
    YSnap = GPDat(6)
  ELSE
    XSnap = 1
    YSnap = 1
  END IF

  CALL MouseXY                 'Display row and column if on

  GetCursor x, y, Button
  IF Button > 2 THEN Button = 2
  IF Button = 2 THEN EXIT SUB
  IF GPDat(17) = -1 THEN    'Then If Snap is On, convert to Snap Coordinates
    '---- GPDat(6) = YSnap Spacing, GPDat(1) = XSnap Spacing
    x = ((x + GPDat(2)) \ GPDat(1)) * GPDat(1)
    y = ((y + GPDat(10)) \ GPDat(6)) * GPDat(6) + (GPDat(6) = GPDat(10))
  END IF

  GOSUB CheckBoundry               'Make sure we stay in the screen

  Ky$ = INKEY$
  SELECT CASE LEN(Ky$)
    CASE 1
      Ky = ASC(Ky$)
      KeyFlg = -1
    CASE 2
      Ky = -ASC(RIGHT$(Ky$, 1))
      KeyFlg = -1
    CASE ELSE
      Ky = 0
  END SELECT

  IF KeyFlg THEN
    SELECT CASE Ky

      CASE 13                 'Enter
        Button = 1
        EXIT SUB
      CASE 27                 'Escape
        Button = 2
        EXIT SUB
      CASE 9                  'Tab
        x = x + 80
      CASE -15                'Shift+Tab
        x = x - 80
      CASE 92, 124            '\|   Toggle line type

        SELECT CASE GPDat(7)

          CASE 16, 17, 22         'Line, Box, Polygon
            Dotted = NOT Dotted

            IF GPDat(68) <> 0 AND GPDat(68) <> -1 THEN
              LType = GPDat(68)
            END IF

            IF Dotted THEN
              IF LType = 0 THEN LType = -1
              GPDat(68) = LType
              SOUND 1000, 1
              SOUND 750, 1
            ELSE
              SOUND 1000, 1
              GPDat(68) = -1
            END IF
          CASE ELSE

        END SELECT

      CASE -66                'F8
        GPDat(5) = NOT GPDat(5)
        IF GPDat(5) THEN
          StatBox 1
        ELSE
          StatBox 2
        END IF


      CASE -68                  'Restore the screen
        Mouse 7, 0, x, x
        Mouse 8, 0, y, y

        SELECT CASE GPDat(7)    'GPDat(7) = Current palette drawing choice

          CASE 25, 33, 35       '25=Flood  33=Tile
            HideCursor          '35=Copy/Move
            SRScreen -1
            ShowCursor
          CASE 36
            SRScreen -1
          CASE 50               'Partial pcx, define pb, mouse or scroll field

          CASE ELSE
            CrossHair x, y
            SRScreen -1
            CrossHair x, y
        END SELECT
        FreeMouse

      CASE -71                'Home
        x = 0
      CASE -72                'Up
        y = y - YSnap
      CASE -73                'PgUp
        y = 0
      CASE -75                'Left
        x = x - XSnap
      CASE -77                'Right
        x = x + XSnap
      CASE -79                'End
        x = GPDat(43)
      CASE -80                'Down
        y = y + YSnap
      CASE -81                'PgDn
        y = GPDat(44)
      CASE -94                'Cntrl + F1  reset mouse
        InitMouse There
        ShowCursor
      CASE -141               'Cntrl + Up
        y = y - 4 * GPDat(71)
      CASE -145               'Cntrl + Dn
        y = y + 4 * GPDat(71)
      CASE -115               'Cntrl + Left
        x = x - 72
      CASE -116               'Cntrl + Right
        x = x + 72

      CASE 70, 102            '"F,f" toggle paint snap coords

        GPDat(65) = NOT GPDat(65)
        IF GPDat(65) THEN
          SOUND 800, 1
          A = GPDat(1)
          B = GPDat(2)
          C = GPDat(6)
          D = GPDat(10)
          GPDat(1) = 8
          GPDat(2) = 4
          GPDat(6) = GPDat(71)
          GPDat(10) = GPDat(71) \ 2
        ELSE
          IF A <> 0 THEN
            SOUND 400, 1
            GPDat(1) = A
            GPDat(2) = B
            GPDat(6) = C
            GPDat(10) = D
          END IF
        END IF

        IF GPDat(5) THEN
          IF GPDat(65) THEN
            RelClr = 0 + 7 * 256
          ELSE
            RelClr = 7 + 7 * 256
          END IF

          CALL GPrint2VE(GPDat(16) + 5, GPDat(15) \ 8 + 13, CHR$(219), RelClr)
        END IF

      CASE 83, 115            '"S,s" Snap on/off

        IF GPDat(7) <> 60 THEN 'AND GPDat(7) <> 23 THEN  ' AND GPDat(7) <> 24 THEN
          GPDat(17) = NOT GPDat(17)
          SOUND 400, 1
        END IF

      CASE 84, 116            '"T,t" Text coords on/off
        GPDat(19) = NOT GPDat(19)

      CASE 82, 114            '"R,r" Relative coords on/off
        IF GPDat(5) THEN      'If Status box on

          GPDat(21) = NOT GPDat(21)

          IF GPDat(21) THEN
            RelClr = 0 + 7 * 256
          ELSE
            RelClr = 7 + 7 * 256
          END IF

          CALL GPrint2VE(GPDat(16) + 5, GPDat(15) \ 8 + 14, CHR$(7), RelClr)

        END IF

      '----- Color Keys

      CASE 48 TO 57
        SELECT CASE GPDat(7)
          '---- Set bevel width
          CASE 75             '3D Panels, Frames, Engraved Box
            BevelWidth = Ky - 48
          '---- Select color
          CASE ELSE           'Everything else
            GPDat(82) = Ky - 48
            IF GPDat(5) THEN LINE (GPDat(15) + 5, GPDat(16) + 5)-(GPDat(15) + 45, GPDat(16) + GPDat(71) + 4), GPDat(82), BF
        END SELECT

      CASE 41
        GPDat(82) = 10
        IF GPDat(5) THEN LINE (GPDat(15) + 5, GPDat(16) + 5)-(GPDat(15) + 45, GPDat(16) + GPDat(71) + 4), GPDat(82), BF
      CASE 33                  'Shift 1
        GPDat(82) = 11
        IF GPDat(5) THEN LINE (GPDat(15) + 5, GPDat(16) + 5)-(GPDat(15) + 45, GPDat(16) + GPDat(71) + 4), GPDat(82), BF
      CASE 64                  'Shift 2
        GPDat(82) = 12
        IF GPDat(5) THEN LINE (GPDat(15) + 5, GPDat(16) + 5)-(GPDat(15) + 45, GPDat(16) + GPDat(71) + 4), GPDat(82), BF
      CASE 35                  'Shift 3
        GPDat(82) = 13
        IF GPDat(5) THEN LINE (GPDat(15) + 5, GPDat(16) + 5)-(GPDat(15) + 45, GPDat(16) + GPDat(71) + 4), GPDat(82), BF
      CASE 36                  'Shift 4
        GPDat(82) = 14
        IF GPDat(5) THEN LINE (GPDat(15) + 5, GPDat(16) + 5)-(GPDat(15) + 45, GPDat(16) + GPDat(71) + 4), GPDat(82), BF
      CASE 37                  'Shift 5
        GPDat(82) = 15
        IF GPDat(5) THEN LINE (GPDat(15) + 5, GPDat(16) + 5)-(GPDat(15) + 45, GPDat(16) + GPDat(71) + 4), GPDat(82), BF

      CASE ELSE

    END SELECT

    GOSUB CheckBoundry
    KeyFlg = 0                ' move the cursor
    Mouse 7, 0, x, x
    Mouse 8, 0, y, y

    FreeMouse

  END IF

  CALL MouseXY


EXIT SUB

CheckBoundry:
  IF y > GPDat(44) - 1 THEN y = GPDat(44) - 1
  IF y < 0 THEN y = 0
  IF x > GPDat(43) - 1 THEN x = GPDat(43) - 1
  IF x < 0 THEN x = 0
RETURN


END SUB

SUB DefCursor (Cursor)

   SELECT CASE Cursor
     CASE 1
       Arrow$ = "ˇÁˇ„ˇ·ˇ‡‡?‡‡‡‡‡" + CHR$(1) + "‡‡‚ˇˇèˇ" + CHR$(0) + "" + CHR$(0) + "" + CHR$(0) + "" + CHR$(0) + "Ä@ >  ê" + CHR$(0) + "ê" + CHR$(0) + "p" + CHR$(0)

     CASE 2
       Zero$ = CHR$(0) + CHR$(0) + CHR$(0)
       Arrow$ = "ˇÁˇ„ˇ·ˇ‡‡?‡‡‡‡‡?‡‚Êˇˇèˇ" + Zero$ + "" + CHR$(0) + "" + CHR$(0) + "" + CHR$(0) + "Ä¿‡ÄÄ" + CHR$(13) + "¿¿" + CHR$(0) + "`" + CHR$(0) + "`" + Zero$

   END SELECT

   GrafCursor 3, 0, Arrow$

END SUB

SUB DotStepBox (x1, y1, x2, y2, Mask, Clr)
  LINE (x1, y1)-STEP(x2 - x1, y2 - y1), Clr, BF, Mask
END SUB

SUB DrawCursor (x, y, CursorWidth, CursorLength, CursorIs, CursorTime) STATIC

  ThisTime = PDQTimer& MOD CursorTime
  IF ThisTime < LastTime OR CursorTime = -1 THEN
      Colr = 15 + 256 * (16 + 8)

      HideCursor
      LineBFVE x, y, x + CursorWidth - 1, y + CursorLength, Colr
      ShowCursor
      CursorIs = NOT CursorIs
      IF CursorTime = -1 THEN
          ThisTime = 32000 'Make sure cursor comes back fast
      END IF
  END IF
  LastTime = ThisTime

END SUB

FUNCTION Eval# (E$, ErrCode)

    ErrCode = -1
    Eval# = 0

    Sign = 0
    S1 = 0
    Decm = 0
    Xpon = 0

    x$ = E$
    T$ = " "
    Tbl$ = "-+.eEdD0123456789"

    Ln = LEN(x$)
    x = 1
    DO UNTIL x > Ln

       LSET T$ = MID$(x$, x, 1)

       OK = INSTR(Tbl$, T$)

       SELECT CASE OK
          CASE 0
             MID$(x$, x) = MID$(x$, x + 1)
             MID$(x$, Ln) = " "
             Ln = Ln - 1
          CASE 1, 2
             Sign = Sign + 1
             IF x < Xpon AND Sign > 1 THEN
                EXIT DO
             ELSEIF x < Xpon THEN
                S1 = x
             END IF
          CASE 3
             IF Decm = 0 THEN
                Decm = x
             ELSE
                EXIT DO
             END IF
          CASE 4 TO 7
             IF Xpon = 0 THEN
                Xpon = x
             ELSE
                EXIT DO
             END IF
          CASE ELSE
       END SELECT

       x = x + 1
    LOOP


    Whole = Decm - S1 - 1
    IF Whole > 308 THEN EXIT FUNCTION

    IF Xpon THEN
       Num# = VAL(LEFT$(x$, Xpon))
       Xpon = VAL(MID$(x$, Xpon + 1))

       Num# = Num# / (10 ^ (Whole - 1))
       Xpon = Xpon + Whole - 1

       IF Xpon > 308 THEN
          EXIT FUNCTION
       ELSEIF Xpon = 308 AND ABS(Num#) > 1.79769313486232# THEN
          EXIT FUNCTION
       END IF

    END IF

    Eval# = VAL(x$)
    ErrCode = 0

END FUNCTION

FUNCTION GQSPath$

  P$ = ExeName$
  Nd = QInstrB2%(-1, P$, "\")
  GQSPath$ = LEFT$(P$, Nd)

END FUNCTION

SUB Grid (x1, y1, x2, y2, xspace, yspace, XHiSpace, YHiSpace, Clr, Clr1)

y1 = y1 - yspace
x1 = -xspace

FOR y = y1 + yspace TO y2 STEP yspace
  FOR x = x1 + xspace TO x2 - xspace STEP xspace
    DrawPointVEOpts x, y, Clr
  NEXT
NEXT

FOR y = y1 + yspace TO y2 - yspace STEP YHiSpace
  FOR x = x1 + xspace TO x2 - xspace STEP XHiSpace
    DrawPointVEOpts x, y, Clr1
  NEXT
NEXT

END SUB

SUB NCBox (x, y, BWidth, BHite, Mask, Clr)

  HideCursor
  LINE (x + 1, y)-STEP(BWidth - 2, 0), Clr, , Mask             'Top Horiz.
  LINE (x + 1, y + BHite)-STEP(BWidth - 2, 0), Clr, , Mask     'Bottom Horiz
  LINE (x, y + 1)-STEP(0, BHite - 2), Clr, , Mask              'Left Vert
  LINE (x + BWidth, y + 1)-STEP(0, BHite - 2), Clr, , Mask     'Right Vert
  ShowCursor

END SUB

FUNCTION NoExtension$ (T$)

  Per = QInstrB2(-1, T$, ".")
  BSlash = QInstrB2(-1, T$, "\")

  IF Per THEN
    IF Per > BSlash THEN
      NoExtension$ = LEFT$(T$, Per - 1)
    ELSE
      NoExtension$ = T$
    END IF
  ELSE
    NoExtension$ = T$
  END IF

END FUNCTION

SUB OpenCurve (XY(), NP, NS) STATIC

    REDIM x(1 TO 2), y(1 TO 2)
    PSET (XY(1, 0), XY(1, 1)), GPDat(82)'SET FIRST POINT OF THE
                                        'OPEN CURVE
    MT = NP - 1
    TS# = MT / NS                       'COMPUTE T PARAMETER INCREMENT

    FOR IT = 1 TO NS

        T# = TS# * IT + 1
        TK = INT(T#)                    'COMPUTE BASIC REFERENCE VALUE

        IF T# >= 2 THEN                 'First Point
           ParabPt XY(), TK - 1, T#, x(1), y(1)
        END IF

        IF T# <= MT THEN                'Second Point
           ParabPt XY(), TK, T#, x(2), y(2)
        END IF

        IF T# < 2 THEN                  'IF T<2, USE ONLY THE SECOND
           IX = x(2)                    'PARABOLA POINT IN THE CURVE
           IY = y(2)

        ELSEIF T# > MT THEN             'IF T>NP-1, USE ONLY THE FIRST
           IX = x(1)                    'PARABOLA POINT IN THE CURVE
           IY = y(1)

        ELSE                            'BLEND PARABOLA POINTS
           LT# = T# - TK                'USING EQUATION (3)
           RT# = TK + 1 - T#
           IX = INT(x(2) * LT# + x(1) * RT# + .5)
           IY = INT(y(2) * LT# + y(1) * RT# + .5)
        END IF

        LINE -(IX, IY), GPDat(82)

    NEXT

END SUB

SUB ParabPt (XY(), TP, T#, x, y) STATIC

      '******* Generate Parabola using equation (2)
      Z1# = (TP - T# + 1) * (TP - T# + 2) / 2'                 PART 1
      Z2# = -(TP - T#) * (TP - T# + 2) '                       PART 2
      Z3# = (TP - T#) * (TP - T# + 1) / 2'                     PART 3

      x = Z1# * XY(TP, 0) + Z2# * XY(TP + 1, 0) + Z3# * XY(TP + 2, 0)
      y = Z1# * XY(TP, 1) + Z2# * XY(TP + 1, 1) + Z3# * XY(TP + 2, 1)

END SUB

SUB PCBox (x1, y1, x2, y2, Outside)

  Clr1 = 15
  Clr = 8'0
  IF Outside THEN SWAP Clr, Clr1

  IF x2 > GPDat(43) - 1 THEN x2 = GPDat(43) - 1

  CALL LineVE(x1, y1, x2, y1, Clr)
  CALL LineVE(x1, y1, x1, y2, Clr)
  CALL LineVE(x2, y1, x2, y2, Clr1)
  CALL LineVE(x1, y2, x2, y2, Clr1)

END SUB

SUB PushButton (x1, y1, x2, y2, Size, White, Gray, HiWhite)

  OS = Size = 1

  LineBFVE x1 + 2, y1 + 2, x2 - 3, y2 - 3, White
  LineBFVE x1 + 1, y2 - Size + OS, x2 - 1, y2 - 1, Gray
  LineBFVE x2 - Size + OS, y1 + 1, x2 - 1, y2 - 1, Gray

  FOR i = 1 TO Size
    LineVE x1 + 1, y1 + i, x2 - i - 1, y1 + i, HiWhite
    LineVE x1 + i, y1 + 1, x1 + i, y2 - i - 1, HiWhite
  NEXT
  LineBVE x1, y1, x2, y2, 0

END SUB

SUB RoundBox (x, y, x1, y1, Clr)

  TDat = GPDat(3)

  IF x1 - x < 2 * GPDat(3) THEN GPDat(3) = (x1 - x) \ 2
  IF y1 - y < 2 * GPDat(3) THEN GPDat(3) = (y1 - y) \ 2

  IF GPDat(31) = 5 THEN OS = GPDat(3) \ 4 + 1

  CALL LineVE(x + GPDat(3), y, x1 - GPDat(3), y, Clr)  'Top horizontal line
  CALL LineVE(x1, y + GPDat(3) - OS, x1, y1 - GPDat(3) + OS, Clr)'Right vertical line
  CALL LineVE(x + GPDat(3), y1, x1 - GPDat(3), y1, Clr)'Bottom horizontal line
  CALL LineVE(x, y + GPDat(3) - OS, x, y1 - GPDat(3) + OS, Clr)'Left vertical line

  CIRCLE (x + GPDat(3), y + GPDat(3) - OS), GPDat(3), Clr, Pi! / 2, Pi!', Aspect!
  CIRCLE (x1 - GPDat(3), y + GPDat(3) - OS), GPDat(3), Clr, 0, Pi! / 2', Aspect!
  CIRCLE (x1 - GPDat(3), y1 - GPDat(3) + OS), GPDat(3), Clr, 2 * Pi! - Pi! / 2, 0', Aspect!
  CIRCLE (x + GPDat(3), y1 - GPDat(3) + OS), GPDat(3), Clr, Pi!, 2 * Pi! - Pi! / 2', Aspect!

  GPDat(3) = TDat

END SUB

SUB SPushButton (x, y, BWidth, BHite, Text$, UpDn, HotKey)
IF UpDn > 0 THEN   'Button up

  LINE (x, y)-STEP(BWidth - 1, BHite - 1), 7, BF         'Button top
  LINE (x, y)-STEP(BWidth - 1, BHite - 1), 0, B          'Button outline

  LINE (x + 1, y + (BHite - 2))-STEP(BWidth - 3, -1), 8, BF
  LINE -STEP(-1, -BHite + 4), 8, BF

  LINE (x + 1, y + 1)-STEP(BWidth - 4, 0), 15
  LINE (x + 1, y + 1)-STEP(0, BHite - 4), 15


 IF LEN(Text$) THEN                                     'If there's text,
   SELECT CASE GPDat(71)
     CASE 14
       OS1 = 1
     CASE 8
       OS1 = 3
     CASE ELSE
       OS1 = 0
   END SELECT

   yRow = y + BHite \ 2 - GPDat(71) \ 2                 ' print it and
   xCol = (x + BWidth \ 2 + 8) \ 8 - (LEN(Text$) \ 2) + 1 ' underline the
   CALL GPrint3VE(yRow, xCol, Text$, 0)                 ' HotKey

   IF GPDat(71) = 8 THEN
     CALL GPrint3VE(yRow, xCol + HotKey, MID$(Text$, HotKey + 1, 1), 15)           ' HotKey
   ELSE
     LINE ((xCol + HotKey) * 8 - 8, yRow + GPDat(71) - 3 + OS1)-STEP(6, 0), 0
   END IF

 END IF

ELSE             'Button Down

  REDIM Array#(GArraySize&(x + 2, y + 2, x + BWidth - 2, y + BHite - 2) \ 8)
  GET (x + 2, y + 2)-STEP(BWidth - 6, BHite - 6), Array#     'Get button top
  LINE (x + 2, y + 2)-STEP(BWidth - 4, BHite - 4), 7, BF     'Button top
  LINE (x + 1, y + 1)-STEP(BWidth - 2, 0), 8
  LINE (x + 1, y + 1)-STEP(0, BHite - 4), 8
  PUT (x + 4, y + 4), Array#, PSET
  ERASE Array#
END IF

END SUB

SUB SRScreen (SaveRest) STATIC

'SaveRest set to 0 will save the screen, -1 will restore the screen
' 2 will disable screen restore and clean up memory

IF SaveRest = 2 THEN
  REDIM ScrnArray(0)                                 'Clean up memory
  ScrnMode = 0
  EXIT SUB
END IF

IF NOT SaveRest THEN                              'If we're saving the screen
  CALL ClearVE
  HideCursor
  IF GPDat(31) = 5 THEN                           'Must be Screen 9
    ScrnMode = 9                                  'Remember we saved Screen 9
    GMove2VE 1, 0, 80, GPDat(44), &HA800, 0       'Save the screen
  ELSEIF GPDat(31) = 8 THEN                       'Must be Screen 12
    IF FRE(-1) < 64000 THEN
      ScrnMode = 0
      BEEP
      ShowCursor
      EXIT SUB
    END IF
    ScrnMode = 12                                 'Remember we saved Screen 12
    REDIM ScrnArray(30000)                        'Array to hold lower 1/3
    ArraySeg = VARSEG(ScrnArray(0))               'Get the array's segment
    CALL GMove4VE(1, 300, 80, 180, ArraySeg, 0)   'Save the lower 1/3
    GMove2VE 1, 0, 80, 300, &HAA00, 0             'Save upper 2/3
  END IF
  ShowCursor
ELSE
  IF ScrnMode THEN                                'If a screen has been saved,
    HideCursor
    IF ScrnMode = 9 THEN                          ' and it was screen 9,
      GMove2VE 1, 0, 80, GPDat(44), &HA800, -1    ' restore it
    ELSE                                          'Must have been screen 12
      GMove2VE 1, 0, 80, 300, &HAA00, -1          'Restore upper 2/3
      ArraySeg = VARSEG(ScrnArray(0))               'Get the array's segment
      CALL GMove4VE(1, 300, 80, 180, ArraySeg, -1)'Restore lower 1/3
    END IF
    CALL ClearVE
    ShowCursor
  END IF                                          ' the same screen again
END IF

END SUB

SUB StepBox (x, y, Wide, Hite, Clr)

  x2 = x + Wide - 1
  y2 = y + Hite - 1

  CALL LineVE(x, y, x2, y, Clr)
  CALL LineVE(x2, y + 1, x2, y2 - 1, Clr)
  CALL LineVE(x, y2, x2, y2, Clr)
  CALL LineVE(x, y + 1, x, y2 - 1, Clr)

END SUB

