DEFINT A-Z

'---- Graphic vertical menu subroutine

'by Phil Cramer
'Copyright (c) 1992 Crescent Software

'$INCLUDE: 'ScrollB.Bi'                'Defines TYPE for ScrollBars
'$INCLUDE: 'Common.Bi'

'---- ASM subs
DECLARE FUNCTION FindLast (BYVAL Address, NumEls)
DECLARE FUNCTION MaxInt% (A, B)
DECLARE FUNCTION MinInt% (A, B)
DECLARE FUNCTION PDQTimer& ()

'---- ASM functions
DECLARE SUB ButtonPress (Button, Status, Count, x, y)
DECLARE SUB ClearVE ()
DECLARE SUB GetCursor (x, y, Button)
DECLARE SUB GMove4VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GMove2VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GPrint0VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB HideCursor ()
DECLARE SUB LineBVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB LineBFVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB ShowCursor ()

'---- BASIC subs
DECLARE SUB HVScrollBar (x1%, y1%, x2%, y2%, HV%, HiWhite%, White%, Gray%, SlideClr%)
DECLARE SUB VScrollBar (Button%, x%, y%, Ky$, ScrollBar AS ANY, Clean%)
DECLARE SUB MoveVPtr (x1%, y1%, x2%, LastY%)
DECLARE SUB DotBox (x%, y%, BWidth%, BHite%, Clr%)
DECLARE SUB SetPBColors (Fld AS ANY, HV%)

'---- BASIC function
DECLARE FUNCTION GetFactor# (x1, y1, Lo#, Hi#)

DIM SHARED LS
CONST AltHotKey$ = "QWERTYUIOP    ASDFGHJKL     ZXCVBNM"

SUB DotBox (x, y, BWidth, BHite, Clr)

  Mask = 21845

  HideCursor
  LINE (x + 1, y)-STEP(BWidth - 2, 0), Clr, , Mask             'Top Horiz.
  LINE (x + 1, y + BHite)-STEP(BWidth - 2, 0), Clr, , Mask     'Bottom Horiz
  LINE (x, y + 1)-STEP(0, BHite - 2), Clr, , Mask              'Left Vert
  LINE (x + BWidth, y + 1)-STEP(0, BHite - 2), Clr, , Mask     'Right Vert
  ShowCursor

END SUB

SUB ListBox (List$(), Choice, MaxLen, MenuRows, Ky$, HotKeyTbl(), Action) STATIC

  IF Action < 3 THEN
    Rows = MenuRows
    HKUpper = UBOUND(HotKeyTbl)
    UpperB = UBOUND(List$)

    DotBx = 0
    LastChoice = 0
    Saved = 0
    ScrollWidth = 0
    Started = 0
    There = 0
    TopOS = 0
    BottomOS = 0
    ScrllBar = 0
    Buf$ = SPACE$(MaxLen)
    MenuHt = Rows

    Row = CSRLIN
    LCol = POS(0)

    Rows = MinInt(FindLast(BYVAL VARPTR(List$(UpperB)), UpperB), Rows)

    TotalRows = FindLast(BYVAL VARPTR(List$(UpperB)), UpperB)

    IF Row + Rows - 1 > GPDat(85) THEN Row = Row - Rows + 1
    IF Row < 1 THEN
      Row = 2
      Rows = CSRLIN
    END IF
    BotRow = Row + Rows - 1

    IF Action < 2 THEN
      x1 = (LCol - 1) * 8 - 8
      x2 = x1 + (MaxLen + 1) * 8 - 1
      y1 = Row * GPDat(71) - GPDat(71)
      IF HKUpper THEN
        y2 = y1 + MenuHt * GPDat(71) - 1
      ELSE
        y2 = y1 + Rows * GPDat(71) - 1
      END IF

      IF Action < 0 THEN                'Save underlying screen
        IF Action = -1 THEN
          REDIM ScrnArray#(1 TO ((MaxLen + 6) * (y2 - y1 + 3) * 4 + 4) \ 8)
          Saved = 1
        ELSEIF Action = -2 THEN
          Saved = 2
        END IF
        SR = 0
        GOSUB BGScrn
      END IF

    END IF

    HideCursor
    LINE (x1, y1)-(x2, y2), GPDat(76) \ 256, BF
    LINE (x1 - 1, y1 - 1)-(x2 + 1, y2 + 1), 0, B
    ShowCursor

    IF (y2 - y1 > 48) AND (TotalRows > Rows) THEN ScrllBar = -1

    EndEl = MinInt(MaxInt(Choice, Rows), UpperB)

    TextClr = GPDat(76)
    HiClr = GPDat(78)

    HiDotClr = HiClr MOD 256      'HiLite BGClr
    BGDotClr = HiClr \ 256        'HiLite FGClr
    NormDotClr = TextClr \ 256    'Text BG

    DotClr = HiDotClr                 'Hilite FGClr

    GOSUB PrintList

    IF ScrllBar THEN
      ScrollWidth = 19
      DIM ScrollBar AS ScrollBar
      ScrollBar.x1 = x2 + 1
      ScrollBar.x2 = x2 + ScrollWidth
      ScrollBar.y1 = y1 - 1
      ScrollBar.y2 = y2 + 1
      ScrollBar.PointerPos = ScrollBar.y1 + 16
      ScrollBar.LargeChange = Rows - 2
      ScrollBar.SmallChange = 1
      ScrollBar.Value = Choice
      ScrollBar.Lo = 1
      ScrollBar.Hi = TotalRows             'pc
      ScrollBar.Action = 1
      ScrollBar.Factor = GetFactor#(ScrollBar.y1, ScrollBar.y2, ScrollBar.Lo, ScrollBar.Hi)
      HideCursor
      HVScrollBar ScrollBar.x1, ScrollBar.y1, ScrollBar.x2, ScrollBar.y2, -1, GPDat(88), GPDat(87), GPDat(89), GPDat(98)
      CALL VScrollBar(0, x, y, "", ScrollBar, Clean)
      ShowCursor
      SetPBColors ScrollBar, -1
    END IF

    IF Action = -3 THEN EXIT SUB
    IF Action THEN Action = 3

    GOSUB GetInf
    LastRow = CurRow

  ELSEIF Action = 5 AND Saved THEN
    SR = -1
    GOSUB BGScrn
    EXIT SUB

  ELSEIF Action = 6 THEN 'Called earlier with Action -3, this "Activates" menu
    UpperB = UBOUND(List$)
    Rows = MinInt(FindLast(BYVAL VARPTR(List$(UpperB)), UpperB), Rows)
    EndEl = MinInt(MaxInt(Choice, Rows), UpperB)
    GOSUB GetInf
    DotBx = 0
    LastRow = CurRow
    IF Choice THEN GOSUB PrintHiLite
    Action = 3

  END IF

  DO
    Ky$ = INKEY$

    ButtonPress 1, Status, Cnt, x, y
    IF Cnt = 0 THEN GetCursor x, y, Button

    IF Button AND 2 THEN Ky$ = CHR$(27)

    IF Button = 1 THEN
      IF y > y1 AND y < y2 AND x > x1 AND x < x2 + ScrollWidth THEN
        IF x < x2 THEN
          MRow = y \ GPDat(71) + 1
          Choice = MaxInt(1, MRow - Row + StartEl - 1)
          Down = -1
          Inside = -1
        ELSE                                       'on ScrollBar
          Inside = 0
          Scrolling = -1
        END IF
      ELSE
        IF HKUpper THEN GOSUB Done
        EXIT SUB
      END IF
    END IF

    IF Button = 0 AND Down THEN
      Down = 0
      Click& = PDQTimer&
      IF Click& - LastClick& < 6 THEN
        Ky$ = CHR$(13)
      END IF
      LastClick& = Click&
    END IF

    IF ScrllBar THEN
      IF Inside THEN Button = 0
      CALL VScrollBar(Button, x, y, K$, ScrollBar, Clean)
      IF Button AND Scrolling THEN Choice = ScrollBar.Value
      IF Button = 0 AND Scrolling THEN Scrolling = 0
    END IF

    SELECT CASE LEN(Ky$)
      CASE 0
        Ky = 0
      CASE 1
        Ky = ASC(Ky$)
      CASE 2
        Ky = -ASC(RIGHT$(Ky$, 1))
    END SELECT

    SELECT CASE Ky

      CASE -72         'Up arrow
        IF Choice - 1 > 0 THEN Choice = Choice - 1
       
      CASE -80         'Down Arrow
        Choice = Choice + 1

      CASE -73         'PgUp
        IF Choice - (Rows - 1) > 0 THEN
          Choice = Choice - (Rows - 1)
        ELSE
          Choice = 1
        END IF
         
      CASE -81         'PgDn
        Choice = Choice + (Rows - 1)

      CASE -79         'End
        Choice = UpperB

      CASE -71         'Home
        Choice = 1

      CASE 13          'Enter
        Action = 4

      CASE 9, -15
        IF HKUpper THEN
          IF DotBx AND Choice = 0 THEN
            DotBx = 0
            CALL DotBox(x1, Row * GPDat(71) - GPDat(71), x2 - x1, GPDat(71) - 1, NormDotClr)
            IF ScrllBar THEN MoveVPtr ScrollBar.x1, ScrollBar.PointerPos, ScrollBar.x2, ScrollBar.PointerPos
            Action = 4
          ELSE
            GOSUB Done
          END IF
          EXIT SUB
        END IF

      CASE 27
        Action = 4

      CASE 33 TO 254   'Letter Keys
        Test = MaxInt(1, Choice)
        DO
          Test = Test + 1
          Tst$ = UCASE$(Ky$)
          IF Test > UpperB THEN Test = 1
          IF UCASE$(LEFT$(List$(Test), 1)) = Tst$ THEN
            Choice = Test
            EXIT DO
          END IF

        LOOP UNTIL Test = MaxInt(1, Choice)

      CASE -50 TO -16  'Look for Alt + HotKey... (Dialog boxes only)
        IF HKUpper THEN
          Ty = ABS(Ky) - 15
            Ky = ASC(MID$(AltHotKey$, Ty, 1))
            Search = LC
            DO
              IF Ky = HotKeyTbl(Search) THEN
                Ky$ = CHR$(Ky)

                IF Choice = 0 THEN
                  CALL DotBox(x1, Row * GPDat(71) - GPDat(71), x2 - x1, GPDat(71) - 1, NormDotClr)
                ELSE
                  GOSUB Done
                END IF
                IF Action THEN Action = 4

                EXIT SUB
              END IF
              Search = Search + 1
              IF Search > HKUpper THEN Search = 1
           LOOP UNTIL Search = LC
           LC = LC + 1
           IF LC > HKUpper THEN LC = 1
         END IF
      CASE ELSE

    END SELECT

    IF Choice = 0 THEN
      IF NOT DotBx THEN
        DotBx = -1
        CALL DotBox(x1, Row * GPDat(71) - GPDat(71), x2 - x1, GPDat(71) - 1, BGDotClr)
      END IF
      EXIT SUB
    END IF

    Choice = MinInt(MaxInt(Choice, 1), TotalRows)   'Dont go beyond TotalRows UBOUND
    
    IF Choice <> LastChoice THEN                    'If we've moved

      Scrl = 0
      ScrollBar.Value = Choice
      IF Choice > EndEl THEN
        EndEl = Choice                              'Scroll down
        Scrl = -1
      END IF


      IF Choice < StartEl THEN
        EndEl = Choice + Rows - 1                   'Scroll Up
        Scrl = -1
      END IF

      GOSUB GetInf

      IF CurRow <> LastRow THEN
        LSET Buf$ = List$(MaxInt(1, LastChoice))    'Clear last hilight
        HideCursor
        GPrint0VE MaxInt(Row, LastRow), LCol - 1, " " + Buf$, TextClr
        ShowCursor
      END IF

      TopOS = 0
      BottomOS = 0
      IF CurRow = Row THEN TopOS = 1
      IF CurRow = BotRow THEN BottomOS = 1
      IF Scrl THEN GOSUB PrintList
      GOSUB PrintHiLite
      LastChoice = Choice
      LastRow = CurRow

    END IF
    
  LOOP WHILE Action = 0

EXIT SUB

GetInf:
  StartEl = EndEl - Rows + 2
  CurRow = Row + MaxInt(1, Choice) - StartEl + 1
RETURN

PrintHiLite:
  LSET Buf$ = List$(MaxInt(1, Choice))
  HideCursor
  GPrint0VE CurRow, LCol - 1, " " + Buf$, HiClr
  ShowCursor
  CALL DotBox(x1, CurRow * GPDat(71) - GPDat(71), x2 - x1, GPDat(71) - 1, DotClr)
RETURN

PrintList:
  R = Row + TopOS
  FOR i = EndEl - Rows + 1 + TopOS TO MinInt(UpperB, EndEl - BottomOS)
    LSET Buf$ = List$(i)
    HideCursor
    GPrint0VE R, LCol - 1, " " + Buf$, TextClr
    ShowCursor
    R = R + 1
  NEXT
RETURN

BGScrn:
  HideCursor
  IF Saved = 1 THEN
    GMove4VE LCol - 2, y1 - 1, MaxLen + 6, y2 - y1 + 3, VARSEG(ScrnArray#(1)), SR
  ELSEIF Saved = 2 THEN
    GMove2VE LCol - 2, y1 - 1, MaxLen + 6, y2 - y1 + 3, GPDat(72), SR
  END IF
  ClearVE
  ShowCursor
RETURN

Done:
  DotClr = BGDotClr
  GOSUB PrintHiLite
  IF ScrllBar THEN MoveVPtr ScrollBar.x1, ScrollBar.PointerPos, ScrollBar.x2, ScrollBar.PointerPos
  DotClr = HiDotClr
  Action = 4
RETURN

  'Action values:
  '-3 Display list and scroll bar only (no hilite)
  '-2 Save to Video
  '-1 Save to array
  '1  Initiate polled mode
  '2  Redisplay
  '3  Polling mode
  '4  Escape or Enter pressed
  '5  Restore screen (Must have been called initially with action = -1 or -2)
  '6  Activates the menu after being called with action -3

  'StartEl = Current List$ element at top of list box
  'CurRow = Screen row of currently selected item
  'El = Element number of the current choice

  'GPDat(76) = Text color
  'GPDat(78) = Hilite color

END SUB

