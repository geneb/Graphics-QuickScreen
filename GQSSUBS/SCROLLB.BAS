DECLARE SUB ScrollPause (Delay%)
DEFINT A-Z

'---- Horizontal and vertical scrollbar routines

'---- Basic Subs
DECLARE SUB Arrows (x%, y%, Wide%, Rotation%)
DECLARE SUB HVScrollBar (X1%, Y1%, X2%, Y2%, HV%, HiWhite%, White%, Gray%, SlidClr%)
DECLARE SUB HScrollPB (X1%, Y1%, X2%, Y2%, Action%)
DECLARE SUB MoveHPtr (X1%, Y1%, LastX%, Y2%)
DECLARE SUB MoveVPtr (Y1%, X1%, LastY%, X2%)
DECLARE SUB Pressbutton (X1%, Y1%, X2%, Y2%)
DECLARE SUB PushButton1 (X1%, Y1%, X2%, Y2%, Size%, White%, Gray%, HiWhite%)
DECLARE SUB SetPBColors (Fld AS ANY, HV%)
DECLARE SUB SetScreen ()
DECLARE SUB SReleaseButton (X1%, Y1%, X2%, Y2%)
DECLARE SUB VScrollPB (X1%, Y1%, X2%, Y2%, Action%)

'---- ASM subs
DECLARE SUB HideCursor ()
DECLARE SUB LineVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB LineBVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB LineBFVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB Mouse (AX%, BX%, CX%, DX%)
DECLARE SUB ShowCursor ()
DECLARE SUB StuffBuf (A$)

'---- Basic Functions
DECLARE FUNCTION GArraySize& (X1, y, X2, Y2)
DECLARE FUNCTION GetFactor# (X1%, X2%, Lo#, Hi#)

'---- ASM Functions
DECLARE FUNCTION GetPointVE% (BYVAL x%, BYVAL y%)
DECLARE FUNCTION MinLong& (A&, B&)
DECLARE FUNCTION MaxLong& (A&, B&)
DECLARE FUNCTION PDQTimer& ()
	       
DIM SHARED TopClr, HiClr, ShadeClr, SlideClr, PageClr

CONST SBWidth = 16

'$INCLUDE: 'ScrollB.bi'                'Defines TYPE for ScrollBars
'$INCLUDE: 'Common.Bi'

REDIM SHARED PB(0)

SUB Arrows (x, y, Wide, Rotation)

  IF Wide < 7 THEN Wide = 7
  Steps = Wide \ 7
  OS1 = Steps * 2
  OS2 = Steps * 3
  PntSize = Steps \ 2

  SELECT CASE Rotation
    CASE 1, 3
      IF PntSize THEN
	LINE (x, y - PntSize + 1)-(x, y + PntSize), 0  'Point
      ELSE
	LINE (x, y)-(x, y + 1), 0
      END IF
    CASE 2, 4
      IF PntSize THEN
	LINE (x - PntSize + 1, y)-(x + PntSize, y), 0  'Point
      ELSE
	LINE (x, y)-(x + 1, y), 0
      END IF
    END SELECT

  SELECT CASE Rotation
    CASE 1                                       'Right
      LINE (x + 6, y - Steps)-(x + 1, y + Steps + 1), 0, BF
      LINE (x + 2, y - OS1)-(x + 2, y + OS1 + 1), 0
      LINE (x + 3, y - OS2)-(x + 3, y + OS2 + 1), 0
    CASE 2                                       'Up
      LINE (x - Steps, y + 6)-(x + Steps + 1, y + 1), 0, BF
      LINE (x - OS1, y + 2)-(x + OS1 + 1, y + 2), 0
      LINE (x - OS2, y + 3)-(x + OS2 + 1, y + 3), 0
    CASE 3                                       'Left
      LINE (x - 6, y - Steps)-(x - 1, y + Steps + 1), 0, BF
      LINE (x - 2, y - OS1)-(x - 2, y + OS1 + 1), 0
      LINE (x - 3, y - OS2)-(x - 3, y + OS2 + 1), 0
    CASE 4                                       'Down
      LINE (x - Steps, y - 6)-(x + Steps + 1, y - 1), 0, BF
      LINE (x - OS1, y - 2)-(x + OS1 + 1, y - 2), 0
      LINE (x - OS2, y - 3)-(x + OS2 + 1, y - 3), 0
  END SELECT

END SUB

FUNCTION GetFactor# (X1, X2, Lo#, Hi#)

   Wide# = (X2 - SBWidth - 1) - (X1 + 2 * SBWidth)
   Range# = Hi# - Lo#
   GetFactor# = Range# / Wide#

END FUNCTION

SUB HScrollBar (Button, x, y, Ky$, ScrollBar AS ScrollBar, Clean) STATIC

  IF Clean THEN
    ScrollBar.PointerPos = (ScrollBar.Value - ScrollBar.Lo) / ScrollBar.Factor + ScrollBar.X1 + SBWidth
    GOSUB CleanUp
    EXIT SUB
  END IF

  IF ScrollBar.Action = 1 THEN
    ScrollBar.Action = 3
    T& = -10000
  END IF

  IF Button = 0 THEN
    IF Pressed THEN
      Clicked = 0
      T& = T& - 100
      Mouse 7, 0, 0, GPDat(43) - 1
      Mouse 8, 0, 0, GPDat(44) - 1
      OnPointer = 0
      Pressed = 0      '1 = left, 2 = right
      IF JustPressed = 1 THEN HScrollPB ScrollBar.X1, ScrollBar.Y1 + 1, ScrollBar.X1 + SBWidth - 1, ScrollBar.Y2 - 1, 2
      IF JustPressed = 2 THEN HScrollPB ScrollBar.X2 - SBWidth, ScrollBar.Y1 + 1, ScrollBar.X2, ScrollBar.Y2 - 1, 2
      JustPressed = 0
      GOSUB CleanUp
      EXIT SUB
    END IF
  END IF
  LastPointerPos = ScrollBar.PointerPos

  LargeChange = 0

  SELECT CASE LEN(Ky$)
    CASE 0
      Ky = 0
    CASE 1
      Ky = ASC(Ky$)
    CASE 2
      Ky = -ASC(RIGHT$(Ky$, 1))
  END SELECT

  IF Button = 1 THEN

    IF x < ScrollBar.X1 OR x > ScrollBar.X2 OR y < ScrollBar.Y1 OR y > ScrollBar.Y2 THEN
      Button = 1000
      HideCursor
      LineBFVE ScrollBar.PointerPos + 2, ScrollBar.Y1 + 2, ScrollBar.PointerPos + SBWidth - 4, ScrollBar.Y2 - 3, TopClr
      ShowCursor
      GOSUB CleanUp
      ScrollBar.PointerPos = ScrollBar.Value / ScrollBar.Factor + ScrollBar.X1 + SBWidth


      EXIT SUB
    END IF

    IF y > ScrollBar.Y1 AND y < ScrollBar.Y2 AND NOT OnPointer THEN
      LastX = x
      SELECT CASE x
	CASE IS < ScrollBar.X1 + SBWidth                   'Left scroll button
	  IF JustPressed = 0 THEN
	    Pressed = -1
	    JustPressed = 1
	    GOSUB CleanUp
	    Mouse 7, 0, x, x
	    Mouse 8, 0, y, y
	    HScrollPB ScrollBar.X1, ScrollBar.Y1 + 1, ScrollBar.X1 + SBWidth - 1, ScrollBar.Y2 - 1, 1
	    Clicked = 1
	  END IF
	  Ky = -75

	CASE IS > ScrollBar.X2 - SBWidth + 1               'Right scroll button
	  Ky = -77
	  IF JustPressed = 0 THEN
	    Pressed = -1
	    JustPressed = 2
	    GOSUB CleanUp
	    Mouse 7, 0, x, x
	    Mouse 8, 0, y, y
	    HScrollPB ScrollBar.X2 - SBWidth, ScrollBar.Y1 + 1, ScrollBar.X2, ScrollBar.Y2 - 1, 1
	    Clicked = 1
	  END IF

	   'On Pointer
	CASE IS < ScrollBar.PointerPos + SBWidth AND x > ScrollBar.PointerPos
	  IF NOT OnPointer THEN
	    GOSUB CleanUp
	    Pressed = -1
	    OnPointer = -1
	    PointerOS = x - ScrollBar.PointerPos
	    Mouse 7, 0, ScrollBar.X1 + SBWidth + PointerOS, ScrollBar.X2 - SBWidth - 1 - (SBWidth - PointerOS)
	    Mouse 8, 0, y, y
	  END IF

	CASE IS > ScrollBar.PointerPos + SBWidth             'Page right
	  IF NOT Left THEN

	    IF Clicked = 0 THEN
	      ButtonClicked& = PDQTimer& + 3
	      XValue& = (x - ScrollBar.X1 - SBWidth - 8) * ScrollBar.Factor + ScrollBar.Lo'Cursor pos
	      ScrollBar.Value = MinLong&(ScrollBar.Value + ScrollBar.LargeChange, XValue&)
	      Clicked = 1
	    ELSE
	      IF PDQTimer& > ButtonClicked& THEN
		XValue& = (x - ScrollBar.X1 - SBWidth - 8) * ScrollBar.Factor + ScrollBar.Lo'Cursor pos
		ScrollBar.Value = MinLong&(ScrollBar.Value + ScrollBar.LargeChange, XValue&)
		ButtonClicked& = PDQTimer&
	      END IF
	    END IF

	    Rite = -1
	    Pressed = -1
	    LargeChange = -1
	  END IF

	CASE IS < ScrollBar.PointerPos AND NOT OnPointer     'Page left
	  IF NOT Rite THEN
	    IF Clicked = 0 THEN
	      ButtonClicked& = PDQTimer& + 3
	      XValue& = (x - ScrollBar.X1 - SBWidth - 8) * ScrollBar.Factor + ScrollBar.Lo
	      ScrollBar.Value = MaxLong&(ScrollBar.Value - ScrollBar.LargeChange, XValue&)
	      Clicked = 1
	    ELSE
	      IF PDQTimer& > ButtonClicked& THEN
		XValue& = (x - ScrollBar.X1 - SBWidth - 8) * ScrollBar.Factor + ScrollBar.Lo
		ScrollBar.Value = MaxLong&(ScrollBar.Value - ScrollBar.LargeChange, XValue&)
		ButtonClicked& = PDQTimer&
	      END IF
	    END IF

	    Left = -1
	    Pressed = -1
	    LargeChange = -1
	  END IF

      END SELECT
    END IF

  END IF

  IF Ky THEN
    SELECT CASE Ky
      CASE -75          'Left, Up arrow
	IF Clicked THEN
	  IF Clicked = 1 THEN
	    ScrollBar.Value = ScrollBar.Value - ScrollBar.SmallChange
	    ButtonClicked& = PDQTimer& + 3
	    Clicked = 2
	  ELSE
	    IF PDQTimer& > ButtonClicked& THEN
	      ButtonClicked& = PDQTimer&
	      ScrollBar.Value = ScrollBar.Value - ScrollBar.SmallChange
	    END IF
	  END IF
	ELSE
	  ScrollBar.Value = ScrollBar.Value - ScrollBar.SmallChange
	END IF

      CASE -77          'Right, Down Arrow

	IF Clicked THEN
	  IF Clicked = 1 THEN
	    ScrollBar.Value = ScrollBar.Value + ScrollBar.SmallChange
	    ButtonClicked& = PDQTimer& + 3
	    Clicked = 2
	  ELSE
	    IF PDQTimer& > ButtonClicked& THEN
	      ButtonClicked& = PDQTimer&
	      ScrollBar.Value = ScrollBar.Value + ScrollBar.SmallChange
	    END IF
	  END IF
	ELSE
	  ScrollBar.Value = ScrollBar.Value + ScrollBar.SmallChange
	END IF

      CASE -115, -141, -73 'Left, Up + Control
	ScrollBar.Value = ScrollBar.Value - ScrollBar.LargeChange
       

      CASE -116, -145, -81 'Right, Down + Control
	ScrollBar.Value = ScrollBar.Value + ScrollBar.LargeChange

      CASE -79         'End
	ScrollBar.Value = 32767

      CASE -71         'Home
	 ScrollBar.Value = -32768
      
    END SELECT

  END IF

  IF OnPointer THEN
    ScrollBar.PointerPos = x - PointerOS
    ScrollBar.Value = (ScrollBar.PointerPos - ScrollBar.X1 - SBWidth) * ScrollBar.Factor + ScrollBar.Lo
  END IF

  IF ScrollBar.Value > ScrollBar.Hi THEN ScrollBar.Value = ScrollBar.Hi
  IF ScrollBar.Value < ScrollBar.Lo THEN ScrollBar.Value = ScrollBar.Lo
  IF NOT OnPointer THEN ScrollBar.PointerPos = (ScrollBar.Value - ScrollBar.Lo) / ScrollBar.Factor + ScrollBar.X1 + SBWidth

  IF ScrollBar.PointerPos > ScrollBar.X2 - (2 * SBWidth + 1) THEN ScrollBar.PointerPos = ScrollBar.X2 - (2 * SBWidth + 1)
  IF ScrollBar.PointerPos < ScrollBar.X1 + SBWidth THEN ScrollBar.PointerPos = ScrollBar.X1 + SBWidth

  IF Rite AND NOT OnPointer THEN       'Right
    IF ScrollBar.PointerPos + SBWidth < ScrollBar.X2 - SBWidth - 1 THEN
      HideCursor
      LineBFVE ScrollBar.PointerPos + SBWidth + 1, ScrollBar.Y1 + 1, ScrollBar.X2 - SBWidth - 1, ScrollBar.Y2 - 1, PageClr
      ShowCursor
    END IF
  END IF

  IF Left AND NOT OnPointer THEN       'Left
    IF ScrollBar.PointerPos > ScrollBar.X1 + SBWidth THEN
      HideCursor
      LineBFVE ScrollBar.X1 + SBWidth, ScrollBar.Y1 + 1, ScrollBar.PointerPos - 1, ScrollBar.Y2 - 1, PageClr
      ShowCursor
    END IF
  END IF

  IF ScrollBar.PointerPos <> LastPointerPos THEN
    T& = T& - 16
    MoveHPtr ScrollBar.PointerPos, ScrollBar.Y1, LastPointerPos, ScrollBar.Y2
    LastPointerPos = ScrollBar.PointerPos
  END IF

  IF ScrollBar.Blink THEN
    IF PDQTimer& > T& + 9 AND (Button = 0 AND NOT LargeChange) THEN
    'IF PDQTimer& > T& + 9 AND Ky = 0 AND NOT LargeChange THEN
      T& = PDQTimer&

      Blnk = NOT Blnk
      BlinkClr = TopClr
      IF Blnk THEN BlinkClr = ShadeClr

      HideCursor
      FOR i = ScrollBar.PointerPos + 2 TO ScrollBar.PointerPos + SBWidth - 3 STEP 2
	LINE (i, ScrollBar.Y1 + 2)-(i, ScrollBar.Y2 - 3), BlinkClr, , 21845
	LINE (i + 1, ScrollBar.Y1 + 2)-(i + 1, ScrollBar.Y2 - 3), BlinkClr, , -21846
      NEXT
      ShowCursor
    END IF
  END IF

EXIT SUB

CleanUp:
  IF Rite THEN                                            'Rite and down
    Rite = 0
    IF ScrollBar.PointerPos + SBWidth < ScrollBar.X2 - SBWidth - 1 THEN
      HideCursor
      LineBFVE ScrollBar.PointerPos + SBWidth + 1, ScrollBar.Y1 + 1, ScrollBar.X2 - SBWidth - 1, ScrollBar.Y2 - 1, SlideClr
      ShowCursor
    END IF
  END IF

  IF Left THEN                                            'Left and Up
    Left = 0
    IF ScrollBar.PointerPos > ScrollBar.X1 + SBWidth THEN
      HideCursor
      LineBFVE ScrollBar.X1 + SBWidth, ScrollBar.Y1 + 1, ScrollBar.PointerPos - 1, ScrollBar.Y2 - 1, SlideClr
      ShowCursor
    END IF
  END IF

  IF Clean THEN
    HideCursor
    LineBFVE ScrollBar.PointerPos + 2, ScrollBar.Y1 + 2, ScrollBar.PointerPos + SBWidth - 3, ScrollBar.Y2 - 3, TopClr
    ShowCursor
  END IF

RETURN
END SUB

SUB HScrollPB (X1, Y1, X2, Y2, Action)

  'Action = 0 Draw button
  'Action = 1 Depress button
  'Action = 2 Raise button

  Hite = (Y2 - Y1)
  Depth = Hite \ 12

  Depth1 = Depth / 2
  IF Depth1 = 0 THEN Depth1 = 1

  HideCursor

  SELECT CASE Action

    CASE 0
      LineBFVE X1, Y1, X2, Y2, TopClr
      LineBVE X1, Y1, X2, Y2, 0

    CASE 1
      REDIM PB(GArraySize&(X1 + 2, Y1 + Depth1 + 1, X2 - 3, Y2 - Depth - 2) \ 2)
      GET (X1 + 2, Y1 + Depth1 + 1)-(X2 - 3, Y2 - Depth - 2), PB
      LineBFVE X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, TopClr
      LineVE X1 + 1, Y1 + 1, X1 + 1, Y2 - 1, ShadeClr
      LineBFVE X1 + 1, Y1 + 1, X2 - 1, Y1 + Depth1, ShadeClr
      PUT (X1 + 4, Y1 + Depth1 + 3), PB, PSET

    CASE 2
      PUT (X1 + 2, Y1 + Depth1 + 1), PB, PSET

  END SELECT

  IF Action <> 1 THEN
    IF Hite > 8 THEN
      LineBFVE X1 + 1, Y1 + 1, X2 - 1, Y1 + Depth1, HiClr           'Bot Shadow
      LineBFVE X1 + 1, Y2 - Depth - 1, X2 - 1, Y2 - 1, ShadeClr 'Top HiLite
    END IF
  
    LineBFVE X1 + 1, Y1 + 1, X1 + 1, Y2 - Depth1 - 1, HiClr  'Left HiLite
    LineBFVE X2 - 2, Y1 + 1 + Depth1, X2 - 1, Y2 - 1, ShadeClr     'Rite Shadow
  END IF

  ShowCursor

END SUB

SUB HVScrollBar (X1, Y1, X2, Y2, HV, HiWhite, White, Gray, SlideClr)

  TopClr = White
  HiClr = HiWhite
  ShadeClr = Gray

  Size = 1
  C = SBWidth \ 2
  LineBFVE X1, Y1, X2, Y2, SlideClr    'White
  LineBVE X1, Y1, X2, Y2, 0


  IF HV > 0 THEN
    HV = HV - 10
    Pointer = -1
  END IF

  IF HV THEN                              'Vertical
    BSize = X2 - X1
    xc = X1 + (BSize \ 2) - 1
    
    VScrollPB X1 + 1, Y1, X2 - 1, Y1 + SBWidth - 1, 0
    CALL Arrows(xc, Y1 + C - 4, BSize - 10, 2)
    VScrollPB X1 + 1, Y2 - SBWidth, X2 - 1, Y2, 0
    CALL Arrows(xc, Y2 - C + 3, BSize - 10, 4)
    IF NOT Pointer THEN PushButton1 X1, Y1 + SBWidth, X2, Y1 + 2 * SBWidth, Size, White, Gray, HiWhite

  ELSE                                    'Horizontal
    BHite = Y2 - Y1 + 1
    yc = Y1 + BHite \ 2 - 1

    HScrollPB X1, Y1 + 1, X1 + SBWidth - 1, Y2 - 1, 0
    CALL Arrows(X1 + C - 5, yc, BHite - 12, 1)
    HScrollPB X2 - SBWidth, Y1 + 1, X2, Y2 - 1, 0
    CALL Arrows(X2 - C + 3, yc, BHite - 12, 3)
    IF NOT Pointer THEN PushButton1 X1 + SBWidth, Y1, X1 + 2 * SBWidth, Y2, Size, White, Gray, HiWhite

  END IF

END SUB

SUB MoveHPtr (X1, Y1, LastX, Y2)

  HideCursor
  IF X1 > LastX THEN              'Going rite
    LineBFVE LastX, Y1 + 1, X1 - 1, Y2 - 1, SlideClr
  ELSEIF X1 <> LastX THEN         'Going left
    LineBFVE X1 + SBWidth + 1, Y1 + 1, LastX + SBWidth, Y2 - 1, SlideClr
  END IF

  PushButton1 X1, Y1, X1 + SBWidth, Y2, 1, TopClr, ShadeClr, HiClr
  ShowCursor

END SUB

SUB MoveVPtr (X1, Y1, X2, LastY)

  HideCursor
  IF LastY > Y1 THEN   'Going up
    LineBFVE X1 + 1, Y1 + SBWidth + 1, X2 - 1, LastY + SBWidth, SlideClr
  ELSEIF Y1 <> LastY THEN
    LineBFVE X1 + 1, Y1, X2 - 1, LastY, SlideClr
  END IF

  PushButton1 X1, Y1, X2, Y1 + SBWidth, 1, TopClr, ShadeClr, HiClr
  ShowCursor

END SUB

SUB Pressbutton (X1, Y1, X2, Y2)

  HideCursor

  TopClr = GetPointVE(X1 + 3, Y1 + 3)
  ShadeClr = GetPointVE(X2 - 1, Y2 - 1)
  HiClr = GetPointVE(X1 + 1, Y1 + 1)

  REDIM PB(GArraySize&(X1 + 3, Y1 + 3, X2 - 3, Y2 - 3) \ 2)

  GET (X1 + 3, Y1 + 3)-(X2 - 3, Y2 - 3), PB
  PUT (X1 + 5, Y1 + 5), PB, PSET
  LineVE X1 + 1, Y1 + 1, X2 - 2, Y1 + 1, ShadeClr
  LineVE X1 + 1, Y1 + 1, X1 + 1, Y2 - 1, ShadeClr
  LineBFVE X1 + 2, Y1 + 2, X2 - 1, Y1 + 4, TopClr
  LineBFVE X1 + 2, Y1 + 3, X1 + 4, Y2 - 1, TopClr

  ShowCursor

END SUB

SUB PushButton1 (X1, Y1, X2, Y2, Size, White, Gray, HiWhite)

  OS = Size = 1

  LineBFVE X1 + 2, Y1 + 2, X2 - 3, Y2 - 3, White
  LineBVE X1, Y1, X2, Y2, 0

  LineBFVE X1 + 1, Y2 - Size + OS, X2 - 1, Y2 - 1, Gray
  LineBFVE X2 - Size + OS, Y1 + 1, X2 - 1, Y2 - 1, Gray

  FOR i = 1 TO Size
    LineVE X1 + 1, Y1 + i, X2 - i - 1, Y1 + i, HiWhite
    LineVE X1 + i, Y1 + 1, X1 + i, Y2 - i - 1, HiWhite
  NEXT

END SUB

SUB ScrollPause (Delay)

  T& = PDQTimer&
  DO
    IF PDQTimer& > T& + Delay THEN EXIT DO
  LOOP

END SUB

SUB SetPBColors (Fld AS ScrollBar, HV)

  HideCursor

  IF HV THEN           'Vertical scrollbar
    Center = Fld.X1 + ((Fld.X2 - Fld.X1 + 1) \ 2)
    HiClr = GetPointVE(Center, Fld.Y1 + 1)
    TopClr = GetPointVE(Center, Fld.Y1 + 3)
    ShadeClr = GetPointVE(Center, Fld.Y2 - 2)

    IF Fld.PointerPos > Fld.Y1 + SBWidth THEN
      SlideClr = GetPointVE(Center, Fld.Y1 + SBWidth)
    ELSE
      SlideClr = GetPointVE(Center, Fld.Y2 - SBWidth - 1)
    END IF

  ELSE                 'Horizontal scroll, Push button
    Center = Fld.Y1 + ((Fld.Y2 - Fld.Y1 + 1) \ 2) - 3
    
    HiClr = GetPointVE(Fld.X1 + 1, Center)
    TopClr = GetPointVE(Fld.X1 + 3, Center)
    ShadeClr = GetPointVE(Fld.X2 - 1, Center)

    IF Fld.PointerPos > Fld.X1 + SBWidth THEN    'If not all the way left
      SlideClr = GetPointVE(Fld.X1 + SBWidth, Center)
    ELSE
      SlideClr = GetPointVE(Fld.X2 - SBWidth - 1, Center)
    END IF
    
  END IF

  IF GPDat(100) > 15 THEN
    PageClr = SlideClr
  ELSEIF GPDat(100) > 0 THEN
    PageClr = GPDat(100)
  ELSE
    PageClr = ShadeClr
  END IF

  ShowCursor

END SUB

SUB SReleaseButton (X1, Y1, X2, Y2)

  HideCursor

  PUT (X1 + 3, Y1 + 3), PB, PSET
			  'Draw Shadow
  LineBFVE X1 + 1, Y2 - 2, X2 - 1, Y2 - 1, ShadeClr
  LineBFVE X2 - 1, Y1 + 1, X2 - 2, Y2 - 1, ShadeClr
			  'Draw HiLite
  LineVE X1 + 1, Y1 + 1, X2 - 1, Y1 + 1, HiClr
  LineVE X1 + 1, Y1 + 1, X1 + 1, Y2 - 1, HiClr
			 'Draw Hilite
  LineVE X1 + 2, Y1 + 2, X2 - 2, Y1 + 2, HiClr
  LineVE X1 + 2, Y1 + 2, X1 + 2, Y2 - 2, HiClr

  ShowCursor


END SUB

SUB VScrollBar (Button, x, y, Ky$, ScrollBar AS ScrollBar, Clean) STATIC

  IF Clean THEN
    ScrollBar.PointerPos = (ScrollBar.Value - ScrollBar.Lo) / ScrollBar.Factor + ScrollBar.Y1 + SBWidth
    GOSUB VCleanUp
    EXIT SUB
  END IF
  
  IF ScrollBar.Action = 1 THEN
    ScrollBar.Action = 3
    T& = -10000
  END IF

  IF Button = 0 THEN
    IF Pressed THEN
      Clicked = 0
      T& = T& - 1000
      Mouse 7, 0, 0, 639
      Mouse 8, 0, 0, 479
      OnPointer = 0
      Pressed = 0      '1 = up, 2 = down
      IF JustPressed = 1 THEN VScrollPB ScrollBar.X1 + 1, ScrollBar.Y1, ScrollBar.X2 - 1, ScrollBar.Y1 + SBWidth - 1, 2
      IF JustPressed = 2 THEN VScrollPB ScrollBar.X1 + 1, ScrollBar.Y2 - SBWidth, ScrollBar.X2 - 1, ScrollBar.Y2, 2
      JustPressed = 0
      GOSUB VCleanUp
      EXIT SUB
    END IF
  END IF
  LastPointerPos = ScrollBar.PointerPos

  LargeChange = 0

  SELECT CASE LEN(Ky$)
    CASE 0
      Ky = 0
    CASE 1
      Ky = ASC(Ky$)
    CASE 2
      Ky = -ASC(RIGHT$(Ky$, 1))
  END SELECT

  IF Button = 1 THEN
   IF x < ScrollBar.X1 OR x > ScrollBar.X2 OR y < ScrollBar.Y1 OR y > ScrollBar.Y2 THEN
     Button = 1000
     HideCursor
     LineBFVE ScrollBar.X1 + 2, ScrollBar.PointerPos + 2, ScrollBar.X2 - 3, ScrollBar.PointerPos + SBWidth - 4, TopClr
     ShowCursor
     GOSUB VCleanUp
     EXIT SUB
   END IF

    IF x > ScrollBar.X1 AND x < ScrollBar.X2 AND NOT OnPointer THEN
      LastY = y

      SELECT CASE y
	CASE IS < ScrollBar.Y1 + SBWidth                                 'Up scroll button
	  IF JustPressed = 0 THEN
	    Pressed = -1
	    JustPressed = 1
	    GOSUB VCleanUp
	    Mouse 7, 0, x, x
	    Mouse 8, 0, y, y
	    VScrollPB ScrollBar.X1 + 1, ScrollBar.Y1, ScrollBar.X2 - 1, ScrollBar.Y1 + SBWidth - 1, 1
	    Clicked = 1
	  END IF
	  Ky = -72

	CASE IS > ScrollBar.Y2 - SBWidth + 1                             'Down scroll button
	  IF JustPressed = 0 THEN
	    Pressed = -1
	    JustPressed = 2
	    GOSUB VCleanUp
	    Mouse 7, 0, x, x
	    Mouse 8, 0, y, y
	    VScrollPB ScrollBar.X1 + 1, ScrollBar.Y2 - SBWidth, ScrollBar.X2 - 1, ScrollBar.Y2, 1
	    Clicked = 1
	  END IF
	  Ky = -80

	CASE IS < ScrollBar.PointerPos + SBWidth AND y > ScrollBar.PointerPos          'On Pointer
	  IF NOT OnPointer THEN
	    GOSUB VCleanUp
	    Pressed = -1
	    OnPointer = -1
	    PointerOS = y - ScrollBar.PointerPos
	    Mouse 7, 0, x, x
	    Mouse 8, 0, ScrollBar.Y1 + SBWidth + PointerOS, ScrollBar.Y2 - SBWidth - 1 - (SBWidth - PointerOS)
	  END IF

	CASE IS > ScrollBar.PointerPos + SBWidth             'Page down
	  IF NOT Up THEN
	    IF Clicked = 0 THEN
	      ButtonClicked& = PDQTimer& + 3
	      YValue& = (y - ScrollBar.Y1 - SBWidth - 8) * ScrollBar.Factor + ScrollBar.Lo
	      ScrollBar.Value = MinLong(ScrollBar.Value + ScrollBar.LargeChange, YValue&)
	      Clicked = 1
	    ELSE
	      IF PDQTimer& > ButtonClicked& THEN
		YValue& = (y - ScrollBar.Y1 - SBWidth - 8) * ScrollBar.Factor + ScrollBar.Lo
		ScrollBar.Value = MinLong(ScrollBar.Value + ScrollBar.LargeChange, YValue&)
		ButtonClicked& = PDQTimer&
	      END IF
	    END IF

	    Down = -1
	    Pressed = -1
	    LargeChange = -1

	  END IF

	CASE IS < ScrollBar.PointerPos AND NOT OnPointer     'Page up
	  IF NOT Down THEN
	    IF Clicked = 0 THEN
	      ButtonClicked& = PDQTimer& + 3
	      YValue& = (y - ScrollBar.Y1 - SBWidth - 8) * ScrollBar.Factor + ScrollBar.Lo
	      ScrollBar.Value = MaxLong(ScrollBar.Value - ScrollBar.LargeChange, YValue&)
	      Clicked = 1
	    ELSE
	      IF PDQTimer& > ButtonClicked& THEN
		YValue& = (y - ScrollBar.Y1 - SBWidth - 8) * ScrollBar.Factor + ScrollBar.Lo
		ScrollBar.Value = MaxLong(ScrollBar.Value - ScrollBar.LargeChange, YValue&)
		ButtonClicked& = PDQTimer&
	      END IF
	    END IF

	    Up = -1
	    Pressed = -1
	    LargeChange = -1

	  END IF

      END SELECT
    END IF

  END IF

  IF Ky THEN
    SELECT CASE Ky
      CASE -72         'Up arrow
	IF Clicked THEN
	  IF Clicked = 1 THEN
	    ScrollBar.Value = ScrollBar.Value - ScrollBar.SmallChange
	    ButtonClicked& = PDQTimer& + 3
	    Clicked = 2
	  ELSE
	    IF PDQTimer& > ButtonClicked& THEN
	      ButtonClicked& = PDQTimer&
	      ScrollBar.Value = ScrollBar.Value - ScrollBar.SmallChange
	    END IF
	  END IF
	ELSE
	  ScrollBar.Value = ScrollBar.Value - ScrollBar.SmallChange
	END IF

      CASE -80         'Down Arrow
	IF Clicked THEN
	  IF Clicked = 1 THEN
	    ScrollBar.Value = ScrollBar.Value + ScrollBar.SmallChange
	    ButtonClicked& = PDQTimer& + 3
	    Clicked = 2
	  ELSE
	    IF PDQTimer& > ButtonClicked& THEN
	      ButtonClicked& = PDQTimer&
	      ScrollBar.Value = ScrollBar.Value + ScrollBar.SmallChange
	    END IF
	  END IF
	ELSE
	  ScrollBar.Value = ScrollBar.Value + ScrollBar.SmallChange
	END IF

      CASE -115, -141, -73 'Control + Left, Up
	ScrollBar.Value = ScrollBar.Value - ScrollBar.LargeChange

      CASE -116, -145, -81 'Control + Right, Down
	ScrollBar.Value = ScrollBar.Value + ScrollBar.LargeChange

      CASE -79         'End
	ScrollBar.Value = 32767

      CASE -71         'Home
	 ScrollBar.Value = -32768

    END SELECT

  END IF

  IF OnPointer THEN
    ScrollBar.PointerPos = y - PointerOS
    ScrollBar.Value = (ScrollBar.PointerPos - ScrollBar.Y1 - SBWidth) * ScrollBar.Factor + ScrollBar.Lo
  END IF

  IF ScrollBar.Value > ScrollBar.Hi THEN ScrollBar.Value = ScrollBar.Hi
  IF ScrollBar.Value < ScrollBar.Lo THEN ScrollBar.Value = ScrollBar.Lo
  IF NOT OnPointer THEN ScrollBar.PointerPos = (ScrollBar.Value - ScrollBar.Lo) / ScrollBar.Factor + ScrollBar.Y1 + SBWidth

  IF ScrollBar.PointerPos > ScrollBar.Y2 - (2 * SBWidth + 1) THEN ScrollBar.PointerPos = ScrollBar.Y2 - (2 * SBWidth + 1)
  IF ScrollBar.PointerPos < ScrollBar.Y1 + SBWidth THEN ScrollBar.PointerPos = ScrollBar.Y1 + SBWidth

  IF Down AND NOT OnPointer THEN       'Down
    IF ScrollBar.PointerPos + SBWidth < ScrollBar.Y2 - SBWidth - 1 THEN
      HideCursor
      LineBFVE ScrollBar.X1 + 1, ScrollBar.PointerPos + SBWidth + 1, ScrollBar.X2 - 1, ScrollBar.Y2 - SBWidth - 1, PageClr 'ShadeClr
      ShowCursor
    END IF
  END IF

  IF Up AND NOT OnPointer THEN         'Up
    IF ScrollBar.PointerPos > ScrollBar.Y1 + SBWidth THEN
      HideCursor
      LineBFVE ScrollBar.X1 + 1, ScrollBar.Y1 + SBWidth, ScrollBar.X2 - 1, ScrollBar.PointerPos - 1, PageClr  'ShadeClr
      ShowCursor
    END IF
  END IF

  IF ScrollBar.PointerPos <> LastPointerPos THEN
    T& = T& - 1000
    MoveVPtr ScrollBar.X1, ScrollBar.PointerPos, ScrollBar.X2, LastPointerPos
    LastPointerPos = ScrollBar.PointerPos
  END IF

  IF ScrollBar.Blink THEN
    IF PDQTimer& > T& + 9 AND (Button = 0 AND NOT LargeChange) THEN
      T& = PDQTimer&

      Blnk = NOT Blnk
      BlinkClr = TopClr
      IF Blnk THEN BlinkClr = ShadeClr

      HideCursor
      FOR i = ScrollBar.PointerPos + 2 TO ScrollBar.PointerPos + SBWidth - 3 STEP 2
	LINE (ScrollBar.X1 + 2, i)-(ScrollBar.X2 - 3, i), BlinkClr, , 21845  '8738'13107 '21845
	LINE (ScrollBar.X1 + 2, i + 1)-(ScrollBar.X2 - 3, i + 1), BlinkClr, , -21846' -30584'-13108  '21846
      NEXT
      ShowCursor
    END IF
  END IF

EXIT SUB

VCleanUp:
 
  IF Down THEN                                            'Rite and down
    Down = 0
    IF ScrollBar.PointerPos + SBWidth < ScrollBar.Y2 - SBWidth - 1 THEN
      HideCursor
      LineBFVE ScrollBar.X1 + 1, ScrollBar.PointerPos + SBWidth + 1, ScrollBar.X2 - 1, ScrollBar.Y2 - SBWidth - 1, SlideClr
      ShowCursor
    END IF
  END IF

  IF Up THEN 'AND ScrollBar.PointerPos <> ScrollBar.X1 + 66 THEN    'Left and Up
    Up = 0
    IF ScrollBar.PointerPos > ScrollBar.Y1 + SBWidth THEN
      HideCursor
      LineBFVE ScrollBar.X1 + 1, ScrollBar.Y1 + SBWidth, ScrollBar.X2 - 1, ScrollBar.PointerPos - 1, SlideClr
      ShowCursor
    END IF

  END IF

  IF Clean THEN
    HideCursor
    LineBFVE ScrollBar.X1 + 2, ScrollBar.PointerPos + 2, ScrollBar.X2 - 3, ScrollBar.PointerPos + SBWidth - 3, TopClr
    ShowCursor
  END IF

RETURN

END SUB

SUB VScrollPB (X1, Y1, X2, Y2, Action)

  'Action = 0 Draw button
  'Action = 1 Depress button
  'Action = 2 Raise button

  Wide = (X2 - X1 + 1)
  Depth = Wide \ 12

  Depth1 = Depth / 2
  IF Depth1 = 0 THEN Depth1 = 1

  HideCursor

  SELECT CASE Action

    CASE 0
      LineBFVE X1, Y1, X2, Y2, TopClr
      LineBVE X1, Y1, X2, Y2, 0

    CASE 1
      REDIM PB(GArraySize&(X1 + Depth1 + 2, Y1 + 2, X2 - Depth - 2, Y2 - 3) \ 2)
      GET (X1 + Depth1 + 2, Y1 + 2)-(X2 - Depth - 2, Y2 - 3), PB
      LineBFVE X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, TopClr
      PUT (X1 + Depth1 + 4, Y1 + 4), PB, PSET
      LineBFVE X1 + 1, Y1 + 1, X1 + Depth1, Y2 - 1, ShadeClr
      LineVE X1 + 1, Y1 + 1, X2 - 1, Y1 + 1, ShadeClr

    CASE 2
      LineBFVE X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, TopClr
      PUT (X1 + Depth1 + 2, Y1 + 2), PB, PSET

  END SELECT
  
  IF Action <> 1 THEN
    LineBFVE X1 + 1, Y2 - 2, X2 - 1, Y2 - 1, ShadeClr                  'Bot Shadow
    IF Wide > 8 THEN
      LineBFVE X2 - Depth - 1, Y1 + 1, X2 - 1, Y2 - 1, ShadeClr        'Rite Shadow
      LineBFVE X1 + 1, Y1 + 2, X1 + Depth1, Y2 - 2, HiClr          'Left HiLite
    END IF
    LineVE X1 + 1, Y1 + 1, X2 - 2 - (Depth / 2), Y1 + 1, HiClr     'Top HiLite
  END IF
  ShowCursor

END SUB

