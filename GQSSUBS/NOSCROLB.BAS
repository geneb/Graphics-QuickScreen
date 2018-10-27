DEFINT A-Z

DECLARE FUNCTION GetFactor# (X1, X2, Lo#, Hi#)
DIM SHARED TopClr, HiClr, ShadeClr, SlideClr

'$INCLUDE: 'ScrollB.bi'                'Defines TYPE for sliders
'$INCLUDE: 'Common.bi'

REDIM SHARED PB(0)

FUNCTION GetFactor# (X1, X2, Lo#, Hi#)

   Wide# = (X2 - SBWidth - 1) - (X1 + 2 * SBWidth)
   Range# = Hi# - Lo#
   GetFactor# = Range# / Wide#

END FUNCTION

SUB HScrollBar (Button, X, Y, Ky$, Scrollbar AS Scrollbar, Clean) STATIC


END SUB

SUB HVScrollBar (X1, y1, X2, y2, HV, HiWhite, White, Gray, SliderClr)

END SUB

SUB MoveHPtr (X1, y1, lastX, y2)

END SUB

SUB MoveVPtr (X1, y1, X2, LastY)

END SUB

SUB SetPBColors (Fld AS Scrollbar, HV)

END SUB

SUB VScrollBar (Button, X, Y, Ky$, Scrollbar AS Scrollbar, Clean) STATIC


END SUB

