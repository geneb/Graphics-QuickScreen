DEFINT A-Z

'by Phil Cramer
'Font routines by Brian Giedt
'Copyright (c) 1992 Crescent Software

'---- BASIC subs
DECLARE SUB CrossHair (x%, y%)
DECLARE SUB CursorXY (x%, y%, Button%, Ky%)
DECLARE SUB DrawText (Xx%, Yy%, Text$, Angle%, Colr%, TextSize#)
DECLARE SUB DrawChar (x%, y%, Letter%, Colr%, Size#, Expand%)
DECLARE SUB DrawCharA (x%, y%, SAngle#, CAngle#, Letter%, Colr%, Size#, Expand%)
DECLARE SUB GDialog (Choice%, DLog() AS ANY, Response$(), Menu$(), Style%, Action%)
DECLARE SUB GetCursor (x, y, Button)
DECLARE SUB HideCursor ()
DECLARE SUB LineVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB LineBVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB LoadFont (FontFile$)
DECLARE SUB LoadFiles (Spec$, FileMenu$(), Found%)
DECLARE SUB SetGPFont (FontNumber%)
DECLARE SUB SRScreen (SaveRest)
DECLARE SUB ShowCursor ()
DECLARE SUB TextBox (x1%, y1%, x2%, y2%)
DECLARE SUB WaitUp ()
DECLARE SUB WaitUpKey ()

'---- ASM functions
DECLARE FUNCTION Exist% (Filename$)
DECLARE FUNCTION LongestStr% (Array$())
DECLARE FUNCTION MaxInt% (A, B)
DECLARE FUNCTION MinInt% (A, B)
DECLARE FUNCTION QPTrim$ (A$)
DECLARE FUNCTION WhichError ()

'---- BASIC functions
DECLARE FUNCTION ErrorMsg$ (Er)
DECLARE FUNCTION GetWidth% (Text$)
DECLARE FUNCTION GQSPath$ ()
DECLARE FUNCTION MsgBox (A$(), Opts)
DECLARE FUNCTION NoExtension$ (F$)

'$INCLUDE: 'DlogType.Bi'
'$INCLUDE: 'Common.GQS'

SUB DrawChar (x, y, Letter, Colr, Size#, Expand) STATIC

  Shift = INT(Size# - .1)
  IF GPDat%(23) THEN
     Shift = Shift + INT(Size#) \ 3 + 1
  END IF
  i = 1
  Char$ = Font$(Letter, GPDat%(34))    'Obtain the font in a local variable
  L = LEN(Char$)                        'and length of the string
  WHILE i < L
      P = ASC(MID$(Char$, i, 1))        'Break the first byte into two 4 bit
      x1 = (P MOD 16) * Size#           'values showing x,y positioning of
      y1 = (P \ 16) * Size#             'the first point on a grid 16x16
      Q = ASC(MID$(Char$, i + 1, 1))    'Do the same with the second number
      x2 = (Q MOD 16) * Size#
      y2 = (Q \ 16) * Size#
      'Draw the connecting line
      LINE (x1 + x, y1 + y)-(x2 + Shift + x, y2 + Shift + y), Colr, BF
      'position two bytes forward in the font string
      i = i + 2
  WEND

END SUB

SUB DrawCharA (x, y, SAngle#, CAngle#, Letter, Colr, Size#, Expand) STATIC

  'SEE DRAWCHAR FOR COMMENTS
  Shift# = INT(Size# - .5)
  IF GPDat%(23) THEN
     Shift# = Shift# + INT(Size#) \ 3 + 1
  END IF
  IF Expand THEN Shift# = Shift# + 1 + INT(Size# / 10)
  i = 1
  Char$ = Font$(Letter, GPDat%(34))
  L = LEN(Char$)
  WHILE i < L
      P = ASC(MID$(Char$, i, 1))
      P1 = (P MOD 16) * Size#
      P2 = (P \ 16) * Size#
      Q = ASC(MID$(Char$, i + 1, 1))
      Q1 = (Q MOD 16) * Size#
      Q2 = (Q \ 16) * Size#
      IF GPDat%(24) <> 90 THEN
	x1 = P1 + P2 * CAngle# + x 'These calculations will tilt the character
	x2 = Q1 + Q2 * CAngle# + x
	y1 = P2 + y
	y2 = Q2 + y
      ELSE
	x1 = P1 * CAngle# + P2 * SAngle# + x 'These calculations will tilt the character
	x2 = Q1 * CAngle# + Q2 * SAngle# + x
	y1 = P2 * CAngle# - P1 * SAngle# + y
	y2 = Q2 * CAngle# - Q1 * SAngle# + y
      END IF
      Shift1# = Shift# * SAngle#
      'The section below will fill in the font depending on whether
      'the line is horizontal or vertical
      IF P1 <> Q1 THEN            'line is horizontal
	 FOR T = 0 TO Shift#
	     Part = T * SAngle#
	     LINE (x1 + Part, y1 + T - Part)-(x2 + Part + Shift1# + Shift#, y2 + T - Shift1# - Part), Colr
	 NEXT
      ELSEIF P2 = Q2 THEN         'just one pixel to be plotted
	 FOR T = 0 TO Shift#
	     Part = T * SAngle#
	     LINE (x1 + Part, y1 + T - Part)-(x2 + Shift1# + Shift# + Part, y2 + T - Shift1# - Part), Colr
	 NEXT
      ELSE                        'Line is vertical
	 FOR T = 0 TO Shift#
	     Part = T * SAngle#
	     LINE (x1 + T + Part, y1 - Part)-(x2 + T + Shift1# + Part, y2 - Part + Shift# - Shift1#), Colr
	 NEXT
      END IF
      i = i + 2
  WEND

END SUB

SUB DrawFont STATIC

  REDIM Menu$(0)
  REDIM Resp$(11)
  REDIM DLog(11) AS Dialog

  FPath$ = FontPath$
  IF RIGHT$(FPath$, 1) <> "\" THEN FPath$ = FPath$ + "\"

  IF NOT Exist(FPath$ + "*.GFN") THEN
    FPath$ = GQSPath$
  END IF
  
  IF NOT Exist(FPath$ + "*.GFN") THEN
    REDIM ErrMsg$(2)
    ErrMsg$(0) = "Error"
    ErrMsg$(1) = "Cannot find font (*.GFN) files"
    ErrMsg$(2) = "Change the path settings in SetPaths dialog box."
    BPress = MsgBox(ErrMsg$(), 21)
    EXIT SUB
  END IF

  LoadFiles FPath$ + "*.GFN", Menu$(), Found
  IF Font$ = "" THEN Font$ = Menu$(1)

  IF FontVSpc = 0 THEN FontVSpc = 4
  IF Scale# <= 0# THEN Scale# = 1

  Resp$(3) = Text$

  Buf$ = SPACE$(4)
  RSET Buf$ = QPTrim$(STR$(Scale#))
  Resp$(6) = Buf$

  RSET Buf$ = QPTrim$(STR$(Rotation))
  Resp$(7) = Buf$

  RSET Buf$ = QPTrim$(STR$(GPDat(24)))
  Resp$(8) = Buf$

  Buf$ = SPACE$(3)
  RSET Buf$ = QPTrim$(STR$(FontVSpc))
  Resp$(9) = Buf$

  RSET Buf$ = QPTrim$(STR$(GPDat(48)))
  Resp$(10) = Buf$

  DLog(0).Heading = "Draw Text"

  IF GPDat(71) = 8 THEN
    MdOS = 11
    MdOS1 = 7
  END IF

  FOR i = 1 TO 2
    DLog(i).DType = 6                            'Push Button
    DLog(i).Col = 56 * 8 - 2   '63
    DLog(i).NumRows = GPDat(80)
    DLog(i).TextLen = 66
  NEXT

  DLog(1).Row = 6 * GPDat(71) + GPDat(71) \ 2 + 3 - MdOS
  DLog(1).Heading = "OK"

  DLog(2).Row = 8 * GPDat(71) + GPDat(71) \ 2 + 3 - MdOS1
  DLog(2).Heading = "Cancel"
  DLog(2).HotKey = 1
  DLog(2).Group = 100

  DLog(3).DType = 8                'Text
  DLog(3).Row = 1
  DLog(3).Col = 1
  DLog(3).Heading = "Text:<1"
  DLog(3).NumRows = 4
  DLog(3).TextLen = 58
  DLog(3).Group = 20
  DLog(3).MenuChoice = 10

  '---- Font menu
  DLog(4).DType = 4                'Menu
  DLog(4).Row = 7
  DLog(4).Col = 9
  DLog(4).Heading = "Font Files:     <"
  DLog(4).NumRows = 5
  DLog(4).Group = 5
  DLog(4).MenuChoice = 1
  FOR i = 1 TO UBOUND(Menu$)
    IF LTRIM$(RTRIM$(Menu$(i))) = Font$ THEN
      DLog(4).MenuChoice = i
      EXIT FOR
    END IF
  NEXT
  
  '---- Font name
  DLog(5).DType = 3                            'Text Entry Field
  DLog(5).Row = 6
  DLog(5).Col = 27
  DLog(5).Heading = "Font:      <10"        '    "<" indicates end of Heading$
  DLog(5).NumRows = 0                          ' number indicates num only etc.
  DLog(5).TextLen = 12
  DLog(5).HotKey = 2

  Resp$(5) = Font$

  FOR i = 6 TO 10
    DLog(i).DType = 3                'Text
    DLog(i).Row = i + 1
    DLog(i).Col = 27
    DLog(i).TextLen = 4
  NEXT

  DLog(6).Heading = "Scale:     <11"
  DLog(7).Heading = "Text Angle:<13"  'Ints +-,
  DLog(7).HotKey = 2
  DLog(8).Heading = "Ital.Angle:<12"  'Ints only
  DLog(9).Heading = "V Spacing: <12"  '16
  DLog(9).TextLen = 3

  DLog(10).Heading = "H Spacing: <12"  '16
  DLog(10).TextLen = 3
  
  DLog(11).DType = 7                            'Frame
  DLog(11).Row = 5
  DLog(11).Col = 6
  DLog(11).NumRows = 6
  DLog(11).TextLen = 46

DO
  Choice = 1
  CALL GDialog(Choice, DLog(), Resp$(), Menu$(), Style, 0)

  IF LEN(LTRIM$(Resp$(3))) = 0 THEN EXIT SUB

  IF Choice <> 2 AND Choice <> 27 THEN

    REDIM Temp$(1 TO 10)

    Text$ = RTRIM$(Resp$(3))
    N = 1

    IF INSTR(Text$, "") THEN
      Begin = 1
      Eos = INSTR(Begin, Text$, "")
      DO
	Temp$(N) = MID$(Text$, Begin, Eos - Begin - 1)
	Begin = Eos + 1
	N = N + 1
	Eos = INSTR(Begin, Text$, "")
      LOOP UNTIL Eos = 0
      N = N - 1
    ELSE
      Temp$(1) = Text$
    END IF

    Font$ = QPTrim$(Resp$(5))

    IF Exist%(FPath$ + Font$) THEN

      IF Font$ <> LastFont$ THEN
	CALL SetGPFont(1)
	CALL LoadFont(FPath$ + Font$)
      END IF

      FHeight = FontHeight%(GPDat%(34))

      Scale# = ABS(VAL(QPTrim$(Resp$(6))))
      IF Scale# > 50 THEN Scale# = 50
      RSET Resp$(6) = QPTrim$(STR$(Scale#))
      
      Rotation = VAL(QPTrim$(Resp$(7)))
      RSET Resp$(7) = QPTrim$(STR$(Rotation))

      GPDat(24) = VAL(QPTrim$(Resp$(8)))              'Ital angle
      RSET Resp$(8) = QPTrim$(STR$(GPDat(24)))

      FontVSpc = VAL(QPTrim$(Resp$(9)))
      RSET Resp$(9) = QPTrim$(STR$(FontVSpc))

      GPDat(48) = VAL(QPTrim$(Resp$(10)))
      RSET Resp$(10) = QPTrim$(STR$(GPDat(48)))

      Longest = 0
      FOR i = 1 TO 10
	IF LEN(Temp$(i)) THEN
	  Length = GetWidth%(Temp$(i))
	  IF Length > Longest THEN Longest = Length
	END IF
      NEXT
      
      W = ((Longest - GPDat(48)) * Scale#) + (Scale# - 1)

      SRScreen 0
      WaitUp
      HideCursor
      GetCursor lastcx, lastcy, Button

      IF Rotation THEN
	CrossHair lastcx, lastcy
      ELSE
	HeightOS = FHeight * Scale#
	FHeight = FHeight * N
	Ht = FHeight * Scale# + (N - 1) * FontVSpc + (Scale# - 1)
	TextBox lastcx, lastcy, lastcx + W, lastcy + Ht
      END IF

      Button = 0
      Ky = 0

      DO                                     'Wait for mouse click
	Corner = 0
	DO
	  CALL CursorXY(x1, y1, Button, Ky)

	  IF Ky = -68 THEN
	    IF Rotation THEN
	      CrossHair x1, y1
	    ELSE
	      TextBox x1, y1, x1 + W, y1 + Ht
	    END IF
	    lastcx = x1
	    lastcy = y1
	  END IF

	  IF lastcx <> x1 OR lastcy <> y1 THEN
	    IF Rotation THEN
	      CrossHair lastcx, lastcy
	      CrossHair x1, y1
	    ELSE
	      TextBox lastcx, lastcy, lastcx + W, lastcy + Ht
	      TextBox x1, y1, x1 + W, y1 + Ht
	    END IF
	    lastcx = x1
	    lastcy = y1
	  END IF
	LOOP UNTIL Button > 0 OR Ky = 13

	IF Button = 1 OR Ky = 13 THEN
	  IF Rotation THEN
	    CrossHair x1, y1
	  ELSE
	    TextBox x1, y1, x1 + W, y1 + Ht
	  END IF

	  FOR i = 1 TO N
	    CALL DrawText(x1, y1 + HOS, Temp$(i), Rotation, GPDat(82), Scale#)
	    HOS = HOS + HeightOS + FontVSpc
	  NEXT

	  HOS = 0

	  IF Rotation THEN
	    CrossHair x1, y1
	  ELSE
	    TextBox x1, y1, x1 + W, y1 + Ht
	  END IF

	  DO
	    x$ = INKEY$
	  LOOP UNTIL x$ = ""

	  WaitUp
	  WaitUpKey
	  GPDat(83) = -1

	END IF

	IF Button = 2 THEN
	  IF Rotation THEN
	    CrossHair x1, y1
	  ELSE
	    TextBox x1, y1, x1 + W, y1 + Ht
	  END IF

	  ShowCursor
	  WaitUp
	  EXIT DO
	END IF
      LOOP

    ELSE
      BEEP
    END IF

    LastFont$ = Font$

  ELSE
    EXIT SUB
  END IF

LOOP

END SUB

'This sub-program draws characters horizontally, or at an angle
'
SUB DrawText (Xx, Yy, Text$, Angle, Colr, TextSize#) STATIC

    SColor = GPDat%(14)          'get shadow color

    Size# = TextSize#            'use copies to preserve incoming vars
    x = Xx
    y = Yy
    Expand = 0

    IF Size# < .001 THEN Size# = 1!
    'Prepare some variables that will be repeatedly used
    SAngle# = SIN((Angle / 180) * (22 / 7))
    CAngle# = COS((Angle / 180) * (22 / 7))

    IF Colr > 127 THEN          'if shadow then calculate size of shadow
	Ofsx = 1 + Size# * .6
	Ofsy = 1 + Size# * .3
    END IF                      'otherwise we don't care about the values
				'  of Ofsx and Ofsy
    IF Angle <> 0 OR GPDat%(24) <> 90 THEN
	IF GPDat%(24) <> 90 THEN
	    SAngle# = 0
	    CAngle# = -COS(((GPDat%(24)) / 180) * (22 / 7))
	    FOR T = 1 TO LEN(Text$)
	      'Give a value 1-95 as an offset into the font set
	      Letter = ASC(MID$(Text$, T, 1)) - 31
	      IF Letter <= UBOUND(FontWidth%, 1) THEN
		'If shadowing then draw the shadow first
		IF Colr > 127 THEN CALL DrawCharA(x + Ofsx, y + Ofsy, SAngle#, CAngle#, Letter, SColor, Size#, Expand)
		'Draw the character
		CALL DrawCharA(x, y, SAngle#, CAngle#, Letter, (Colr AND 127), Size#, Expand)
		'Calculate position for next character to be drawn
		x = x + (FontWidth%(Letter, GPDat%(34)) + GPDat%(48) + GPDat%(59)) * Size#
	      END IF
	    NEXT
	ELSE
	  FOR T = 1 TO LEN(Text$)
	    'Give a value 1-95 as an offset into the font set
	    Letter = ASC(MID$(Text$, T, 1)) - 31
	    IF Letter <= UBOUND(FontWidth%, 1) THEN
	      'If shadowing then draw the shadow first
	      IF Colr > 127 THEN CALL DrawCharA(x + Ofsx, y + Ofsy, SAngle#, CAngle#, Letter, SColor, Size#, Expand)
	      'Draw the character
	      CALL DrawCharA(x, y, SAngle#, CAngle#, Letter, (Colr AND 127), Size#, Expand)
	      'Calculate position for next character to be drawn
	      x = x + CAngle# * (FontWidth%(Letter, GPDat%(34)) + GPDat%(48) + GPDat%(59)) * Size#
	      y = y - SAngle# * (FontWidth%(Letter, GPDat%(34)) + GPDat%(48) + GPDat%(59)) * Size#
	    END IF
	  NEXT
	END IF
    ELSE
	FOR T = 1 TO LEN(Text$)
	    'SEE ABOVE FOR COMMENTS
	    Letter = ASC(MID$(Text$, T, 1)) - 31
	    IF Letter <= UBOUND(FontWidth%, 1) THEN
	      IF Colr > 127 THEN CALL DrawChar(x + Ofsx, y + Ofsy, Letter, SColor, Size#, Expand)
	      CALL DrawChar(x, y, Letter, (Colr AND 127), Size#, Expand)
	      x = x + (FontWidth%(Letter, GPDat%(34)) + GPDat%(48) + GPDat%(59)) * Size#
	    END IF
	NEXT
    END IF

END SUB

FUNCTION GetWidth% (Text$) STATIC

    FW = 0
    FOR T = 1 TO LEN(Text$)
	L = ASC(MID$(Text$, T, 1)) - 31
	FW = FW + FontWidth%(L, GPDat%(34)) + GPDat%(48)
    NEXT
    GetWidth% = FW

END FUNCTION

SUB LoadFont (FontFile$) STATIC

   LocalFile$ = NoExtension$(FontFile$) + ".GFN"

   Filenum = FREEFILE
   OPEN LocalFile$ FOR INPUT AS #Filenum
   INPUT #Filenum, GPLen                    'the first record contains ASCII
   CLOSE #Filenum                           '  representation of record length

   OPEN LocalFile$ FOR RANDOM AS Filenum LEN = GPLen
   FIELD #Filenum, GPLen AS GPL$

   GET #Filenum                             'bypass the first record
   FOR GPCnt = 1 TO UBOUND(Font$, 1)
       GET #Filenum                         'get next record from file
       GPChar = ASC(LEFT$(GPL$, 1))         'first byte contains Length of $
       Font$(GPCnt, GPDat%(34)) = MID$(GPL$, 2, GPChar) 'extract string
       Max = ASC(MID$(GPL$, 2, 1)) MOD 16
       FOR T = 3 TO GPChar + 1
	  Ma = ASC(MID$(GPL$, T, 1)) MOD 16
	  IF Ma > Max THEN Max = Ma
       NEXT
       IF GPCnt = 41 THEN  'Check for the height of the font by checking "H"
	  Max2 = ASC(MID$(GPL$, 2, 1)) \ 16
	  FOR T = 3 TO GPChar + 1
	     Ma2 = ASC(MID$(GPL$, T, 1)) \ 16
	     IF Ma2 > Max2 THEN Max2 = Ma2
	  NEXT
	  FontHeight%(GPDat%(34)) = Max2
       END IF
       FontWidth%(GPCnt, GPDat%(34)) = Max
       IF EOF(Filenum) THEN EXIT FOR
   NEXT
   'Set the width of the space character here
   FontWidth%(1, GPDat%(34)) = GPDat%(48)

   CLOSE #Filenum

   CALL SetGPFont(GPDat%(34) + 1)

END SUB

'This Routine changes the current GraphPak font
'
SUB SetGPFont (FontNumber%) STATIC

    'Check for invalid FontNumber
    IF FontNumber% < 0 OR FontNumber% > GPDat%(33) THEN EXIT SUB

    GPDat%(34) = FontNumber% - 1     'Change the current font to FontNumber%
    GPDat%(36) = GPDat%(48) + FontHeight%(GPDat%(34))
    GPDat%(35) = FontWidth%(56, GPDat%(34))

END SUB

'This sub-program sets the character spacing for the GraphPak fonts
'
SUB SetGPSpacing (SpacingH%, SpacingV%) STATIC

    GPDat%(47) = SpacingH%
    GPDat%(48) = SpacingV%
    GPDat%(36) = GPDat%(48) + FontHeight%(GPDat%(34))

END SUB

SUB TextBox (x1, y1, x2, y2)

  x1 = MaxInt(0, x1)
  x2 = MinInt(GPDat(43) - 1, x2)
  LineBVE x1, y1, x2, y2, 6152

END SUB

