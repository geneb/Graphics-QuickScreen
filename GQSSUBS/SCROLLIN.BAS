DEFINT A-Z

'Graphic single line text input routine with scrolling

'by Phil Cramer
'Copyright (c) 1992 Crescent Software

DECLARE SUB GCursor (x%, Y%, CursorLength%, CursorIs%, CursorTime%)
DECLARE SUB GetCursor (x, Y, Button)
DECLARE SUB GPrint0VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB HideCursor ()
DECLARE SUB Mouse (AX%, BX%, CX%, DX%)
DECLARE SUB ProperName (Work$)
DECLARE SUB ShowCursor ()
DECLARE SUB WaitUp ()

DECLARE FUNCTION INSTRTBL2 (Start, Source$, Table$)
DECLARE FUNCTION PDQTimer& ()
DECLARE SUB UPPER (A$)

'$INCLUDE: 'Scrollin.Bi'

CONST Filter1$ = "1234567890- "         'Mask for Integers ONLY
CONST Filter2$ = "1234567890.^ed+-/ "   'Mask for Single/Double precision ONLY
CONST Filter3$ = "123456789-() "        'User definable Mask

'$INCLUDE: 'Common.Bi'

'********************* Scrollable single line Editor *************************
'
'Copyright (c) 1991 Crescent Software
'Written by Phil Cramer
'
'
'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
'³  ScrollIn arguments:                                                      ³
'³                                                                           ³
'³  Edit$   The string to be edited. May be any length                       ³
'³                                                                           ³
'³  Start   On entry Start is the first character to be displayed in the edit³
'³          window. On exit, Start holds the column where the cursor was last³              ³
'³                                                                           ³
'³  Wide    The width of the edit window                                     ³
'³                                                                           ³
'³  MaxLen  The maximum allowable length of Edit$. If MaxLen = Wide then no  ³
'³          scrolling will occur                                             ³
'³                                                                           ³
'³  Filter  Determines the type of text to be entered and may be set to the  ³
'³          any of the following numbers:                                    ³
'³                                                                           ³
'³            0  All regular keys will be accepted                           ³
'³            1  Integer characters only                                     ³
'³            2  Integer, Single or Double precision characters only         ³
'³            3  User defined                                                ³
'³            4  Converts all letters to upper case                          ³
'³            5  Capitalizes the first letter of each word                   ³
'³                                                                           ³
'³   Ky     On entry Ky is the column at which to place the cursor. On exit  ³
'³          Ky returns the key used to Exit ScrollIn.BAS                     ³           ³
'³                                                                           ³
'³  EdClr   The color to use while editing as used by GPrint0VE              ³
'³                                                                           ³
'³  NormClr The color to use when editing is complete                        ³
'³                                                                           ³
'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
'
SUB ScrollIn (Ed$, Ky$, Scroll AS Scroll) STATIC


  IF Scroll.Action < 3 THEN
    Row = CSRLIN                             'Get the Edit row
    Scroll.CurCol = POS(0)                   'Get the Edit Column
    OS = Scroll.CurCol                       'Calculate the offset from Column 1
    LCol = Scroll.CurCol                     'Left side of window
    RCol = LCol + Scroll.Wide - 1            'Right side of window
    MPressed = 0

    Edit$ = Ed$
    Ed$ = SPACE$(Scroll.MaxLen)              'Dim the string
    LSET Ed$ = Edit$                         'Place the Edit$ in the Field
    Clr = Scroll.EdClr                       'Start with Edit Color
    GOSUB PrintIt                            'Print The string

    IF Scroll.Action = -1 THEN
      GOSUB MoveCursor
      EXIT SUB
    END IF

    Scroll.CurCol = Scroll.CurCol + Scroll.Ky - 1 'Find the cursor column

    '--** Start + Scroll.CurCol - OS = The current char position in the string

    IF Scroll.Insert THEN
      CursorLength = GPDat(71)               'Start with full cursor
    ELSE
      CursorLength = 2
    END IF

    CursorTime = 3                           'Set cursor blink rate
    IF Scroll.Action = 1 OR Scroll.Action = 2 THEN Scroll.Action = 3
    T2& = 0
  END IF


  DO
                                             'Draw the cursor
    IF NOT Time THEN CALL GCursor(Scroll.CurCol * 8 - 8, Row * GPDat(71), CursorLength, CursorOn, CursorTime)

    Ky$ = INKEY$

    SELECT CASE LEN(Ky$)
      CASE 0
        Scroll.Ky = 0
      CASE 1
        Scroll.Ky = ASC(Ky$)
      CASE 2
        Scroll.Ky = -ASC(RIGHT$(Ky$, 1))
      CASE ELSE
    END SELECT

    'MMMMMMMMMMMMMMMMMMMMMM Start of Mouse Code MMMMMMMMMMMMMMMMMMMMMMMM
    'M   If you do not require a mouse, eliminate the code from here   M
    'M   to the next block of 'MMMMMM statements                       M
    'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

    CALL GetCursor(Scroll.Col, Scroll.Row, Button)


    IF Button = 1 THEN                        'If they left clicked
      MCol = Scroll.Col \ 8 + 1
      MRow = Scroll.Row \ GPDat(71) + 1

                                              ' find new cursor position
      IF MRow = Row AND MCol >= LCol AND MCol <= RCol THEN

        GOSUB MoveCursor
        Scroll.CurCol = MCol
        
        IF NOT Pressed THEN                   'Set the timer if first time
          T& = PDQTimer&
        ELSE
          IF PDQTimer& - T& > 2 THEN Time = -1'Wait 2 ticks before we scroll
        END IF
          
        IF MCol = RCol AND Time THEN          'If we're holding down the
          IF PDQTimer& + 5 > T2& THEN
            T2& = PDQTimer&
            Scroll.CurCol = Scroll.CurCol + 1 ' mouse button on the far
            x = 1                             ' right column, scroll right
          END IF
        END IF

        IF MCol = LCol AND Time THEN          'If we're holding down the
          Scroll.CurCol = Scroll.CurCol - 1   ' mouse button on the far
          x = 1                               ' left button, scroll left
        END IF
        Pressed = -1                          'Set the Button pressed flag

      ELSE                                    'They pressed outside the window
        MPressed = -1                         'Set flag
        Scroll.Ky = 13                        'Make like Enter pressed
      END IF

    ELSE
      Pressed = 0                             'Reset Flags
      Time = 0
    END IF

    'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    'M                        End of Mouse Code                       M
    'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

    IF Scroll.Ky THEN
      GOSUB MoveCursor
    END IF

    SELECT CASE Scroll.Ky
      CASE 0

      '---- BackSpace
      CASE 8
        IF Scroll.Start + Scroll.CurCol - OS > 1 THEN
          IF Scroll.Insert THEN
            MID$(Ed$, Scroll.Start + Scroll.CurCol - OS - 1) = MID$(Ed$, Scroll.Start + Scroll.CurCol - OS) + " "
          ELSE
            MID$(Ed$, Scroll.Start + Scroll.CurCol - OS + (Scroll.Start <> 0), 1) = " "
          END IF
          Scroll.CurCol = Scroll.CurCol - 1
          GOSUB PrintIt
          x = 1
        END IF

    '  Don't allow Tab and Shift-Tab for GQS
    '
    '  '---- Tab                             'Move forward 8 spaces at a time
    '  CASE 9
    '    Scroll.CurCol = Scroll.CurCol + 8
    '    x = 8
    '
    '  '---- Shift Tab                       'Move backward 8 spaces at a time
    '  CASE -15
    '    Scroll.CurCol = Scroll.CurCol - 8
    '    x = 8

      '---- Escape
      CASE 27
        Clr = Scroll.NormClr
        LSET Ed$ = Edit$                     'Return un-edited string
        GOSUB PrintIt
        Ed$ = ""                             'Clear the copy
        Scroll.Action = 4
        EXIT SUB                             'Outta' here


      '---- Alt R
      CASE -19                               'Restore original field
        LSET Ed$ = Edit$
        GOSUB PrintIt
        Scroll.Changed = 0

      '---- Alt C                            'Clear the field
      CASE -46
        Scroll.Changed = -1
        Ed$ = SPACE$(Scroll.MaxLen)
        GOSUB PrintIt

      '---- Left Arrow
      CASE -75
        Scroll.CurCol = Scroll.CurCol - 1
         x = 1

      '---- Right Arrow
      CASE -77
        Scroll.CurCol = Scroll.CurCol + 1
        x = 1

      '---- Home
      CASE -71
        Scroll.Start = 1
        Scroll.CurCol = -1
        x = 1

      '----  Up, PgUp, Down, PgDown, Enter
      CASE -72, -73, -80, -81, 13
        Clr = Scroll.NormClr
        GOSUB PrintIt
        Edit$ = ""                           'Clear restore buffer
        IF MPressed THEN Scroll.Ky = 1000    'Return code for mouse click
        'Scroll.Start = Scroll.CurCol - OS + 1 'Return current cursor column
        Scroll.Action = 4
        EXIT SUB

      '---- End
      CASE -79
        FOR i = Scroll.MaxLen TO 1 STEP -1   'Find last character in the string
          IF MID$(Ed$, i, 1) <> " " THEN
            Scroll.Start = i - Wide
            EXIT FOR
          END IF
        NEXT

        IF i > Scroll.Wide THEN              'Not found in the current window
          Scroll.CurCol = RCol + 1           'Scroll right
          x = 1
        ELSE                                 'Found in the current window
          Scroll.Start = 1
          Scroll.CurCol = i + OS
        END IF

      '---- Ins                              'Toggle Insert Status
      CASE -82
        Scroll.Insert = NOT Scroll.Insert
        IF Scroll.Insert THEN
          CursorLength = GPDat(71)
        ELSE
          CursorLength = 2                   'Insert OFF, use small cursor
        END IF                               'Insert ON use full cursor

      '---- Delete                           'Remove a character
      CASE -83
        MID$(Ed$, Scroll.Start + Scroll.CurCol - OS) = MID$(Ed$, Scroll.Start + Scroll.CurCol - OS + 1) + " "
        GOSUB PrintIt
        Scroll.Changed = -1

      '---- Control Right                    'Move Wide spaces -1 to the right
      CASE -116
        Scroll.CurCol = Scroll.CurCol + Scroll.Wide - 1
        x = Scroll.Wide - 1


      '---- Control Left                     'Move Wide spaces +1 to the left
      CASE -115
        Scroll.CurCol = Scroll.CurCol - Scroll.Wide + 1
        x = Scroll.Wide + 1

      '---- All normal keys                  'Handle regular keys
      CASE 32 TO 255
        Found = -1
        SELECT CASE Scroll.Filter
          CASE 1                             'Integers Only
            IF INSTRTBL2(1, Ky$, Filter1$) = 0 THEN Found = 0
          CASE 2                             'Single/Double precision nums only
            IF INSTRTBL2(1, Ky$, Filter2$) = 0 THEN Found = 0
          CASE 3                      'User defined
            IF INSTRTBL2(1, Ky$, Filter3$) = 0 THEN Found = 0
          CASE 4
           Ky$ = UCASE$(Ky$)

        END SELECT

        IF Found THEN                        'If its an acceptable key
          Scroll.Changed = -1
          IF Scroll.Insert THEN
            MID$(Ed$, Scroll.Start + Scroll.CurCol - OS) = Ky$ + MID$(Ed$, Scroll.Start + Scroll.CurCol - OS)
            Scroll.CurCol = Scroll.CurCol + 1
            GOSUB PrintIt
          ELSE
            MID$(Ed$, Scroll.Start + Scroll.CurCol - OS, 1) = Ky$
            GOSUB PrintIt
            Scroll.CurCol = Scroll.CurCol + 1
          END IF
        END IF
        x = 1
      CASE ELSE

    END SELECT

    '---- Make sure we stay within the field
    IF Scroll.CurCol > RCol THEN
      Scroll.CurCol = RCol
      Scroll.Start = Scroll.Start + x
      GOSUB PrintIt
    ELSEIF Scroll.CurCol < LCol THEN
      Scroll.CurCol = LCol
      Scroll.Start = Scroll.Start - x
      GOSUB PrintIt
    END IF

  LOOP WHILE Scroll.Action = 0

  EXIT SUB


PrintIt:
  IF Scroll.MaxLen - Scroll.Start + 1 < Scroll.Wide THEN Scroll.Start = Scroll.MaxLen - Scroll.Wide + 1
  IF Scroll.Start < 1 THEN Scroll.Start = 1
  IF Scroll.Filter = 5 THEN ProperName Ed$
  HideCursor
  GPrint0VE Row, LCol, MID$(Ed$, Scroll.Start, Scroll.Wide), Clr
  ShowCursor
RETURN

MoveCursor:
  IF CursorOn THEN
    CALL GCursor(Scroll.CurCol * 8 - 8, Row * GPDat(71), CursorLength, CursorOn, -1)
  END IF
RETURN

END SUB

