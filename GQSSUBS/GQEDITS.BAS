'---- QEdit Graphics mode text editor

'by Don Malin, Graphics mode adaptation by Phil Cramer
'Copyright (c) 1988-1990 Crescent Software


DEFINT A-Z

'$INCLUDE: 'QEditype.BI'

'----- assembler calls (Internal QPro subs used by QEdit, not part of GWS)
DECLARE SUB DeleteStr (BYVAL Address, NumEls)
DECLARE SUB GetVMode (Mode, Page, PageSize&, Rows, Columns)
DECLARE SUB InsertStr (BYVAL Address, Inserted$, NumEls)

'----- BASIC subs
DECLARE SUB ButtonPress (Button%, Status%, Count%, LastX%, LastY%)
DECLARE SUB GAPrint0 (array$(), Start, NumEls, FirstChar, NumChars, Colr)
DECLARE SUB GAPrintT0 (array() AS ANY, Start, ElSize, NumEls, FirstChar, NumChars, Colr)
DECLARE SUB GQEditCursor (X%, Y%, CursorLength%, CursorOn%, CursorTime%, Clr%)
DECLARE SUB GetCursor (X%, Y%, Button%)
DECLARE SUB GPrint0VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GScrollVE (BYVAL Col1%, BYVAL Line1%, BYVAL Cols%, BYVAL Lines%, BYVAL ColDelta%, BYVAL LineDelta%)
DECLARE SUB HideCursor ()
DECLARE SUB LineBFVE (BYVAL x1, BYVAL y1, BYVAL x2, BYVAL y2, BYVAL Clr)
DECLARE SUB Mouse (AX%, BX%, CX%, DX%)
DECLARE SUB ShowCursor ()
DECLARE SUB TextCursor (X, Y, CursorLength, CursorOn, CursorTime, Colr)

'----- ASM Functions  (Internal QPro functions used by QEdit, not part of GWS)
DECLARE FUNCTION ASCII% (Ky$)
DECLARE FUNCTION Blanks% (Text$)
DECLARE FUNCTION FindLast% (BYVAL Address, Size)
DECLARE FUNCTION MaxInt% (Num1, Num2)
DECLARE FUNCTION MinInt% (Num1, Num2)
DECLARE FUNCTION Null% (Text$)
DECLARE FUNCTION PeekBuf% ()
DECLARE FUNCTION PDQTimer& ()
DECLARE FUNCTION QInstrB% (Start%, Text$, Search$)
DECLARE FUNCTION QPLen% (Text$)
DECLARE FUNCTION ShiftKey ()

'$INCLUDE: 'Common.Bi'

DIM SHARED LastTime

CONST SkipTbl$ = " ^*()[]-=+,.<>/\"             'Word delimiters


'----- Returns the number of columns to advance at the end of a line
DEF FnSpaces2Pad (Text$)
    STATIC Temp

    SELECT CASE ASCII(RIGHT$(Text$, 1))
       CASE 46, 33, 63          '".", "!", "?"
          Temp = 2
       CASE 32, -1              'Space
          Temp = 0
       CASE ELSE
          Temp = 1
    END SELECT
    FnSpaces2Pad = Temp
END DEF

SUB GAPrint0 (array$(), Start, NumEls, FirstChar, NumChars, Colr)
  'Prints any portion of a variable length string array in Graphics mode

  Col = POS(1)
  LineRow = CSRLIN

  FOR i = Start TO Start + NumEls - 1
    X$ = MID$(array$(i), FirstChar, NumChars)
    CALL GPrint0VE(LineRow, Col, X$ + SPACE$(NumChars - LEN(X$)), Colr)
    LineRow = LineRow + 1
  NEXT

END SUB

SUB GQEditCursor (X, Y, CursorLength, CursorOn, CursorTime, Clr) STATIC

  ThisTime = PDQTimer& MOD CursorTime
  IF ThisTime < LastTime OR CursorTime = -1 THEN

    HideCursor                     'Draw/Erase the cursor
    CALL LineBFVE(X, Y, X + 6, Y - CursorLength + 1, Clr)
    ShowCursor

    CursorOn = NOT CursorOn
    IF CursorTime = -1 THEN
      ThisTime = 32000             'Make sure cursor comes back fast
    END IF
  END IF
  LastTime = ThisTime

END SUB

'****************************************************************************
'  Title: QEdit - Text editing sub-program
'     by: Don Malin  6/27/89, Graphics adaptation by Phil Cramer 9/2/91
'
'Purpose: Displays a word processing window and allows the user to edit text.
'         Horizontal and vertical scrolling can be used to edit a string
'         array passed to the routine.  Margins and word wrap are supported.
'
' Syntax: Call QEdit (Arg1$(), Arg2$, Arg3%, Arg4 as EditInfo)
'
' Inputs: Array$() - Text array to be edited.  Must be a conventional
'                    (not fixed-length) string array.
'         X$       - Last Key pressed
'         Action   - Action flag  (0 = Take full control, return only on Esc
'                                 (1 = Init window
'                                 (2 = Re-display but don't save window)
'                                 (3 = Just check keyboard and return
'                                 (5 = Clean up memory and exit
'         Ed       - Structure defined in QEdit.Bas MAIN and the Caller's
'                    MAIN (see $INCLUDE file - QEditype.BI)
'
'  Notes: It is up to you to determine the probable amount of string space
'         required for your text array.
'****************************************************************************
'
SUB QEdit (array$(), X$, Action%, Ed AS EditInfo) STATIC

    Ed.MErr = 0

    '----- Action of 0 or 1
    IF Action < 2 THEN
       Ed.MErr = 0                              'No errors yet

       WrapWas = Ed.Wrap
       Ed.HTab = MaxInt(Ed.HTab, 1)             'Horizontal Tab spaces
       Insrt = Ed.InsStat
       IF Insrt = 0 THEN Insrt = -1             'Insert mode set to ON
                                                'make 1 to default to OT
       CtrlP = 32

       GetVMode 0, 0, 0, ScrRows, 80            'Get current line mode

       Ed.TSRow = CSRLIN                        'Calc the top line of window
       Ed.LSCol = POS(0)                        'Calc Left margin of window

       Ed.Rows = MinInt(MaxInt(Ed.Rows, 1), ScrRows - CSRLIN + 1)
       Ed.Wide = MinInt(MaxInt(Ed.Wide, 1), 80 - POS(0) + 1)

       IF Ed.LC <= 0 THEN Ed.LC = 1             'Set window column to 1
       IF Ed.CurCol <= 0 THEN Ed.CurCol = 1     'establish cursor column at 1
       IF Ed.TL <= 0 THEN Ed.TL = 1             'Top of window row number
       IF Ed.CurLine <= 0 THEN Ed.CurLine = 1   'make top line the cursor col.
       GOSUB WindowLimits

       BGClr = Ed.AColor \ 256                  'Get the text background color

       IF BGClr = 15 THEN                       'If text background = 15
         CursorClr = 6159                       ' use a black cursor
       ELSE                                     ' otherwise, make it hi-white
         CursorClr = 15 + 256 * 24 XOR BGClr
       END IF

       CursorTime = 4                           'Blink 4 times a second
       DelClr = (Ed.AColor \ 256) + 6144        'Color for delete hilight
       LastTime = 0

    END IF

    '----- Action of 5
    IF Action = 5 THEN
       GOSUB CursorOff                          'Turn off the cursor
       LineBuf$ = ""                            'Clean up memory
       Buf$ = ""
       Action = 1                               'Reset action to 1
       EXIT SUB                                 'Bail out
    END IF
   
    '----- Action of 0, 1, 2 - Display the edit window
    IF Action < 3 OR Action = 4 THEN

       '----- Find the last element being used in the text array
       ArrayEnd = UBOUND(array$)
       Ed.LCount = FindLast%(VARPTR(array$(ArrayEnd)), ArrayEnd)

       IF Ed.Wrap THEN     'If word wrap is on, use "" paragraph marker
          Par$ = CHR$(20)  '(you could use CHR$(255) for invisable marker)
          Ed.Wrap = MaxInt(MinInt(Ed.Wrap, 255), 10)
       ELSE                'Otherwise,
          Par$ = ""        '  use a null character
       END IF
  
       '----- If word wrap is on, format all the text.
       IF Ed.Wrap THEN
          IF Action < 3 THEN
             WrapLine = 1
             GOSUB WrapAll
          ELSE
             WrapLine = Ed.CurLine
             GOSUB WrapDown
             GOSUB WrapUp
          END IF
          WrapWas = Ed.Wrap
       END IF
                          
       LineBuf$ = SPACE$(Ed.Wide)               'Temp buffer used for printing
       RePrint = -1                             'Set flag to print the window
                                                'Make sure the cursor stays in
                                                '  the window
       Ed.CurLine = MinInt(Ed.CurLine, Ed.TL + Ed.Rows - 1)
       Ed.CurCol = MinInt(Ed.CurCol, Ed.LC + Ed.Wide - 1)

       GOSUB CsrSize                            'Set the cursor size
       IF Action <> 0 THEN Action = 3         'PC 3/11/94 version 1.11
    END IF


    '----- Main editing (key processing) loop
    DO

        '----- Position the cursor
        IF Action <> 2 THEN CALL GQEditCursor((Ed.LSCol + Ed.CurCol - Ed.LC) * 8 - 8, (Ed.TSRow + Ed.CurLine - Ed.TL + 1) * GPDat(71) - GPDat(71) - 1, CursorLength, CursorOn, CursorTime, CursorClr)

        Ed.UnKnownKey = 0
        LineWas = Ed.CurLine

        '----- Handle regular keys
        IF QPLen(X$) = 1 THEN
           GOSUB CursorOff
    
           SELECT CASE ASCII(X$)

              '----- All Non Control Characters
              CASE IS >= CtrlP
                 X = QPLen(array$(Ed.CurLine))  'See if line is long enough
                 IF X < 256 AND Ed.CurCol < 256 THEN
                    IF Ed.CurLine < ArrayEnd OR Ed.Wrap = 0 OR X < Ed.Wrap THEN
                       IF Ed.CurCol > X THEN    'Make space for new character
                          array$(Ed.CurLine) = array$(Ed.CurLine) + SPACE$(Ed.CurCol - X)
                       END IF
                                                'Update line count if past
                       Ed.LCount = MaxInt%(Ed.LCount, Ed.CurLine) ' end
                                                'Insert character in line
                       IF Insrt = -1 AND Ed.CurCol <= X THEN
                          array$(Ed.CurLine) = LEFT$(array$(Ed.CurLine), Ed.CurCol - 1) + X$ + MID$(array$(Ed.CurLine), Ed.CurCol)
                          HideCursor
                          GPrint0VE Ed.TSRow + Ed.CurLine - Ed.TL, Ed.LSCol + (Ed.CurCol - Ed.LC), MID$(array$(Ed.CurLine), Ed.CurCol, Ed.Wide - (Ed.CurCol - Ed.LC)), Ed.AColor
                          ShowCursor
                          IF Ed.Wrap THEN
                             WrapLine = Ed.CurLine
                             GOSUB WrapDown
                          END IF
                       ELSE                     'Concat character to end
                          MID$(array$(Ed.CurLine), Ed.CurCol, 1) = X$
                          HideCursor
                          GPrint0VE Ed.TSRow + Ed.CurLine - Ed.TL, Ed.LSCol + (Ed.CurCol - Ed.LC), X$, Ed.AColor
                          ShowCursor

                          IF Ed.Wrap AND Ed.CurCol > Ed.Wrap THEN
                             WrapLine = Ed.CurLine
                             GOSUB WrapDown
                          END IF
                       END IF

                       Ed.CurCol = Ed.CurCol + 1'Update the current column
                       GOSUB WindowLimits
                       Ed.Changed = -1          'We just changed the text
                       LineEd = -1
                    END IF
                 END IF

                 CtrlP = 32

              '----- Back Space
              CASE 8
                 IF Ed.CurCol > 1 THEN          'If not at column 1,

                    Ed.CurCol = Ed.CurCol - 1   '  decrement the cursor posit.
                    IF Insrt = -1 THEN          'If in Insert mode, drag the
                                                '  line 1 char. left
                       array$(Ed.CurLine) = LEFT$(array$(Ed.CurLine), Ed.CurCol - 1) + MID$(array$(Ed.CurLine), Ed.CurCol + 1)
                       IF Ed.Wrap THEN
                          WrapLine = Ed.CurLine
                          GOSUB WrapUp          'Wrap words up
                       END IF
                                                'Not in Insert mode, blank char
                    ELSEIF Ed.CurCol = QPLen(array$(Ed.CurLine)) THEN
                       array$(Ed.CurLine) = LEFT$(array$(Ed.CurLine), Ed.CurCol - 1)
                    ELSEIF Ed.CurCol < QPLen(array$(Ed.CurLine)) THEN
                       MID$(array$(Ed.CurLine), Ed.CurCol, 1) = " "
                       LineEd = -1
                    END IF                      'Put edited line in a buffer
                    IF Ed.CurCol < Ed.LC THEN   'If we're at left window col,
                       Ed.LC = Ed.LC - 1        '  decrement window column
                       RePrint = -1
                    ELSE
                       LSET LineBuf$ = MID$(array$(Ed.CurLine), Ed.LC)
                       GPrint0VE Ed.TSRow + Ed.CurLine - Ed.TL, Ed.LSCol, LineBuf$, Ed.AColor
                    END IF
                                                'If in Insert mode, go up
                 ELSEIF Ed.CurLine > 1 THEN
                    Temp = FnSpaces2Pad(array$(Ed.CurLine - 1))
                                                'Copy line to line above
                    Ed.CurCol = QPLen(array$(Ed.CurLine - 1)) + Temp + 1
                    array$(Ed.CurLine - 1) = array$(Ed.CurLine - 1) + SPACE$(Temp) + array$(Ed.CurLine)
                                                'Update window column
                    Ed.LC = MaxInt%(Ed.CurCol - Ed.Wide + 1, Ed.LC)

                    Temp = Ed.CurLine           'Delete the current line
                    GOSUB DeleteLine
                   
                    Ed.CurLine = Ed.CurLine - 1
                    GOSUB CursorUp              'Back up a line
                    RePrint = -1                'Set flag to re-print window
                    IF Ed.Wrap THEN
                       WrapLine = Ed.CurLine
                       GOSUB WrapDown           'Wrap words down
                    END IF
                 END IF
                 Ed.Changed = -1
                  
              '----- Enter
              CASE 13
                 IF Ed.CurLine < ArrayEnd THEN

                    X = Blanks(array$(Ed.CurLine)) + 1
                                                'If in Insert mode, insert a
                    IF Insrt = -1 THEN
                                                '  new line below.
                       IF Ed.LCount < ArrayEnd THEN
                          Ed.LCount = Ed.LCount + 1
                                                'New line is right of cursor
                          Temp$ = SPACE$(MinInt(X, Ed.CurCol) - 1) + RTRIM$(MID$(array$(Ed.CurLine), Ed.CurCol))
                          Temp = Ed.CurLine + 1
                          GOSUB InsertLine
                                                'Current line is left of
                                                '  cursor & paragraph marker
                          array$(Ed.CurLine) = RTRIM$(LEFT$(array$(Ed.CurLine), Ed.CurCol - 1))
                          Ed.Changed = -1
                       END IF
                    END IF
                     
                    IF X > 1 OR QPLen(array$(Ed.CurLine)) THEN Ed.CurCol = X
                    Ed.CurLine = MinInt%(Ed.CurLine + 1, ArrayEnd)
                    GOSUB WindowLimits

                    RePrint = -1                'Set flag to re-print window
                 END IF

              '----- TAB
              CASE 9
                                                'Make current column "Ed.HTab"
                                                '  to the right
                 N = ((Ed.CurCol + Ed.HTab - 1) \ Ed.HTab) * Ed.HTab + 1
                 N = MinInt(N, 255 - Ed.HTab)
                 IF N <= Ed.Wrap OR Ed.Wrap = 0 THEN
                                                'If in Insert mode
                    IF Insrt = -1 AND Ed.CurCol < QPLen(array$(Ed.CurLine)) THEN
                                                '  insert "Ed.HTab" spaces
                       array$(Ed.CurLine) = LEFT$(array$(Ed.CurLine), Ed.CurCol - 1) + SPACE$(N - Ed.CurCol) + MID$(array$(Ed.CurLine), Ed.CurCol)
                       IF Ed.Wrap THEN
                          WrapLine = Ed.CurLine
                          GOSUB WrapDown        'Wrap words down
                       END IF
                       Ed.Changed = -1
                    END IF
                                               
                    Ed.CurCol = N
                                                'Is it off the right of window?
                    IF Ed.CurCol > Ed.LC + Ed.Wide - 1 THEN
                       Ed.LC = Ed.LC + Ed.HTab  'Make it "Ed.HTab" to the right
                       RePrint = -1             'Set flag to re-print window
                    ELSE                        'Move to the left margin
                                                'Put new line in buffer
                       LSET LineBuf$ = MID$(array$(Ed.CurLine), Ed.LC)
                       HideCursor                         'Print the buffer
                       GPrint0VE Ed.CurLine + Ed.TSRow - 1, Ed.LSCol, LineBuf$, Ed.AColor
                       ShowCursor
                    END IF
                 END IF

              '----- Escape
              CASE 27
                 IF Action = 0 THEN             'If not using Action flag,
                    LineBuf$ = ""               'Clean up memory
                    Buf$ = ""
                    EXIT DO                     'see ya later oscillator
                 ELSE
                    Ed.UnKnownKey = -1
                 END IF
                 
              '----- Ctrl Y - Delete a Line
              CASE 25
                 IF Ed.CurLine <= Ed.LCount THEN 'Disallow if we're past the last line
                                                 'Calc the screen line number
                    X = Ed.TSRow + Ed.CurLine - Ed.TL
                                                 'Paint the line before we delete it
                    HideCursor                   '  just for effect
                    LineBFVE Ed.LSCol * 8 - 8, X * GPDat(71) - GPDat(71), (Ed.LSCol + Ed.Wide) * 8 - 9, X * GPDat(71) - 1, DelClr
                    ShowCursor
                    Temp = Ed.CurLine            'Delete the current line
                    GOSUB DeleteLine
                    TimeOld& = PDQTimer + 1
                    WHILE PDQTimer < TimeOld&: WEND'Wait an 1/18 of a second
                    RePrint = -1                 'Set flag to re-print window
                    Ed.Changed = -1
                 END IF
            
              '----- Ctrl N - Insert a Line
              CASE 14
                 Temp = Ed.CurLine
                 Temp$ = ""
                 InsertingTxt = 0
                 GOSUB InsertLine
                 RePrint = -1                    'Set flag to re-print window
                 Ed.Changed = -1

              '----- Ctrl P - Imbed characters
              CASE 16
                 CtrlP = 0
                 LineEd = -1

              CASE ELSE
                 Ed.UnKnownKey = -1

           END SELECT

           X& = FRE(array$(0))
           IF X& < 1280 THEN
              BEEP
              Ed.MErr = 1                       'Set error flag
           END IF


        '----- Handle Extended (2 Char) key codes
        ELSEIF QPLen(X$) = 2 THEN
           GOSUB CursorOff
           
           SELECT CASE ASCII(RIGHT$(X$, 1))     'Get ASCII value of right 1

              '----- Cursor UP
              CASE 72
                 IF PeekBuf = -72 THEN
                    Ed.CurLine = Ed.CurLine - 2
                    X$ = INKEY$
                 ELSE
                    Ed.CurLine = Ed.CurLine - 1
                 END IF
                 GOSUB CursorUp                 'Go do it

              '----- Cursor DOWN
              CASE 80
                 IF PeekBuf = -80 THEN
                    Ed.CurLine = Ed.CurLine + 2
                    X$ = INKEY$
                 ELSE
                    Ed.CurLine = Ed.CurLine + 1
                 END IF
                 GOSUB CursorDown               'Go do it

              '----- Cursor LEFT
              CASE 75
                 IF Ed.CurCol > 1 THEN
                    IF PeekBuf = -75 THEN
                       Ed.CurCol = Ed.CurCol - 2
                       X$ = INKEY$
                    ELSE
                       Ed.CurCol = Ed.CurCol - 1
                    END IF
                    GOSUB CursorLeft
                                                'Are we in word wrap mode?
                 ELSEIF Ed.Wrap AND Ed.CurLine > 1 THEN
                    Ed.CurCol = Ed.Wrap         'Set column to right margin
                    Ed.LC = MaxInt%(Ed.Wrap - Ed.Wide + 1, 1)
                    Ed.CurLine = Ed.CurLine - 1
                    GOSUB CursorUp              'Let "CursorUp" handle it
                    RePrint = -1
                    ColBlock = 0
                 END IF

              '----- Cursor RIGHT
              CASE 77
                 IF Ed.CurCol < 256 THEN
                    IF PeekBuf = -77 THEN
                       Ed.CurCol = Ed.CurCol + 2
                       X$ = INKEY$
                    ELSE
                       Ed.CurCol = Ed.CurCol + 1
                    END IF
                    GOSUB CursorRight
                                                'If at right margin
                    IF Ed.Wrap AND Ed.CurCol > Ed.Wrap THEN
                       Ed.CurCol = 1            'Set current column to 1
                       Ed.LC = 1                'Set window column to 1
                       Ed.CurLine = Ed.CurLine + 1
                       GOSUB CursorDown         'Let "CursorDown" move down
                                                '  a line
                       RePrint = -1
                       ColBlock = 0
                    END IF
                 END IF

              '----- Ctrl LEFT - move one word left
              CASE 115
                 DO
                    IF Ed.CurCol = 1 AND Ed.CurLine > 1 THEN
                       Ed.CurLine = Ed.CurLine - 1
                       Ed.CurCol = MaxInt(QPLen(array$(Ed.CurLine)), 1)
                       ColBlock = 0
                    END IF
                      
                    X = -1
                    DO WHILE Ed.CurCol > 1
                       Ed.CurCol = Ed.CurCol - 1
                       IF X THEN
                          IF INSTR(SkipTbl$, MID$(array$(Ed.CurLine), Ed.CurCol, 1)) = 0 THEN X = 0
                       ELSE
                          IF INSTR(SkipTbl$, MID$(array$(Ed.CurLine), Ed.CurCol, 1)) THEN
                             Ed.CurCol = Ed.CurCol + 1
                             EXIT DO
                          END IF
                       END IF
                    LOOP
                 LOOP WHILE Ed.CurLine > 1 AND INSTR(SkipTbl$, MID$(array$(Ed.CurLine), Ed.CurCol, 1))
                
                 GOSUB WindowLimits             'If off left of window, adjust

              '----- Ctrl RIGHT - move one word right
              CASE 116
                 DO
                    X = -1
                    DO WHILE Ed.CurCol <= QPLen(array$(Ed.CurLine))
                       Ed.CurCol = Ed.CurCol + 1 'Increment column position
                       IF X THEN
                          IF INSTR(SkipTbl$, MID$(array$(Ed.CurLine), Ed.CurCol, 1)) THEN X = 0
                       ELSE
                          IF INSTR(SkipTbl$, MID$(array$(Ed.CurLine), Ed.CurCol, 1)) = 0 THEN EXIT DO
                       END IF
                    LOOP
                                                'If past end of line,
                    IF Ed.CurCol > QPLen(array$(Ed.CurLine)) AND Ed.CurLine < Ed.LCount THEN
                       Ed.CurCol = 1            '  Set current column to 1
                       Ed.CurLine = Ed.CurLine + 1 'Increment line number
                       ColBlock = 0
                    ELSE
                       EXIT DO
                    END IF
                 LOOP WHILE INSTR(SkipTbl$, MID$(array$(Ed.CurLine), Ed.CurCol, 1))
                                                'If off right of window, adjust
                 GOSUB WindowLimits

              '----- Cursor HOME
              CASE 71                           'Starting at column 1
                 X = Blanks%(array$(Ed.CurLine)) + 1
                 IF X = Ed.CurCol THEN X = 1
                 Ed.CurCol = X
                 GOSUB WindowLimits

              '----- Cursor END
              CASE 79                           'Set current column to end +
                 Ed.CurCol = QPLen(array$(Ed.CurLine)) + FnSpaces2Pad(array$(Ed.CurLine)) + 1
                 GOSUB WindowLimits

              '----- Ctrl HOME
              CASE 119                          'Make current line top of screen
                 Ed.CurLine = Ed.CurLine - (CSRLIN - Ed.TSRow)
                
              '----- Ctrl END
              CASE 117                          'Make bottom current line
                 Ed.CurLine = Ed.CurLine + ((Ed.TSRow + Ed.Rows - 1) - CSRLIN)

              '----- PgUp
              CASE 73
                 IF Ed.TL > 1 THEN              'ignore if already at the top
                    X = Ed.TL                   'save Ed.TL for a moment
                    Ed.TL = MaxInt%(1, Ed.TL - Ed.Rows)
                    X = X - Ed.TL               'calc dif. between new and old
                    Ed.CurLine = Ed.CurLine - X 'don't move cursor unless we have to
                    RePrint = -1                'Set flag to re-print window
                 END IF

              '----- PgDn
              CASE 81                           'almost at end, work backwards
                 IF Ed.TL > ArrayEnd - Ed.Rows * 2 THEN
                    X = Ed.CurLine - Ed.TL      '  from end of array
                    Ed.TL = ArrayEnd - Ed.Rows + 1
                    Ed.CurLine = Ed.TL + X      'Calc new current line
                    RePrint = -1                'Set flag to re-print window
                 ELSE
                    Ed.TL = Ed.TL + Ed.Rows     'calc top line of next page
                    Ed.CurLine = Ed.CurLine + Ed.Rows 'update current line
                    RePrint = -1                'Set flag to re-print window
                 END IF

              '----- Ctrl PgUp
              CASE 132
                 Ed.CurCol = 1                  'Make column 1 current
                 Ed.CurLine = 1                 'ditto for the current line
                 Ed.TL = 1                      'Set current top of screen
                 Ed.LC = 1                      'Set current left of screen
                 RePrint = -1                   'Set flag to re-print window

              '----- Ctrl PgDn
              CASE 118                          'we're already there - ignore
                 IF Ed.TL <= ArrayEnd - Ed.Rows THEN
                    IF Ed.LCount > 0 THEN       'empty file - go to the top
                                                'Cursor past the end, but see
                       IF Ed.CurLine > Ed.LCount THEN '  exactly where we are
                          X = Ed.CurLine - Ed.LCount
                                                'Last line showing on screen
                          IF X < Ed.Rows AND Ed.TL <= Ed.LCount THEN
                                                '  make that the current line
                             Ed.CurLine = Ed.LCount
                             LOCATE CSRLIN - X  '  and put the cursor there
                          END IF                '  too
                       END IF

                       Ed.CurLine = Ed.LCount   'Make the last line current
                       X = Ed.CurLine - Ed.TL   'See where cursor is relative
                                                '  to end
                                                'Last line is on the screen,
                       IF X <= 0 OR X >= Ed.Rows THEN
                                                '  don't re-display, just move
                                                'establish top line to display
                          Ed.TL = Ed.CurLine - (Ed.Rows - 1)
                                                'Can't have negative line #
                          Ed.TL = MaxInt%(Ed.TL, 1)
                          RePrint = -1          'Set flag to re-print window
                       END IF
                                                'Locate at end of line
                       Ed.CurCol = QPLen(array$(Ed.CurLine)) + 1
                       IF Ed.CurCol < Ed.LC THEN
                          Ed.LC = MaxInt(1, Ed.CurCol - Ed.Wide + 1)
                          RePrint = -1
                       ELSEIF Ed.CurCol > Ed.LC + Ed.Wide - 1 THEN
                          Ed.LC = Ed.CurCol - Ed.Wide + 1
                          RePrint = -1
                       END IF

                    END IF
                 END IF

              '----- Toggle INSERT mode
              CASE 82
                IF ShiftKey THEN           'Shift-Ins = Paste Buf$
                  IF LEN(Buf$) THEN        'See if there is something in Buf$
                    Temp = Ed.CurLine      'Remember which line
                    InsertingTxt = -1      'Flag that we're inserting text
                    GOSUB InsertLine       'Go do it
                    RePrint = -1           'Set flag to re-print window
                    Ed.Changed = -1
                  END IF
                ELSE
                  Insrt = Insrt * -1       'toggle insert on and off
                  GOSUB CsrSize            'set cursor size accordingly
                END IF

              '----- Delete
              CASE 83
                 'if past end, pad with spaces to avoid Illegal Function Call
                 'IF Ed.CurCol > QPLen(Array$(Ed.CurLine)) THEN Array$(Ed.CurLine) = Array$(Ed.CurLine) + SPACE$(Ed.CurCol - QPLen(Array$(Ed.CurLine)) - 1)

                 'if at or past end of line, delete last character in the line
                 IF Ed.CurCol > QPLen(array$(Ed.CurLine)) THEN
                    array$(Ed.CurLine) = array$(Ed.CurLine) + SPACE$(Ed.CurCol - QPLen(array$(Ed.CurLine)) - 1)

                    'if not on last line concatenate the next line to this one
                    IF Ed.CurLine < ArrayEnd THEN array$(Ed.CurLine) = array$(Ed.CurLine) + LTRIM$(array$(Ed.CurLine + 1))

                    Temp = Ed.CurLine + 1
                    GOSUB DeleteLine

                    RePrint = -1                'Set flag to re-print window
                    IF Ed.Wrap THEN
                       WrapLine = Ed.CurLine
                       GOSUB WrapDown           'Wrap words down
                    END IF
                 ELSE                           'Within line, delete current
                                                '  character
                    array$(Ed.CurLine) = LEFT$(array$(Ed.CurLine), Ed.CurCol - 1) + MID$(array$(Ed.CurLine), Ed.CurCol + 1)
                                                'Put new text in line buffer
                    LSET LineBuf$ = MID$(array$(Ed.CurLine), Ed.LC)
                    HideCursor
                    GPrint0VE Ed.TSRow + Ed.CurLine - Ed.TL, Ed.LSCol, LineBuf$, Ed.AColor
                    ShowCursor
                 END IF
                 IF Ed.Wrap THEN
                    WrapLine = Ed.CurLine
                    GOSUB WrapUp                'Wrap words up
                 END IF
                 Ed.Changed = -1
                 LineEd = -1

                  
              '----- Back TAB
              CASE 15                           'Calc spaces to next stop
                 X = (((Ed.CurCol - Ed.HTab - 2) \ Ed.HTab) + 1) * Ed.HTab + 1
                 X = MaxInt(X, 1)
                                                'If in Insert mode and not at
                                                '  end of line,
                 IF Insrt = -1 AND Ed.CurCol <= QPLen(array$(Ed.CurLine)) THEN
                                                'drag line to the left
                    array$(Ed.CurLine) = LEFT$(array$(Ed.CurLine), X - 1) + MID$(array$(Ed.CurLine), Ed.CurCol)
                    IF Ed.Wrap THEN
                       WrapLine = Ed.CurLine
                       GOSUB WrapUp             'Wrap words up
                    END IF
                    Ed.Changed = -1
                 END IF

                 Ed.CurCol = X                  'Adjust current column posit.

                 IF Ed.CurCol < Ed.LC THEN      'If off left of window,
                    Ed.LC = Ed.CurCol           '  adjust current window col
                    RePrint = -1                'Set flag to re-print window
                 ELSE                           'Otherwise,
                                                'Put new text in line buffer
                    LSET LineBuf$ = MID$(array$(Ed.CurLine), Ed.LC)
                    HideCursor
                    GPrint0VE Ed.TSRow + Ed.CurLine - Ed.TL, Ed.LSCol, LineBuf$, Ed.AColor
                    ShowCursor
                 END IF
               
              CASE ELSE                         'Ignore all other keys
                 Ed.UnKnownKey = -1

           END SELECT

        END IF

        IF LineEd AND Ed.CurLine <> LineWas THEN
           array$(LineWas) = RTRIM$(array$(LineWas))
           LineEd = 0
        END IF

        '----- Re-display the window if flag set or block marking complete
        IF RePrint THEN
           LOCATE Ed.TSRow, Ed.LSCol, 0         'APrint the screen
           HideCursor                           'Turn Mouse cursor off
           GAPrint0 array$(), Ed.TL, Ed.Rows, Ed.LC, Ed.Wide, Ed.AColor
           ShowCursor                           'Turn mouse cursor back on
           RePrint = 0                          'Turn Flag off
        END IF

        '----- Position the cursor
        LOCATE Ed.TSRow + Ed.CurLine - Ed.TL, Ed.LSCol + (Ed.CurCol - Ed.LC), 1
      
        '----- If just displaying the window, bail out
        IF Action = 1 OR Action = 2 OR Action = 4 THEN
           X$ = ""
           Action = 3                           'Reset Action code to 3 (idle)
           EXIT DO
        END IF

        '----- Check for a key press or mouse action
        DO
           'PC version 1.11 3/11/94
           IF Action = 0 THEN CALL GQEditCursor((Ed.LSCol + Ed.CurCol - Ed.LC) * 8 - 8, (Ed.TSRow + Ed.CurLine - Ed.TL + 1) * GPDat(71) - GPDat(71) - 1, CursorLength, CursorOn, CursorTime, CursorClr)

           X$ = INKEY$                          'Get key press

           'MMMMMMMMMMMMMMMM Start of Mouse Handling Code MMMMMMMMMMMMMMMMMMM
           'MM   If you hate rodents, exterminate the code from here to    MM
           'MM   the next set of "MMM" comments.                           MM
           'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
           '----- Get Mouse Coords. and Botton Info.
                                                'Check for button presses
           ButtonPress 1, Temp, Ed.Presses, X, Y
          
           '----- If User pressed a button
           IF Ed.Presses OR Temp THEN
              IF Ed.Presses = 0 THEN GetCursor X, Y, Temp
              Ed.MRow = (Y \ GPDat(71)) + 1     'Convert X, Y to Row and Col.

              Ed.MCol = (X \ 8) + 1

              '----- If Button just pressed
              IF Ed.Presses THEN
                 '----- Are we within the ROW limit of the TEXT?
                 IF Ed.MRow >= Ed.TSRow AND Ed.MRow < Ed.TSRow + Ed.Rows THEN

                    '----- Are we within the COLUMN limit of the TEXT?
                    IF Ed.MCol >= Ed.LSCol AND Ed.MCol < Ed.LSCol + Ed.Wide THEN
                       GOSUB CursorOff
                                                'Set the cursor row and column
                       Ed.CurLine = Ed.MRow - Ed.TSRow + Ed.TL ' to the mouse location
                       Ed.CurCol = Ed.MCol - Ed.LSCol + Ed.LC
                    END IF
                 END IF
              END IF
           END IF
          
           'MMMMMMMMMMMMMMMMMMMMMM End of Mouse Code MMMMMMMMMMMMMMMMMMMMMMMM

        LOOP UNTIL QPLen(X$) OR Ed.Presses OR Action <> 0

    LOOP WHILE Action = 0                       'Go back for another key if
                                                '  not in polling mode.

    Ed.InsStat = Insrt                           'Show the caller the Insert
                                                '  state

    EXIT SUB                                    'Bail out


'****************************************************************************
'*********************   Start of Subroutine code   *************************
'****************************************************************************

'----- Adjust the size depending on monitor type, and insert mode
CsrSize:
    IF Insrt = -1 THEN                          'If in insert mode,
       CursorLength = GPDat(71)
    ELSE                                        'Overtype mode,
       CursorLength = 2
    END IF
RETURN

WindowLimits:
    IF Ed.CurCol >= Ed.LC + Ed.Wide THEN
       Ed.LC = Ed.CurCol - Ed.Wide + 1
       RePrint = -1
    ELSEIF Ed.CurCol < Ed.LC THEN
       Ed.LC = MaxInt(1, Ed.CurCol - Ed.Wide)
       RePrint = -1
    END IF

    IF Ed.CurLine >= Ed.TL + Ed.Rows THEN
       Ed.TL = Ed.CurLine - Ed.Rows + 1
       RePrint = -1
    ELSEIF Ed.CurLine < Ed.TL THEN
       Ed.TL = Ed.CurLine
       RePrint = -1
    END IF
RETURN

'----- Move the cursor down a line
CursorDown:
    IF Ed.CurLine >= ArrayEnd THEN Ed.CurLine = ArrayEnd
                                     'if we're at the bottom of the screen
    Temp = (Ed.CurLine - Ed.TL + 1) - Ed.Rows
    IF Temp > 0 THEN
       Ed.TL = Ed.TL + Temp          '  show the top line being one higher,
       X = Ed.TSRow + Ed.Rows - 1
       HideCursor                    'Scroll the window up
       GScrollVE Ed.LSCol - 1, Ed.TSRow * GPDat(71) - GPDat(71), Ed.Wide, Ed.Rows * GPDat(71), 0, -GPDat(71) * Temp
       LineBFVE Ed.LSCol * 8 - 8, X * GPDat(71) - (Temp * GPDat(71)), (Ed.LSCol + Ed.Wide - 1) * 8 - 1, X * GPDat(71) - 1, BGClr

       FOR N = 1 TO Temp
           GPrint0VE X - N + 1, Ed.LSCol, MID$(array$(Ed.CurLine - N + 1), Ed.LC, Ed.Wide), Ed.AColor
       NEXT

       ShowCursor
    END IF
RETURN

'----- Move the cursor up a line
CursorUp:
    IF Ed.CurLine < 1 THEN Ed.CurLine = 1 'ignore if already on first line

    Temp = Ed.TL - Ed.CurLine
    IF Temp > 0 THEN                    'if we're at the top of the screen
       Ed.TL = Ed.TL - Temp             '  show the top line being one less,
       HideCursor                       'Scroll the window down
       GScrollVE Ed.LSCol - 1, Ed.TSRow * GPDat(71) - GPDat(71), Ed.Wide, Ed.Rows * GPDat(71), 0, GPDat(71) * Temp
       LineBFVE Ed.LSCol * 8 - 8, Ed.TSRow * GPDat(71) - GPDat(71), (Ed.LSCol + Ed.Wide - 1) * 8 - 1, Ed.TSRow * GPDat(71) - GPDat(71) + Temp * GPDat(71) - 1, BGClr
       FOR N = Temp TO 1 STEP -1
           GPrint0VE Ed.TSRow + N - 1, Ed.LSCol, MID$(array$(Ed.CurLine + N - 1), Ed.LC, Ed.Wide), Ed.AColor
       NEXT
       ShowCursor
    END IF
RETURN

'----- Move cursor left
CursorLeft:
    IF Ed.CurCol < 1 THEN Ed.CurCol = 1
      
    Temp = Ed.LC - Ed.CurCol
    IF Temp > 0 THEN                            'If off the left of screen
       Ed.LC = Ed.LC - Temp                     'Decrement window column
       HideCursor
       GScrollVE Ed.LSCol - 1, Ed.TSRow * GPDat(71) - GPDat(71), Ed.Wide, Ed.Rows * GPDat(71), Temp, 0
       LOCATE Ed.TSRow, Ed.LSCol
       GAPrint0 array$(), Ed.TL, Ed.Rows, Ed.LC, Temp, Ed.AColor
       ShowCursor
    END IF
RETURN

'----- Move cursor right
CursorRight:
    IF Ed.CurCol > 255 THEN Ed.CurCol = 255
                                                'If off right of window,
    Temp = (Ed.CurCol - Ed.LC + 1) - Ed.Wide
    IF Temp > 0 THEN
       Ed.LC = Ed.LC + Temp                     '  increment window column
       HideCursor
       GScrollVE Ed.LSCol - 1, Ed.TSRow * GPDat(71) - GPDat(71), Ed.Wide, Ed.Rows * GPDat(71), -Temp, 0
       LOCATE Ed.TSRow, Ed.LSCol + Ed.Wide - Temp
       GAPrint0 array$(), Ed.TL, Ed.Rows, Ed.LC + Ed.Wide - Temp, Temp, Ed.AColor
       ShowCursor
    END IF
RETURN

'----- Wrap all text to current right margin
WrapAll:
    DO UNTIL WrapLine > Ed.LCount               'Stop at last used element
        IF QPLen(array$(WrapLine)) THEN         'If it's not a blank line,
           GOSUB WrapDown                       '  wrap paragraph down and
           GOSUB WrapUp                         '  then up.
           WrapLine = Lin + 1                   'Look at line past paragraph
        ELSE                                    '  "Lin" is set in "WrapUp"
           WrapLine = WrapLine + 1              'Line was blank, look at next
        END IF
    LOOP
RETURN

'----- Wrap words down to form a paragraph
WrapDown:
    Lin = WrapLine                              'Make temp copy of line #
    LinLen = QPLen(array$(Lin))                 'Get the length of cur. line
                                                'If it's too short, bail out
    'IF LinLen < 2 OR Array$(Lin) = Par$ THEN RETURN

    P = INSTR(array$(Lin), Par$)                'Look for a paragraph marker
                                                'Do lines that are too long
    DO WHILE LinLen > Ed.Wrap OR P

       IF P > 0 AND P <= Ed.Wrap + 1 THEN       'If marker is in range,
          B = P                                 '  set cutoff to marker
          array$(Lin) = LEFT$(array$(Lin), P - 1) + MID$(array$(Lin), P + 1)
       ELSE                                     'Starting at the right margin,
                                                'look backwards for a blank
          i = QInstrB%(Ed.Wrap + 1, array$(Lin), " ")
          IF i = 0 THEN i = Ed.Wrap + 1         'If no blanks, chop it off

          B = Blanks%(MID$(array$(Lin), i)) + i 'Now look fwd. for non-blank
       END IF
       

       IF B <= LinLen THEN                      'If we didn't go past the end,
          Lin = Lin + 1                         '  increment the line number
          IF Lin > ArrayEnd THEN                'Past end?  Bail out
             Lin = Lin - 1
             Ed.MErr = 2
             'IF Action = 0 THEN EXIT SUB
             EXIT DO
          END IF

          LinLen = QPLen(array$(Lin))           'Get the length of new line
                                                'If this is a blank line or
                                                '  starts with a space
          IF LinLen = 0 OR Blanks%(array$(Lin)) OR ASCII(array$(Lin)) = 20 THEN
             Temp = Lin                         'Insert remainder of last line
             Temp$ = MID$(array$(Lin - 1), B)   '  into a new line
             GOSUB InsertLine
          ELSE                                  'Add remainder of prev. line
                                                '  to this line
             array$(Lin) = MID$(array$(Lin - 1), B) + SPACE$(FnSpaces2Pad(array$(Lin - 1))) + array$(Lin)
          END IF
                                                'If we just wrapped current
          IF Lin - 1 = Ed.CurLine THEN          '  line and the cursor was
             IF Ed.CurCol >= B THEN             '  within the wrapped words,
                Ed.CurCol = Ed.CurCol - B + 1   '  move the cursor to the new
                Ed.LC = ((Ed.CurCol \ Ed.Wide) * Ed.Wide) + 1'  line at the end of the
                Ed.CurLine = MinInt(Lin, ArrayEnd)           '  wrapped words
                Ed.TL = MaxInt%(Ed.TL, Ed.CurLine - Ed.Rows + 1)
             END IF
          END IF

          LinLen = QPLen(array$(Lin))           'Get length of the new line
                                                'Trim the prev. line
          array$(Lin - 1) = RTRIM$(LEFT$(array$(Lin - 1), B - 1))
          RePrint = -1                          'Set flag to re-print window

       ELSE                                     'Words weren't found past wrap
          Lin = Lin + 1                         'Increment the line number
          IF Lin > ArrayEnd THEN
             Lin = Lin - 1
             Ed.MErr = 2
             'IF Action = 0 THEN EXIT SUB
             EXIT DO                            'Past end?  Bail out
          END IF
          LinLen = QPLen(array$(Lin))           'Get its length
       END IF

       P = INSTR(array$(Lin), Par$)             'Look for a paragraph marker

    LOOP                                        'Process the new line

    IF P THEN
       array$(Lin) = LEFT$(array$(Lin), P - 1) + MID$(array$(Lin), P + 1)
    END IF

RETURN

'----- Wrap words Up to re-form a paragraph
WrapUp:
    Lin = WrapLine                              'Make temp copy of line #
    '----- If the line isn't nul and current line isn't past the end and the
    '----- current line doen't have a paragraph marker on the end.
    DO UNTIL Null(array$(Lin)) OR Lin >= Ed.LCount

        P = INSTR(array$(Lin + 1), Par$)
        IF P THEN
           array$(Lin + 1) = LEFT$(array$(Lin + 1), P - 1) + MID$(array$(Lin + 1), P + 1)
        END IF
        LinLen = QPLen(array$(Lin + 1))         'Get the length of next line
                                                'If it has Par marker or is
                                                '  blank, bail out
        IF Blanks%(array$(Lin + 1)) THEN EXIT DO

        Temp = FnSpaces2Pad(array$(Lin))

        IF Lin = WrapLine AND Ed.Wrap > WrapWas THEN
           Extra = WrapWas - QPLen(array$(Lin)) - Temp + 1
           GOSUB GetWordPos
           IF i THEN                            'Words were found to fill so,
              IF INSTR(LEFT$(array$(Lin + 1), i - 1), " ") THEN
                 EXIT DO
              END IF
           END IF
        END IF

        Extra = Ed.Wrap - QPLen(array$(Lin)) - Temp + 1

        GOSUB GetWordPos

        IF i THEN
           IF Lin = WrapLine AND Ed.Wrap <= WrapWas THEN
              IF INSTR(LEFT$(array$(Lin + 1), i - 1), " ") THEN
                 EXIT DO
              END IF
           END IF
                                                'Add words from next line
           array$(Lin) = array$(Lin) + SPACE$(Temp) + LEFT$(array$(Lin + 1), i)
                                                'Now delete them from next
           array$(Lin + 1) = LTRIM$(MID$(array$(Lin + 1), i + 1))
           RePrint = -1                         'Set "RePrint" flag

           IF LEN(array$(Lin + 1)) = 0 THEN     'If nothing is left of next
                                                '  line, delete it.
              Temp = Lin + 1
              GOSUB DeleteLine

              Lin = Lin - 1
              IF Lin + 1 > Ed.LCount THEN EXIT DO 'Off the end, bail out
           END IF
        END IF

        Lin = Lin + 1
    LOOP

RETURN

GetWordPos:
    IF Extra > 0 THEN                           'If there is extra space,
       IF Extra >= LinLen THEN
          i = LinLen
       ELSE
          B = Extra
          IF MID$(array$(Lin + 1), B, 1) <> " " THEN
             B = QInstrB(B, array$(Lin + 1), " ")
          END IF

          FOR i = B TO 1 STEP -1
              IF MID$(array$(Lin + 1), i, 1) <> " " THEN EXIT FOR
          NEXT
       END IF
    ELSE
       i = 0
    END IF
RETURN

'----- Insert an element into the text array at "Temp"
InsertLine:
    IF Temp <= Ed.LCount THEN
       IF InsertingTxt THEN
         InsertingTxt = 0
         T$ = Buf$
       ELSE
         T$ = Temp$
       END IF

       Ed.LCount = MinInt%(Ed.LCount + 1, ArrayEnd)
       InsertStr VARPTR(array$(Temp)), T$, Ed.LCount - Temp
    ELSE
       Ed.LCount = Temp
       array$(Temp) = Temp$
    END IF
RETURN

'----- Delete an element from the text array
DeleteLine:
    IF Temp <= Ed.LCount THEN
       IF Temp < Ed.LCount THEN
          Buf$ = array$(Temp)                   'Save deleted string
          DeleteStr VARPTR(array$(Temp)), Ed.LCount - Temp
       END IF
       array$(Ed.LCount) = ""
       Ed.LCount = Ed.LCount - 1                'Update last line number
    END IF
RETURN

CursorOff:
  IF CursorOn THEN
    CALL GQEditCursor((Ed.LSCol + Ed.CurCol - Ed.LC) * 8 - 8, (Ed.TSRow + Ed.CurLine - Ed.TL) * GPDat(71) - 1, CursorLength, CursorOn, -1, CursorClr)
  END IF
RETURN

END SUB          'Thats all folks.  And you thought editing was easy?

