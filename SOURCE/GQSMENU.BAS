DECLARE FUNCTION AltMenu% (MChoice$(), Stat%())
'*****  PullDnMsG   - Pull Down menu subprogram with Hot Key support.
'                     Resembles and behaves like MicroSoft's pull downs.
'                     The routine PullDnMsg is for EGA and VGA graphics modes

'by Don Malin & Phil Cramer & Brian C. Giedt
'Copyright (c) 1989, 1990 Crescent Software

'08-08-1990 Modified Mouse code to handle Logitec mouse (set LMRow/Col to -1)

DEFINT A-Z



DECLARE SUB LineVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB LineBVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB LineBFVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB GMove2VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GPrint0VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint2VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB DrawByteVE (BYVAL ScreenOffset%, BYVAL BitPattern%, BYVAL ByteColor%)
DECLARE SUB HideCursor ()
DECLARE SUB ShowCursor ()
DECLARE SUB ButtonPress (Button, Status, Count, x, y)
DECLARE SUB GetCursor (x, y, Button)
DECLARE SUB WaitUp ()


DECLARE SUB DotPatternMSG (BarLine%, StartCol%, Wdth%, Clr%)
DECLARE SUB UnderlineEntryMSG (y%, UnderScorePos%, ScoreColor%)
DECLARE SUB BarPrintMSG (MChoice$(), Stat%(), HKey)

DECLARE FUNCTION PullMenKeyMSG% (Ky$, MChoice$(), Stat%())
DECLARE FUNCTION AltKey ()

CONST BarLine = 1               'Line number for the menu bar
CONST BarLCol = 1               'Left column where the bar starts
CONST BarRCol = 80              'Right column where the bar ends

                                'Alt Key translation table
CONST AltChars$ = "1234567890     QWERTYUIOP    ASDFGHJKL     ZXCVBNM"

'$INCLUDE: 'COMMON.GQS'

DIM SHARED Temp, N, LastMenu, MRow, MCol, LCol, RCol, Button, AltUsed

FUNCTION AltMenu (MChoice$(), Stat())


  SaveToAddress = &HA800
  IF GPDat%(31) = 8 THEN
    SaveToAddress = &HAA00     'Address above Address of line 480
  END IF


  HideCursor
  GMove2VE 1, 0, 47, GPDat(71) + 1, SaveToAddress, 0
  ShowCursor
  BarPrintMSG MChoice$(), Stat(), -1
  Col = 3
  GOSUB PrintHiLite
  Menu = 0

  DO
    Ky$ = INKEY$
    GetCursor x, y, Button

    IF Button > 1 THEN
      WaitUp
      Ky$ = CHR$(27)
    END IF

    SELECT CASE LEN(Ky$)
      CASE 0
        Ky = 0
      CASE 1
        Ky = ASC(Ky$)
      CASE 2
        Ky = -ASC(RIGHT$(Ky$, 1))
    END SELECT

    IF Button = 1 THEN
      Row = y \ GPDat(71) + 1
      Col = x \ 8 + 1
      IF Row = 1 AND Col < 46 THEN
        Ky = 13

        SELECT CASE Col

          CASE 1 TO 8       'File menu
            AltMenu = 0
          CASE 9 TO 14      'Edit menu
            AltMenu = 1
          CASE 15 TO 20     'Draw menu
            AltMenu = 2
          CASE 21 TO 30     'Settings
            AltMenu = 3
          CASE 31 TO 45
            AltMenu = 4

        END SELECT
        EXIT FUNCTION
      ELSE
        WaitUp
        Ky = 27
      END IF
    END IF

    'IF AltKey THEN Ky = 27

    SELECT CASE Ky
      CASE 27
        HideCursor
        GMove2VE 1, 0, 47, GPDat(71) + 1, SaveToAddress, -1
        ShowCursor
        AltMenu = 27
        EXIT FUNCTION

      CASE -77         'right arrow
        Menu = Menu + 1

      CASE -75         'left arrow
        Menu = Menu - 1

      CASE 13, -72, -80 'Enter, down, up
        AltMenu = Menu
        EXIT FUNCTION

      CASE 70, 102, 69, 101, 68, 100, 83, 115, 67, 99
        Test$ = UCASE$(Ky$)
        FOR i = 0 TO 4
          IF UCASE$(MID$(MChoice$(0, i), Stat(0, i) \ 256 + 1, 1)) = Test$ THEN
            AltMenu = i
            EXIT FUNCTION
          END IF
        NEXT

    END SELECT

    IF Menu > 4 THEN Menu = 0
    IF Menu < 0 THEN Menu = 4

    IF Menu <> LastMenu THEN

      BarPrintMSG MChoice$(), Stat(), -1
      LastMenu = Menu
      SELECT CASE Menu
        CASE 0
          Col = 3
        CASE 1
         Col = 9
        CASE 2
          Col = 15
        CASE 3
          Col = 21
        CASE 4
          Col = 31
      END SELECT
      GOSUB PrintHiLite

   END IF

 LOOP

PrintHiLite:
  HideCursor
  GPrint0VE 1, Col, " " + MChoice$(0, Menu) + " ", 15

  IF GPDat(71) = 8 THEN
  '  GPrint0VE 1, Col + Stat(0, Menu) \ 256 + 1, MID$(MChoice$(0, Menu), Stat(0, Menu) \ 256 + 1, 1), 15
  ELSE
   LineVE (Col + Stat(0, Menu) \ 256) * 8, GPDat(71) - 1, (Col + Stat(0, Menu) \ 256) * 8 + 6, GPDat(71) - 1, 15
  END IF
  ShowCursor
RETURN

END FUNCTION

'****************************************************************************
'Displays the menu bar on the line specified by the constant "BarLine".
'Also initializes a "Hot Key" table for use with "PullMenKeyMSG"
'
'   MChoice$() -
'     The "MChoice$" Array holds the Menu Choices.
'     MChoice$(0, Menus) hold the Bar Titles.
'
'   Stat() -
'     This is an integer array parallel to the MChoice$ array.
'     A value in the low byte of elements (M, 0) indicates the offset + 2
'     from the left menu title to display.  The high byte of the element
'     indicates the offset of the hot key letter.
'
'Note: This is separate so your Program can display the Menu Bar before it
'      makes any calls to the PullMenKeyMSG or PullDnMSG Sub Programs.
'****************************************************************************
'
SUB BarPrintMSG (MChoice$(), Stat(), HKey) STATIC

    SHARED HotKeys$                             'Share "Menu" Hot key table
   
    LastMenu = UBOUND(MChoice$, 2)               'Get the last menu number
    HotKeys$ = STRING$(LastMenu + 1, 0)         'Make string for Hot key table
    Bar$ = SPACE$(BarRCol - BarLCol + 1)        'Make a string for Menu Bar
    Temp = 4                                    'Put first Menu title @ pos. 4
    HideCursor
    LINE (0, 0)-(8 * 46, GPDat(71) - 1), 7, BF

    FOR N = 0 TO LastMenu                       'Examine each Menu title
                                                'Get the Hot key character
                                                '  from the given offset
        HotChr$ = UCASE$(MID$(MChoice$(0, N), Stat(0, N) \ 256 + 1, 1))
        HotKey = INSTR(AltChars$, HotChr$)      'Get it's position in table
        IF HotKey <= 15 THEN                    'Is it a number?
           HotKey = 119 + HotKey                'Add 119
           IF HotKey = 119 THEN HotKey = 0      'Special case for "M"
        END IF
        MID$(HotKeys$, N + 1) = CHR$(HotKey)    'Put the character in table


          'Print the Menu Entry
        GPrint0VE BarLine, Temp, MChoice$(0, N), GPDat%(74)
        IF HKey THEN
          IF GPDat(71) = 8 THEN
            GPrint0VE BarLine, Temp + Stat(0, N) \ 256, MID$(MChoice$(0, N), 1 + Stat(0, N) \ 256, 1), GPDat(81)
          ELSE
            UnderlineEntryMSG BarLine * GPDat%(71) - 2, Temp + Stat(0, N) \ 256, GPDat%(74)
          END IF
        END IF

        IF N < LastMenu THEN                    'Get position for next title
           Temp = Temp + Stat(0, N + 1) MOD 256 + LEN(MChoice$(0, N)) + 2
        END IF
    NEXT

    LINE (0, -1)-(8 * 46, GPDat(71)), 0, B

    ShowCursor

END SUB

SUB DotPatternMSG (BarLine, StartCol, Wdth, Clr) STATIC

  HideCursor
  BitPattern = 170
  FOR M = (BarLine - 1) * GPDat%(71) + 1 TO BarLine * GPDat%(71)
    Address = M * 80 + StartCol
    BitPattern = NOT BitPattern
    FOR N = 1 TO Wdth
      DrawByteVE Address, BitPattern, Clr
      Address = Address + 1
    NEXT
  NEXT
  ShowCursor

END SUB

'****************************************************************************
' Description: Pull Down Menu Sub Program
'          by: Don Malin,  (c) Crescent Software 1988
'    modified: Phil Cramer & Brian C. Giedt, (c) 1990
'
' Parameters:
'
'   MChoice$() -
'     The "MChoice$" Array holds the Menu Choices.
'     MChoice$(0, Menus) hold the Bar Titles.
'     MChoice$(Choices, Menu) hold the Window Choices.
'     A "-" value for a choice indicates a dividing line across the menu.
'
'   Stat() -
'     This is an integer array parallel to the MChoice$ array.
'     Any value in the low byte of an element indicates that the choice is
'     inactive (Non Selectable).  The high byte of the element indicates
'     the offset (0 based) of the hot key letter.
'
'   Menu -
'     Current Menu Number.  A nonzero value on entry will cause that menu
'     to be displayed.
'
'   Choice -
'     Current Menu Choice.  A nonzero value on entry will cause that
'     choice to be displayed.
'
'   Ky$ -
'     This string will contain the character or key code that was used to
'     exit the routine.
'
'   Action -
'     Action is a flag which can be used to control the routine in a
'     multi tasking (polled) mode.  The Action commands are as follows:
'
'       0 - PullDnMS will take control until an active choice is selected
'           or the [Esc] key is pressed.  (Single Tasking Mode)
'       1 - Used on initial entry for multitasking mode.  Only saves the
'           background screen, displays the Menu bar and the current menu.
'           Sets the Action variable to 3 for successive calls.
'       2 - Redisplay the current menu.  Reset Action to 3
'       3 - On entry means just check the keyboard and handle keys used by
'           the routine and exit back to the caller.  On exit means nothing
'           was selected.
'       4 - User made a selection from an active choice or pressed the
'           Escape key so you should check the Menu and Choice variables.
'       5 - Restore the screen, erase the screen buffer used by the routine.
'
'****************************************************************************
'
SUB PullDnMsG (MChoice$(), Stat(), Menu, Choice, Ky$, Action) STATIC

    LOCATE , , 0

    AltReleased = NOT AltKey%

    IF Action = 4 THEN Action = 1       'See if they forgot to reinit. Action
    IF Action < 3 THEN                  'Redisplaying?
       IF Action < 2 THEN               'Redisplaying whole menu?
          LastMenu = UBOUND(MChoice$, 2) 'Find Last Bar Menu Choice
          LastChoice = 0
          Wdth = 0
                                       
          SaveToAddress = &HA800
          IF GPDat%(31) = 8 THEN
             SaveToAddress = &HAA00     'Address above Address of line 480
          END IF

          HideCursor
          IF Action = -1 THEN
            Action = 0
          ELSE                          'Save the Bar Line
            GMove2VE 1, (BarLine - 1) * GPDat%(71), 47, GPDat%(71) + 1, SaveToAddress, 0
          END IF

          BarPrintMSG MChoice$(), Stat(), 0 'Print the Bar Choices
          ShowCursor
          Wdth = 0
       END IF

       ArrayEnd = UBOUND(MChoice$, 1)

       GOSUB PullDisplay                'Display the Menu box
       
       IF Action THEN Action = 3        'Reset the Action flag to idle state

       PrevChoice = Choice
       PrevMenu = Menu
       ButtonPress 1, Down, Presses, MCol, MRow
       IF Down = 0 THEN GOSUB PullHiLite
    END IF

                                        'If Action flag=5, Restore the screen
    IF Action = 5 THEN
       GOSUB CleanUp
       PrevChoice = Choice
       PrevMenu = Menu
       AltUsed = 0
       EXIT SUB                         'Bail out
    END IF

    IF Choice <> PrevChoice THEN GOSUB PullHiLite
    IF Menu <> PrevMenu THEN GOSUB PullDisplay



    DO

       Ky$ = INKEY$

       'Mouse code starts here
       ButtonPress 1, Down, Presses, MCol, MRow
       IF Presses = 0 THEN GetCursor MCol, MRow, Button
       IF MRow = 0 THEN
         MRow = 1
       ELSE
         MRow = ((MRow - 1) \ GPDat%(71)) + 1
       END IF
       MCol = (MCol \ 8) + 1

       IF (MRow <> LMRow OR MCol <> LMCol OR Presses) AND Down = 1 THEN
          WasPressed = -1
          IF MRow - BarLine <= LastChoice AND MRow - BarLine > 0 AND MCol >= StartCol AND MCol <= EnBar + 2 THEN
             Choice = MRow - BarLine
             IF MChoice$(Choice, Menu) = "-" THEN
                Choice = PrevChoice
                WasPressed = 0
             END IF
             GOSUB PullHiLite
          ELSEIF MRow = BarLine THEN
             Temp = BarLCol + 2
             FOR N = 0 TO LastMenu
                 IF MCol >= Temp AND MCol <= Temp + LEN(MChoice$(0, N)) + 1 THEN EXIT FOR
                 IF N < LastMenu THEN
                    Temp = Temp + Stat(0, N + 1) MOD 256 + LEN(MChoice$(0, N)) + 2
                 END IF
             NEXT
             IF N <= LastMenu THEN Menu = N
             IF Menu <> PrevMenu THEN GOSUB PullDisplay
          END IF
          
       ELSEIF Button AND 2 THEN
          Ky$ = CHR$(27)
          DO
             GetCursor MCol, MRow, Button
          LOOP WHILE Button AND 2
       END IF

       IF Down = 0 AND WasPressed THEN
          WasPressed = 0
          IF MRow - BarLine <= LastChoice AND MRow - BarLine > 0 AND MCol >= StartCol AND MCol <= EnBar + 2 THEN
             Ky$ = CHR$(13)
          ELSEIF MRow = BarLine THEN
             IF Wdth = 0 THEN
                Ky$ = CHR$(13)
             ELSE
                GOSUB PullHiLite
             END IF
          ELSE
             Ky$ = CHR$(27)
          END IF
       END IF
          
       IF Down = 0 THEN
          LMRow = -1
          LMCol = -1
       ELSE
          LMRow = MRow
          LMCol = MCol
       END IF
       'End of mouse handling code

       IF LEN(Ky$) THEN
          K = ASC(RIGHT$(Ky$, 1))
          IF LEN(Ky$) = 2 THEN
             IF AltKey THEN
                IF K >= 120 THEN K = K - 119
                K = ASC(MID$(AltChars$, K, 1))
             ELSE
                K = -K
             END IF
          END IF

          SELECT CASE K
             CASE 13, 27
                IF Stat(Choice, Menu) MOD 256 = 0 OR K = 27 THEN
                   IF Action = 0 THEN
                      GOSUB CleanUp
                      EXIT SUB
                   ELSE
                      Action = 4
                   END IF
                ELSEIF K = 13 THEN
                   Ky$ = ""
                END IF

             CASE 32 TO 254
                IF Wdth AND K <> 45 THEN 'See if User pressed a Choice Letter
                                                'Make Key Uppercase
                   IF K >= 97 AND K <= 122 THEN K = K - 32

                   Temp = 0
             
                   DO
                      Choice = Choice + 1
                      IF Choice > LastChoice THEN
                         IF Temp THEN EXIT DO
                         Choice = 1
                         Temp = Choice
                      END IF
                      Ltr = ASC(UCASE$(MID$(MChoice$(Choice, Menu), Stat(Choice, Menu) \ 256 + 1, 1)))
                   LOOP WHILE K <> Ltr OR Stat(Choice, Menu) MOD 256

                   IF K <> Ltr OR Choice > LastChoice THEN
                      Choice = PrevChoice
                   ELSE
                      Ky$ = CHR$(13)
                      AltUsed = AltKey
                      IF Action = 0 THEN
                         GOSUB CleanUp
                         EXIT SUB
                      ELSE
                         Action = 4
                      END IF
                   END IF
                END IF

             CASE -72                   'Up Arrow Key
                IF Wdth THEN
                   Choice = Choice - 1 + (MChoice$(Choice - 1, Menu) = "-")
                   IF Choice < 1 THEN Choice = LastChoice
                END IF

             CASE -80                   'Down Arrow Key
                IF Wdth THEN
                   IF Choice >= LastChoice THEN Choice = 0
                   Choice = Choice + 1 - (MChoice$(Choice + 1, Menu) = "-")
                END IF

             CASE -71                   'Home Key
                IF Wdth THEN Choice = 1

             CASE -79                   'End Key
                IF Wdth THEN Choice = LastChoice

             CASE -75                   'Left Arrow Key
                IF Menu = 0 THEN        'If on Far Left Bar Choice, go to
                   Menu = LastMenu      '  Far Right Choice
                ELSE
                   Menu = Menu - 1      'Otherwise Decrement Bar Choice
                END IF
                GOSUB PullDisplay       'Display the New Choice Window

             CASE -77                   'Right Arrow Key
                IF Menu = LastMenu THEN 'If on Far Right Bar Choice, go to
                   Menu = 0             '  Far Left Choice
                ELSE
                   Menu = Menu + 1      'Otherwise Increment Bar Choice
                END IF
                GOSUB PullDisplay       'Display the New Choice Window

             CASE ELSE
          END SELECT

          GOSUB PullHiLite
       END IF

       PrevChoice = Choice
       PrevMenu = Menu
    LOOP WHILE Action = 0

    
    EXIT SUB

PullHiLite:
    HideCursor
    IF Wdth AND Choice <> PrevChoice THEN
       IF PrevChoice THEN
          IF Stat(PrevChoice, Menu) MOD 256 THEN
             Clr = GPDat%(77)   'Inactive menu choice
          ELSE
             Clr = GPDat%(76)   'Active menu choice
          END IF
          GPrint2VE GPDat%(71) * (PrevChoice + BarLine - 1) + 1, StartCol, "  " + MChoice$(PrevChoice, Menu) + STRING$(Wdth - LEN(MChoice$(PrevChoice, Menu)) + 2, " "), Clr
          IF GPDat(71) = 8 THEN
            GPrint2VE GPDat%(71) * (PrevChoice + BarLine - 1) + 1, StartCol + 2 + Stat(PrevChoice, Menu) \ 256, MID$(MChoice$(PrevChoice, Menu), 1 + Stat(PrevChoice, Menu) \ 256, 1), GPDat(81)
          ELSE
            UnderlineEntryMSG GPDat%(71) * (PrevChoice + BarLine) - 1, StartCol + 2 + Stat(PrevChoice, Menu) \ 256, Clr
          END IF
       END IF

       IF Choice THEN
          GPrint2VE GPDat%(71) * (Choice + BarLine - 1) + 1, StartCol, "  " + MChoice$(Choice, Menu) + STRING$(Wdth - LEN(MChoice$(Choice, Menu)) + 2, " "), GPDat%(78)

          IF GPDat(71) = 8 THEN
            GPrint2VE GPDat%(71) * (Choice + BarLine - 1) + 1, StartCol + 2 + Stat(Choice, Menu) \ 256, MID$(MChoice$(Choice, Menu), 1 + Stat(Choice, Menu) \ 256, 1), 15'GPDat(81)
          ELSE
            UnderlineEntryMSG GPDat%(71) * (Choice + BarLine) - 1, StartCol + 2 + Stat(Choice, Menu) \ 256, GPDat%(78)
          END IF

          IF Stat(Choice, Menu) MOD 256 THEN
             DotPatternMSG Choice + BarLine, StartCol + 1, LEN(MChoice$(Choice, Menu)), GPDat%(78) \ 256
          END IF
       END IF
    END IF
    ShowCursor
RETURN


PullDisplay:
    HideCursor
                                        'Restore the screen
    IF Menu <> PrevMenu OR Action < 2 THEN
       IF Wdth THEN
          GMove2VE Save2Pos, Save1Pos, Save4Pos, Save3Pos, SaveToAddress + &H100, -1
          Wdth = 0
       END IF
       IF StartCol THEN
                                        'Re-paint the Menu bar
         GPrint0VE BarLine, StartCol, " " + MChoice$(0, PrevMenu) + " ", GPDat%(74)
       END IF
       IF Menu <> PrevMenu OR Choice = 0 THEN Choice = 1
       PrevChoice = Choice
    END IF
    PrevMenu = Menu                     'Save the menu number

    StartCol = BarLCol + 2              'Find the column positions to start
    FOR C = 0 TO Menu - 1               '  and end the highlight on Bar Menu
        StartCol = StartCol + LEN(MChoice$(0, C)) + 2 + Stat(0, C + 1) MOD 256
    NEXT
    EnCol = StartCol + LEN(MChoice$(0, Menu)) + 1
                                       
                                        'Hilite the Bar Menu Choice
    GPrint0VE BarLine, StartCol, " " + MChoice$(0, Menu) + " ", GPDat%(78)
    ShowCursor
   
    Wdth = 0                            'Find the Width and # of Choices of
                                        '  the new menu
    FOR C = 1 TO ArrayEnd
       ChLen = LEN(MChoice$(C, Menu))
       IF ChLen = 0 THEN EXIT FOR
       IF ChLen > Wdth THEN Wdth = ChLen
    NEXT
    LastChoice = C - 1

    IF StartCol + Wdth + 2 > 80 THEN    'If the Right Side of the window is
       StartCol = 80 - Wdth - 2         '  off the screen, adjust the Left
    END IF                              '  Side of the window

    EnBar = StartCol + Wdth + 1         'Set the Right Side of the Window

    IF Wdth THEN                        'If there are window choices, print
                                        '  them
       HideCursor
  
       Save1Pos = BarLine * GPDat%(71)
       Save2Pos = StartCol - 1                     'The - 1 will take care of the line drawn down the left side
       Save3Pos = (LastChoice + 1) * GPDat%(71)    'The + 1 is for the shadow region
       Save4Pos = Wdth + 4 + 2 + 1                 'The + 1 is for the shadow region
       GMove2VE Save2Pos, Save1Pos, Save4Pos, Save3Pos, SaveToAddress + &H100, 0
       GPDat%(72) = SaveToAddress + &H100 + (Save3Pos * Save4Pos + 16) \ 16
     
       'CALL LineBFVE(StartCol * 8 - 8, GPDat%(71) + 1, (StartCol + Wdth + 3) * 8 - 1, (LastChoice + 1) * GPDat%(71), GPDat%(76) \ 256)

       LINE (StartCol * 8 - 8, GPDat%(71) + 1)-((StartCol + Wdth + 3) * 8 - 1, (LastChoice + 1) * GPDat%(71)), 7, BF'GPDat%(76) \ 256)


       'CALL LineBVE(StartCol * 8 - 9, GPDat%(71), (StartCol + Wdth + 3) * 8, (LastChoice + 1) * GPDat%(71) + 1, GPDat%(76) MOD 256)

       LINE (StartCol * 8 - 9, GPDat%(71))-((StartCol + Wdth + 3) * 8, (LastChoice + 1) * GPDat%(71) + 1), 0, B

       FOR C = 1 TO LastChoice
           IF MChoice$(C, Menu) = "-" THEN
               CALL LineVE(StartCol * 8 - 8, (C + 1) * GPDat%(71) - GPDat%(71) \ 2, (StartCol + Wdth + 3) * 8 - 1, (C + 1) * GPDat%(71) - GPDat%(71) \ 2, GPDat%(76) MOD 256)
           ELSE
               IF Stat(C, Menu) MOD 256 THEN
                 Clr = GPDat%(77)
               ELSE
                 Clr = GPDat%(76)
               END IF
               GPrint2VE GPDat%(71) * (C + BarLine - 1) + 1, StartCol + 2, MChoice$(C, Menu), Clr

               IF GPDat(71) = 8 THEN
                 GPrint2VE GPDat%(71) * (C + BarLine - 1) + 1, StartCol + 2 + Stat(C, Menu) \ 256, MID$(MChoice$(C, Menu), 1 + Stat(C, Menu) \ 256, 1), GPDat(81)
               ELSE
                 UnderlineEntryMSG GPDat%(71) * (C + BarLine) - 1, StartCol + 2 + Stat(C, Menu) \ 256, Clr
               END IF

           END IF
       NEXT
       ShowCursor

       PrevChoice = 0
       Choice = 1
       GOSUB PullHiLite

    END IF

RETURN


CleanUp:
    HideCursor
    IF Wdth THEN
      GMove2VE Save2Pos, Save1Pos, Save4Pos, Save3Pos, SaveToAddress + &H100, -1
      Wdth = 0
    END IF
    GMove2VE 1, (BarLine - 1) * GPDat%(71), 47, GPDat%(71) + 1, SaveToAddress, -1
    ShowCursor
RETURN

END SUB

'****************************************************************************
'Sets the low byte of a word (Stat() element) without affecting the high byte.
'Used to set the Status byte of an element of the "Stat" array without
'disturbing the "Hot key offset" byte.
'
'  Status -
'    Status word to be set.  ie. Stat(Choice, Menu)
'
'  Truth -
'    Value to set.  ie. "Truth = LEN(FileName$) = 0"
'
'Example: CALL SetStatus(Stat(5, 0), LEN(FileName$) = 0)
'****************************************************************************
'
SUB SetStatus (Status, Truth) STATIC

    Status = 256 * (Status \ 256) + ABS(Truth)

END SUB

SUB UnderlineEntryMSG (y, UnderScorePos, ScoreColor) STATIC

  IF GPDat(71) = 8 THEN
    OS = 1
  ELSE
    OS = 0
  END IF

  LineVE UnderScorePos * 8 - 8, y + OS, UnderScorePos * 8 - 2, y + OS, ScoreColor MOD 256

END SUB

