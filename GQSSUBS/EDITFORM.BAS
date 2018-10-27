DEFINT A-Z

'****** EDITFORM.BAS - GRAPHIC Form Editing Module ********************************
'       by Donald R. Malin and Phil Cramer
'       Copyright (c) 1989, 1990 Crescent Software, Inc.
'
'The following routines are provided to manage editing of data entry forms
'from your programs:
'
'    EditForm - This is the routine that handles all User input, cursor and
'               mouse activity.  It is a pollable routine so that your
'               program can monitor all the User's activity while editing.
'  PrintArray - Prints the contents of fields on the screen form.  This
'               routine is used internally by [EditForm] but can be used in
'               your program to force a redisplay after altering any data.
'   SaveField - Checks and converts a fields data and places it in the record
'               buffer [Form$(0, 0)].  As above, this routine is used
'               internaly by [EditForm], but can be used by your program to
'               check and format data before calling [PrintArray].
'UnPackBuffer - Copies and converts all data from the record buffer
'               [Form$(0, 0)] and places it in the individual elements of the
'               [Form$()] array.
'****************************************************************************

'$INCLUDE: 'ScrollB.Bi'                'Defines TYPE for scrollbars
'$INCLUDE: 'FldInfo.bi'                'Defines TYPE for field info
'$INCLUDE: 'EditForm.BI'               'Defines TYPE for form editing info
'$INCLUDE: 'QEditype.bi'               'Defines TYPE for notes editing info
'$INCLUDE: 'Common.Bi'
'$INCLUDE: 'ScrollIn.Bi'               'Defines TYPE for scrolling text fields

'---- Control Constants
CONST StayOnField = 0                  'When true (-1) causes cursor to stay
                                       '  on fields after typing off the end.
CONST SkipProtected = -1               'When true (-1) causes the cursor to
                                       '  skip over "Protected" fields.
CONST UpDnArrows = -1                  'When true (-1) enables up down arrows
                                       '  for moving from field to field
'---- Internal Constants
CONST IntChars$ = " +-1234567890"               'Legal integer characters
CONST FloatChars$ = " +-.1234567890EeDd"        '  "   floating point chars.
CONST MoneyChars$ = " $+-,.1234567890"          '  "   currency characters

CONST BlankInt% = -32767                        'Specifies blank integer
CONST BlankLng& = -2147483647                   '       "        long int.
CONST BlankSng! = -3.402823E+38                 '       "        single
'CONST BlankSng! = -1E+38                       'UnREM when using /MBF
CONST BlankDbl# = -1.79769313486231D+308        'Specifies blank double
'CONST BlankDbl# = -1E+38                       'UnREM when using /MBF
CONST BlankDate$ = "  -  -19  "

CONST SBWidth = 16                              'Button Width for scrollbars

'---- BASIC subs
DECLARE SUB CalcFields (StartOfForm%, FldNo%, Form$(), Fld() AS FieldInfoG)
DECLARE SUB Format (Float#, Fld AS FieldInfoG, Image$)
DECLARE SUB FixDate (Dat$)
DECLARE SUB FreeMouse ()
DECLARE SUB GCursor (X%, Y%, CursorLength%, CursorIs%, CursorTime%)
DECLARE SUB HScrollBar (Button%, X%, Y%, Ky$, ScrollBar AS ANY, Clean%)
DECLARE SUB ListBox (List$(), Choice, MaxLen, Rows, Ky$, HotKeyTbl(), Action)
DECLARE SUB Message (Msg$, Row)
DECLARE SUB MoveHPtr (x1%, y1%, LastX%, y2%)
DECLARE SUB MoveVPtr (x1%, y1%, LastY%, x2%)
DECLARE SUB PrintArray (FirstFld%, LastFld%, Form$(), Fld() AS FieldInfoG)
DECLARE SUB PressPButton (x1%, y1%, x2%, y2%, PB%())
DECLARE SUB QEdit (Array$(), X$, Action%, Ed AS ANY)
DECLARE SUB ReleasePButton (x1%, y1%, x2%, y2%, PB%())
DECLARE SUB SaveField (FldNo%, Form$(), Fld() AS ANY, BadFld%)
DECLARE SUB ScrollIn (Ed$, Ky$, Scroll AS ANY)
DECLARE SUB SetPBColors (Fld AS ANY, HV%)
DECLARE SUB ShadowBox (x1, y1, x2, y2, InOut)
DECLARE SUB VScrollBar (Button%, X%, Y%, Ky$, ScrollBar AS ANY, Clean%)

'---- BASIC functions
DECLARE FUNCTION EndOfForms% (Fld() AS FieldInfoG)
DECLARE FUNCTION ESwap$ (D$)
DECLARE FUNCTION GArraySize& (x1, y1, x2, y2)
DECLARE FUNCTION GetFactor# (x1%, x2%, Lo#, Hi#)
DECLARE FUNCTION Value# (E$, ErrCode%)

'---- ASM subs
DECLARE SUB ButtonPress (ButNo%, Down%, Presses%, X%, Y%)
DECLARE SUB ClearVE ()
DECLARE SUB FClose (Handle%)
DECLARE SUB FGet (Handle, Text$)
DECLARE SUB FGetA (Handle%, SEG Element AS ANY, NumBytes&)
DECLARE SUB FOpen (Filename$, Handle%)
DECLARE SUB FSeek (Handle, Offset&)
DECLARE SUB GetVMode (Mode, Page, PageSize&, Rows, Columns)
DECLARE SUB GetCursor (X, Y, Button)
DECLARE SUB GMove2VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GMove4VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GPrint0VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB HideCursor ()
DECLARE SUB LineVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB LineBVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB LineBFVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB ProperName (Text$)
DECLARE SUB ShowCursor ()
DECLARE SUB StuffBuf (Ky$)
DECLARE SUB Tokenize (Calc$, Fld() AS FieldInfoG)

'---- ASM functions
DECLARE FUNCTION ASCII% (Strng$)
DECLARE FUNCTION Date2Num% (Dat$)
DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION Exist% (Filename$)
DECLARE FUNCTION FindLast% (BYVAL Address, Size)
DECLARE FUNCTION FldNum% (FldName$, Fld() AS ANY)
DECLARE FUNCTION FUsing$ (Number$, Image$)
DECLARE FUNCTION GetPointVE% (BYVAL x1, BYVAL y1)
DECLARE FUNCTION KeyDown ()
DECLARE FUNCTION LongestStr (Array$())
DECLARE FUNCTION MaxInt% (Var1%, Var2%)
DECLARE FUNCTION MinInt% (Var1%, Var2%)
DECLARE FUNCTION Null% (Text$)
DECLARE FUNCTION Num2Date$ (Days)
DECLARE FUNCTION PDQTimer& ()
DECLARE FUNCTION QPRTrim$ (Text$)

DIM Ed AS EditInfo
DIM SHARED Temp$, HiClr, ShadeClr, TopClr
DIM SHARED CursorClr, Csrx, Csry, CursorLength, CursorIs

'
'****************************************************************************
'Handles all User input, cursor and mouse activity.  It is a pollable routine
'so that your program can monitor all the User's activity while editing.
'
'Form$() holds the field data, Help messages and formulas for the form.  It
'  must be dimensioned to the number of fields by 2 [REDIM Form$(Fields, 2)].
'  Field data is kept in the first column [Form$(N, 0)], Help messages are
'  kept in the second column [Form$(N, 1)] and Formulas go in the third
'  [Form$(N, 2)].
'
'Fld() is a TYPE array containing information about each field [Row, Column,
'  Type, etc.]
'
'Frm is a TYPE containing general information about the current edit session
'  such as the current field number, whether the form has been change, etc..
'  See "EditForm.BI" for details.
'
'Action is a flag used to control the form in the following ways:
'  Action = 1 - Initialize the form for editing.  Pads all Form$ elements
'               to their propper lengths and formats.  Displays the contents
'               of all fields on the form.  Resets Action to 3.
'  Action = 3 - This is the idle state where the actual editing is done.
'****************************************************************************
'
SUB EditFormG (Form$(), Fld() AS FieldInfoG, Frm AS FormInfoG, Action) STATIC

  SHARED Ed AS EditInfo, Instate

  '---- Initialize variables/arrays if action isn't 3
  IF Action < 2 THEN

    IF Action = -1 THEN
      PBFocusFld = 0
      MFocusFld = 0
    ELSEIF Action = -2 THEN
      IF PBFocusFld THEN GOSUB RestorePB
      IF MFocusFld THEN
        HideCursor                'Tab HiLite
        LineBVE Fld(MFocusFld).LCol, Fld(MFocusFld).Row, Fld(MFocusFld).RCol, Fld(MFocusFld).ScratchI, Fld(MFocusFld).Decimals + 24 * 256
        ShowCursor
        MFocusFld = 0
      END IF
      EXIT SUB
    ELSEIF Action = -3 THEN       'Action -3 for turning off the text cursor
      GOSUB MoveCursor
      Action = 3
      EXIT SUB
    END IF

    REDIM Hk(0)
    REDIM PB(0)
    DIM Scroll AS Scroll
    DIM ScrollBar AS ScrollBar
    PressedDown = 0
    CursorTime = 4
    LastTime = 0
    Msg$ = ""

    Frm.Edited = 0                          'New form hasn't been edited
    Instate = Frm.InsStat
    IF Frm.InsStat THEN                     'Set QEdit's insert state
      Ed.InsStat = Frm.InsStat
    ELSE
      Ed.InsStat = 1
    END IF
       
    Frm.PrevFld = 0                         'Set previous field # to 0
                                            'Check current field #
    IF Frm.FldNo <= Frm.StartEl THEN Frm.FldNo = Frm.StartEl + 1
                                            'Pad the record buffer
    IF LEN(Form$(0, 0)) = 0 THEN
      Form$(0, 0) = SPACE$(Fld(0).StorLen)
    END IF

    EndOfForm = Frm.StartEl + Fld(Frm.StartEl).Fields
    BottomRow = 0
    TopRow = 60

    '---- Check all field strings for length and format
    FOR N = 1 TO EndOfForms(Fld())

      IF Fld(N).FType = 0 THEN N = N + 1
        
      IF Fld(N).FType < MouseFld THEN
        TRow = Fld(N).Row
        BRow = TRow
      ELSE
        TRow = Fld(N).Row \ GPDat(71) + 1
        BRow = Fld(N).ScratchI \ GPDat(71) + 1
      END IF

      BottomRow = MaxInt(BottomRow, BRow)
      TopRow = MinInt(TopRow, TRow)

      '---- Skip Notes fields and Buttons
      SELECT CASE Fld(N).FType

        CASE ScrollFld, NotesFld, PButton, MouseFld
          'No formatting required

        CASE ELSE

          Temp = LEN(Form$(N, 0))           'Get current length

          Temp2 = Fld(N).RCol - Fld(N).LCol + 1
          IF Fld(N).FType >= HScrollFld THEN Temp2 = 6

          IF Temp < Temp2 THEN              'field too short?
            Form$(N, 0) = Form$(N, 0) + SPACE$(Temp2 - Temp)
          ELSEIF Temp > Temp2 THEN          'field too long
            Form$(N, 0) = LEFT$(Form$(N, 0), Temp2)
          END IF

          SELECT CASE Fld(N).FType          'Check formating
            CASE DateFld, EuroDateFld
              MID$(Form$(N, 0), 3) = "-"
              MID$(Form$(N, 0), 6) = "-"
              IF MID$(Form$(N, 0), 7, 2) = "  " THEN
                MID$(Form$(N, 0), 7, 2) = "19"
              END IF
            CASE PhoneFld
              MID$(Form$(N, 0), 1) = "("
              MID$(Form$(N, 0), 5) = ")"
              MID$(Form$(N, 0), 10) = "-"
            CASE SoSecFld
              MID$(Form$(N, 0), 4) = "-"
              MID$(Form$(N, 0), 7) = "-"
            CASE ZipFld
              MID$(Form$(N, 0), 6) = "-"
            CASE HScrollFld, VScrollFld
              IF LEN(RTRIM$(Form$(N, 0))) THEN Fld(N).Value = VAL(LTRIM$(Form$(N, 0)))
            CASE ELSE

          END SELECT
                                            'Save field into buffer
          SaveField N, Form$(), Fld(), BadFld

      END SELECT

    NEXT

'    GOSUB MoveCursor
                                            'Display the field on form
    PrintArray Frm.StartEl + 1, EndOfForm, Form$(), Fld()
    ScrRows = GPDat(85)

  END IF



  IF Action < 3 THEN
    Frm.PrevFld = 0
    Action = 3                              'Set Action to 3 (idle state)
    GOSUB SetCursor                         'Set the cursor size
  END IF



  '---- Are we moving to a new field?
  IF Frm.FldNo <> Frm.PrevFld OR TermKey THEN

    GOSUB MoveCursor

    IF SkipProtected AND NOT PressedDown THEN
      '---- Jump over protected fields
      IF Frm.FldNo > Frm.PrevFld THEN       'Going forward?
        GOSUB ForwardSkipProtected
      ELSE                                  'Going backwards
        GOSUB BackwardSkipProtected
      END IF
    END IF
       
    IF Fld(Frm.FldNo).FType < MouseFld THEN
      HideCursor
      BGClr = GetPointVE(Fld(Frm.FldNo).LCol * 8 - 1, Fld(Frm.FldNo).Row * GPDat(71) - 1)
      ShowCursor
      TextClr = Fld(Frm.FldNo).Value + BGClr * 256
      CursorClr = (BGClr XOR 15) + 6144                    'White Cursor
      IF BGClr = 15 THEN CursorClr = (BGClr XOR 0) + 6144  'Black Cursor
    END IF

    IF PBFocusFld THEN GOSUB RestorePB
    IF MFocusFld THEN
      HideCursor                        'Tab HiLite
      LineBVE Fld(MFocusFld).LCol, Fld(MFocusFld).Row, Fld(MFocusFld).RCol, Fld(MFocusFld).ScratchI, Fld(MFocusFld).Decimals + 24 * 256
      ShowCursor
      MFocusFld = 0
    END IF

    '---- Setup for Field
    Frm.DoingMult = 0                       'Init. multiple choice flag
    DoingNotes = 0                          'Init. Notes field flag
    Frm.PrevFld = Frm.FldNo                 'Set previous field #
    Frm.FldEdited = 0                       'Init. "field edited" flag
    LeftCol = Fld(Frm.FldNo).LCol           'Set left column for field
    Scrolling = 0
    Sliding = 0
    Frm.Button = 0

    IF Fld(Frm.FldNo).FType = NotesFld THEN 'Set up for "Notes" field
      Ed.Changed = 0
                                            'Set number of rows
      Ed.Rows = Fld(Frm.FldNo).ScratchI - Fld(Frm.FldNo).Row + 1
                                            'Set width
      Ed.Wide = Fld(Frm.FldNo).RCol - Fld(Frm.FldNo).LCol + 1
      Ed.Wrap = Ed.Wide - 1                 'Set word wrap column

      X& = FRE(Temp$) - 2000                'Create array to hold text
      REDIM Work$((X& \ 2 + LEN(Form$(Frm.FldNo, 0))) \ Ed.Wrap)
      Work$(1) = Form$(Frm.FldNo, 0)        'Put text in first line.
                                            '  QEdit will format it later
      HideCursor                            'Find the color of edit window
      Ed.AColor = Fld(Frm.FldNo).Value + BGClr * 256
      ShowCursor

      Ed.Frame = 0                          'No frame around edit window
      IF Frm.InsStat THEN                   'Set QEdit's insert state
        Ed.InsStat = Frm.InsStat
      ELSE
        Ed.InsStat = 1
      END IF
      Ed.Presses = 0                        'No mouse presses yet
                                            'Put cursor at upper left
      LOCATE Fld(Frm.FldNo).Row, Fld(Frm.FldNo).LCol, 0
      QAction = 1                           'Set QEdit's action flag to
                                            '  initialize state
      Ky$ = ""                              'Clear previous key press
      Ed.TL = 1
      Ed.LC = 1
      Ed.CurLine = 1
      Ed.CurCol = 1
      DoingNotes = -1                       'Set "Notes" flag
    END IF                                  'End of GQEdit set up

    FieldWas$ = Form$(Frm.FldNo, 0)
    Mask$ = ""
    Frm.TxtPos = 1                          'Start editing at left column
    PosWas = Frm.TxtPos
    FirstKey = -1                           'Flag first key

    FType = Fld(Frm.FldNo).FType            'Get fields type
    IF FType = Relational THEN FType = Fld(Frm.FldNo).ScratchI
    IF FType <> MouseFld THEN LMFld = 0

    SELECT CASE FType                       'Do additional setup
      CASE StrFld
        EdType = 0                          'Set flag for normal string
      CASE PropStrFld
        EdType = Propr
      CASE UCaseStrFld
        EdType = UCase                      'Flag "Upper Case" strings
      CASE NumericStrFld
        EdType = Intgr                      'Flag "Numeric" strings
      CASE ScrollFld
        Scroll.Start = Fld(Frm.FldNo).ScratchI  '(Starting column)
        Scroll.Wide = Fld(Frm.FldNo).RCol - Fld(Frm.FldNo).LCol + 1
        Scroll.MaxLen = Fld(Frm.FldNo).StorLen
        Scroll.Filter = Fld(Frm.FldNo).Decimals
        Scroll.Ky = 1
        Scroll.EdClr = Fld(Frm.FldNo).Value + BGClr * 256
        Scroll.NormClr = Scroll.EdClr
        Scroll.Action = 1
        Scroll.Insert = Frm.InsStat
        Scroll.Changed = 0
        Scrl$ = Form$(Frm.FldNo, 0)         'Put text in the window
        Frm.Presses = 0
        LOCATE Fld(Frm.FldNo).Row, Fld(Frm.FldNo).LCol, 0
        Scrolling = -1
        LCount = 0
      CASE IntFld, LongIntFld
        EdType = Intgr                      'Flag "Integer" editing
      CASE SngFld, DblFld
        EdType = FloatP                     'Flag "Floating Point" editing
      CASE MoneyFld
        EdType = Money                      'Flag "Currency" editing
      CASE DateFld, EuroDateFld
        EdType = Intgr                      'Flag "Integer" editing
        Mask$ = ""
      CASE PhoneFld
        EdType = Intgr                      'Flag "Integer" editing
        Frm.TxtPos = 2
        Mask$ = ""
      CASE SoSecFld
        EdType = Intgr                      'Flag "Integer" editing
        Mask$ = ""
      CASE ZipFld
        EdType = Intgr                      'Flag "Integer" editing
        Mask$ = ""
      CASE LogicalFld
        EdType = LogicalFld                 'Flag "Logical" editing
        LogicChars$ = MID$(Fld(Frm.FldNo).RelFile, 1, 1) + MID$(Fld(Frm.FldNo).RelFile, 3, 1)
      CASE MultChAFld
        EdType = MultFld                    'Flag Multiple choice editing
        IF GPDat(90) THEN
          GPDat(76) = TextClr
          GPDat(78) = BGClr + Fld(Frm.FldNo).Value * 256
        END IF
      CASE PButton
        EdType = ButtonFld
        IF Frm.FldNo <> PBFocusFld THEN
          GOSUB SavePB
          GOSUB Focus
        END IF
        PBFocusFld = Frm.FldNo
      CASE MouseFld
        EdType = MFld
        IF Frm.FldNo <> MFocusFld THEN
          HideCursor                        'Tab HiLite
          LineBVE Fld(Frm.FldNo).LCol, Fld(Frm.FldNo).Row, Fld(Frm.FldNo).RCol, Fld(Frm.FldNo).ScratchI, Fld(Frm.FldNo).Decimals + 24 * 256
          ShowCursor
        END IF
        MFocusFld = Frm.FldNo
      CASE HScrollFld, VScrollFld
        Frm.Presses = 0
        GOSUB MoveCursor
        EdType = ScrollBarFld
        ScrollBar.Action = 1
        Sliding = -1
        ScrollBar.x1 = Fld(Frm.FldNo).LCol
        ScrollBar.y1 = Fld(Frm.FldNo).Row
        ScrollBar.x2 = Fld(Frm.FldNo).RCol
        ScrollBar.y2 = Fld(Frm.FldNo).ScratchI
        ScrollBar.PointerPos = Fld(Frm.FldNo).RelHandle
        ScrollBar.LargeChange = Fld(Frm.FldNo).RelFld
        ScrollBar.SmallChange = Fld(Frm.FldNo).Indexed
        ScrollBar.Value = Fld(Frm.FldNo).Value  'Value
        ScrollBar.Lo = Fld(Frm.FldNo).LowRange
        ScrollBar.Hi = Fld(Frm.FldNo).HiRange
        ScrollBar.Blink = -1
        LastScrollPos = ScrollBar.Value
        LastVal = ScrollBar.Value
        IF FType = HScrollFld THEN
          SetPBColors ScrollBar, 0
          ScrollBar.Factor = GetFactor#(ScrollBar.x1, ScrollBar.x2, ScrollBar.Lo, ScrollBar.Hi)
        ELSE
          SetPBColors ScrollBar, -1
          ScrollBar.Factor = GetFactor#(ScrollBar.y1, ScrollBar.y2, ScrollBar.Lo, ScrollBar.Hi)
        END IF

      CASE ELSE

    END SELECT

  END IF



  '---- Poll for key/mouse activity
  TermKey = 0                               'Init. "Un-handled" key flag

  IF Frm.DoingMult THEN                     'Multiple Choice field

    IF LEN(Msg$) = 0 THEN

      Frm.Presses = 0                       'No mouse presses
      IF VAction < 0 THEN  '(-1, -2)

        '---- Calculate position for dropdown list
        Rw = Fld(Frm.FldNo).Row + 1
        IF Fld(Frm.FldNo).LCol + VertLen + 3 > 80 THEN
          C = 80 - VertLen - 2
        ELSE
          C = Fld(Frm.FldNo).LCol + 1
        END IF

        IF Rw > ScrRows - GPDat(99) - 1 THEN
          Rw = Fld(Frm.FldNo).Row - MinInt(GPDat(99), FindLast(BYVAL VARPTR(Work$(UBOUND(Work$))), UBOUND(Work$)))
        END IF

        LOCATE Rw, C                        'Position ULC of list
      END IF
                                            'Poll ListBox
      ListBox Work$(), Choice, VertLen, GPDat(99), Ky$, Hk(), VAction
      GetCursor Frm.Mx, Frm.My, Frm.Button

    END IF

  ELSEIF Scrolling THEN
    ScrollIn Scrl$, Ky$, Scroll

    GetCursor Frm.Mx, Frm.My, Frm.Button
    Frm.KeyCode = Scroll.Ky

    IF Frm.Button = 1 THEN
      Frm.MRow = Scroll.Row \ GPDat(71) + 1
      Frm.MCol = Scroll.Col \ 8 + 1
    END IF

    IF Scroll.Ky = 1000 THEN Scrolling = 0  'Clicked outside field

  ELSEIF DoingNotes THEN                    'Notes field
    QEdit Work$(), Ky$, QAction, Ed         'Poll "QEdit"

    IF Ed.UnKnownKey AND LagKey <> -59 THEN
      TermKey = -1                          'Key not handled by "QEdit"?
    END IF

    Frm.Presses = Ed.Presses                'Copy QEdit's mouse info.
    Frm.MRow = Ed.MRow
    Frm.MCol = Ed.MCol
    GetCursor Frm.Mx, Frm.My, Frm.Button

  ELSEIF Fld(Frm.FldNo).FType >= HScrollFld THEN  'Scrollbar

    GetCursor Frm.Mx, Frm.My, Frm.Button
    Ky$ = INKEY$

    IF Fld(Frm.FldNo).FType = HScrollFld THEN
      HScrollBar Frm.Button, Frm.Mx, Frm.My, Ky$, ScrollBar, 0
    ELSE
      VScrollBar Frm.Button, Frm.Mx, Frm.My, Ky$, ScrollBar, 0
    END IF

    Fld(Frm.FldNo).Value = ScrollBar.Value
    Fld(Frm.FldNo).RelHandle = ScrollBar.PointerPos
    Form$(Frm.FldNo, 0) = LTRIM$(STR$(Fld(Frm.PrevFld).Value))
                  
    IF Frm.Button = 1 THEN
      Frm.MRow = Frm.My \ GPDat(71) + 1
      Frm.MCol = Frm.Mx \ 8 + 1
    END IF

    IF Frm.Button = 1000 THEN
      Frm.MRow = Frm.My \ GPDat(71) + 1
      Frm.MCol = Frm.Mx \ 8 + 1
      Sliding = 0
      Frm.Presses = 1
    END IF

  ELSE                                      'Regular fields

    '---- Draw the cursor
    IF Fld(Frm.FldNo).FType < MouseFld THEN
      Csrx = (LeftCol + Frm.TxtPos - 1) * 8 - 8
      Csry = Fld(Frm.FldNo).Row * GPDat(71)
      GCursor Csrx, Csry, CursorLength, CursorIs, CursorTime
    END IF
    
    Ky$ = INKEY$
   
    '---- Check Mouse activity
    ButtonPress 1, N, Frm.Presses, X, Y
    CALL GetCursor(Frm.Mx, Frm.My, Frm.Button)

    IF Frm.Presses THEN
      Frm.MRow = (Frm.My \ GPDat(71)) + 1   'Convert X, Y to Row and Col.
      Frm.MCol = (Frm.Mx \ 8) + 1
    END IF

  END IF



 
  '---- Handle mouse presses
  IF (Frm.Presses OR (Frm.Button = 1 AND Fld(Frm.FldNo).FType >= MouseFld)) AND NOT PressedDown THEN  'Mouse button pressed?

    Frm.Button = 1

    GOSUB MoveCursor

    FOR N = Frm.StartEl + 1 TO EndOfForm    'Check all fields to see if
                                            '   mouse pressed in one
      SELECT CASE Fld(N).FType

        CASE MouseFld, PButton, HScrollFld, VScrollFld
          MouseX = Frm.Mx
          MouseY = Frm.My
          FldBot = Fld(N).ScratchI

        CASE NotesFld
          MouseX = Frm.MCol
          MouseY = Frm.MRow
          FldBot = Fld(N).ScratchI

        CASE ELSE
          MouseX = Frm.MCol
          MouseY = Frm.MRow
          FldBot = Fld(N).Row

      END SELECT

      IF MouseY >= Fld(N).Row AND MouseY <= FldBot THEN
        IF MouseX >= Fld(N).LCol AND MouseX <= Fld(N).RCol THEN

          IF NOT (SkipProtected AND Fld(N).Protected) THEN
            IF N = Frm.FldNo THEN           'Pressed on the current field?

              SELECT CASE Fld(N).FType

                CASE MultChAFld
                  Ky$ = CHR$(13)

                CASE LogicalFld
                  Ky$ = CHR$(32)

                CASE NotesFld, ScrollFld

                CASE IS < MouseFld
                  Frm.TxtPos = Frm.MCol - LeftCol + 1

                CASE MouseFld
                  IF Frm.Presses = 0 THEN
                    GOSUB MouseFldOn
                    MouseActivated = -1
                    Frm.KeyCode = Frm.FldNo + 255
                    EXIT SUB
                  END IF

                CASE PButton
                  IF Frm.Presses = 0 THEN
                    GOSUB ButtonDown
                    MouseActivated = -1
                  END IF

                CASE ELSE
              END SELECT

              FirstKey = 0

            ELSE                            'New field
              SELECT CASE Fld(N).FType

                CASE IS < MouseFld
                  Frm.TxtPos = Fld(N).LCol

                CASE ELSE
              END SELECT

              Scrolling = 0
              DoingNotes = 0
              Frm.FldNo = N                 'Set the new field number
            END IF

            EXIT FOR                        'All done, bail out
          END IF
        END IF
      END IF
    NEXT

    IF N > EndOfForm AND Fld(Frm.FldNo).FType <> PButton AND Fld(Frm.FldNo).FType <> MouseFld THEN              'Not on any fields?
      TermKey = -1
      Frm.FldNo = Frm.PrevFld
    END IF

  END IF

  
  '---- Mouse let go on Push Button or Mouse field
  IF Frm.Button = 0 AND PressedDown AND NOT KeyActivated THEN

    SELECT CASE FType
      CASE PButton
        GOSUB ButtonUp

      CASE MouseFld

        IF Fld(PressedEl).LowRange = 0 THEN
          GOSUB MouseFldOff
        ELSE
          GOSUB SetToggleState
          PressedDown = 0
        END IF

      CASE ELSE
    END SELECT

    IF Fld(PressedEl).Value = 13 THEN
      Frm.KeyCode = 13
      EXIT SUB
    ELSE
      GOSUB GetKyString
    END IF

  END IF


  '---- Hot key or Enter pressed on button or mouse field
  IF KeyActivated THEN
    IF NOT KeyDown% THEN

      SELECT CASE Fld(PressedEl).FType
        CASE PButton
          GOSUB ButtonUp

        CASE MouseFld
          IF Fld(PressedEl).LowRange = 0 AND PressedDown THEN
            GOSUB MouseFldOff
          ELSE
            GOSUB SetToggleState
            PressedDown = 0
          END IF

      END SELECT

      GOSUB GetKyString
      KeyReleased = -1
      KeyActivated = 0

    END IF
  END IF


  '---- Turn key $ into code
  SELECT CASE LEN(Ky$)
    CASE 0
      Frm.KeyCode = 0                       'No key pressed
    CASE 1
      Frm.KeyCode = ASC(Ky$)                'Single character key
    CASE ELSE
      Frm.KeyCode = -ASC(RIGHT$(Ky$, 1))    'Extended keys are negative
  END SELECT

  
  '---- Handle keys from "Multiple Choice" and "Notes" fields
  IF Frm.DoingMult THEN

    IF LEN(Msg$) = 0 THEN
      IF Frm.KeyCode = 13 OR Frm.KeyCode = 27 THEN
        Frm.DoingMult = 0                   'All done picking a choice
                                            'Erase the menu
        ListBox Work$(), Choice, VertLen, 0, Ky$, Hk(), 5

        IF Frm.KeyCode = 13 THEN            'Enter pressed
          LSET Form$(Frm.FldNo, 0) = Work$(Choice) 'Copy the choice to edit $
          EdType = 0
          Frm.FldEdited = -1                'Field was edited
        ELSE
          Frm.KeyCode = 0                   'Clear Escape key
        END IF

        ERASE Work$                         'Clean up temporary array
      END IF
    END IF

    IF LEN(Msg$) THEN
      Msg$ = ""
      Message Msg$, HelpRow                 'Null string clears box
    END IF

  ELSEIF DoingNotes THEN
    GOSUB ClearHelpMsg

    SELECT CASE Frm.KeyCode                 'Check for exit keys
      CASE 9, -15, -117, -119
        DoingNotes = 0
        TermKey = -1
      CASE -80                              'Down arrow. If at end of
        IF UpDnArrows AND Ed.CurLine = UBOUND(Work$) THEN '  array, goto next field
          DoingNotes = 0
          TermKey = -1
        END IF
      CASE -72                              'Up arrow. If at top of array
        IF UpDnArrows AND Ed.CurLine = 1 THEN '  goto previous field
          DoingNotes = 0
          TermKey = -1
        END IF
      CASE -59                              'Help key
        GOSUB SetUpHelp

      CASE ELSE
    END SELECT

    IF DoingNotes THEN
      SWAP Frm.KeyCode, LagKey
    END IF

  ELSEIF Scrolling THEN
    GOSUB ClearHelpMsg

    SELECT CASE Frm.KeyCode                 'Check for exit keys
      CASE -80, -72
        IF UpDnArrows THEN
          Scrolling = 0
          TermKey = -1
        END IF
      CASE 9, -117, -15, -119, 13
        Scrolling = 0
        TermKey = -1
      CASE -77        'Right
        IF Scroll.CurCol = Fld(Frm.FldNo).RCol AND Scroll.Start = Fld(Frm.FldNo).StorLen - (Fld(Frm.FldNo).RCol - Fld(Frm.FldNo).LCol) THEN
          Scrolling = 0
          TermKey = -1
          Frm.TxtPos = Fld(Frm.FldNo).StorLen
        END IF
      CASE -75        'Left
        IF Scroll.CurCol = Fld(Frm.FldNo).LCol AND Scroll.Start = 1 THEN LCount = LCount + 1
        IF LCount = 2 THEN
          Scrolling = 0
          TermKey = -1
        END IF
      CASE -59                              'Help key
        GOSUB SetUpHelp
      CASE ELSE
   END SELECT

  ELSEIF Sliding THEN
    GOSUB ClearHelpMsg

    SELECT CASE Frm.KeyCode                 'Check for exit keys
      CASE 9, -15
        TermKey = -1
        Sliding = 0
      CASE -59                              'Help key
        GOSUB SetUpHelp
      CASE ELSE
    END SELECT

  END IF

  '---- Handle keys in regular fields
  IF Frm.KeyCode <> 0 AND NOT DoingNotes AND NOT Frm.DoingMult AND NOT Scrolling AND NOT Sliding THEN

    GOSUB MoveCursor                        'Turn the cursor off

    '---- Branch according to the key pressed
    SELECT CASE Frm.KeyCode

      '---- Backspace
      CASE 8
        IF EdType <> MultFld AND EdType < MFld THEN
          Frm.TxtPos = Frm.TxtPos - 1       'Back up the text pointer
             
          IF Frm.TxtPos > 0 THEN            'Still within the field?
            LOCATE , LeftCol + Frm.TxtPos - 1, 0   'Locate 1 to the left

            IF ASCII(MID$(Mask$, Frm.TxtPos)) = 29 THEN
              DO
                IF Frm.TxtPos = 1 THEN EXIT DO
                Frm.TxtPos = Frm.TxtPos - 1       'Back up the text pointer
              LOOP UNTIL ASCII(MID$(Mask$, Frm.TxtPos)) <> 29
            END IF

            IF ASCII(MID$(Mask$, Frm.TxtPos)) <> 29 THEN
              IF Frm.InsStat THEN           'Truncate the string
                GOSUB DeleteChar
              ELSE                          'Blank the letter
                MID$(Form$(Frm.FldNo, 0), Frm.TxtPos) = " "
              END IF

              HideCursor
              GPrint0VE Fld(Frm.FldNo).Row, LeftCol + Frm.TxtPos - 1, MID$(Form$(Frm.FldNo, 0), Frm.TxtPos), TextClr'  Fld(Frm.FldNo).Value + BGClr * 256
              ShowCursor

              Frm.FldEdited = -1

            END IF
          END IF
        END IF

      '---- Letter keys
      CASE 32 TO 255
        SELECT CASE EdType                  'Filter keys
          CASE 0                            'Normal strings (no setup)

          CASE Propr                        'Capitalize first letter of
            IF Frm.TxtPos = 1 THEN          '  words in "Proper Name"
              Ky$ = UCASE$(Ky$)             '  Fields
            ELSEIF MID$(Form$(Frm.FldNo, 0), Frm.TxtPos - 1, 1) = " " THEN
              Ky$ = UCASE$(Ky$)
            END IF
          CASE UCase
            Ky$ = UCASE$(Ky$)               'Make key upper case

          CASE Intgr                        'Skip if not in legal table
            IF INSTR(IntChars$, Ky$) = 0 THEN EXIT SUB

          CASE FloatP                       'Skip if not in legal table
            IF INSTR(FloatChars$, Ky$) = 0 THEN EXIT SUB

          CASE ButtonFld
            IF NOT KeyReleased THEN GOSUB KBButtonDown

          CASE MFld
            IF NOT KeyReleased THEN GOSUB KBMouseFldOn

          CASE Money                        'Skip if not in legal table
            IF INSTR(MoneyChars$, Ky$) = 0 THEN EXIT SUB

          CASE LogicalFld
            Ky$ = UCASE$(Ky$)               'Make it upper case
            IF Frm.KeyCode = 32 THEN        'Space bar pressed. Toggle it
              IF INSTR(LogicChars$, Form$(Frm.FldNo, 0)) = 2 THEN
                Ky$ = MID$(LogicChars$, 1, 1)
              ELSE
                Ky$ = MID$(LogicChars$, 2, 1)
              END IF
            END IF                          'Skip if not one of the two
            IF INSTR(LogicChars$, Ky$) = 0 THEN EXIT SUB
            FirstKey = -1                   'Force previous letter to be
                                            '  cleared
          CASE MultFld
            GOSUB SetupMult
            StuffBuf Ky$
            EXIT SUB

          CASE ELSE
            Ky$ = ""

        END SELECT

        IF FirstKey THEN                    'Blank if first key and
          SELECT CASE FType                 '  number field
            CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld', LogicalFld
              LSET Form$(Frm.FldNo, 0) = ""
              Frm.TxtPos = 1
            CASE ELSE
          END SELECT
        END IF

        IF EdType < MouseFld AND EdType >= 0 THEN

          IF Frm.InsStat THEN               'Expand the text string
            Nd = INSTR(Frm.TxtPos, Mask$, "")

            IF Nd = 0 THEN Nd = LEN(Form$(Frm.FldNo, 0)) + 1
            MID$(Form$(Frm.FldNo, 0), Frm.TxtPos) = Ky$ + MID$(Form$(Frm.FldNo, 0), Frm.TxtPos, Nd - Frm.TxtPos - 1)
          ELSE                              'Put the new letter in string
            MID$(Form$(Frm.FldNo, 0), Frm.TxtPos) = Ky$
          END IF

          Csrx = LeftCol * 8 - 8
          HideCursor
          GPrint0VE Fld(Frm.FldNo).Row, LeftCol, Form$(Frm.FldNo, 0), TextClr'  Fld(Frm.FldNo).Value + BGClr * 256
          ShowCursor
          Frm.FldEdited = -1

          IF EdType = LogicalFld THEN       'If this is a Logical field,
                                            '  check formulas for reference
            CalcFields Frm.StartEl, Frm.FldNo, Form$(), Fld()
            IF Frm.KeyCode = 32 THEN Frm.TxtPos = Frm.TxtPos - 1
          END IF

          IF FType <> MultChAFld THEN
            IF NOT StayOnField OR Frm.TxtPos < LEN(Form$(Frm.FldNo, 0)) THEN
              Frm.TxtPos = Frm.TxtPos + 1 'Advance the cursor
            END IF
          END IF

        END IF

      '---- Left arrow
      CASE -75
        Frm.TxtPos = Frm.TxtPos - 1         'Decrement the text pointer

      '---- Right arrow
      CASE -77
        Frm.TxtPos = Frm.TxtPos + 1         'Decrement the text pointer

      '---- Home
      CASE -71
        Frm.TxtPos = 1                      'Move text pointer to 1

      '---- End
      CASE -79                              'Look backwards for non-blank
        IF EdType < MouseFld THEN
          FOR N = LEN(Form$(Frm.FldNo, 0)) TO 1 STEP -1
            IF ASCII(MID$(Form$(Frm.FldNo, 0), N, 1)) <> 32 THEN EXIT FOR
          NEXT
          Frm.TxtPos = MinInt(N + 1, LEN(Form$(Frm.FldNo, 0)))
        END IF

      '---- Insert key
      CASE -82
        Frm.InsStat = NOT Frm.InsStat       'Toggle the Insert state
        GOSUB SetCursor

      '---- Delete
      CASE -83                              'Truncate the text
        IF EdTye <> MultFld AND EdType < MouseFld THEN
          GOSUB DeleteChar                  'Print the truncated part
          HideCursor
          GPrint0VE Fld(Frm.FldNo).Row, Fld(Frm.FldNo).LCol + Frm.TxtPos - 1, MID$(Form$(Frm.FldNo, 0), Frm.TxtPos), TextClr 'Fld(Frm.FldNo).Value + BGClr * 256
          ShowCursor
          Frm.FldEdited = -1
        END IF

      '---- Up Arrow Procedure
      CASE -72
        IF UpDnArrows THEN

          IF Frm.FldNo > Frm.StartEl + 1 THEN 'If not on first field

            TFldNo = Frm.FldNo

            IF Fld(TFldNo).FType < MouseFld THEN
              Row = Fld(TFldNo).Row         'Save current row #
              LCol = Fld(TFldNo).LCol + Frm.TxtPos - 1
              RCol = LCol
            ELSE                            'Convert x/y to Col, Row
              Row = Fld(TFldNo).ScratchI \ GPDat(71) + 1'Save current row #
              LCol = Fld(TFldNo).LCol \ 8 + 1
              RCol = Fld(TFldNo).RCol \ 8 + 1
            END IF

            NxtRow = TopRow                 'Init. "Next Row" variable
            LastRow = TopRow

            '---- First look directly above
            Found = 0
            FOR N = Frm.StartEl + 1 TO EndOfForm
              IF N <> TFldNo AND Fld(N).FType < HScrollFld AND NOT (SkipProtected AND Fld(N).Protected) THEN
                IF Fld(N).FType < MouseFld THEN
                  TestLCol = Fld(N).LCol
                  TestRCol = Fld(N).RCol
                  TestRow = Fld(N).Row
                ELSE
                  TestLCol = Fld(N).LCol \ 8 + 1
                  TestRCol = Fld(N).RCol \ 8 + 1
                  TestRow = Fld(N).ScratchI \ GPDat(71) + 1
                END IF

                IF TestRCol >= LCol AND TestLCol <= RCol THEN
                  Flg = -1
                  IF Fld(N).FType > MultChAFld THEN
                    Flg = Fld(N).LCol <> Fld(TFldNo).RCol AND Fld(N).RCol <> Fld(TFldNo).LCol
                  END IF
                  IF TestRow <= Row AND Flg THEN    'Found a potential
                    NxtRow = MaxInt(NxtRow, TestRow)
                    IF NxtRow > LastRow THEN
                      LastRow = NxtRow
                      Found = -1
                      FldNumber = N
                    END IF
                  END IF
                END IF

              END IF
            NEXT

            IF Found THEN Frm.FldNo = FldNumber

            LastCol = 80
            NxtRow = TopRow                 'Init. "Next Row" variable
            LastRow = TopRow

            IF NOT Found THEN               'Find next closest thing
              FOR N = Frm.StartEl + 1 TO EndOfForm
                IF N <> TFldNo AND Fld(N).FType < HScrollFld AND NOT (SkipProtected AND Fld(N).Protected) THEN

                  IF Fld(N).FType < MouseFld THEN
                    TestLCol = Fld(N).LCol
                    TestRow = Fld(N).Row
                  ELSE
                    TestLCol = Fld(N).LCol \ 8 + 1
                    TestRow = Fld(N).Row \ GPDat(71) + 1
                  END IF

                  IF TestRow < Row THEN     'Found a potential
                    NxtRow = MaxInt(NxtRow, TestRow)
                    IF NxtRow > LastRow THEN
                      LastRow = NxtRow
                      IF LCol < LastCol THEN
                        LastCol = LCol
                        Frm.FldNo = N
                        Found = -1
                      END IF
                    END IF
                  END IF
                END IF
              NEXT
            END IF

            IF NOT Found THEN
              Frm.FldNo = Frm.StartEl + 1
            END IF

          ELSE
            Frm.FldNo = Frm.StartEl - 1
          END IF

        END IF

      '---- Down Arrow Procedure
      CASE -80
        IF UpDnArrows THEN

          IF Frm.FldNo < EndOfForm THEN     'If not on last field

            TFldNo = Frm.FldNo

            IF Fld(TFldNo).FType < MouseFld THEN
              Row = Fld(TFldNo).Row   'Save current row #
              LCol = Fld(TFldNo).LCol + Frm.TxtPos - 1
              RCol = LCol
            ELSE                            'Convert x/y to Col, Row
              Row = Fld(TFldNo).ScratchI \ GPDat(71) + 1'Save current row #
              LCol = Fld(TFldNo).LCol \ 8 + 1
              RCol = Fld(TFldNo).RCol \ 8 + 1
            END IF

            NxtRow = BottomRow + 1  'Init. "Next Row" variable
            LastRow = BottomRow + 1

            '---- First look directly below
            Found = 0
            FOR N = Frm.StartEl + 1 TO EndOfForm
              IF N <> TFldNo AND Fld(N).FType < HScrollFld AND NOT (SkipProtected AND Fld(N).Protected) THEN
                IF Fld(N).FType < MouseFld THEN
                  TestLCol = Fld(N).LCol
                  TestRCol = Fld(N).RCol'TestLCol
                  TestRow = Fld(N).Row
                ELSE
                  TestLCol = Fld(N).LCol \ 8 + 1
                  TestRCol = Fld(N).RCol \ 8 + 1
                  TestRow = Fld(N).Row \ GPDat(71) + 1
                END IF

                IF TestRCol >= LCol AND TestLCol <= RCol THEN 'AND Fld(N).ScratchI <> Fld(TFldNo).ScratchI THEN
                  Flg = -1
                  IF Fld(N).FType > MultChAFld THEN
                     Flg = Fld(N).LCol <> Fld(TFldNo).RCol AND Fld(N).RCol <> Fld(TFldNo).LCol
                  END IF
                  IF TestRow >= Row AND Flg THEN      'Found a potential
                    NxtRow = MinInt(NxtRow, TestRow)
                    IF NxtRow < LastRow THEN
                      Found = -1
                      LastRow = NxtRow
                      Frm.FldNo = N
                    END IF
                  END IF
                END IF
              END IF
            NEXT

            LastCol = 80
            IF NOT Found THEN               'Find next closest thing
              FOR N = Frm.StartEl + 1 TO EndOfForm
                IF N <> TFldNo AND Fld(N).FType < HScrollFld AND NOT (SkipProtected AND Fld(N).Protected) THEN

                  IF Fld(N).FType < MouseFld THEN
                    TestLCol = Fld(N).LCol
                    TestRow = Fld(N).Row
                  ELSE
                    TestLCol = Fld(N).LCol \ 8 + 1
                    TestRow = Fld(N).Row \ GPDat(71) + 1
                  END IF
                  
                  IF TestRow > Row THEN     'Found a potential
                    NxtRow = MinInt(NxtRow, TestRow)
                    IF NxtRow < LastRow THEN
                      LastRow = NxtRow
                      IF LCol < LastCol THEN
                        LastCol = LCol
                        Frm.FldNo = N
                        Found = -1
                      END IF
                    END IF
                  END IF
                END IF
              NEXT
            END IF

            IF NOT Found THEN
              Frm.FldNo = EndOfForm
            END IF

          ELSE
            Frm.FldNo = EndOfForm + 1
          END IF

        END IF

      '---- Enter
      CASE 13
        SELECT CASE EdType
          CASE MultFld                      'If on a Multiple choice field
            GOSUB SetupMult
            Frm.KeyCode = 0

          CASE ButtonFld
            IF NOT PressedDown AND NOT KeyReleased THEN
              GOSUB ButtonDown
              KeyActivated = -1
            END IF

            IF KeyReleased THEN
              Frm.KeyCode = Fld(Frm.FldNo).Value
            ELSE
              Frm.KeyCode = 0
            END IF

          CASE MFld
            IF NOT PressedDown AND NOT KeyReleased THEN
              GOSUB MouseFldOn
              KeyActivated = -1
              Frm.KeyCode = Frm.FldNo + 255
              EXIT SUB
            END IF
              
            IF KeyReleased THEN
              Frm.KeyCode = Fld(Frm.FldNo).Value
            ELSE
              Frm.KeyCode = 0
            END IF

          CASE ELSE                         'Regular fields
            Frm.FldNo = Frm.FldNo + 1       'Bump field number

        END SELECT

      CASE -68 TO -60                       'Function keys
        SELECT CASE Fld(Frm.FldNo).FType
          CASE PButton
            IF NOT KeyReleased THEN GOSUB KBButtonDown

          CASE MouseFld
            IF NOT KeyReleased THEN GOSUB KBMouseFldOn

        END SELECT

      '---- Tab
      CASE 9
        Frm.FldNo = Frm.FldNo + 1           'Bump field number

      '---- Shift Tab pressed
      CASE -15
        Frm.FldNo = Frm.FldNo - 1           'Decrement field number

      '---- Ctrl Home
      CASE -119
        Frm.FldNo = Frm.StartEl + 1         'Set to first field

      '---- Ctrl End
      CASE -117
        Frm.FldNo = EndOfForm               'Set to last field

      '---- Page up
      CASE -73
        Frm.FldNo = Frm.StartEl

      '---- Page down
      CASE -81
        Frm.FldNo = EndOfForm + 1

      '---- F1 - Help
      CASE -59
        GOSUB SetUpHelp

      CASE ELSE                             'Unknown key?
        TermKey = -1                        'Set flag

    END SELECT

    KeyReleased = 0
    MouseActivated = 0
    FirstKey = 0                            'No longer the first key for
                                            '  field
  END IF

  '---- See if the cursor has moved
  Movement = Frm.TxtPos - PosWas
  IF Movement THEN
    GOSUB MoveCursor
    IF Frm.PrevFld = Frm.FldNo THEN

      '---- Adjust position
      DO
        '---- Did we get past the end of the Edit$?
        IF Frm.TxtPos > LEN(Form$(Frm.FldNo, 0)) THEN
          Frm.FldNo = Frm.FldNo + 1         'Bump the field number
          Frm.TxtPos = 1
          EXIT DO
        '---- Did we get past the beginning of the Edit$?
        ELSEIF Frm.TxtPos < 1 THEN
          Frm.FldNo = Frm.FldNo - 1         'Decrement field number
          Frm.TxtPos = 1
          EXIT DO
        END IF
        '---- Skip over delimiters
        IF ASCII(MID$(Mask$, Frm.TxtPos)) <> 29 THEN EXIT DO
        Frm.TxtPos = Frm.TxtPos + Movement
        IF FType = DateFld OR FType = EuroDateFld THEN
          IF Frm.TxtPos = 7 THEN Frm.TxtPos = 9
        END IF
      LOOP

    END IF
    PosWas = Frm.TxtPos
  END IF

  '---- Did we move to a new field or encounter an unknown key
  IF Frm.FldNo <> Frm.PrevFld OR TermKey THEN

    '---- Erase prvious messages
    IF LEN(Msg$) THEN
      Msg$ = ""
      Message Msg$, HelpRow                 'Null string clears box
    END IF


    SELECT CASE Fld(Frm.PrevFld).FType

      CASE PButton
        IF PressedDown THEN
          GOSUB ButtonUp
          Frm.KeyCode = Fld(Frm.PrevFld).Value
        END IF
        IF Frm.FldNo > 0 AND Frm.FldNo <= EndOfForm THEN GOSUB RestorePB

      CASE MouseFld
        IF PressedDown THEN
          IF Fld(Frm.PrevFld).LowRange = 0 THEN
            GOSUB MouseFldOff
            Frm.KeyCode = Fld(Frm.PrevFld).Value
          ELSE
            IF NOT KeyActivated THEN GOSUB SetToggleState
            PressedDown = 0
            Frm.KeyCode = Fld(Frm.PrevFld).Value
          END IF
        END IF

        IF Frm.FldNo > 0 AND Frm.FldNo <= EndOfForm THEN
          HideCursor
          LineBVE Fld(Frm.PrevFld).LCol, Fld(Frm.PrevFld).Row, Fld(Frm.PrevFld).RCol, Fld(Frm.PrevFld).ScratchI, Fld(Frm.PrevFld).Decimals + 24 * 256
          ShowCursor
          MFocusFld = 0
        END IF

      CASE NotesFld
        QEdit Work$(), Ky$, 5, Ed           'Turn off QEdit's cursor
        Frm.FldEdited = Ed.Changed          'Copy editors "Changed" flag
        IF Frm.FldEdited THEN               'Was the text edited?
          Form$(Frm.PrevFld, 0) = ""        'Un-wrap the text and put it
          Temp = UBOUND(Work$)              '  in the form array
          Ed.LCount = FindLast%(VARPTR(Work$(Temp)), Temp)
          FOR N = 1 TO Ed.LCount
            Form$(Frm.PrevFld, 0) = Form$(Frm.PrevFld, 0) + Work$(N) + ""
          NEXT
        END IF
                                            'If moving to a new field,
        IF Frm.FldNo <> Frm.PrevFld THEN
          ERASE Work$ ' erase the array
        END IF
        Frm.InsStat = Ed.InsStat = -1       'Copy QEdit's insert state
        GOSUB SetCursor

      CASE HScrollFld, VScrollFld

        IF ScrollBar.Value <> LastScrollPos THEN Frm.FldEdited = -1

        IF Fld(Frm.PrevFld).FType = HScrollFld THEN
          HScrollBar X, Y, Frm.Button, Ky$, ScrollBar, -1
        ELSE
          VScrollBar X, Y, Frm.Button, Ky$, ScrollBar, -1
        END IF
        Fld(Frm.PrevFld).Value = ScrollBar.Value
        Fld(Frm.PrevFld).RelHandle = ScrollBar.PointerPos

        Form$(Frm.PrevFld, 0) = LTRIM$(STR$(Fld(Frm.PrevFld).Value))

      CASE ScrollFld
        Frm.FldEdited = Scroll.Changed      'Copy ScrollIn "Changed" flag

        IF Frm.FldEdited THEN               'Was the text edited?
          Form$(Frm.PrevFld, 0) = Scrl$
        END IF
                                            'If moving to a new field,
        IF Frm.FldNo <> Frm.PrevFld THEN Scrl$ = ""
        Fld(Frm.PrevFld).ScratchI = Scroll.Start

        Frm.InsStat = Scroll.Insert         'Copy ScrollIn's insert state
        GOSUB SetCursor

    END SELECT

    '---- If the field was edited, save and print it.
    IF Frm.FldEdited THEN
      Frm.Edited = -1                       'Set "Form Changed" flag
                                            'Save the field into buffer
                                            '  and format numbers
      SaveField Frm.PrevFld, Form$(), Fld(), BadFld
                                            'Use previous entry
      IF BadFld THEN SWAP Form$(Frm.PrevFld, 0), FieldWas$

      IF Fld(Frm.PrevFld).FType <> NotesFld AND Fld(Frm.PrevFld).FType <> ScrollFld THEN  'If it's not a "Notes" field
        GOSUB MoveCursor
        PrintArray Frm.PrevFld, Frm.PrevFld, Form$(), Fld()
      END IF

      '---- Display message if value out of range
      IF BadFld THEN
        '---- Make Range strings for message
        IF FType = DateFld THEN             'US date field
          Temp$ = Num2Date$(CINT(Fld(Frm.PrevFld).LowRange))
          Temp2$ = Num2Date$(CINT(Fld(Frm.PrevFld).HiRange))
        ELSEIF FType = EuroDateFld THEN     'European date field
          Temp$ = ESwap$(Num2Date$(CINT(Fld(Frm.PrevFld).LowRange)))
          Temp2$ = ESwap$(Num2Date$(CINT(Fld(Frm.PrevFld).HiRange)))
        ELSE                                'Number fields
          Temp$ = STR$(Fld(Frm.PrevFld).LowRange)
          Temp2$ = STR$(Fld(Frm.PrevFld).HiRange)
        END IF
            
        '---- Make the message string
        IF BadFld = -1 THEN
          Msg$ = "Value out of range!  ÄÄÄ(" + FieldWas$ + ")" + CHR$(13)
        ELSE
          Msg$ = "Invalid Date!  ÄÄÄ(" + FieldWas$ + ")" + CHR$(13)
        END IF
        Msg$ = Msg$ + "Valid range: " + Temp$ + " .. " + Temp2$

        GOSUB GetHelpRow
        Message Msg$, HelpRow               'Display the message box
        BEEP

        LeftCol = Fld(Frm.PrevFld).LCol     'Reset editing parameters
        Frm.TxtPos = 1
        FirstKey = -1
        Frm.FldNo = Frm.PrevFld
        Frm.KeyCode = 0
        EXIT SUB                            'Bail out
      END IF

      '---- Recalculate fields.  NOTE: This routine can be called from
      '      your module after changing a number.
      IF UBOUND(Form$, 2) THEN
        CalcFields Frm.StartEl, Frm.PrevFld, Form$(), Fld()
      END IF

    END IF

  END IF

  '---- Make sure we stay within the form
  IF Frm.FldNo > EndOfForm THEN             'Off the end?
    GOSUB SetNextPage
  ELSEIF Frm.FldNo <= Frm.StartEl THEN      'Past the beginning?
    GOSUB SetPrevPage
  END IF
   
  EXIT SUB




'---- GOSUBS

DeleteChar:
  Nd = INSTR(Frm.TxtPos + 1, Mask$, "")    'Look for skip character
  IF Nd = 0 THEN Nd = LEN(Form$(Frm.FldNo, 0)) + 1
  MID$(Form$(Frm.FldNo, 0), Frm.TxtPos) = MID$(Form$(Frm.FldNo, 0), Frm.TxtPos + 1, Nd - Frm.TxtPos - 1) + " "
RETURN

'---- Adjust the cursor size
SetCursor:
  Instate = Frm.InsStat                     'Overstrike = small cursor
  IF Frm.InsStat THEN                       'Insert = large cursor (opp QB)
    CursorLength = GPDat(71)
  ELSE
    CursorLength = 2
  END IF
RETURN

SetupMult:
  FOR C = 0 TO UBOUND(Choice$, 2)           'Find the array column that
    N = 0                                   '  goes with this field
    DO
      IF VAL(MID$(Choice$(0, C), N + 1)) = Frm.FldNo THEN EXIT FOR
      N = INSTR(N + 1, Choice$(0, C), ",")
    LOOP WHILE N
  NEXT

  IF C <= UBOUND(Choice$, 2) THEN           'If we found it, make a temp.
    REDIM Work$(UBOUND(Choice$))            '  array to hold choicesTemp$ = SPACE$(Fld(Frm.FldNo).StorLen)
    Temp$ = SPACE$(Fld(Frm.FldNo).StorLen)

    Choice = 1                              'Preset Choice to 1
    Row = 0                                 'Init number of rows
    VertLen = 0
    FOR N = 1 TO UBOUND(Choice$)            'Read choices into array
      IF LEN(Choice$(N, C)) THEN
        LSET Temp$ = Choice$(N, C)
                                            'If current $ matches Choice $
        IF Temp$ = Form$(Frm.FldNo, 0) THEN Choice = N
        Work$(N) = Choice$(N, C)
        VertLen = MaxInt(LEN(Work$(N)), VertLen)
        Row = Row + 1                       'Bump number of rows
      ELSE
        EXIT FOR
      END IF
    NEXT
    Temp$ = ""
    Frm.DoingMult = -1                      'Set Multiple choice flag
    VAction = -2
    IF GPDat(93) THEN VAction = -1
  ELSE
    Frm.FldNo = Frm.FldNo + 1               'Bump field number
  END IF
RETURN

ForwardSkipProtected:
  DO WHILE Fld(Frm.FldNo).Protected
    Frm.FldNo = Frm.FldNo + 1
    IF Frm.FldNo > EndOfForm THEN
      GOSUB SetNextPage
    END IF
  LOOP
RETURN

BackwardSkipProtected:
  DO WHILE Fld(Frm.FldNo).Protected
    Frm.FldNo = Frm.FldNo - 1
    IF Frm.FldNo <= Frm.StartEl THEN
      GOSUB SetPrevPage
    END IF
  LOOP
RETURN

SetNextPage:
  N = Frm.StartEl + Fld(Frm.StartEl).Fields + 1
  IF N < UBOUND(Fld) THEN
    IF Fld(N).Fields THEN
      Frm.StartEl = N
      Frm.FldNo = Frm.StartEl + 1
      EndOfForm = N + Fld(N).Fields
      IF Fld(Frm.FldNo).Protected AND SkipProtected THEN
        GOSUB ForwardSkipProtected
      END IF
      Action = 1
      EXIT SUB
    END IF
  END IF
  Frm.FldNo = EndOfForm
  Frm.PrevFld = 0
  IF Fld(Frm.FldNo).Protected AND SkipProtected THEN
    GOSUB BackwardSkipProtected
  END IF

RETURN

SetPrevPage:
  FOR N = Frm.StartEl - 1 TO 0 STEP -1
    IF Fld(N).FType = 0 THEN EXIT FOR
  NEXT

  IF N < Frm.StartEl AND N >= 0 THEN
    Frm.FldNo = Frm.StartEl - 1
    Frm.StartEl = N
    EndOfForm = N + Fld(N).Fields
    GOSUB BackwardSkipProtected
    Action = 1
    EXIT SUB
  ELSE
    Frm.FldNo = Frm.StartEl + 1
    IF (Fld(Frm.FldNo).Protected AND SkipProtected) THEN
      GOSUB ForwardSkipProtected
    END IF
  END IF

RETURN

SetUpHelp:
  IF LEN(Msg$) = 0 THEN
    IF LEN(Form$(Frm.FldNo, 1)) THEN        'If this field has a help $,
      Msg$ = Form$(Frm.FldNo, 1)            '  use it.
    ELSE
      Msg$ = "No help for this field."
    END IF
    GOSUB GetHelpRow
    Message Msg$, HelpRow                   'Display the message
  END IF
RETURN

GetHelpRow:
  IF BadFld AND (Frm.PrevFld <> Frm.FldNo) THEN
    FldNumber = Frm.PrevFld
  ELSE
    FldNumber = Frm.FldNo
  END IF

  IF FType < MouseFld THEN
    HelpRow = Fld(FldNumber).Row + 2
  ELSE
    IF Fld(FldNumber).Row > GPDat(44) \ 2 THEN
      HelpRow = 5
    ELSE
      HelpRow = (GPDat(85) - 6) - LEN(Form$(FldNumber, 1)) \ 64
    END IF
  END IF
RETURN

ClearHelpMsg:
  IF Frm.KeyCode AND LEN(Msg$) THEN         'Clear any help message
    Msg$ = ""
    Message Msg$, HelpRow
  END IF
RETURN

MoveCursor:
  IF CursorIs THEN
    GCursor Csrx, Csry, CursorLength, CursorIs, -1
  END IF
RETURN


SavePB:
  PBHite = Fld(Frm.FldNo).ScratchI - Fld(Frm.FldNo).Row + 1
  PBLCol = Fld(Frm.FldNo).LCol \ 8 + 1
  PBRCol = Fld(Frm.FldNo).RCol \ 8 + 1
  PBCols = PBRCol - PBLCol + 1

  HSize = ((PBRCol - PBLCol + 1) * 4) * 3 + 4 'Number of bytes
  VSize = ((PBHite - 8) * 4) * 2 + 4

  REDIM Top(HSize \ 2 - 2)                  'Convert to int array starting @ 0
  REDIM Bot(HSize \ 2 - 2)
  REDIM LSide(VSize \ 2 - 2)
  REDIM RSide(VSize \ 2 - 2)

  HideCursor
  GMove4VE PBLCol, Fld(Frm.FldNo).Row + 1, PBCols, 3, VARSEG(Top(0)), 0
  GMove4VE PBLCol, Fld(Frm.FldNo).ScratchI - 3, PBCols, 3, VARSEG(Bot(0)), 0
  GMove4VE PBLCol, Fld(Frm.FldNo).Row + 4, 2, PBHite - 8, VARSEG(LSide(0)), 0
  GMove4VE PBRCol - 1, Fld(Frm.FldNo).Row + 4, 2, PBHite - 8, VARSEG(RSide(0)), 0
  ClearVE
  ShowCursor
RETURN

RestorePB:
  HideCursor
  GMove4VE PBLCol, Fld(PBFocusFld).Row + 1, PBCols, 3, VARSEG(Top(0)), -1
  GMove4VE PBLCol, Fld(PBFocusFld).ScratchI - 3, PBCols, 3, VARSEG(Bot(0)), -1
  GMove4VE PBLCol, Fld(PBFocusFld).Row + 4, 2, PBHite - 8, VARSEG(LSide(0)), -1
  GMove4VE PBRCol - 1, Fld(PBFocusFld).Row + 4, 2, PBHite - 8, VARSEG(RSide(0)), -1
  ClearVE
  ShowCursor
  PBFocusFld = 0
RETURN

Focus:
  HideCursor
  FHClr = GetPointVE(Fld(Frm.FldNo).LCol + 2, Fld(Frm.FldNo).Row + 2)
  FLClr = GetPointVE(Fld(Frm.FldNo).RCol - 2, Fld(Frm.FldNo).ScratchI - 2)

  '---- Draw inner black outline, Top hilite, Left hilite, Bottom Shadow, Right shadow
  LineBVE Fld(Frm.FldNo).LCol + 1, Fld(Frm.FldNo).Row + 1, Fld(Frm.FldNo).RCol - 1, Fld(Frm.FldNo).ScratchI - 1, 0
  LineVE Fld(Frm.FldNo).LCol + 3, Fld(Frm.FldNo).Row + 3, Fld(Frm.FldNo).RCol - 3, Fld(Frm.FldNo).Row + 3, FHClr
  LineVE Fld(Frm.FldNo).LCol + 3, Fld(Frm.FldNo).Row + 3, Fld(Frm.FldNo).LCol + 3, Fld(Frm.FldNo).ScratchI - 3, FHClr
  LineVE Fld(Frm.FldNo).LCol + 3, Fld(Frm.FldNo).ScratchI - 3, Fld(Frm.FldNo).RCol - 3, Fld(Frm.FldNo).ScratchI - 3, FLClr
  LineVE Fld(Frm.FldNo).RCol - 3, Fld(Frm.FldNo).Row + 3, Fld(Frm.FldNo).RCol - 3, Fld(Frm.FldNo).ScratchI - 3, FLClr
  ShowCursor
  PBFocusFld = Frm.FldNo
RETURN

ButtonDown:
  CALL PressPButton(Fld(Frm.FldNo).LCol + 1, Fld(Frm.FldNo).Row + 1, Fld(Frm.FldNo).RCol - 1, Fld(Frm.FldNo).ScratchI - 1, PB())
  PressedEl = Frm.FldNo
  PressedDown = -1
RETURN

ButtonUp:
  CALL ReleasePButton(Fld(PressedEl).LCol + 1, Fld(PressedEl).Row + 1, Fld(PressedEl).RCol - 1, Fld(PressedEl).ScratchI - 1, PB())
  PressedDown = 0
  KeyActivated = 0
RETURN

KBButtonDown:
  IF NOT MouseActivated THEN
    '---- Pressing the hot key?
    IF (Frm.KeyCode = Fld(Frm.FldNo).Value) OR (Frm.KeyCode - 32 = Fld(Frm.FldNo).Value) THEN
      IF NOT PressedDown THEN GOSUB ButtonDown
      Frm.KeyCode = 0
      KeyActivated = -1
    END IF
  END IF
RETURN

MouseFldOn:
  HideCursor
  LineBFVE Fld(Frm.FldNo).LCol + 1, Fld(Frm.FldNo).Row + 1, Fld(Frm.FldNo).RCol - 1, Fld(Frm.FldNo).ScratchI - 1, Fld(Frm.FldNo).RelFld + 6144
  ShowCursor
  PressedEl = Frm.FldNo
  PressedDown = -1
RETURN

MouseFldOff:
  HideCursor
  LineBFVE Fld(PressedEl).LCol + 1, Fld(PressedEl).Row + 1, Fld(PressedEl).RCol - 1, Fld(PressedEl).ScratchI - 1, Fld(PressedEl).RelFld + 6144
  ShowCursor
  PressedDown = 0
  KeyActivated = 0
RETURN

KBMouseFldOn:
  IF NOT MouseActivated THEN
    '---- Pressing the hot key?
    IF (Frm.KeyCode = Fld(Frm.FldNo).Value) OR (Frm.KeyCode - 32 = Fld(Frm.FldNo).Value) THEN
      IF NOT PressedDown THEN
        GOSUB MouseFldOn
        Frm.KeyCode = Frm.FldNo + 255
      ELSE
        Frm.KeyCode = 0
      END IF
      KeyActivated = -1
    END IF
  END IF
RETURN

SetToggleState:
  IF Fld(PressedEl).LowRange THEN
    IF Form$(PressedEl, 0) <> "X" THEN
      Form$(PressedEl, 0) = "X"
      Fld(PressedEl).LowRange = -2
    ELSEIF Form$(PressedEl, 0) = "X" THEN
      Form$(PressedEl, 0) = " "
      Fld(PressedEl).LowRange = -1
    END IF
  END IF
RETURN

GetKyString:
  IF Fld(PressedEl).Value < 0 THEN
    Ky$ = CHR$(0) + CHR$(ABS(Fld(PressedEl).Value))
  ELSE
    Ky$ = CHR$(Fld(PressedEl).Value)
  END IF
RETURN

END SUB

FUNCTION EndOfForms (Fld() AS FieldInfoG) STATIC

  s = 0
  DO
    LFld = s + Fld(s).Fields
    IF LFld + 1 < UBOUND(Fld) THEN
      IF Fld(LFld + 1).Fields THEN
        s = LFld + 1
      END IF
    END IF
  LOOP WHILE s = LFld + 1

  EndOfForms = LFld

END FUNCTION

FUNCTION ESwap$ (Dt$)

  Tmp$ = Dt$
  MID$(Tmp$, 1, 2) = MID$(Dt$, 4, 2)
  MID$(Tmp$, 4, 2) = MID$(Dt$, 1, 2)
  ESwap$ = Tmp$
  
END FUNCTION

'---- Fixes improperly formatted date strings
'
SUB FixDate (Dat$) STATIC
      
  Psn = 1                                     'Examine first character
  DO                                          'Trim the number part
    Temp$ = LTRIM$(RTRIM$(MID$(Dat$, Psn, 2)))
    IF LEN(Temp$) = 1 THEN                    'If it's only one character,
      MID$(Dat$, Psn) = "0" + Temp$           'Add a leading 0
    END IF
    MID$(Dat$, Psn + 2) = "-"                 'Force "-" for delimiter
    Psn = Psn + 3                             'Set to next part
  LOOP UNTIL Psn > 4

  Temp$ = LTRIM$(RTRIM$(MID$(Dat$, 7)))       'Get the trimmed year part
  IF LEN(Temp$) < 4 THEN                      'If its not 4 characters,
    IF Temp$ <> "19" AND Temp$ <> "20" THEN   '  add century to year
      MID$(Dat$, 7) = "19" + Temp$
    END IF
  END IF
  Temp$ = ""

END SUB

'---- Returns the field number for a given field name.
'
FUNCTION FldNum% (FldName$, Fld() AS FieldInfoG) STATIC
    
  IF LEN(Test$) = 0 THEN Test$ = SPACE$(8)
  LSET Test$ = UCASE$(FldName$)

  FOR N = 1 TO Fld(0).Fields
    IF Fld(N).FldName = Test$ THEN EXIT FOR
  NEXT

  IF N <= Fld(0).Fields THEN
    FldNum = N
  ELSE
    FldNum = 0
  END IF

END FUNCTION

'---- Formats a number into a "Field" string (Image$)
'
SUB Format (Float#, Fld AS FieldInfoG, Image$) STATIC

  FType = Fld.FType                           'Get the field's type
  IF FType = Relational THEN FType = Fld.ScratchI
                                              'If places greater than -1
  IF Fld.Decimals > -1 AND Fld.FType < HScrollFld THEN '  do formatting
    EffLen = LEN(Image$)                      'Find effective length
    LSET Image$ = STRING$(EffLen, "#")        'Fill field with "#"s

    IF Fld.Decimals THEN                      'If there are decimal places
      EffLen = EffLen - Fld.Decimals - 1      'Find place to put ",."
                                              'Put them in field
      IF EffLen > 0 THEN MID$(Image$, EffLen) = ",."
    ELSE                                      'Otherwise
                                              '  put a "," at end of string
      IF EffLen > 0 THEN MID$(Image$, EffLen) = ","
    END IF

    IF FType = MoneyFld THEN MID$(Image$, 1) = "$$"'If this is a currency field,
                                              '  put "$$" at beginning
    LSET Image$ = FUsing$(STR$(Float#), Image$)'Format the number into the
                                              '  field
    IF FType = MoneyFld THEN
      IF INSTR(Fld.ScratchS, "$") = 0 THEN
        Temp$ = QPRTrim$(Fld.ScratchS)
        D = INSTR(Image$, "$")
        p = D - LEN(Temp$) + 1
        IF LEN(Temp$) > 1 THEN
          Temp$ = Temp$ + " "
          p = p - 1
        END IF
        p = MaxInt(1, p)
        MID$(Image$, p, D) = Temp$
      END IF
    END IF

  ELSE                                        'Otherwise let basic make the
    RSET Image$ = LTRIM$(STR$(Float#))        '  string
  END IF

END SUB

SUB GCursor (X, Y, CursorLength, CursorIs, CursorTime) STATIC

  SHARED CursorClr

  ThisTime = PDQTimer& MOD CursorTime
  IF ThisTime < LastTime OR CursorTime = -1 THEN
    HideCursor
                            'based on bottom left corner of cursor
    LineBFVE X, Y - 1, X + 7, Y - CursorLength, CursorClr

    ShowCursor
    CursorIs = NOT CursorIs
    IF CursorTime = -1 THEN
      ThisTime = 32000      'Make sure cursor comes back fast
    END IF
  END IF
  LastTime = ThisTime

END SUB

'---- Displays or clears a message within a box on screen
'
SUB Message (Msg$, Row) STATIC

  IF LEN(Msg$) THEN                           'If the message isn't null

    Msg$ = LEFT$(Msg$, 350)                   '6 lines max

    REDIM MArray$(10)                         'Make a temporary array to
                                              '  hold the parsed up lines
    MesLen = LEN(Msg$)                        'Get the total length of the
    Wdth = MinInt(58, MesLen)                 '  help message
                                            
    Ln = 0                                    'Line number
    Strt = 1                                  'Starting parse position

    '---- Parse the message into lines
    DO
      Nd = Strt + Wdth                        'Guess at the end of line

      CR = INSTR(Strt, Msg$, CHR$(13))        'Look for Chr$(13)
      IF CR > 0 AND CR < Nd THEN              'Chop line off
        Nd = CR
      ELSE                                    'Loop backwards to find
        WHILE MID$(Msg$, Nd, 1) <> " " AND Nd <= MesLen AND Nd > Strt
          Nd = Nd - 1                         '  blank space
        WEND
      END IF

      Ln = Ln + 1                             'Increment the Line number

      '---- Pull out the new line
      MArray$(Ln) = MID$(Msg$, Strt, Nd - Strt)
      Strt = Nd + 1                           'Reset starting posit. for
                                              '  next line
    LOOP UNTIL Nd >= MesLen OR Ln > 10        'Get more unless @ end of mes

    Wdth = LongestStr(MArray$())

    Rows = GPDat(85)
    BotRow = Row + Ln + 1                     'Find the bottom row #
    IF BotRow >= Rows THEN
      Row = Row - Ln - 4
      BotRow = Row + Ln + 1
    END IF

    BoxWdth = Wdth + 2                        'Find the outer box width
    ColPos = 40 - (BoxWdth \ 2)               'Starting position for
                                              '   centered box
    HideCursor
    SaveRows = (BotRow - Row + 1) * GPDat(71) + 7
       
    OS = 0
    IF GPDat(71) = 8 THEN OS = 2

    TopLn = (Row * GPDat(71) - GPDat(71)) - OS
    BotLn = (BotRow * GPDat(71)) - OS
    MLCol = ColPos * 8
    MRCol = (ColPos + BoxWdth - 1) * 8
    IF GPDat(71) > 8 THEN BotLn = BotLn - 4


    IF GPDat(93) THEN
      Sz& = (BoxWdth + 3) * SaveRows * 4& + 4
      IF FRE(-1) < Sz& + 1000 OR (Sz& \ 4) > 32000 THEN
        BEEP
        EXIT SUB
      END IF
      REDIM MsgArray&(Sz& \ 4)
      GMove4VE ColPos - 1, TopLn, BoxWdth + 3, SaveRows, VARSEG(MsgArray&(0)), 0
    ELSE
      GMove2VE ColPos - 1, TopLn, BoxWdth + 3, SaveRows, GPDat(72), 0
    END IF
    ClearVE
       
    LineBFVE MLCol - 12, TopLn, MRCol, BotLn, GPDat(95)
    ShadowBox MLCol - 12, TopLn, MRCol, BotLn, -1
    ShadowBox MLCol - 9, TopLn + 6, MRCol - 3, BotLn - 3, 0
    LineBVE MLCol - 8, TopLn + 7, MRCol - 4, BotLn - 4, GPDat(94)
    '---- Draw shadow
    LineBFVE MLCol, BotLn + 1, MRCol + 4, BotLn + 5, 0
    LineBFVE MRCol + 1, TopLn + 8, MRCol + 5, BotLn + 5, 0

    FOR i = 1 TO Ln
      GPrint0VE Row + i, ColPos + 1, MArray$(i), GPDat(94) + GPDat(95) * 256
    NEXT
    ShowCursor

    Pass = 1                                  'Set flag say we've been here
    ERASE MArray$                             'Don't need this anymore

  ELSEIF Pass = 1 THEN                        'If we've been here before,

    HideCursor
    IF GPDat(93) THEN
      GMove4VE ColPos - 1, TopLn, BoxWdth + 3, SaveRows, VARSEG(MsgArray&(0)), -1
    ELSE
      GMove2VE ColPos - 1, TopLn, BoxWdth + 3, SaveRows, GPDat(72), -1
    END IF
    ClearVE
    ShowCursor                                '  restore the screen
       
    ERASE MsgArray&
    Pass = 0                                  'Reset flag for next call
  END IF

END SUB

SUB PressPButton (x1, y1, x2, y2, PB())

  Size& = GArraySize&(x1 + 3, y1 + 3, x2 - 3, y2 - 3) \ 2

  IF (Size& < FRE(-1) \ 2) AND (Size& < 32000) THEN
    REDIM PB(Size&)

    HideCursor
    LineBVE x1, y1, x2, y2, 0

    TopClr = GetPointVE(x1 + 4, y1 + 4)
    ShadeClr = GetPointVE(x2 - 1, y2 - 2)
    HiClr = GetPointVE(x1 + 1, y1 + 2)

    GET (x1 + 3, y1 + 3)-(x2 - 3, y2 - 3), PB
    PUT (x1 + 5, y1 + 5), PB, PSET

    LineBFVE x1 + 1, y1 + 1, x2 - 1, y1 + 4, TopClr
    LineBFVE x1 + 1, y1 + 1, x1 + 4, y2 - 1, TopClr

    LineVE x1 + 1, y1 + 1, x2 - 1, y1 + 1, ShadeClr
    LineVE x1 + 1, y1 + 1, x1 + 1, y2 - 1, ShadeClr
    ShowCursor
  ELSE
    REDIM PB(0)
  END IF

END SUB

'****************************************************************************
'Prints the contents of fields on the screen form.  This routine is used
'internally by [EditForm] but can be called from your program to force a
'redisplay after altering any data.
'
'FirstFld is the first field number to print.
'
'LastFld is the last field to print.  These two parameters are used to
'  specify a range of fields to print.
'
'Form$() is the form data array containing the text of all fields.
'
'Fld() is the field definition TYPE array.
'****************************************************************************
'
SUB PrintArray (FirstFld, LastFld, Form$(), Fld() AS FieldInfoG) STATIC

  SHARED Ed AS EditInfo, Instate, SlideClr
  DIM Scroll AS Scroll

  HideCursor                                  'Turn mouse cursor off
  IF CursorIs THEN                            'Turn blinking cursor off
    GCursor Csrx, Csry, CursorLength, CursorIs, -1
  END IF


  FirstFld = MaxInt(1, FirstFld)              'Make sure first and last are
  IF LastFld = 0 THEN LastFld = EndOfForms%(Fld()) '  set

  FOR N = FirstFld TO LastFld                 'Do each field within range

    IF Fld(N).FType = 0 THEN N = N + 1

    IF Fld(N).FType > 21 THEN
      DIM ScrollBar AS ScrollBar
      ScrollBar.x1 = Fld(N).LCol
      ScrollBar.x2 = Fld(N).RCol
      ScrollBar.y1 = Fld(N).Row
      ScrollBar.y2 = Fld(N).ScratchI
    END IF

    SELECT CASE Fld(N).FType
      CASE NotesFld                           'Is this a "Notes" field

        IF Instate THEN                       'Set QEdit's insert state
          Ed.InsStat = Instate
        ELSE
          Ed.InsStat = 1
        END IF

        Ed.Rows = Fld(N).ScratchI - Fld(N).Row + 1
        Ed.Wide = Fld(N).RCol - Fld(N).LCol + 1
        Ed.Wrap = Ed.Wide - 1
        HideCursor

        BGClr = GetPointVE(Fld(N).LCol * 8 - 1, Fld(N).Row * GPDat(71) - 1)'GPDat(71))
        Ed.AColor = Fld(N).Value + BGClr * 256
           
        Ed.Frame = 0

        X& = FRE("") - 1000                   'Create array to hold text
        REDIM Work$((X& \ 2 + LEN(Form$(N, 0))) \ Ed.Wrap)
        Work$(1) = Form$(N, 0)                'Put text in array

        LOCATE Fld(N).Row, Fld(N).LCol
        QEdit Work$(), "", 1, Ed              'Let QEdit display the text

        ShowCursor
        ERASE Work$                           'Erase the temporary array

      CASE ScrollFld
        '---- Read background color from the screen
        BGClr = GetPointVE(Fld(N).LCol * 8 - 1, Fld(N).Row * GPDat(71) - 1)'GPDat(71))
        Scroll.Start = Fld(N).ScratchI  '(Starting column)
        Scroll.Ky = 1'Scroll.Start
        Scroll.EdClr = Fld(N).Value + BGClr * 256
        Scroll.NormClr = Scroll.EdClr
        Scroll.Action = -1
        Scroll.MaxLen = Fld(N).StorLen
        Scroll.Wide = Fld(N).RCol - Fld(N).LCol + 1

        LOCATE Fld(N).Row, Fld(N).LCol, 0
        ScrollIn Form$(N, 0), Ky$, Scroll

      CASE IS < MouseFld                      'Regular fields
        '---- Read background color from the screen
        BGClr = GetPointVE(Fld(N).LCol * 8 - 1, Fld(N).Row * GPDat(71) - 1)
        GPrint0VE Fld(N).Row, Fld(N).LCol, Form$(N, 0), Fld(N).Value + BGClr * 256

      CASE MouseFld
        LastRange = Fld(N).LowRange

        IF Form$(N, 0) = "X" AND Fld(N).LowRange = -1 THEN
          Fld(N).LowRange = -2
        END IF

        IF Form$(N, 0) = " " AND Fld(N).LowRange = -2 THEN
          Fld(N).LowRange = -1
        END IF

        IF LastRange <> Fld(N).LowRange THEN
          HideCursor
          LineBFVE Fld(N).LCol + 1, Fld(N).Row + 1, Fld(N).RCol - 1, Fld(N).ScratchI - 1, Fld(N).RelFld + 6144
          ShowCursor
        END IF

      CASE HScrollFld         'H ScrollBar
        ScrollBar.PointerPos = Fld(N).RelHandle
        ScrollBar.x1 = Fld(N).LCol
        SetPBColors ScrollBar, 0
        PtrWas = Fld(N).RelHandle
        Factor# = GetFactor#(Fld(N).LCol, Fld(N).RCol, Fld(N).LowRange, Fld(N).HiRange)
        Fld(N).RelHandle = (Fld(N).Value - Fld(N).LowRange) / Factor# + Fld(N).LCol + SBWidth
        MoveHPtr Fld(N).RelHandle, Fld(N).Row, PtrWas, Fld(N).ScratchI

      CASE VScrollFld         'V ScrollBar
        ScrollBar.PointerPos = Fld(N).RelHandle
        ScrollBar.y1 = Fld(N).Row
        SetPBColors ScrollBar, -1
        PtrWas = Fld(N).RelHandle
        Factor# = GetFactor#(Fld(N).Row, Fld(N).ScratchI, Fld(N).LowRange, Fld(N).HiRange)
        Fld(N).RelHandle = (Fld(N).Value - Fld(N).LowRange) / Factor# + Fld(N).Row + SBWidth
        MoveVPtr Fld(N).LCol, Fld(N).RelHandle, Fld(N).RCol, PtrWas
      CASE ELSE

    END SELECT

  NEXT

  ShowCursor

END SUB

SUB ReleasePButton (x1, y1, x2, y2, PB())

  IF UBOUND(PB) THEN
    HideCursor
    PUT (x1 + 3, y1 + 3), PB, PSET
                          'Draw Shadow
    LineBFVE x1 + 1, y2 - 2, x2 - 2, y2 - 1, ShadeClr
    LineBFVE x2 - 1, y1 + 2, x2 - 2, y2 - 1, ShadeClr
                          'Draw HiLite
    LineVE x1 + 1, y1 + 1, x2 - 2, y1 + 1, HiClr
    LineVE x1 + 1, y1 + 1, x1 + 1, y2 - 2, HiClr
                         'Draw Hilite
    LineVE x1 + 2, y1 + 2, x2 - 3, y1 + 2, HiClr
    LineVE x1 + 2, y1 + 2, x1 + 2, y2 - 3, HiClr
    ShowCursor
  END IF

END SUB

'****************************************************************************
'Checks and converts a fields data and places it in the record buffer
'[Form$(0, 0)].  Numbers are converted to IEEE format for the buffer and also
'formatted for display and replaced in their element of the array.  Dates are
'converted to integers and check for validity.  Fields that contain sub-fields
'such as Phone Numbers and Zips Code are packed to remove delimiters before
'being placed in the buffer.
'
'Note: This routine is used internaly by [EditForm], but can be called by
'your program to check and format data before calling [PrintArray] to display.
'
'FldNo is the Field number to save.
'
'Form$() is the form data array.  Form$(FldNo, 0) must contain the data to be
'  checked, formatted and saved.
'
'Fld() is the field definition TYPE array.
'
'BadFld is a flag which will be returned with a -1 to indicate that the data
'  was invalid or out of range.
'****************************************************************************
'
SUB SaveField (FldNo, Form$(), Fld() AS FieldInfoG, BadFld) STATIC

  BadFld = 0                                  'No problems yet

  FType = Fld(FldNo).FType                    'Get the field's type
  IF FType = Relational THEN FType = Fld(FldNo).ScratchI

  SELECT CASE FType                           'Branch according to type
                                              'Numeric fields
    CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld, HScrollFld, VScrollFld

      IF Null(Form$(FldNo, 0)) THEN           'If the field is blank, set
        SELECT CASE FType                     '  value to flag it as blank
          CASE IntFld
            Temp$ = MKI$(BlankInt)
          CASE LongIntFld
            Temp$ = MKL$(BlankLng&)
          CASE SngFld
            Temp$ = MKS$(BlankSng!)
          CASE DblFld, MoneyFld
            Temp$ = MKD$(BlankDbl#)
          CASE HScrollFld, VScrollFld
            Temp$ = MKI$(BlankInt)
        END SELECT

      ELSE
                                              'Get the value of the string
        IF Fld(FldNo).FType = HScrollFld OR Fld(FldNo).FType = VScrollFld THEN
          Float# = Fld(FldNo).Value
        ELSE
          Float# = Value#(Form$(FldNo, 0), ErrCode)
        END IF
                                              'Check for out of range
        IF ErrCode OR Float# > Fld(FldNo).HiRange OR Float# < Fld(FldNo).LowRange THEN
          BadFld = -1
          EXIT SUB
        ELSE
                                              'Format number back into array
          Format Float#, Fld(FldNo), Form$(FldNo, 0)
                                               
          SELECT CASE FType                   'Set the buffer to IEEE format
            CASE IntFld, HScrollFld, VScrollFld
              Temp$ = MKI$(INT(Float#))
            CASE LongIntFld
              Temp$ = MKL$(CLNG(Float#))
            CASE SngFld
              Temp$ = MKS$(CSNG(Float#))
            CASE DblFld, MoneyFld
              Temp$ = MKD$(Float#)
          END SELECT

        END IF
      END IF

      MID$(Form$(0, 0), Fld(FldNo).Fields) = Temp$

    CASE DateFld, EuroDateFld

      FixDate Form$(FldNo, 0)                 'Make date $ the proper format
                                              '  NN-NN-NNNN
                                              'Convert to an integer and
                                              '  check validity
      IF Form$(FldNo, 0) = BlankDate$ THEN
        Days = BlankInt
      ELSE
        IF FType = DateFld THEN               'US dates
          Days = Date2Num(Form$(FldNo, 0))
          BadFld = Num2Date$(Days) <> Form$(FldNo, 0)
        ELSE                                  'European dates
          Temp$ = Form$(FldNo, 0)
          Temp$ = ESwap$(Temp$)
          Days = Date2Num(Temp$)
          BadFld = Num2Date$(Days) <> Temp$
        END IF
        BadFld = BadFld * 2
      END IF

      IF Days > BlankInt THEN                 'Check range
        BadFld = BadFld OR Days > Fld(FldNo).HiRange OR Days < Fld(FldNo).LowRange
      END IF

      IF NOT BadFld THEN                      'Save number to buffer as IEEE
        MID$(Form$(0, 0), Fld(FldNo).Fields) = MKI$(Days)
      END IF

    CASE PhoneFld                             'Pack Phone number $
      MID$(Form$(0, 0), Fld(FldNo).Fields) = MID$(Form$(FldNo, 0), 2, 3) + MID$(Form$(FldNo, 0), 7, 3) + MID$(Form$(FldNo, 0), 11, 4)

    CASE SoSecFld                             'Pack Social Security number $
      MID$(Form$(0, 0), Fld(FldNo).Fields) = MID$(Form$(FldNo, 0), 1, 3) + MID$(Form$(FldNo, 0), 5, 2) + MID$(Form$(FldNo, 0), 8, 4)

    CASE ZipFld                               'Pack Zip Code $
      MID$(Form$(0, 0), Fld(FldNo).Fields) = MID$(Form$(FldNo, 0), 1, 5) + MID$(Form$(FldNo, 0), 7, 4)

    CASE NotesFld                             'Do nothing for Notes field

    CASE PropStrFld                           'Capitalize first letters of
      ProperName Form$(FldNo, 0)              '  words for Proper Name $
      MID$(Form$(0, 0), Fld(FldNo).Fields) = Form$(FldNo, 0)
         
    CASE UCaseStrFld                          'Capitalize Upper Case fields
      LSET Form$(FldNo, 0) = UCASE$(Form$(FldNo, 0))
      MID$(Form$(0, 0), Fld(FldNo).Fields) = Form$(FldNo, 0)

    CASE MouseFld
      IF Fld(FldNo).LowRange THEN MID$(Form$(0, 0), Fld(FldNo).Fields) = Form$(FldNo, 0)

      'IF Fld(N).LowRange THEN MID$(Form$(0, 0), Fld(FldNo).Fields) = Form$(FldNo, 0)

    CASE PButton                              'Do not save

    CASE ELSE                                 'All others, just save text
      MID$(Form$(0, 0), Fld(FldNo).Fields) = Form$(FldNo, 0)

  END SELECT

END SUB

SUB ShadowBox (x1, y1, x2, y2, Outside)

  Clr1 = GPDat(96)
  Clr = GPDat(97)
  IF Outside THEN SWAP Clr, Clr1

  CALL LineVE(x1, y1, x2, y1, Clr)
  CALL LineVE(x1, y1, x1, y2, Clr)
  CALL LineVE(x2, y1, x2, y2, Clr1)
  CALL LineVE(x1, y2, x2, y2, Clr1)

END SUB

'****************************************************************************
'Copies and converts all data from the record buffer [Form$(0, 0)] and places
'it in the individual elements of the [Form$()] array.  This routine can be
'called from your program after reading a record from a random access data
'file (BASIC, Btrieve or ISAM) which uses Form$(0, 0) as the record buffer.
'Note:  If your programs do not use random access files, this routine can be
'deleted from this module.
'
'FirstFld is the field number to start unpacking.
'
'LastFld is the last field number to unpack.
'
'Form$() is the form data array to operate on.  Form$(0, 0) must contain all
'  data within the range for unpacking.  Numeric data must be in packed IEEE
'  form.  Dates must be IEEE integers.  Fields that contain sub-fields such
'  as Phone numbers and Zip Codes must not contain any delimiters.
'
'Fld() is the field definition TYPE array.
'****************************************************************************
'
SUB UnPackBuffer (FirstFld, LastFld, Form$(), Fld() AS FieldInfoG) STATIC

  FirstFld = MaxInt(1, FirstFld)              'Check first and last params.

  IF LastFld = 0 THEN LastFld = EndOfForms(Fld())

  FOR N = FirstFld TO LastFld                 'Do each field within range

    IF Fld(N).FType = 0 THEN
      N = N + 1
    END IF

    FType = Fld(N).FType                      'Get the fields type
    IF FType = Relational THEN FType = Fld(N).ScratchI


    SELECT CASE FType
      CASE HScrollFld, VScrollFld
        Length = 6
      CASE ScrollFld
        Length = Fld(N).StorLen
      CASE ELSE
        Length = Fld(N).RCol - Fld(N).LCol + 1
    END SELECT


    IF LEN(Form$(N, 0)) <> Length AND FType <> NotesFld AND FType <> PButton THEN
      Form$(N, 0) = SPACE$(Length)
    END IF

    SELECT CASE FType                         'Process acording to type
                                              'Number fields
      CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld, HScrollFld, VScrollFld
        SELECT CASE FType
          CASE IntFld, HScrollFld, VScrollFld 'Make a number
            Float# = CVI(MID$(Form$(0, 0), Fld(N).Fields, 2))
            Blank = Float# = BlankInt         'Check for a blank
          CASE LongIntFld
            Float# = CVL(MID$(Form$(0, 0), Fld(N).Fields, 4))
            Blank = Float# = BlankLng&
          CASE SngFld
            Float# = CVS(MID$(Form$(0, 0), Fld(N).Fields, 4))
            Blank = Float# = BlankSng!
          CASE DblFld, MoneyFld
            Float# = CVD(MID$(Form$(0, 0), Fld(N).Fields, 8))
            Blank = Float# <= BlankDbl#
        END SELECT

        IF Blank THEN                         'If it isn't blank, format it
          LSET Form$(N, 0) = ""
        ELSE
          Format Float#, Fld(N), Form$(N, 0)
        END IF

      CASE DateFld                            'Get date number
        X = CVI(MID$(Form$(0, 0), Fld(N).Fields, 2))
        IF X > BlankInt THEN                  'If it isn't blank, convert
          LSET Form$(N, 0) = Num2Date$(X)     '  number into date $
        ELSE                                  'Otherwise,
          LSET Form$(N, 0) = BlankDate$       '  make a blank date $
        END IF

      CASE EuroDateFld                        'Get date number
        X = CVI(MID$(Form$(0, 0), Fld(N).Fields, 2))
        IF X > BlankInt THEN                  'If it isn't blank, convert
          LSET Form$(N, 0) = ESwap$(Num2Date$(X)) '  number into date $
        ELSE                                  'Otherwise,
          LSET Form$(N, 0) = BlankDate$       '  make a blank date $
        END IF

      CASE PhoneFld
        LSET Form$(N, 0) = "(   )    -    "   'Make a phone # template then
                                              '  fill in the blanks
        MID$(Form$(N, 0), 2) = MID$(Form$(0, 0), Fld(N).Fields, 3)
        MID$(Form$(N, 0), 7) = MID$(Form$(0, 0), Fld(N).Fields + 3, 3)
        MID$(Form$(N, 0), 11) = MID$(Form$(0, 0), Fld(N).Fields + 6, 4)

      CASE SoSecFld
        LSET Form$(N, 0) = "   -  -    "      'Make a template then fill in
                                              '  the blanks
        MID$(Form$(N, 0), 1) = MID$(Form$(0, 0), Fld(N).Fields, 3)
        MID$(Form$(N, 0), 5) = MID$(Form$(0, 0), Fld(N).Fields + 3, 2)
        MID$(Form$(N, 0), 8) = MID$(Form$(0, 0), Fld(N).Fields + 5, 4)

      CASE ZipFld
        LSET Form$(N, 0) = "     -    "       'Make a template then fill in
                                              '  the blanks
        MID$(Form$(N, 0), 1) = MID$(Form$(0, 0), Fld(N).Fields, 5)
        MID$(Form$(N, 0), 7) = MID$(Form$(0, 0), Fld(N).Fields + 5, 4)

      CASE NotesFld, PButton

      CASE ELSE                               'Just copy other field types
        LSET Form$(N, 0) = MID$(Form$(0, 0), Fld(N).Fields, Fld(N).StorLen)

    END SELECT

  NEXT

END SUB

'---- Returns the value of a numeric string which may contain non numeric
'     characters such as "$," also returns an error if value would cause
'     an overflow
'
FUNCTION Value# (E$, ErrCode) STATIC

  ErrCode = -1                                'Guilty until proven otherwise
  Value# = 0                                  'No value yet
   
  Temp$ = RTRIM$(E$)                          'Make a copy of string
  Ln = LEN(Temp$)                             'Save the length of string
  Sign = 0                                    'number of signs
  S1 = 0                                      'Mantissa's sign position
  Decm = 0                                    'Decimal position
  Xpon = 0                                    'Exponent position
  N = 0                                       'Number of digits
  X = 1                                       'Start at the first character

  DO UNTIL X > Ln                             'Check each character
                                              'See if character is in table
    Ok = INSTR(FloatChars$, MID$(Temp$, X, 1))

    IF N = 0 THEN
      IF Ok >= 15 AND Ok <= 18 THEN Ok = 0
    END IF

    SELECT CASE Ok                            'Branch according to position

      CASE 0, 1                               'Illegal character
        MID$(Temp$, X) = MID$(Temp$, X + 1)'Erase it
        MID$(Temp$, Ln) = " "
        X = X - 1                             'Back up one position
        Ln = Ln - 1                           'Decrement length

      CASE 2, 3                               'Sign character
        Sign = Sign + 1                       'Bump number of signs
        IF Sign > 2 THEN EXIT DO              'If too many signs, bail out
        IF Xpon = 0 THEN                      'Is this the mantissa's sign?
          S1 = X                              'Save position
        END IF

      CASE 4                                  'Decimal place
        IF Decm = 0 THEN                      'If its the first decimal,
          Decm = X                            '  save it's position
        ELSE                                  'Otherwise bail out
          EXIT DO
        END IF

      CASE 15 TO 18                           'Exponent character
        IF Xpon = 0 THEN                      'If its the first one,
          Xpon = X                            '  save it's position
        ELSE                                  'Otherwise bail out
          EXIT DO
        END IF

      CASE ELSE
        N = N + 1

    END SELECT

    X = X + 1                                 'Bump character pointer
  LOOP


  Whole = Decm - S1 - 1                       'Find number of digits before
                                              '  decimal place
  IF Whole > 308 THEN EXIT FUNCTION           'Bail out if too many

  IF Xpon THEN                                'If there is an exponent,
    Num# = VAL(LEFT$(Temp$, Xpon))            'Get mantissa's value
    Xpon = VAL(MID$(Temp$, Xpon + 1))         'Get exponents value

    Num# = Num# / (10 ^ (Whole - 1))          'Shift decimal to right of
                                              '  first digit
    Xpon = ABS(Xpon + Whole - 1)              'Adjust exponent

    IF Xpon > 308 THEN                        'See if number will overflow
      EXIT FUNCTION
    ELSEIF Xpon = 308 AND ABS(Num#) > 1.79769313486232# THEN
      EXIT FUNCTION
    END IF

  END IF

  Value# = VAL(Temp$)                         'Assign the function
  ErrCode = 0                                 'Clear error flag

END FUNCTION

