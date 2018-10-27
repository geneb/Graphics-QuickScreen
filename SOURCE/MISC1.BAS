DEFINT A-Z

'---- BASIC subs
DECLARE SUB DOSMsg ()
DECLARE SUB FileThere (FileName$, Button)
DECLARE SUB GDialog (Choice%, DLog() AS ANY, Response$(), Menu$(), Style%, Action%)
DECLARE SUB LoadFiles (Spec$, FileMenu$(), Found)
DECLARE SUB PCBox (X1%, Y1%, X2%, Y2%, Outside%)
DECLARE SUB StatBox (Show%)
DECLARE SUB Tokenize (Calc$, Fld() AS ANY)

'---- BASIC functions
DECLARE FUNCTION BaseName$ (FileName$)
DECLARE FUNCTION GArraySize& (X1, Y1, X2, Y2)
DECLARE FUNCTION MsgBox (Msg$(), Opt)
DECLARE FUNCTION NoExtension$ (T$)

'---- ASM subs
DECLARE SUB BCopyT (SEG FroEl, SEG ToEl, Elsize, NumEls)
DECLARE SUB BLPrint (LPTNo, Work$, ErrCount)
DECLARE SUB CDir (Path$)
DECLARE SUB Chime (a)
DECLARE SUB FClose (Handle)
DECLARE SUB FCreate (FileName$)
DECLARE SUB FGet (Handle, Text$)
DECLARE SUB FGetT (Handle, Dest AS ANY, NumBytes)
DECLARE SUB FGetA (Handle, SEG Element AS ANY, NumBytes&)
DECLARE SUB FOpen (FileName$, Handle)
DECLARE SUB FPut (Handle, Text$)
DECLARE SUB FPutA (Handle, SEG Element AS ANY, NumBytes&)
DECLARE SUB FPutT (Handle, Source, NumBytes)
DECLARE SUB FreeMouse ()
DECLARE SUB FSeek (Handle, a&)
DECLARE SUB GPrint2VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB HideCursor ()
DECLARE SUB InsertT (SEG Element, Elsize, NumEls)
DECLARE SUB KillFile (FileName$)
DECLARE SUB LineBVE (BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, BYVAL LineColor%)
DECLARE SUB Motion (MValue%)
DECLARE SUB Mouse (a, b, c, d)
DECLARE SUB ScrnDump2 (DPI$, LptNumber%, Translate%, XMultiplier%, YMultiplier%, LandOrPort%)
DECLARE SUB Sequence (a$)
DECLARE SUB SetDrive (Drive$)
DECLARE SUB SetError (ErrCode)
DECLARE SUB ShowCursor ()
DECLARE SUB WaitUp ()

'---- ASM Functions
DECLARE FUNCTION Bin2Num% (Bin$)
DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION ErrorMsg$ (Er)
DECLARE FUNCTION Exist% (FileSpec$)
DECLARE FUNCTION FCount (FileSpec$)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION FLof& (Handle)
DECLARE FUNCTION FLInput$ (Handle, Buffer$)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION FindLast% (BYVAL Address, Size)
DECLARE FUNCTION GetDir$ (Drive$)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION InstrTblB% (Start, T$, Tbl$)
DECLARE FUNCTION MinInt (a, b)
DECLARE FUNCTION MaxInt (a, b)
DECLARE FUNCTION ParsPath$ (FileName$)
DECLARE FUNCTION QInstrB (Start, a$, b$)
DECLARE FUNCTION QPTrim$ (a$)
DECLARE FUNCTION WhichError ()

'$INCLUDE: 'DlogType.Bi'
'$INCLUDE: 'FldInfo.bi'
'$INCLUDE: 'Common.GQS'

SUB ArrangeFlds (Fld() AS FieldInfoG, Calc$(), Help$())

  
  DIM Temp AS FieldInfoG
  REDIM Menu$(1 TO Fld(0).Fields)
  FOR i = 1 TO Fld(0).Fields
    Menu$(i) = Fld(i).FldName
  NEXT
  Menu$(1) = Menu$(1) + " "
  REDIM DLog(4) AS Dialog
  REDIM Resp$(4)
  
  DLog(0).Heading = "Rearrange Fields"
  IF GPDat(71) = 8 THEN MdOS = 4
  DLog(1).DType = 6
  DLog(1).Heading = "OK"
  DLog(1).Row = 3 * GPDat(71) + MdOS + GPDat(71) \ 2
  DLog(1).Col = 18 * 8 - 2
  DLog(1).NumRows = GPDat(80)'2 * GPDat(71) - GPDat(71) \ 2
  DLog(1).TextLen = 66

  DLog(2).DType = 6
  DLog(2).Heading = "Cancel"
  DLog(2).Row = 5 * GPDat(71) + (3 * MdOS) + GPDat(71) \ 2
  DLog(2).Col = 18 * 8 - 2
  DLog(2).NumRows = GPDat(80)'2 * GPDat(71) - GPDat(71) \ 2
  DLog(2).TextLen = 66'76
  DLog(2).Group = 1000

  DLog(3).DType = 5
  DLog(3).Row = 1
  DLog(3).Col = 2
  DLog(3).Heading = "Select the field to move:"

  DLog(4).DType = 4
  DLog(4).Row = 4
  DLog(4).Col = 3
  DLog(4).Heading = "Fields: <"
  DLog(4).NumRows = 8
  DLog(4).MenuChoice = 1

  Move = -1

  Choice = 1
  Action = 1

  DO
    CALL GDialog(Choice, DLog(), Resp$(), Menu$(), Style, Action)


    IF Action = 4 THEN
      IF Choice = 2 OR Choice = 27 THEN
        Action = 5
      ELSE
        IF Move THEN
          Field1 = VAL(Resp$(4))
          IF Field1 = 0 THEN Field1 = 1
          Cho$ = QPTrim$(Menu$(Field1))
          FOR N = Field1 TO Fld(0).Fields - 1
            Menu$(N) = Menu$(N + 1)
          NEXT

          LSET Menu$(N) = ""
          DLog(3).Heading = "Insert " + Cho$ + " before:     "
          Action = 2
        ELSE
          Temp = Fld(Field1)
          Calc$ = Calc$(Field1)
          Help$ = Help$(Field1)

          FOR N = Field1 TO Fld(0).Fields - 1
            Fld(N) = Fld(N + 1)
            Calc$(N) = Calc$(N + 1)
            Help$(N) = Help$(N + 1)
          NEXT

          Field1 = VAL(Resp$(4))
          IF Field1 = 0 THEN Field1 = 1

          FOR N = Fld(0).Fields TO Field1 + 1 STEP -1
            Menu$(N) = Menu$(N - 1)
            Fld(N) = Fld(N - 1)
            Calc$(N) = Calc$(N - 1)
            Help$(N) = Help$(N - 1)
          NEXT

          Menu$(N) = Cho$
          Fld(N) = Temp
          Calc$(N) = Calc$
          Help$(N) = Help$
          DLog(3).Heading = "Select the field to move:"
          Action = 2
          Menu$(1) = Menu$(1) + SPACE$(9 - LEN(Menu$(1)))
          GPDat(84) = 1      'Set field Defs changed flag
        END IF
        Move = NOT Move
      END IF
    END IF

  LOOP UNTIL Action = 5

  CALL GDialog(Choice, DLog(), Resp$(), Menu$(), Style, Action)
  ERASE DLog, Resp$

END SUB

FUNCTION BaseName$ (FileName$)

  L = LEN(FileName$)
  IF L THEN

    Temp$ = NoExtension$(FileName$)        'Strip the extension
    Start = InstrTblB(-1, Temp$, ":\")

    IF Start THEN
      Temp$ = RIGHT$(Temp$, LEN(Temp$) - Start)
    END IF

    '---- One last test in case a period exists in the file name
    BaseName$ = NoExtension$(Temp$)

  ELSE
    BaseName$ = ""
  END IF

END FUNCTION

SUB DemoType (FileName$, Frm, SvLd, Fs) STATIC

  REDIM Menu$(0)
  REDIM Resp$(6)
  REDIM DLog(6)  AS Dialog
  DLog(0).Heading = "Make Demo"
 
  IF GPDat(71) = 8 THEN MdOS = 8
  DLog(1).DType = 6                            'Push Button
  DLog(1).Row = 1 * GPDat(71) - MdOS + GPDat(71) \ 2 + 3
  DLog(1).Col = 27 * 8 - 2
  DLog(1).Heading = "OK"
  DLog(1).NumRows = GPDat(80)
  DLog(1).TextLen = 66

  IF GPDat(71) = 8 THEN MdOS = 3
  DLog(2).DType = 6                            'Push Button
  DLog(2).Row = 3 * GPDat(71) - MdOS + GPDat(71) \ 2 + 3
  DLog(2).Col = 27 * 8 - 2
  DLog(2).Heading = "Cancel"
  DLog(2).NumRows = GPDat(80)
  DLog(2).TextLen = 66
  DLog(2).Group = 100

  IF Frm THEN
    Resp$(3) = "O"
  ELSE
    Resp$(4) = "O"
  END IF

  IF SvLd THEN Resp$(6) = "X"
  IF Fs THEN Resp$(5) = "X"

  DLog(3).DType = 2                'Option Button
  DLog(3).Row = 2
  DLog(3).Col = 4
  DLog(3).Heading = "Use .FRM file"
  DLog(3).Group = 1
  DLog(3).HotKey = 5

  DLog(4).DType = 2                'Option Button
  DLog(4).Row = 3
  DLog(4).Col = 4
  DLog(4).Heading = "Use BASIC Module"
  DLog(4).Group = 1
  DLog(4).HotKey = 4

  OS = 0
  IF GPDat(71) = 8 THEN OS = 1
  Resp$(5) = FileName$
  DLog(5).DType = 3
  DLog(5).Row = 6 + OS
  DLog(5).Col = 2
  DLog(5).Heading = "Demonstration File Name:<10"
  DLog(5).TextLen = 8
  
  DLog(6).DType = 7                            '
  DLog(6).Heading = "Field definitions"
  DLog(6).Row = 1
  DLog(6).Col = 1
  DLog(6).NumRows = 2
  DLog(6).TextLen = 23

  DChoice = 1

  CALL GDialog(DChoice, DLog(), Resp$(), Menu$(), 0, 0)

  IF DChoice <> 2 AND DChoice <> 27 THEN
    Frm = Resp$(3) = "O"
    FileName$ = UCASE$(Resp$(5))
  ELSE
    Frm = 2
  END IF

  ERASE DLog, Resp$

END SUB

SUB FileThere (FileName$, Button) STATIC

    Button = 0
    IF Exist(FileName$) THEN
      REDIM Msg$(2)
      Msg$(0) = "Warning"
      Msg$(1) = "The file " + FileName$ + " already exists."
      Msg$(2) = "Overwrite?"
      Button = MsgBox(Msg$(), 112)
      ERASE Msg$
    END IF

END SUB

SUB FreeMouse
  Mouse 7, 0, 0, GPDat(43) - 1   'Width
  Mouse 8, 0, 0, GPDat(44) - 1   'Height
END SUB

SUB GetFldDef (FrmName$, StartEl, Fld() AS FieldInfoG, Form$()) STATIC

  Temp$ = NoExtension$(FrmName$) + ".FRM"

  FOpen Temp$, Handle
  Temp$ = ""
  IF DOSError THEN
    EXIT SUB
  END IF
  FGetT Handle, NumFlds, 2
  NumBytes& = (NumFlds + 1) * FrmWdth
  FSeek Handle, 0&
  FGetA Handle, Fld(StartEl), NumBytes&

  Byte$ = " "
  FGet Handle, Byte$

  Buffer$ = SPACE$(400)    '400 bytes

  DO UNTIL DOSError
    IF Byte$ = "H" THEN
      FOR N = StartEl + 1 TO StartEl + NumFlds
        Form$(N, 1) = FLInput$(Handle, Buffer$)
        IF DOSError THEN
          EXIT FOR
        END IF
      NEXT
    ELSE
      FOR N = StartEl + 1 TO StartEl + NumFlds
        Form$(N, 2) = FLInput$(Handle, Buffer$)
        IF DOSError THEN
          EXIT FOR
        END IF
        'IF LEN(Form$(N, 2)) THEN Tokenize Form$(N, 2), Fld()
      NEXT
    END IF

    FGet Handle, Byte$
  LOOP
  Buffer$ = ""
  FClose Handle

  '------ Calculate pointers into the whole record buffer
  IF StartEl = 0 THEN
    Fld(StartEl + 1).Fields = 1
  ELSE
    Fld(StartEl + 1).Fields = Fld(StartEl - 1).Fields + Fld(StartEl - 1).StorLen
  END IF

  FOR N = StartEl + 2 TO StartEl + Fld(StartEl).Fields
    Fld(N).Fields = Fld(N - 1).Fields + Fld(N - 1).StorLen
  NEXT

  Fld(0).StorLen = Fld(N - 1).Fields + Fld(N - 1).StorLen - 1

  SetError 0

END SUB

SUB HelpMsg

  REDIM DLog(22) AS Dialog
  REDIM Resp$(22)
  REDIM Menu$(0)

  DLog(0).Heading = "Key Assignments"

  DLog(1).DType = 6                                 'Push Button
  DLog(1).Row = 13 * GPDat(71) - GPDat(71) \ 2 + 4'- 3 * Space8
  DLog(1).Col = 58 * 8 - 2
  DLog(1).Heading = "OK"
  DLog(1).NumRows = GPDat(80)
  DLog(1).TextLen = 66
  
  Row = 0
  FOR i = 2 TO 12
    DLog(i).DType = 5
    DLog(i).Row = Row
    DLog(i).Col = 3
    Row = Row + GPDat(71)
  NEXT

  Row = 0
  FOR i = 13 TO 22
  DLog(i).DType = 5
    DLog(i).Row = Row
    DLog(i).Col = 35
    Row = Row + GPDat(71)
  NEXT

  DLog(2).Heading = "Function Keys"
  DLog(2).DType = 7
  DLog(2).Row = 1
  DLog(2).TextLen = 30
  DLog(2).NumRows = 10
  DLog(2).Col = 1

  DLog(3).Heading = "F1  - This Help screen"
  DLog(4).Heading = "F2  - Tile Palette"
  DLog(5).Heading = "F3  - Palette Editor"
  DLog(6).Heading = "F4  - Select Cursor Type"
  DLog(7).Heading = "F5  - Select Line Type"
  DLog(8).Heading = "F6  - System Settings"
  DLog(9).Heading = "F7  - Draw Text menu"
  DLog(10).Heading = "F8  - Turn Status Box On/Off"
  DLog(11).Heading = "F9  - Memory status"
  DLog(12).Heading = "F10 - Restore the Screen"

  DLog(13).Heading = "Special Keys"
  DLog(13).DType = 7
  DLog(13).Row = 1
  DLog(13).TextLen = 34
  DLog(13).NumRows = 6
  DLog(13).Col = 33

  DLog(14).Heading = "A  - Toggle Arc Rotation"
  DLog(15).Heading = "F  - Toggle Text snap on/off"
  DLog(16).Heading = "R  - Show Relative Coordinates"
  DLog(17).Heading = "S  - Toggle Snap On/Off"
  DLog(18).Heading = "T  - Toggle x/y-Row/Col Display"
  DLog(19).Heading = "|  - Toggle Dotted Line On/Off"

  DLog(20).Heading = "Color Keys"
  DLog(20).Row = 9
  DLog(20).DType = 7
  DLog(20).TextLen = 34
  DLog(20).NumRows = 2
  DLog(20).Col = 33

  DLog(21).Row = DLog(19).Row + 3 * GPDat(71)
  DLog(21).Heading = "0-9       - Selects color 0-9"
 
  DLog(22).Row = DLog(21).Row + GPDat(71)
  DLog(22).Heading = "Shift+0-5 - Selects color 10-15"

  Choice = 1
  CALL GDialog(Choice, DLog(), Resp$(), Menu$(), Style, 0)
  ERASE DLog, Resp$

END SUB

SUB LineType STATIC


  REDIM Menu$(0)
  REDIM Resp$(12)
  REDIM DLog(12)  AS Dialog
  DLog(0).Heading = "Select Line Type"

  Choice = LastChoice
  IF LastChoice = 0 OR GPDat(68) = -1 THEN Choice = 3
  Resp$(Choice) = "O"
  IF QPTrim$(Mask$) = "" THEN Mask$ = "1111111111111111"
  Resp$(12) = Mask$

  DLog(1).DType = 6                            'Push Button
  DLog(1).Row = 1 * GPDat(71) + GPDat(71) \ 2 + 3
  DLog(1).Col = 46 * 8 - 2
  DLog(1).Heading = "OK"
  DLog(1).NumRows = GPDat(80)
  DLog(1).TextLen = 66

  IF GPDat(71) = 8 THEN MdOS = 6
  DLog(2).DType = 6                            'Push Button
  DLog(2).Row = 3 * GPDat(71) + GPDat(71) \ 2 + 3 + MdOS
  DLog(2).Col = 46 * 8 - 2
  DLog(2).Heading = "Cancel"
  DLog(2).NumRows = GPDat(80)
  DLog(2).TextLen = 66
  DLog(2).Group = 100


  DLog(3).DType = 2                'Option Button
  DLog(3).Row = 2
  DLog(3).Col = 4
  DLog(3).Heading = "A ÿ            ÿ"
  DLog(3).Group = 1

  DLog(4).DType = 2                'Option Button
  DLog(4).Row = 3
  DLog(4).Col = 4
  DLog(4).Heading = "B ÿ            ÿ"
  DLog(4).Group = 1

  DLog(5).DType = 2                'Option Button
  DLog(5).Row = 4
  DLog(5).Col = 4
  DLog(5).Heading = "C ÿ            ÿ"
  DLog(5).Group = 1

  DLog(6).DType = 2                'Option Button
  DLog(6).Row = 5
  DLog(6).Col = 4
  DLog(6).Heading = "D ÿ            ÿ"
  DLog(6).Group = 1

  DLog(7).DType = 2                'Option Button
  DLog(7).Row = 2
  DLog(7).Col = 25
  DLog(7).Heading = "E ÿ            ÿ"
  DLog(7).Group = 1

  DLog(8).DType = 2                'Option Button
  DLog(8).Row = 3
  DLog(8).Col = 25
  DLog(8).Heading = "F ÿ            ÿ"
  DLog(8).Group = 1

  DLog(9).DType = 2                'Option Button
  DLog(9).Row = 4
  DLog(9).Col = 25
  DLog(9).Heading = "G ÿ            ÿ"
  DLog(9).Group = 1

  DLog(10).DType = 2                'Option Button
  DLog(10).Row = 5
  DLog(10).Col = 25
  DLog(10).Heading = "Custom Mask"
  DLog(10).Group = 1
  DLog(10).HotKey = 7

  DLog(11).DType = 7                            '
  DLog(11).Row = 1
  DLog(11).Col = 2
  DLog(11).NumRows = 4
  DLog(11).TextLen = 42

  DLog(12).DType = 3                'Text
  DLog(12).Row = 7
  DLog(12).Col = 10
  DLog(12).Heading = "Custom Line Mask:<11"
  DLog(12).TextLen = 16
  DLog(12).HotKey = 12

  DChoice = 1
  Style = 4

  CALL GDialog(DChoice, DLog(), Resp$(), Menu$(), Style, 0)
              
  IF DChoice <> 2 AND DChoice <> 27 THEN
    FOR i = 3 TO 10
      IF Resp$(i) = "O" THEN EXIT FOR
    NEXT
    Choice = i
    LastChoice = Choice

    SELECT CASE Choice
      CASE 3
        GPDat(68) = -1
      CASE 4
        GPDat(68) = -30584
      CASE 5
        GPDat(68) = -3856
      CASE 6
        GPDat(68) = -897
      CASE 7
        GPDat(68) = -28680
      CASE 8
        GPDat(68) = -27652
      CASE 9
        GPDat(68) = -772
      CASE 10
        GPDat(68) = Bin2Num%(Resp$(12))
    END SELECT
  END IF
  Mask$ = Resp$(12)

  ERASE DLog, Resp$

END SUB

SUB LoadFile (Drive$, Path$, BName$, Ext$, Opts, Choice)

  IF Path$ <> "\" THEN BSlash$ = "\"
  Spec$ = Drive$ + ":" + Path$ + BSlash$ + "*" + Ext$
  REDIM FileMenu$(0)
  LoadFiles Spec$, FileMenu$(), Found
  REDIM DLog(8) AS Dialog
  REDIM Resp$(8)

  DLog(0).Heading = "Open File"
  IF GPDat(71) = 8 THEN MdOS = 4
  DLog(1).DType = 6
  DLog(1).Heading = "OK"
  DLog(1).Row = 12 * GPDat(71) - MdOS
  DLog(1).Col = 22 * 8 - 2
  DLog(1).NumRows = GPDat(80)'2 * GPDat(71) - GPDat(71) \ 2
  DLog(1).TextLen = 66

  DLog(2).DType = 6
  DLog(2).Heading = "Cancel"
  DLog(2).Row = 12 * GPDat(71) - MdOS
  DLog(2).Col = 32 * 8 - 2
  DLog(2).NumRows = GPDat(80)'2 * GPDat(71) - GPDat(71) \ 2
  DLog(2).TextLen = 66
  DLog(2).Group = 1000

  DLog(3).DType = 3                            'Text Entry Field
  DLog(3).Row = 1
  DLog(3).Col = 2
  DLog(3).Heading = "File Name<10"        '      "<" indicates end of Heading$
  DLog(3).NumRows = 0                          ' number indicates num only etc.
  DLog(3).TextLen = 29
  DLog(3).HotKey = 5
  Resp$(3) = "*" + Ext$

  DLog(4).DType = 4                'Menu
  DLog(4).Row = 5
  DLog(4).Col = 3
  DLog(4).Heading = "Files:           <"
  DLog(4).NumRows = 9
  DLog(4).Group = 3
  DLog(4).MenuChoice = 0

  DLog(5).DType = 5
  DLog(5).Row = GPDat(71) * 2 - GPDat(71) \ 2
  DLog(5).Col = 2
  DLog(5).Heading = Drive$ + ":" + Path$

  IF Opts THEN
    Opts = 0
    DLog(6).DType = 1                'Check button
    DLog(6).Row = 6
    DLog(6).Col = 22
    DLog(6).Heading = "PCX Image"
    Resp$(6) = "X"

    DLog(7).DType = 1                'Check button
    DLog(7).Row = 7
    DLog(7).Col = 22
    DLog(7).Heading = "Form Definitions"
    DLog(7).HotKey = 5
    Resp$(7) = "X"

    DLog(8).DType = 7                'Frame
    DLog(8).Row = 5
    DLog(8).Col = 19
    DLog(8).Heading = "Load Options"
    DLog(8).NumRows = 2
    DLog(8).TextLen = 22
  END IF

Choice = 1
  Action = 1
  DO
    CALL GDialog(Choice, DLog(), Resp$(), FileMenu$(), Style, Action)

    IF Action = 4 THEN
      IF Choice = 2 OR Choice = 27 THEN
        Action = 5
      ELSE
        TSpec$ = LTRIM$(RTRIM$(Resp$(3)))
        IF TSpec$ <> Spec$ THEN                  'They typed something

          '---- No extension, add the default
          IF NoExtension$(TSpec$) = TSpec$ THEN TSpec$ = TSpec$ + Ext$

          'IF INSTR(TSpec$, ".") = 0 THEN TSpec$ = TSpec$ + Ext$


          Drv = INSTR(TSpec$, ":")               'Is there a drive?
          IF Drv = 2 THEN Drv = -1
          Pth = INSTR(TSpec$, "\")               'Is there a path?
          IF NOT Drv THEN SetDrive (Drive$)
          IF NOT Pth THEN CDir (Path$)

          IF TSpec$ = ".." THEN                  'Go back one directory?
            TSpec$ = "..\*" + Ext$
            X = -1
          ELSEIF Drv AND LEN(TSpec$) = 2 THEN    'New Drive only?
            TSpec$ = TSpec$ + "\*.*"
            X = -1
          END IF

          IF Exist(TSpec$) THEN  'Could be file name in default dir or new path
            Per = QInstrB(-1, TSpec$, ".")       'Find the .Ext
            Ext$ = RIGHT$(TSpec$, LEN(TSpec$) - Per + 1)
            IF Drv THEN Drive$ = UCASE$(LEFT$(TSpec$, 1))'Find drive if spec'd
            Path$ = ParsPath$(TSpec$)            'Parse out the path
            SetDrive (Drive$)                    'Set the drive
            CDir (Path$)                         'Set the path
            Path$ = GetDir(Drive$)               'Get the new path

            IF INSTR(TSpec$, "?") OR INSTR(TSpec$, "*") THEN  'New Display!
              IF X THEN TSpec$ = "*" + Ext$
              LoadFiles TSpec$, FileMenu$(), Found     'Load the new file menu
              Action = 2
              DLog(4).MenuChoice = 0
              DLog(4).NumRows = 9
              Resp$(3) = "*" + Ext$              'Reset spec
              IF Path$ = "" THEN Path$ = "\"
              LSET DLog(5).Heading = UCASE$(Drive$ + ":" + Path$)
            ELSE         'No wild cards use it
              BName$ = BaseName$(TSpec$)
              Action = 5
            END IF
          ELSE
            BEEP
            IF DOSError% THEN
              CALL GDialog(Choice, DLog(), Resp$(), FileMenu$(), Style, 5)
              DOSMsg
              Action = 1
            ELSE
              Action = 2
            END IF
          END IF
        END IF
      END IF
    END IF
  LOOP UNTIL Action = 5

  IF Resp$(6) = "X" THEN Opts = 2
  IF Resp$(7) = "X" THEN Opts = Opts + 4

  CALL GDialog(Choice, DLog(), Resp$(), FileMenu$(), Style, Action)

END SUB

SUB LoadFiles (Spec$, FileMenu$(), Found)

  Found = -1
  NumFiles = FCount%(Spec$)
  IF NumFiles = 0 THEN
    NumFiles = 1
    Found = 0
  END IF

  REDIM FileMenu$(NumFiles)
  FOR i = 1 TO NumFiles
    FileMenu$(i) = SPACE$(12)
  NEXT
  FileMenu$(0) = Spec$
  CALL ReadFile(BYVAL VARPTR(FileMenu$(0)))
  CALL SortStr(BYVAL VARPTR(FileMenu$(1)), NumFiles, 0)

  FOR i = 1 TO NumFiles
    FileMenu$(i) = FileMenu$(i) + " "
  NEXT

END SUB

FUNCTION ParsPath$ (FileName$) STATIC

  IF LEN(FileName$) THEN
    IF INSTR(FileName$, ":") = 2 THEN FileName$ = RIGHT$(FileName$, LEN(FileName$) - 2)
    BSlash = QInstrB(-1, FileName$, "\")
    IF BSlash THEN
      ParsPath$ = LEFT$(FileName$, BSlash - 1)
    ELSE
      ParsPath$ = ""
    END IF
  END IF

END FUNCTION

SUB PrintScrn STATIC

  REDIM DPI$(6 TO 10)
  DPI$(6) = ""
  DPI$(7) = "075"
  DPI$(8) = "100"
  DPI$(9) = "150"
  DPI$(10) = "300"

  REDIM DLog(14) AS Dialog
  REDIM Resp$(14)
  REDIM Menu$(0)

  DLog(0).Heading = "Print Screen"

  FOR i = 1 TO 2
    DLog(i).DType = 6                            'Push Button
    DLog(i).Row = 6 * GPDat(71) + GPDat(71) \ 2 + 3
    DLog(i).NumRows = GPDat(80)
    DLog(i).TextLen = 66
  NEXT

  DLog(1).Col = 23 * 8 - 2
  DLog(1).Heading = "OK"
  DLog(2).Col = 33 * 8 - 2
  DLog(2).Heading = "Cancel"
  DLog(2).Group = 100

  IF NOT DotMatrix THEN
    Resp$(4) = "O"
  ELSE
    Resp$(3) = "O"
  END IF

  FOR i = 3 TO 4
    DLog(i).DType = 2                'Option Button
    DLog(i).Row = i - 1
    DLog(i).Col = 3
    DLog(i).Group = 1
  NEXT
  DLog(3).Heading = "Dot Matrix"
  DLog(4).Heading = "Laser Jet"

  FOR i = 5 TO 7
    DLog(i).DType = 1
    DLog(i).Row = i
    DLog(i).Col = 3
  NEXT
  DLog(5).Heading = "Portrait mode"
  DLog(6).Heading = "Tiled colors"
  DLog(7).Heading = "Swap colors"

  IF Portrait THEN Resp$(5) = "X"
  IF Translate THEN Resp$(6) = "X"
  IF Inverse THEN Resp$(7) = "X"

  IF DP = 0 OR DP = 6 THEN
    Resp$(8) = "O"
  ELSE
    Resp$(DP + 1) = "O"
  END IF

  FOR i = 8 TO 11
    DLog(i).DType = 2                'Option Button
    DLog(i).Row = i - 6
    DLog(i).Col = 22
    DLog(i).Group = 2
  NEXT

  DLog(8).Heading = "75  Dots per Inch"
  DLog(9).Heading = "100 Dots per Inch"
  DLog(10).Heading = "150 Dots per Inch"
  DLog(11).Heading = "300 Dots per Inch"

  DLog(12).DType = 7
  DLog(12).Heading = "Select Printer"
  DLog(12).Row = 1
  DLog(12).Col = 1
  DLog(12).NumRows = 2
  DLog(12).TextLen = 17

  DLog(13).Heading = "Laser Scaling"
  DLog(13).TextLen = 22
  DLog(13).DType = 7
  DLog(13).Row = 1
  DLog(13).Col = 20
  DLog(13).NumRows = 4

  DChoice = 1
  CALL GDialog(DChoice, DLog(), Resp$(), Menu$(), Style, 0)

  IF DChoice <> 2 AND DChoice <> 27 THEN

    DotMatrix = Resp$(3) = "O"
    Portrait = Resp$(5) = "X"
    Translate = Resp$(6) = "X"
    Inverse = Resp$(7) = "X"

    FOR i = 8 TO 11
      IF Resp$(i) = "O" THEN EXIT FOR
    NEXT
    DP = i - 1

    IF DotMatrix THEN
      DP = 6
    END IF

    LPTNum = GPDat(9) + 256

    IF Inverse THEN LPTNum = LPTNum - 256
 
    HideCursor
    ScrnDump2 DPI$(DP), LPTNum, Translate, 1, 1, NOT Portrait
    ShowCursor

    IF LPTNum = -1 THEN
      REDIM Msg$(1)
      Msg$(0) = "Printer Error"
      Msg$(1) = "Your printer is not ready."
      Butn = MsgBox(Msg$(), 121)
      ERASE Msg$, DLog, Resp$, DPI$
      EXIT SUB
    END IF
    IF NOT DotMatrix THEN
      BLPrint GPDat(9), CHR$(12), ErrCount              'Formfeed
    ELSE
      BLPrint GPDat(9), CHR$(27) + "@", ErrCount        'Reset
    END IF

  END IF

  ERASE DLog, Resp$, DPI$

END SUB

SUB SaveFile (Drive$, Path$, BName$, Ext$, SChoice, Heading$, ErCode, Button)

  'Save Choice on entry determines whether or not we have additional controls
  ' On exit, it returns -1 if partial pcx was checked
  'ErCode returns error code

  IF Path$ = "\" THEN
    BSlash$ = ""
  ELSE
    BSlash$ = "\"
  END IF
  SSpec$ = Drive$ + ":" + Path$ + BSlash$ + BName$ + Ext$


  REDIM DLog(6) AS Dialog
  REDIM Resp$(6)
  REDIM Menu$(0)

  DLog(0).Heading = Heading$

  DLog(1).DType = 6                            'Push Button
  DLog(1).Col = 32 * 8 - 2
  DLog(1).Heading = "OK"
  DLog(1).NumRows = GPDat(80)
  DLog(1).TextLen = 66

  DLog(2).DType = 6                            'Push Button
  DLog(2).Col = 42 * 8 - 2
  DLog(2).Heading = "Cancel"
  DLog(2).NumRows = GPDat(80)
  DLog(2).TextLen = 66
  DLog(2).Group = 1000

  DLog(3).DType = 3                            'Text entry
  DLog(3).Row = 1
  DLog(3).Col = 3
  DLog(3).Heading = "File Name: <10"
  DLog(3).TextLen = 35
  Resp$(3) = BName$ + Ext$

  DLog(4).DType = 5
  DLog(4).Row = 2 * GPDat(71) - GPDat(71) \ 2
  DLog(4).Col = 3
  IF Path$ = "" THEN Path$ = "\"
  DLog(4).Heading = Drive$ + ":" + Path$

  SELECT CASE SChoice

    CASE 0              'Plain vanilla
      DLog(1).Row = 3 * GPDat(71) - GPDat(71) \ 2 + 2 * Space8
      DLog(2).Row = 3 * GPDat(71) - GPDat(71) \ 2 + 2 * Space8

      IF GPDat(71) = 8 THEN
        DLog(5).DType = 5
        DLog(5).Row = 3 * GPDat(71)
      ELSE
        DLog(5).DType = 5
        DLog(5).Row = 2 * GPDat(71)
      END IF

    CASE IS > 0         'Partial, BASIC form definition
      IF GPDat(71) = 8 THEN MdOS = 8

      DLog(1).Row = 4 * GPDat(71) - GPDat(71) \ 2 - 2 - MdOS
      DLog(2).Row = 4 * GPDat(71) - GPDat(71) \ 2 - 2 - MdOS

      DLog(5).DType = 1
      DLog(5).Row = 4
      DLog(5).Col = 4
      DLog(5).Heading = "Save partial .PCX Image"

      IF SChoice > 1 THEN
        DLog(6).DType = 1
        DLog(6).Row = 5
        DLog(6).Col = 4
        DLog(6).Heading = "Make BASIC form module"
        DLog(1).Row = DLog(1).Row + GPDat(71)
        DLog(2).Row = DLog(2).Row + GPDat(71)
      END IF

    CASE ELSE

  END SELECT

  Button = 1

  CALL GDialog(Button, DLog(), Resp$(), Menu$(), Style, Action)

  IF Button <> 2 AND Button <> 27 THEN

    SChoice = Resp$(5) = "X"                          '0 or -1
    IF Resp$(6) = "X" THEN SChoice = SChoice + 2      '1 or 2

    TSpec$ = RTRIM$(Resp$(3))
    IF INSTR(TSpec$, ":") = 2 THEN Drv = -1
    IF INSTR(TSpec$, "\") THEN Pth = -1

    TSpec$ = NoExtension$(TSpec$) + Ext$

    IF NOT Pth THEN
      IF Path$ = "\" THEN
        BSlash$ = ""
      ELSE
        BSlash$ = "\"
      END IF
      TSpec$ = Drive$ + ":" + Path$ + BSlash$ + TSpec$
    ELSEIF NOT Drv THEN
      TSpec$ = Drive$ + ":" + TSpec$
    END IF

    IF TSpec$ <> SSpec$ THEN FileThere TSpec$, ExistButton


    IF ExistButton < 2 THEN

      FCreate (TSpec$)
      IF DOSError THEN
        ErCode = WhichError
        DOSMsg
      ELSE
        KillFile TSpec$
        ErCode = 0
        IF INSTR(TSpec$, ":") THEN Drive$ = LEFT$(TSpec$, 1)
        IF INSTR(TSpec$, "\") THEN Path$ = ParsPath$(TSpec$)
        BName$ = BaseName(TSpec$)
      END IF
    ELSE
      ErCode = -1
    END IF
  ELSE
    SChoice = 0
  END IF

  TSpec$ = ""
  SSpec$ = ""

END SUB

SUB SetCrossHair
  DSize = 7
  REDIM DLog(DSize) AS Dialog
  REDIM Resp$(DSize)
  REDIM DMenu$(0)

  DLog(0).Heading = "Select Cursor"
  DLog(1).DType = 6                            'Push Button
  DLog(1).Row = 1 * GPDat(71) + GPDat(71) \ 2 + 3 - 2 * Space8
  DLog(1).Col = 22 * 8 - 2
  DLog(1).Heading = "OK"
  DLog(1).NumRows = GPDat(80)
  DLog(1).TextLen = 66

  DLog(2).DType = 6                            'Push Button
  DLog(2).Row = 3 * GPDat(71) + GPDat(71) \ 2 + 2 + Space8
  DLog(2).Col = 22 * 8 - 2
  DLog(2).Heading = "Cancel"
  DLog(2).NumRows = GPDat(80)
  DLog(2).TextLen = 66
  DLog(2).Group = 100

  DLog(3).DType = 2                'Option Button
  DLog(3).Row = 2
  DLog(3).Col = 2
  DLog(3).Heading = "Cross Hair"
  DLog(3).TextLen = 3
  DLog(3).Group = 1

  DLog(4).DType = 2                'Option Button
  DLog(4).Row = 3
  DLog(4).Col = 2
  DLog(4).Heading = "Full Cross Hair"
  DLog(4).TextLen = 3
  DLog(4).Group = 1
  DLog(4).HotKey = 6

  DLog(5).DType = 2                'Option Button
  DLog(5).Row = 4
  DLog(5).Col = 2
  DLog(5).Heading = "X Cross Hair"
  DLog(5).TextLen = 3
  DLog(5).Group = 1

  DLog(6).DType = 2                'Option Button
  DLog(6).Row = 5
  DLog(6).Col = 2
  DLog(6).Heading = "Square"
  DLog(6).TextLen = 3
  DLog(6).Group = 1
  DLog(6).HotKey = 4
  Choice = 1

  DLog(7).DType = 7                            '
  DLog(7).Row = 1
  DLog(7).Col = 0
  DLog(7).NumRows = 4
  DLog(7).TextLen = 20

  Choice = 1
  Resp$(2 + GPDat(18)) = "O"

  CALL GDialog(Choice, DLog(), Resp$(), DMenu$(), Style, 0)

  FOR i = 3 TO 6
    IF Resp$(i) = "O" THEN EXIT FOR
  NEXT
  GPDat(18) = i - 2

  ERASE DLog, Resp$

END SUB

SUB SetEnvironment (ShowGrid) STATIC

  REDIM DLog(29) AS Dialog
  REDIM Resp$(29)
  REDIM DMenu$(0)

  Resp$(6) = SPACE$(2)              'Background color
  Resp$(13) = SPACE$(2)             'X snap
  Resp$(14) = SPACE$(2)             'YSnap

  Resp$(25) = SPACE$(3)             'Corner Radius
  Resp$(26) = SPACE$(3)             'Brush Size
  Resp$(27) = SPACE$(3)             'Mouse sensitivity
  Resp$(18) = SPACE$(2)             'BevelWidth
  Resp$(20) = SPACE$(2)             'Bevel Highlight
  Resp$(21) = SPACE$(2)             'Bevel Shadow
  
  RSET Resp$(6) = LTRIM$(STR$(GPDat(57)))
  RSET Resp$(25) = LTRIM$(STR$(GPDat(3)))
  RSET Resp$(13) = LTRIM$(STR$(GPDat(1) + 1))
  RSET Resp$(26) = LTRIM$(STR$(GPDat(8) * 2))
  RSET Resp$(14) = LTRIM$(STR$(GPDat(6) + 1))
  RSET Resp$(27) = LTRIM$(STR$(GPDat(4)))
  RSET Resp$(18) = LTRIM$(STR$(BevelWidth))
  RSET Resp$(20) = LTRIM$(STR$(BevelHi))
  RSET Resp$(21) = LTRIM$(STR$(BevelLo))

  Resp$(28) = LTRIM$(STR$(GPDat(9)))'Printer port

  DLog(0).Heading = "System Settings"

  DLog(1).DType = 6                                 'Push Button
  DLog(1).Row = 2 * GPDat(71) - GPDat(71) \ 2
  DLog(1).Col = 36 * 8 - 2
  DLog(1).Heading = "OK"
  DLog(1).NumRows = GPDat(80)'109
  DLog(1).TextLen = 66

  DLog(2).DType = 6                                 'Push Button
  DLog(2).Row = 2 * GPDat(71) - GPDat(71) \ 2
  DLog(2).Col = 46 * 8 - 2
  DLog(2).Heading = "Cancel"
  DLog(2).NumRows = GPDat(80)'109
  DLog(2).TextLen = 66
  DLog(2).Group = 1000

  DLog(3).DType = 7
  DLog(3).Heading = "Block Options "
  DLog(3).Row = 1
  DLog(3).Col = 1
  DLog(3).TextLen = 31
  DLog(3).NumRows = 2

  DLog(4).DType = 2
  DLog(4).Heading = "Copy Block"
  DLog(4).Row = 2
  DLog(4).Col = 4
  DLog(4).Group = 1
  DLog(4).HotKey = 6

  DLog(5).DType = 2
  DLog(5).Heading = "Move Block"
  DLog(5).Row = 3
  DLog(5).Col = 4
  DLog(5).Group = 1
  
  IF GPDat(20) THEN
    Resp$(4) = "O"
  ELSE
    Resp$(5) = "O"
  END IF

  DLog(6).DType = 3
  DLog(6).Heading = "BG Color:<12"
  DLog(6).TextLen = 2
  DLog(6).Row = 2
  DLog(6).Col = 19

  DLog(7).DType = 1
  DLog(7).Heading = "XOR On"
  DLog(7).Row = 3
  DLog(7).Col = 20
  DLog(7).HotKey = 2
  IF GPDat(66) THEN Resp$(7) = "X"
  
  DLog(8).DType = 7
  DLog(8).Heading = "Status Display "
  DLog(8).Row = 5
  DLog(8).Col = 1
  DLog(8).TextLen = 31
  DLog(8).NumRows = 2

  DLog(9).DType = 2
  DLog(9).Heading = "Text Coords"
  DLog(9).Row = 6
  DLog(9).Col = 4
  DLog(9).Group = 2

  DLog(10).DType = 2
  DLog(10).Heading = "Pixel Coords"
  DLog(10).Row = 7
  DLog(10).Col = 4
  DLog(10).Group = 2

  IF GPDat(19) THEN
    Resp$(9) = "O"
  ELSE
    Resp$(10) = "O"
  END IF

  DLog(11).DType = 1
  DLog(11).Heading = "Status On"
  DLog(11).Row = 7
  DLog(11).Col = 20

  IF GPDat(5) THEN
    Resp$(11) = "X"
    ShowStatus = -1
  ELSE
    ShowStatus = 0
  END IF

  DLog(12).DType = 7
  DLog(12).Heading = "Snap Settings "
  DLog(12).Row = 9
  DLog(12).Col = 1
  DLog(12).TextLen = 31
  DLog(12).NumRows = 2

  DLog(13).DType = 3                              'Text entry
  DLog(13).Row = 10
  DLog(13).Col = 3
  DLog(13).Heading = "X Snap Space:<12"
  DLog(13).TextLen = 2

  DLog(14).DType = 3                              'Text entry
  DLog(14).Row = 11
  DLog(14).Col = 3
  DLog(14).Heading = "Y Snap Space:<12"
  DLog(14).TextLen = 2

  DLog(15).DType = 1
  DLog(15).Row = 10
  DLog(15).Col = 22
  DLog(15).Heading = "Save"
  DLog(15).HotKey = 2
  IF GPDat(14) THEN Resp$(15) = "X"

  DLog(16).DType = 1
  DLog(16).Row = 11
  DLog(16).Col = 22
  DLog(16).Heading = "Snap On"
  DLog(16).HotKey = 1
  IF GPDat(17) THEN Resp$(16) = "X"

  DLog(17).DType = 7
  DLog(17).Heading = "3D Properties"
  DLog(17).Row = 13
  DLog(17).Col = 1
  DLog(17).TextLen = 31
  DLog(17).NumRows = 2

  DLog(18).DType = 3                              'Text entry
  DLog(18).Row = 14
  DLog(18).Col = 3
  DLog(18).Heading = "Bevel Width:<12"
  DLog(18).TextLen = 2

  DLog(19).DType = 1
  DLog(19).Row = 14
  DLog(19).Col = 20
  DLog(19).Heading = "Border"
  DLog(19).TextLen = 2
  DLog(19).HotKey = 4
  IF Border THEN Resp$(19) = "X"

  DLog(20).DType = 3                              'Text entry
  DLog(20).Row = 15
  DLog(20).Col = 3
  DLog(20).Heading = "Hi Color:   <12"
  DLog(20).TextLen = 2

  DLog(21).DType = 3                              'Text entry
  DLog(21).Row = 15
  DLog(21).Col = 19
  DLog(21).Heading = "Lo Color:<12"
  DLog(21).TextLen = 2

  DLog(22).DType = 1
  DLog(22).Row = 7
  DLog(22).Col = 37
  DLog(22).Heading = "Pixel Grid On"
  DLog(22).HotKey = 6
  IF GPDat(13) THEN Resp$(22) = "X"

  DLog(23).DType = 1
  DLog(23).Row = 8
  DLog(23).Col = 37
  DLog(23).Heading = "Show Grid"
  DLog(23).HotKey = 1

  IF ShowGrid THEN Resp$(23) = "X"

  DLog(24).DType = 1
  DLog(24).Row = 9
  DLog(24).Col = 37
  DLog(24).Heading = "Clear on Delete"
  DLog(24).HotKey = 9

  IF GPDat(25) THEN Resp$(24) = "X"

  DLog(25).DType = 3                              'Text entry
  DLog(25).Row = 12
  DLog(25).Col = 36
  DLog(25).Heading = "Corner Radius:<12"
  DLog(25).TextLen = 3
  DLog(25).HotKey = 9

  DLog(26).DType = 3                              'Text entry
  DLog(26).Row = 13
  DLog(26).Col = 36
  DLog(26).Heading = "Brush Size:   <12"
  DLog(26).TextLen = 3
  DLog(26).HotKey = 8

  DLog(27).DType = 3                              'Text entry
  DLog(27).Row = 14
  DLog(27).Col = 36
  DLog(27).Heading = "Mouse Sens.:  <12"
  DLog(27).TextLen = 3
  DLog(27).HotKey = 8

  DLog(28).DType = 3                              'Text entry
  DLog(28).Row = 15
  DLog(28).Col = 36
  DLog(28).Heading = "Printer Port: <12"
  DLog(28).TextLen = 1
  DLog(28).HotKey = 3

  DLog(29).DType = 7
  DLog(29).Heading = "Misc."
  DLog(29).Row = 5
  DLog(29).Col = 34
  DLog(29).TextLen = 21
  DLog(29).NumRows = 10

  EChoice = 1
  CALL GDialog(EChoice, DLog(), Resp$(), DMenu$(), Style, 0)

  IF EChoice <> 2 AND EChoice <> 27 THEN
    GPDat(57) = VAL(Resp$(6))   'Background color

    GPDat(1) = VAL(Resp$(13)) - 1 'X snap
    GPDat(6) = VAL(Resp$(14)) - 1 'Y snap

    IF GPDat(1) < 1 THEN GPDat(1) = 1
    GPDat(2) = GPDat(1) \ 2

    IF GPDat(6) < 1 THEN GPDat(6) = 1
    GPDat(10) = GPDat(6) \ 2

    GPDat(3) = VAL(Resp$(25))   'Corner radius

    TempVal = VAL(Resp$(26))
    IF TempVal < 1 THEN TempVal = 1
    GPDat(8) = TempVal \ 2 'Brush Size

    GPDat(13) = 0

    ShowGrid = Resp$(23) = "X"
    GPDat(66) = Resp$(7) = "X"

    IF Resp$(11) <> "X" AND GPDat(5) THEN
      GPDat(5) = 0
      Measured = -1
      StatBox 2
    END IF

    GPDat(5) = Resp$(11) = "X"
    GPDat(9) = VAL(Resp$(28))

    IF GPDat(5) AND NOT ShowStatus THEN StatBox 1

    BevelWidth = VAL(LTRIM$(Resp$(18)))
    Border = Resp$(19) = "X"
    BevelHi = VAL(Resp$(20))
    BevelLo = VAL(Resp$(21))


    GPDat(13) = Resp$(22) = "X"
    GPDat(14) = Resp$(15) = "X"
    GPDat(17) = Resp$(16) = "X"
    GPDat(19) = Resp$(9) = "O"
    GPDat(20) = Resp$(4) = "O"
    GPDat(25) = Resp$(24) = "X"


    IF LEN(Resp$(27)) THEN
      GPDat(4) = VAL(Resp$(27))
      Motion (GPDat(4))
    END IF
  END IF

  ERASE DLog, Resp$

END SUB

SUB StatBox (Show) STATIC
  'Draws or clears the status box

  SELECT CASE Show
    CASE 1
      IF NOT Used THEN
        Used = -1
        REDIM StArray#(GArraySize&(1, 1, 160, GPDat(71) + 11) \ 8)
      END IF

      GPDat(16) = MinInt(GPDat(16), GPDat(44) - (GPDat(71) + 12))

      HideCursor
      GET (GPDat(15), GPDat(16))-(GPDat(15) + 159, GPDat(16) + GPDat(71) + 10), StArray#
      LINE (GPDat(15), GPDat(16))-(GPDat(15) + 158, GPDat(16) + GPDat(71) + 9), 7, BF
      LINE (GPDat(15) + 2, GPDat(16) + GPDat(71) + 10)-STEP(156, 0), 0
      LINE (GPDat(15) + 159, GPDat(16) + 2)-STEP(0, GPDat(71) + 7), 0
      CALL PCBox(GPDat(15), GPDat(16), GPDat(15) + 158, GPDat(16) + GPDat(71) + 9, -1)
      CALL PCBox(GPDat(15) + 3, GPDat(16) + 3, GPDat(15) + 47, GPDat(16) + GPDat(71) + 6, 0)
      CALL PCBox(GPDat(15) + 52, GPDat(16) + 3, GPDat(15) + 155, GPDat(16) + GPDat(71) + 6, 0)
      LineBVE GPDat(15) + 4, GPDat(16) + 4, GPDat(15) + 46, GPDat(16) + GPDat(71) + 5, 0
      LineBVE GPDat(15) + 53, GPDat(16) + 4, GPDat(15) + 154, GPDat(16) + GPDat(71) + 5, 0
      LINE (GPDat(15) + 5, GPDat(16) + 5)-(GPDat(15) + 45, GPDat(16) + GPDat(71) + 4), GPDat(82), BF
      IF GPDat(21) THEN
        RelClr = 0 + 7 * 256
      ELSE
        RelClr = 7 + 7 * 256
      END IF
      CALL GPrint2VE(GPDat(16) + 5, GPDat(15) \ 8 + 14, CHR$(7), RelClr)
      IF GPDat(65) THEN
        RelClr = 0 + 7 * 256
      ELSE
        RelClr = 7 + 7 * 256
      END IF
      CALL GPrint2VE(GPDat(16) + 5, GPDat(15) \ 8 + 13, CHR$(219), RelClr)
      ShowCursor
    CASE 2
      HideCursor                 ' turn it off
      PUT (GPDat(15), GPDat(16)), StArray#, PSET
      ShowCursor
      Used = 0
    CASE 3
      REDIM StArray#(0)          'Release memory
      Used = 0
  END SELECT

END SUB

SUB SvForm (ScrName$, Fld() AS FieldInfoG, Calc$(), Help$())

  DIM Test AS FieldInfoG

  CrLf$ = CHR$(13) + CHR$(10)

  IF Fld(1).RCol THEN

    Fld(0).Fields = 0
    Fld(0).StorLen = 0
    N = 1

    DO WHILE Fld(N).RCol <> 0 AND Fld(0).Fields < MaxFields
      Fld(0).StorLen = Fld(0).StorLen + Fld(N).StorLen
      Fld(0).Fields = Fld(0).Fields + 1
      N = N + 1
    LOOP

    Nam$ = UCASE$(BaseName$(ScrName$))

    Path$ = NoExtension$(ScrName$)

    Ext$ = ".FRM"

    IF GPDat(84) THEN
      IF FileSize&(Path$ + ".DAT") > 2 OR Exist(Path$ + ".DBF") THEN

        Ext$ = ".NEW"
        OPEN Path$ + ".FRM" FOR RANDOM AS #1 LEN = FrmWdth
        GET #1, 1, Test
        Recs = Test.Fields
        FOR N = 2 TO Recs + 1
          GET #1, N, Test
          IF Test.StorLen <> Fld(N - 1).StorLen THEN EXIT FOR
          'IF Test.FType <> Fld(N - 1).FType THEN EXIT FOR
          IF Fld(N - 1).FType = Relational THEN
            IF Test.ScratchI <> Fld(N - 1).ScratchI THEN EXIT FOR
          END IF
        NEXT
        CLOSE #1

        IF N - 2 <> Fld(0).Fields THEN
          IF N <= Recs THEN
            BEEP
            REDIM Msg$(2)
            Msg$(0) = "Attention"
            Msg$(1) = "New form definition requires that `" + Nam$ + ".DAT'"
            Msg$(2) = "be rebuilt!  Save New Definition?"
            Butn = MsgBox(Msg$(), 112)

            IF Button <> 1 THEN
              Inf.FrmChanged = 0
              EXIT SUB
            END IF
          END IF
        ELSE
          IF Exist(Path$ + ".NEW") THEN KILL Path$ + ".NEW"
            Ext$ = ".FRM"
        END IF
      END IF
    END IF

    IF Exist(Path$ + ".NEW") THEN Ext$ = ".NEW"


    FrmName$ = Path$ + Ext$


    FCreate FrmName$
    FOpen FrmName$, Handle
    BLen& = (Fld(0).Fields + 1) * FrmWdth
    FPutA Handle, Fld(0), BLen&

    '---- FrmWdth = Num bytes in TYPE 'FieldInfo'

    IF DOSError THEN
      GPDat(84) = -1
      REDIM Msg$(1)
      Msg$(0) = "Error"
      Msg$(1) = ErrorMsg$(WhichError)
      Butn = MsgBox(Msg$(), 21)
      FClose Handle
      KillFile FrmName$
      EXIT SUB
    END IF


    IF FindLast(VARPTR(Calc$(MaxFields)), MaxFields) THEN
      FPut Handle, "C"
      FOR N = 1 TO Fld(0).Fields
        FPut Handle, UCASE$(Calc$(N)) + CrLf$
      NEXT
    END IF


    IF FindLast(VARPTR(Help$(MaxFields)), MaxFields) THEN
      FPut Handle, "H"
      FOR N = 1 TO Fld(0).Fields
         FPut Handle, Help$(N) + CrLf$
      NEXT
    END IF

    FClose Handle
    GPDat(84) = 0

    FrmName$ = Path$ + ".BI"
    FCreate FrmName$
    FOpen FrmName$, Handle

    FPut Handle, "TYPE " + Nam$ + CrLf$
    FOR N = 1 TO Fld(0).Fields

      OK = -1
      IF Fld(N).FType = PButton THEN OK = 0
      IF Fld(N).FType = MouseFld AND Fld(N).LowRange = 0 THEN OK = 0

      IF OK THEN
        FPut Handle, "    " + Fld(N).FldName$ + " AS "

        SELECT CASE Fld(N).FType
          CASE IntFld, DateFld, EuroDateFld, HScrollFld, VScrollFld
            Temp$ = "INTEGER"
          CASE LongIntFld', MultChFld, RecNoFld, TotRecsFld
            Temp$ = "LONG"
          CASE SngFld
            Temp$ = "SINGLE"
          CASE DblFld, MoneyFld
            Temp$ = "DOUBLE"
          CASE MouseFld
            Temp$ = "STRING * 1"
          CASE ELSE
            Temp$ = "STRING *" + STR$(Fld(N).StorLen)
        END SELECT

        IF LEN(Temp$) THEN FPut Handle, Temp$ + CrLf$
      END IF

    NEXT

    FPut Handle, "END TYPE" + CrLf$
    FClose Handle

  END IF

END SUB

