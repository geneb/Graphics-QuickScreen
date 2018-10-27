DEFINT A-Z

'---- Graphic Screen and Form design main module

'by Phil Cramer
'Copyright (c) 1992 Crescent Software

'---- BASIC subs
DECLARE SUB ArrangeFlds (Fld() AS ANY, Calc$(), Help$())
DECLARE SUB BarPrintMSG (MChoice$(), Stat%(), HKey)
DECLARE SUB CheckMem (Chk, Amt&)
DECLARE SUB CopyFlds (Fld() AS ANY, Calc$(), Help$(), NumFlds)
DECLARE SUB CrossHair (x%, y%)
DECLARE SUB CursorXY (x%, y%, Button%, Ky%)
DECLARE SUB DefCursor (Num)
DECLARE SUB DOSMsg ()
DECLARE SUB DrawFont ()
DECLARE SUB DrawPalette (x%, y%, BWidth%, BHite%, DChoice%, Clr%)
DECLARE SUB EditFormG (Form$(), Fld() AS ANY, Frm AS ANY, Action%)
DECLARE SUB Flood (Tile$)
DECLARE SUB FreeMouse ()
DECLARE SUB GetFldDef (FrmName$, StartEl%, Fld() AS ANY, Form$())
DECLARE SUB GetDefs (FldNum%, FldLen%, Calc$(), Help$(), Fld() AS ANY)
DECLARE SUB GetXORBox (x1, y1, x2, y2, Corner, xOffset, yOffset, Paste, PasteBuf(), ANDArray())
DECLARE SUB GDialog (Choice%, DLog() AS ANY, TResp$(), Menu$(), Style, Action%)
DECLARE SUB GQSMessage (A$(), Row%, Dir%)
DECLARE SUB Grid (x1%, y1%, x2%, y2%, xspace%, yspace%, XHiSpace%, YHiSpace%, Clr%, Clr1%)
DECLARE SUB GPalette (x%, y%, BWidth%, BHite%, DChoice%, Clr%, PasteBuf(), ANDArray())
DECLARE SUB HelpMsg ()
DECLARE SUB HFlip ()
DECLARE SUB LineType ()
DECLARE SUB LoadFile (Drive$, Path$, BName$, Ext$, Opt, ExitButton)
DECLARE SUB LoadFiles (Spec$, FileMenu$(), FieldInfoG)
DECLARE SUB LoadWindowsImage (FileName$, XORArray%(), ANDArray%(), ErrCode%)
DECLARE SUB MakeDemo (FileName$, Fld() AS ANY, Calc$(), Help$())
DECLARE SUB MakeFrmModule (FilNam$, Fld() AS ANY, Calc$(), Help$())
DECLARE SUB MakeUniqueFldName (FldNo%, Form() AS ANY)
DECLARE SUB MouseBox (x1, y1, lastx2, lasty2, LeaveIt, Button, Clr)
DECLARE SUB MoveField (Fld() AS ANY, Copy%, PasteBuf%(), ANDArray(), Calc$(), Help$())
DECLARE SUB PaintBrush (XStep%, YStep%)
DECLARE SUB PaletteEdit (x%, y%, BWidth%, BHite%, Done%)
DECLARE SUB PCBox (x1%, y1%, x2%, y2%, Outside%)
DECLARE SUB PrintText ()
DECLARE SUB PullDnMsG (MChoice$(), Stat%(), Menu%, Choice%, Ky$, Action%)
DECLARE SUB PrintFRM (SBName$, Fld() AS ANY, Calc$(), Help$())
DECLARE SUB PrintScrn ()
DECLARE SUB PutTile (x1, y1, x2, y2, PasteBuf())
DECLARE SUB SetEnvironment (ShowGrid)
DECLARE SUB SelTile (x, y, PasteBuf())
DECLARE SUB SetCrossHair ()
DECLARE SUB SetScreen ()
DECLARE SUB StatBox (A%)
DECLARE SUB SaveFile (Drive$, Path$, BName$, Ext$, SChoice, Heading$, SErrCode%, Choice)
DECLARE SUB SmoothLine (OC)
DECLARE SUB SRScreen (SaveRest)
DECLARE SUB SvForm (ScrName$, Form() AS ANY, Calc$(), Help$())
DECLARE SUB Tokenize (Calc$, Fld() AS ANY)
DECLARE SUB VFlip ()

'---- BASIC Functions
DECLARE FUNCTION AltMenu% (MChoice$(), Stat%())
DECLARE FUNCTION BaseName$ (FileName$)
DECLARE FUNCTION EPal$ ()
DECLARE FUNCTION GArraySize& (x1, y1, x2, y2)
DECLARE FUNCTION GQSPath$ ()
DECLARE FUNCTION MsgBox (A$(), Opts)
DECLARE FUNCTION NoExtension$ (F$)

'---- ASM subs
DECLARE SUB CDir (NewDir$)
DECLARE SUB DeInstallKeyDown ()
DECLARE SUB DispPCXVE (BYVAL Display%)
DECLARE SUB FClose (Handle)
DECLARE SUB FCreate (FileName$)
DECLARE SUB FGetA (Handle, SEG Element AS ANY, NumBytes&)
DECLARE SUB FGet (Handle, Buf$)
DECLARE SUB FOpen (FileName$, Handle)
DECLARE SUB FPut (Handle, Dest$)
DECLARE SUB FPutA (Handle, SEG Element AS ANY, NumBytes&)
DECLARE SUB FSeek (Handle, Location&)
DECLARE SUB GetCursor (x%, y%, Button%)
DECLARE SUB HideCursor ()
DECLARE SUB InitMouse (MouseThere%)
DECLARE SUB InstallKeyDown ()
DECLARE SUB KillFile (FileName$)
DECLARE SUB LineBVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB LineBFVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB Motion (MValue%)
DECLARE SUB Mouse (AX%, BX%, CX%, DX%)
DECLARE SUB PositionPCXVE (BYVAL LineNo, BYVAL Col)
DECLARE SUB ReadDir (BYVAL Address)
DECLARE SUB ReadFile (BYVAL Address)
DECLARE SUB SavePCXVE (FileName$)
DECLARE SUB SAVEPCXREGIONVE (FileName$, BYVAL x1, BYVAL y1, BYVAL x2, BYVAL y2)
DECLARE SUB SetDrive (Drive$)
DECLARE SUB SetPaletteEGA (BYVAL Reg, BYVAL Value)
DECLARE SUB SetPalTripleVGA (BYVAL Reg, BYVAL R, BYVAL G, BYVAL B)
DECLARE SUB ShowCursor ()
DECLARE SUB StuffBuf (A$)
DECLARE SUB WaitUp ()

'---- ASM functions
DECLARE FUNCTION AltKey ()
DECLARE FUNCTION DCount% (DirSpec$)
DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION ErrorMsg$ (Er)
DECLARE FUNCTION Exist% (FileName$)
DECLARE FUNCTION FCount% (FileSpec$)
DECLARE FUNCTION FLof& (Handle)
DECLARE FUNCTION FLInput$ (Handle, Buff$)
DECLARE FUNCTION FUsing$ (A$, Mask$)
DECLARE FUNCTION GetDir$ (Drive$)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION GWFileSize& (FileName$)
DECLARE FUNCTION MaxInt (A, B)
DECLARE FUNCTION MinInt (A, B)
DECLARE FUNCTION Monitor% ()
DECLARE FUNCTION MultMonitor%
DECLARE FUNCTION Null% (Work$)
DECLARE FUNCTION OpenPCXFile% (FileName$, Array$)
DECLARE FUNCTION PDQTimer& ()
DECLARE FUNCTION QPTrim$ (A$)
DECLARE FUNCTION WhichError ()

'$INCLUDE: 'EditForm.BI'
'$INCLUDE: 'FldInfo.bi'
'$INCLUDE: 'DLogType.Bi'
'$INCLUDE: 'GetVid.bas'
'$INCLUDE: 'GPFont.GW'

DIM Frm AS FormInfoG
DIM Blank AS FormInfoG
IF GPDat(31) = 8 THEN VGA = -1
SetScreen

InstallKeyDown

IF GPDat(31) = 8 THEN SetPalTripleVGA 8, 28, 28, 28

REDIM Fld(MaxFields)  AS FieldInfoG             'Use CONST MaxFields
REDIM Calc$(MaxFields)
REDIM Help$(MaxFields)
REDIM FileMenu$(0)
REDIM PasteBuf(0)
REDIM ANDArray(0)

REDIM Sav(15)

'---- Set Defaults
FldNum = 1
Delay = 18                      '1/18 secs before restoring mouse cursor

GPDat(3) = 15                   'Round box radius
GPDat(4) = 5                    'Mouse Sensitivity
GPDat(5) = 0                    'XYOn/Color on
GPDat(7) = 0                    'Current palette drawing, 50 disable f10, 60 diasable snap on/off
GPDat(8) = 4                    '1/2 Paintbrush size
GPDat(9) = 1                    'Default LPT number
GPDat(11) = GPDat(43) \ 2       'Screen X Center
GPDat(12) = GPDat(44) \ 2       'Screen Y Center
GPDat(13) = -1                  'Pixel edit Grid On/Off
GPDat(14) = 0                   'Save grid snap settings
GPDat(15) = 59 * 8              'Status box X ULx Corner coord
GPDat(16) = 5                   'Status box x ULy Corner coord
GPDat(18) = 1                   'Cursor type 1 = crosshair, 2 = Full CH, 3 = X
GPDat(19) = 0                   'Text coords on/off
GPDat(20) = -1                  'Copy/Move
GPDat(21) = 0                   'Relative coords
GPDat(22) = 15 + 24 * 256       'Cursor color
GPDat(24) = 90                  'Italic angle
GPDat(25) = -1                  'Clear fields when deleted

GPDat(57) = 0                   'BackGround Color

GPDat(65) = -1                  'TextSnap On
GPDat(66) = 0                   'XOR On/OFF
GPDat(68) = -1                  'Default line style

GPDat(74) = 0 + 256 * 7
GPDat(76) = 256 * 7             'List color
GPDat(77) = 8 + 256 * 7
GPDat(78) = 15 + 0 * 256        'Hilite color

GPDat(81) = 15 + 7 * 256        '60 and 43 line mode hot key color
GPDat(82) = 15                  'Starting Color
GPDat(83) = 0                   'Drawing changed flag
GPDat(84) = 0                   'Form definition changed

GPDat(87) = 7                   'scroll bar button color    (for list boxes)
GPDat(88) = 15                  'scroll bar highlight color
GPDat(89) = 8                   'scroll bar shadow color
GPDat(90) = -1                  '

GPDat(93) = -1                  'Save listbox/ message to array
GPDat(94) = 0                   'Message text color
GPDat(95) = 7                   'Message bg color
GPDat(96) = 15                  'Message hilite color
GPDat(97) = 8                   'Message shade color
GPDat(98) = 7                   'Scroll bar slide color
GPDat(99) = 7                   'Number of list choices before adding scroll bar


ShowGrid = 0

'----Set default drives, paths, extentions
Drive$ = CHR$(GetDrive%)
LDrive$ = Drive$
SDrive$ = Drive$
PasteDrive$ = Drive$

Path$ = GetDir$(Drive$)
LPath$ = Path$
SPath$ = Path$
PastePath$ = Path$

IconPath$ = Drive$ + ":" + Path$
TilePath$ = IconPath$         'Path$ for tile palette
FontPath$ = IconPath$

'3D box and frame settings
BevelWidth = 1
Border = -1
Drop = -1
BevelHi = 15
BevelLo = 8



'---- Look for configuration file
IF Exist%("GQS.CNF") THEN
  CnfPath$ = Path$
  IF RIGHT$(CnfPath$, 1) <> "\" THEN CnfPath$ = CnfPath$ + "\"
ELSE
  CnfPath$ = GQSPath$
END IF

IF Exist%(CnfPath$ + "GQS.CNF") THEN

  Buff$ = SPACE$(80)
  FOpen CnfPath$ + "GQS.CNF", HandleS

  CNF$ = FLInput$(HandleS, Buff$)
  IconPath$ = QPTrim$(CNF$)

  CNF$ = FLInput$(HandleS, Buff$)
  TilePath$ = QPTrim$(CNF$)

  CNF$ = FLInput$(HandleS, Buff$)
  FontPath$ = QPTrim$(CNF$)

  FGetA HandleS, SEG GPDat(1), 50
  FGetA HandleS, SEG GPDat(57), 2
  FGetA HandleS, SEG GPDat(65), 2

  FGetA HandleS, SEG ShowGrid, 2
  FGetA HandleS, SEG BevelWidth, 2
  FGetA HandleS, SEG Border, 2
  FGetA HandleS, SEG BevelHi, 2
  FGetA HandleS, SEG BevelLo, 2

  FClose HandleS
  Buff$ = ""

END IF

'---- Override defaults
IF NOT GPDat(14) THEN
  GPDat(1) = 8                  'X Snap spacing
  GPDat(2) = GPDat(1) \ 2       '1/2 X Snap Spacing
  GPDat(6) = GPDat(71)          'Y Snap spacing
  GPDat(10) = GPDat(6) \ 2      '1/2 Y Snap Spacing
  GPDat(17) = -1                'Snap on/off
END IF

BName$ = "*"
SBName$ = "WORKING"
PasteBName$ = "IMAGE"
TileBName$ = BName$

Ext$ = ".PCX"
LExt$ = Ext$
SExt$ = Ext$
PasteExt$ = ".GMP"
TileExt$ = ".Til"

TileTLine = 57
TileULCol = 3
Palx = 204
Paly = 100
NewScrn = -1
Measured = -1

XCoord$ = SPACE$(5)
YCoord$ = SPACE$(5)
		     '0         1         2     3456789012345
DefaultEGAPal$ = CHR$(0) + CHR$(1) + CHR$(2) + "89:;<=>?"
EGAPal$ = DefaultEGAPal$

REDIM StArray#(GArraySize&(1, 1, 159, GPDat(71) + 10) \ 8)


Motion (GPDat(4))
DefCursor 2
ShowCursor                              'Turn it on
FreeMouse

IF GPDat(5) THEN StatBox (1)

'----- Define the Menu Choices here
REDIM Menu$(12, 4)                      'Allocate space for the Menu Choices
REDIM Stat(12, 4)                       'Define the choice status array

'----- Define the menus
Menu$(0, 0) = "File"

Menu$(1, 0) = "New Screen..."
Menu$(2, 0) = "Open..."
Menu$(3, 0) = "Save"
Menu$(4, 0) = "Save As..."
 Stat(4, 0) = 5 * 256
Menu$(5, 0) = "-"
Menu$(6, 0) = "Save Paste Buf..."
 Stat(6, 0) = 2 * 256
Menu$(7, 0) = "Load Paste Buf..."
Menu$(8, 0) = "-"
Menu$(9, 0) = "Print Screen..."
Menu$(10, 0) = "-"
Menu$(11, 0) = "DOS Shell"
Menu$(12, 0) = "Exit Program"
 Stat(12, 0) = 1 * 256


Menu$(0, 1) = "Edit"

Menu$(1, 1) = "Copy Block"
Menu$(2, 1) = "Move Block"
Menu$(3, 1) = "Paste"
 Stat(3, 1) = 1
Menu$(4, 1) = "Tile Paste Image"
 Stat(4, 1) = 1

Menu$(5, 1) = "Flip Horizontal"
 Stat(5, 1) = 5 * 256
Menu$(6, 1) = "Flip Vertical"
 Stat(6, 1) = 5 * 256
Menu$(7, 1) = "Measure"
 Stat(7, 1) = 3 * 256



Menu$(0, 2) = "Draw"

Menu$(1, 2) = "Print Text"
Menu$(2, 2) = "Draw Text...   F7"
Menu$(3, 2) = "Tile...        F2"
Menu$(4, 2) = "Open Curve"
Menu$(5, 2) = "Closed Curve"
Menu$(6, 2) = "Scroll Bar"
Menu$(7, 2) = "3D Panel   (Drop)"
Menu$(8, 2) = "3D Panel   (Pop) "
 Stat(8, 2) = 1 * 256
Menu$(9, 2) = "3D Frame   (Drop)"
 Stat(9, 2) = 3 * 256
Menu$(10, 2) = "3D Frame   (Pop) "
 Stat(10, 2) = 4 * 256
Menu$(11, 2) = "Engraved Box"
' Stat(11, 2) = 4 * 256




Menu$(0, 3) = "Settings"

Menu$(1, 3) = "Cursor...     F4"
Menu$(2, 3) = "Line Type...  F5"
Menu$(3, 3) = "Palette...    F3"
Menu$(4, 3) = "Status Box    F8"
 Stat(4, 3) = 7 * 256
Menu$(5, 3) = "System...     F6"
Menu$(6, 3) = "Set Paths..."
 Stat(6, 3) = 1 * 256


Menu$(0, 4) = "Compose Fields"

Menu$(1, 4) = "Enter Field Definitions..."
Menu$(2, 4) = "Copy Fields"
Menu$(3, 4) = "Move Fields"
Menu$(4, 4) = "Rearrange Fields..."
Menu$(5, 4) = "Print Field Definitions"
Menu$(6, 4) = "Make Demo..."
 Stat(6, 4) = 5 * 256

Menu$(7, 4) = "-"
Menu$(8, 4) = "Try Data Entry in Form"
Action = 0

IF ShowGrid THEN
  HideCursor
  CALL Grid(0, 0, GPDat(43), GPDat(44), 8, GPDat(71), 32, GPDat(71) * 2, 8, 7)
  ShowCursor
END IF

CALL StuffBuf(CHR$(27))

DO
  CALL CursorXY(Mx, My, Button, Ky)

  IF Button = 1 AND GPDat(5) = -1 THEN    'See if it clicked on the status box
    IF Mx > GPDat(15) AND Mx < GPDat(15) + 158 AND My >= GPDat(16) AND My <= GPDat(16) + GPDat(71) + 9 THEN
      
      HideCursor
      StatBox (2)

      lastx = GPDat(15)
      lasty = GPDat(16)
      LineBVE GPDat(15), GPDat(16), GPDat(15) + 158, GPDat(16) + GPDat(71) + 9, 6152
      ShowCursor
      
      xOS = ((Mx - GPDat(15)) \ 8) * 8
      yOS = My - GPDat(16)
      CALL Mouse(7, 0, xOS, GPDat(43) - 170 + xOS) 'Limit Horizontal range
      CALL Mouse(8, 0, yOS, GPDat(44) - GPDat(71) - 12 + yOS) 'Limit Vertical range

      DO
	GetCursor x, y, Button
	x = ((x + 4) \ 8) * 8                 'GPDat(1) = XSnap Spacing
	x = x - xOS
	y = y - yOS

	IF x <> lastx OR y <> lasty THEN
	  HideCursor
	  LineBVE lastx, lasty, lastx + 158, lasty + GPDat(71) + 9, 6152
	  LineBVE x, y, x + 158, y + GPDat(71) + 9, 6152
	  ShowCursor
	  lastx = x
	  lasty = y
	END IF
      LOOP UNTIL Button = 0

      FreeMouse
      xOS = 0
      yOS = 0
      GPDat(15) = x' - xOS
      GPDat(16) = y' - yOS
      HideCursor
      LineBVE x, y, x + 158, y + GPDat(71) + 9, 6152
      ShowCursor
      StatBox (1)
      Button = 0
    END IF
  END IF

  IF Button = 2 THEN                          'Draw the palette
    px = 180
    PClr = 8
    
    IF Mx < 223 THEN                          'Set screen limits for palette
      px = 15
    ELSEIF Mx > 440 THEN
      px = 334
    END IF
    
    IF My > GPDat(44) - 105 THEN My = GPDat(44) - 105
    IF My < 8 THEN My = 8
    PHite = 18
    IF GPDat(31) = 5 THEN PHite = 18
    CALL GPalette(px, My, 36, PHite, DChoice, 8, PasteBuf(), ANDArray())
    Ky = 0
    GPDat(7) = 0
  END IF
  
  '----  F10 = restore screen, F8 = statusbox on/off, |\ toggle dotted/solid
  '      Aa = toggle arc rotation

  SELECT CASE Ky

    CASE -59            'F1  Help
      CALL HelpMsg

    CASE -60            'F2  Tile
      Menu = 2
      MenuChoice = 3
      FunKey = -1

    CASE -61            'F3  Palette
      Menu = 3
      MenuChoice = 3
      FunKey = -1

    CASE -62            'F4  Cursor
      Menu = 3
      MenuChoice = 1
      FunKey = -1

    CASE -63            'F5  Line Type
      Menu = 3
      MenuChoice = 2
      FunKey = -1

    CASE -64            'F6  Environment
      Menu = 3
      MenuChoice = 5
      FunKey = -1

    CASE -65            'F7  Draw Text
      Menu = 2
      MenuChoice = 2
      FunKey = -1

    CASE -67            'F9  Display memory status
      REDIM Msg$(3)
      Msg$(0) = "Available Memory"
      Msg$(1) = "String space:  " + FUsing$(STR$(FRE("")), ",######")
      Msg$(2) = "Far memory:    " + FUsing$(STR$(FRE(-1)), ",######")
      Msg$(3) = "Stack space:   " + FUsing$(STR$(FRE(-2)), ",######")
      Butn = MsgBox(Msg$(), 21)

  END SELECT
   
  IF AltKey OR Button = 1 OR Ky = 27 OR FunKey THEN
    WaitUp
    Ky$ = ""

    MAction = 0
    IF AltKey THEN
      MAction = -1
      M = AltMenu(Menu$(), Stat())
      IF M = 27 THEN
	FunKey = -1                           'Escape, use Funkey to skip menu
	M = Menu
	Ky$ = CHR$(27)
      END IF
      Menu = M
    END IF

    IF NOT FunKey THEN
      IF GPDat(86) AND NOT GPDat(70) THEN     'If nothing in paste buf
	Stat(3, 1) = 0                        ' or used by tile palette
	Stat(4, 1) = 0
	Stat(6, 0) = 0 + 2 * 256              'Enable save paste buf
      ELSE
	Stat(3, 1) = 1
	Stat(4, 1) = 1
	Stat(6, 0) = 1 + 2 * 256
      END IF

      IF GPDat(84) THEN FrmStarted = -1       'Form Def changed
      IF Fld(0).Fields = 0 THEN FrmStarted = 0
      IF FrmStarted THEN
	Stat(2, 4) = 0
	Stat(3, 4) = 0
	IF Fld(0).Fields > 1 THEN Stat(4, 4) = 0
	Stat(5, 4) = 0
	Stat(6, 4) = 0 + 5 * 256
	Stat(8, 4) = 0
      ELSE
	Stat(2, 4) = 1
	Stat(3, 4) = 1
	Stat(4, 4) = 1
	Stat(5, 4) = 1
	Stat(6, 4) = 1 + 5 * 256
	Stat(8, 4) = 1
	Action = 0
      END IF

      PullDnMsG Menu$(), Stat(), Menu, MenuChoice, Ky$, MAction

    END IF

    FunKey = 0

    IF Ky$ <> CHR$(27) THEN

      SELECT CASE Menu
	
	CASE 0                                '---- File Menu
	  SELECT CASE MenuChoice
	    CASE 1                            'New Screen
	      DSize = 7
	      REDIM DLog(DSize) AS Dialog
	      REDIM Resp$(DSize)
	      REDIM DMenu$(0)
	      DLog(0).Heading = "Select Screen Mode"
	      DLog(1).DType = 6               'Push Button
	      DLog(1).Row = 1 * GPDat(71) + GPDat(71) \ 2 + 1 - 2 * Space8
	      DLog(1).Col = 31 * 8 - 2
	      DLog(1).Heading = "OK"
	      DLog(1).NumRows = GPDat(80)
	      DLog(1).TextLen = 66

	      DLog(2).DType = 6               'Push Button
	      DLog(2).Row = 3 * GPDat(71) + GPDat(71) \ 2 + 1 + Space8
	      DLog(2).Col = 31 * 8 - 2
	      DLog(2).Heading = "Cancel"
	      DLog(2).NumRows = GPDat(80)
	      DLog(2).TextLen = 66
	      DLog(2).HotKey = 1
	      DLog(2).Group = 100

	      ScrnOpts = 6
	      FrameRows = 4
	      IF NOT VGA THEN
		ScrnOpts = 4
		FrameRows = 2
	      END IF

	      FOR i = 3 TO ScrnOpts
		DLog(i).DType = 2             'Option Button
		DLog(i).Row = i - 1
		DLog(i).Col = 3
		DLog(i).TextLen = 3
		DLog(i).Group = 1
	      NEXT

	      DLog(3).Heading = "EGA 640 x 350, 25 Lines"
	      DLog(4).Heading = "EGA 640 x 350, 43 Lines"
	      DLog(5).Heading = "VGA 640 x 480, 30 Lines"
	      DLog(6).Heading = "VGA 640 x 480, 60 Lines"

	      DLog(4).HotKey = 2
	      DLog(6).HotKey = 1
	       
	      DLog(7).DType = 7
	      DLog(7).Row = 1
	      DLog(7).Col = 1
	      DLog(7).NumRows = FrameRows
	      DLog(7).TextLen = 28

	      Choice = 1
	      Resp$(2 + GPDat(64)) = "O"

	      CALL GDialog(Choice, DLog(), Resp$(), DMenu$(), Style, 0)

	      IF Choice <> 2 AND Choice <> 27 THEN
		FOR i = 3 TO 6
		 IF Resp$(i) = "O" THEN EXIT FOR
		NEXT

		GOSUB ChkStatus               'If status box on, turn it off
		StatBox 3                     'Reset status box

		SELECT CASE i - 2
		  CASE 1                      'Screen 9, 25 lines
		    GPDat(85) = 25
		    EGAPal$ = DefaultEGAPal$

		  CASE 2                      'Screen 9, 43 lines
		    GPDat(85) = 43
		    EGAPal$ = DefaultEGAPal$

		  CASE 3                      'Screen 12, 30 lines
		    GPDat(85) = 30

		  CASE 4                      'Screen 12, 60 lines
		    GPDat(85) = 60
		END SELECT

		GOSUB SetMode

		REDIM Text$(2)
		REDIM Resp$(0)
		REDIM Fld(MaxFields) AS FieldInfoG
		REDIM Calc$(MaxFields)
		REDIM Help$(MaxFields)
		BName$ = "WORKING"
		SBName$ = BName$
		FrmStarted = 0
		GPDat(82) = 15
		LineBFVE 0, 0, GPDat%(43), GPDat%(44), GPDat(57)  'Background Color
		IF ShowGrid THEN CALL Grid(0, 0, GPDat(43), GPDat(44), 8, GPDat(71), 32, GPDat(71) * 2, 8, 7)
		Motion (GPDat(4))
		GPDat(17) = -1                'Turn snap on
		FreeMouse
		ShowCursor
		IF GPDat(5) THEN
		  StatBox 1
		END IF

	      END IF

	      CASE 2                          '---- Open
		LoadFile LDrive$, LPath$, LBName$, LExt$, LoadOpts, XButton

		IF XButton <> 2 AND XButton <> 27 THEN
		  IF LPath$ = "\" THEN
		    BSlash$ = ""
		  ELSE
		    BSlash$ = "\"
		  END IF
		  TFName$ = LDrive$ + ":" + LPath$ + BSlash$ + LBName$ + LExt$

		  GOSUB NotSaved  'Make sure we saved existing before reload
		  IF XtButton <> 2 THEN

		    GOSUB LoadScrn
		    GPDat(83) = 0
		    GPDat(84) = 0
		    NewScrn = 0   'NewScrn only tracks blank starting screens
		  END IF
		  XtButton = 0

		END IF

	      CASE 3                          '---- Save File
		SXButton = 0
		IF SBName$ = "WORKING" THEN
		  SChoice = 1 + Fld(0).Fields
		  CALL SaveFile(SDrive$, SPath$, SBName$, SExt$, SChoice, "Save File", SErrCode, SXButton)
		  IF SXButton <> 2 THEN LastSBName$ = SBName$
		END IF
		IF SXButton <> 2 AND SXButton <> 27 AND SErrCode = 0 THEN GOSUB SavFile

	      CASE 4                          '---- Save as
		SChoice = 1 + Fld(0).Fields
		LastSBName$ = UCASE$(SBName$)
		CALL SaveFile(SDrive$, SPath$, SBName$, SExt$, SChoice, "Save File As", SErrCode, XButton)
		IF XButton <> 2 AND XButton <> 27 AND SErrCode = 0 THEN
		  PartialSaved = 0
		  GOSUB SavFile
		END IF

	      CASE 6                          '---- Save Paste Buffer
		SPChoice = 0
		CALL SaveFile(PasteDrive$, PastePath$, PasteBName$, PasteExt$, SPChoice, "Save Paste Buffer", PErCode, XButton)

		IF PastePath$ = "\" THEN
		  BSlash$ = ""
		ELSE
		  BSlash$ = "\"
		END IF
		TFName$ = PasteDrive$ + ":" + PastePath$ + BSlash$ + PasteBName$ + PasteExt$

		IF XButton <> 2 AND XButton <> 27 THEN
		  IF NOT PErCode AND XButton <> 2 THEN
		    GMPBytes& = UBOUND(PasteBuf, 1)
		    GMPBytes& = GMPBytes& * 2
		    FCreate (TFName$)
		    FOpen TFName$, Handle
		    CALL FPutA(Handle, SEG PasteBuf(0), GMPBytes&)
		    FClose Handle
		    IF DOSError THEN
		      DOSMsg
		    END IF
		  END IF
		END IF

	      CASE 7                          '---- Load Paste Buffer
		PLoadOpts = 0                 'No check boxes
		LoadFile PasteDrive$, PastePath$, PasteBName$, PasteExt$, PLoadOpts, XButton
		IF PastePath$ = "\" THEN
		  BSlash$ = ""
		ELSE
		  BSlash$ = "\"
		END IF
		TFName$ = PasteDrive$ + ":" + PastePath$ + BSlash$ + PasteBName$ + PasteExt$

		IF XButton <> 2 AND XButton <> 27 THEN
		  REDIM ANDArray(0)

		  SELECT CASE UCASE$(PasteExt$)
		    CASE ".GMP"
		      GMPBytes& = GWFileSize&(TFName$)
		      REDIM PasteBuf(GMPBytes& \ 2)
		      FOpen TFName$, Handle

		      IF DOSError THEN
			DOSMsg
		      ELSE
			WinImage = 0
			GPDat(67) = 1             'Paste buff Loaded flag
			GPDat(86) = -1            'Contents in buff flag
			GPDat(70) = 0             'No longer used by tile palette
			FGetA Handle, SEG PasteBuf(0), GMPBytes&
			FClose Handle
		      END IF

		    CASE ".ICO", ".BMP", ".CUR"
		      REDIM ANDArray(0)

		      CALL DefCursor(1)
		      CALL LoadWindowsImage(TFName$, PasteBuf(), ANDArray(), ErrCode)
		      CALL DefCursor(2)

		      SELECT CASE ErrCode
			CASE IS > 0
			  DOSMsg
			CASE -1, -2, -3
			  REDIM Msg$(1)
			  IF ErrCode = -1 THEN            '256 or greater
			    Msg$(1) = "Cannot load: " + TFName$
			  ELSEIF ErrCode = -2 THEN        'Bad extension
			    Msg$(1) = "Too many colors!"
			  ELSE                            '
			    Msg$(1) = "File too large for paste buffer!"
			  END IF

			  Butn = MsgBox(Msg$(), 21)

			CASE 0
			  WinImage = UCASE$(PasteExt$) <> ".BMP"
			  GPDat(67) = 1             'Paste buff Loaded flag
			  GPDat(86) = -1            'Contents in buff flag
			  GPDat(70) = 0             'No longer used by tile palette

		      END SELECT

		    CASE ELSE

		  END SELECT

		END IF

		SetDrive (Drive$)
		CDir (Path$)
		
	      CASE 9                          '---- Print Screen
		PrintScrn

	      CASE 11                         '---- DOS Shell
		LSChoice = SChoice

		TempWork$ = SBName$
		IF GPDat(83) OR GPDat(84) OR NewScrn THEN   'If not saved
		  NewScrn = 0
		  SErrCode = 0
		  BackUp = -1
		  SBName$ = "GQSBack"
		  DStat = GPDat(83)
		  FStat = GPDat(84)
		  SChoice = 0
		  GOSUB SavFile
		  GPDat(83) = DStat
		  GPDat(84) = FStat
		END IF
		SCREEN 0

		SHELL

		IF GPDat(31) = 5 THEN
		  SCREEN 9
		  IF GPDat(71) = 8 THEN WIDTH , 43
		ELSE
		  SCREEN 12
		  IF GPDat(71) = 8 THEN WIDTH , 60
		END IF
		FreeMouse

		IF NOT BackUp THEN SBName$ = TempWork$

		IF BackUp THEN LastSBName$ = ""
		BackUp = 0

		TFName$ = LDrive$ + ":" + LPath$ + BSlash$ + SBName$ + LExt$

		GOSUB LoadScrn
		ShowCursor

		IF Exist%("GQSBack.PCX") THEN
		  KillFile "GQSBack.PCX"
		  KillFile "GQSBack.FRM"
		END IF

		SBName$ = TempWork$
		Motion (GPDat(4))
		SChoice = LSChoice

	      CASE 12                         '---- Quit
		GOSUB NotSaved
		IF XtButton < 2 THEN

		  '----Create configuration file
		  FCreate CnfPath$ + "GQS.CNF"
		  FOpen CnfPath$ + "GQS.CNF", HandleS
		  FPut HandleS, IconPath$ + CHR$(13) + CHR$(10)
		  FPut HandleS, TilePath$ + CHR$(13) + CHR$(10)
		  FPut HandleS, FontPath$ + CHR$(13) + CHR$(10)

		  FPutA HandleS, SEG GPDat(1), 50
		  FPutA HandleS, SEG GPDat(57), 2
		  FPutA HandleS, SEG GPDat(65), 2

		  FPutA HandleS, SEG ShowGrid, 2
		  FPutA HandleS, SEG BevelWidth, 2
		  FPutA HandleS, SEG Border, 2
		  FPutA HandleS, SEG BevelHi, 2
		  FPutA HandleS, SEG BevelLo, 2

		  FClose HandleS
		  EXIT DO

		END IF
		XtButton = 0

	      CASE ELSE
	   
	    END SELECT

	CASE 1                                '---- Edit
	  SELECT CASE MenuChoice

	    CASE 1                            '---- Copy block
	      CM = GPDat(20)
	      GPDat(20) = -1
	      Clr = 14 + 24 * 256
	      Clr1 = 15 + 24 * 256
	      Corner = 0
	      SRScreen 0                      'Save the screen
	      CALL MouseBox(x1, y1, x2, y2, Corner, Button, Clr)
	      IF Button <> 2 THEN
		GPDat(7) = 35
		CALL GetXORBox(x1, y1, x2, y2, Corner, 0, 0, 0, PasteBuf(), ANDArray())
	      END IF
	      SRScreen 2                      'Clear the saved screen
	      GPDat(20) = CM

	    CASE 2                            '---- Move Block
	      CM = GPDat(20)
	      GPDat(20) = 0
	      Clr = 14 + 24 * 256
	      Clr1 = 15 + 24 * 256
	      Corner = 0
	      SRScreen 0             'Save the screen
	      CALL MouseBox(x1, y1, x2, y2, Corner, Button, Clr)
	      IF Button <> 2 THEN
		GPDat(7) = 35
		CALL GetXORBox(x1, y1, x2, y2, Corner, 0, 0, 0, PasteBuf(), ANDArray())
	      END IF
	      SRScreen 2                      'Clear the saved screen
	      GPDat(20) = CM

	    CASE 3                            '---- Paste
	      CM = GPDat(20)
	      GPDat(20) = -1
	      GPDat(7) = 35
	      SRScreen 0
	      CALL GetXORBox(x1, y1, x2, y2, Corner, 0, 0, -1, PasteBuf(), ANDArray())
	      SRScreen 2                      'Clear the saved screen
	      GPDat(7) = 0
	      GPDat(20) = CM

	    CASE 4                            '---- Tile Paste Buffer Image
	      SRScreen 0                  'Save the screen
	      DO
		DO
		  CALL MouseBox(x1, y1, x2, y2, 0, Button, 15 + 24 * 256)
		LOOP UNTIL Button > 0

		IF Button = 1 THEN
		  HideCursor
		  CALL PutTile(x1, y1, x2, y2, PasteBuf())
		  ShowCursor
		END IF

	      LOOP UNTIL Button = 2

	      SRScreen 2

	    CASE 5
	      GPDat(7) = 50                  'Disable F10 - Undo
	      HFlip

	    CASE 6
	      GPDat(7) = 50                  'Disable F10 - Undo
	      VFlip

	    CASE 7                            '---- Measure
	      Measured = NOT Measured
		 
	      IF NOT GPDat(5) THEN
		StatBox (1)
		GPDat(5) = -1
	      END IF

	      IF Measured THEN
		GPDat(5) = 0
		StatBox (2)
	      ELSE
		TR = GPDat(21)                'Remember if relative is on/off
		GPDat(21) = -1

		HideCursor
		GetCursor lastmx, lastmy, MButton
		CrossHair lastmx, lastmy

		DO
		  CALL CursorXY(Mx1, My1, MButton, mKy)
		  IF lastmx <> Mx1 OR lastmy <> My1 THEN
		    CrossHair lastmx, lastmy
		    CrossHair Mx1, My1
		    lastmx = Mx1
		    lastmy = My1
		  END IF
		LOOP UNTIL MButton = 2

		CrossHair Mx1, My1
		ShowCursor
		GPDat(21) = TR                'Restore previouse setting
	      END IF

	  END SELECT

	CASE 2                                '---- Draw
	  IF MenuChoice > 3 THEN SRScreen 0

	  SELECT CASE MenuChoice              'Branch according to "Choice"

	    CASE 1
	      CALL PrintText

	    CASE 2
	      GPDat(7) = 36
	      CALL DrawFont
	      
	    CASE 3                            '---- Tile Palette
	      GPDat(7) = 60                   'Disable snap on/off
	      SRScreen 2
	      CALL SelTile(TileULCol, TileTLine, PasteBuf())
	      LoadOpts = 0                    'No check boxes

	    CASE 4                            '---- Open Curve
	      CALL SmoothLine(1)
					
	    CASE 5                            '---- Closed Curve
	      CALL SmoothLine(0)

	    CASE 6
	      GPDat(7) = 34
	      CALL MouseBox(x, y, x1, y1, 8, Button, 15 + 24 * 256)

'-- NEW
	    CASE 7   '3D panel  drop
	      Drop = -1
	      CALL MouseBox(x, y, x1, y1, 9, Button, 15 + 24 * 256)

	    CASE 8   '3D panel  pop
	      Drop = 0
	      CALL MouseBox(x, y, x1, y1, 9, Button, 15 + 24 * 256)

	    CASE 9   '3D frame  drop
	      GPDat(7) = 75     'Set bevel width, not color
	      Drop = -1
	      CALL MouseBox(x, y, x1, y1, 10, Button, 15 + 24 * 256)

	    CASE 10  '3D frame pop
	      GPDat(7) = 75
	      Drop = 0
	      CALL MouseBox(x, y, x1, y1, 10, Button, 15 + 24 * 256)

	    CASE 11
	      GPDat(7) = 75
	      CALL MouseBox(x, y, x1, y1, 11, Button, 15 + 24 * 256)
'--
	  END SELECT

	  SRScreen 2

	CASE 3                                '----Settings
	  SELECT CASE MenuChoice
	    CASE 1                            '----Cursor
	      CALL SetCrossHair

	    CASE 2
	      CALL LineType
	       
	    CASE 3                            '---- Palette
	      GPDat(7) = 60                   'Disable snap on/off
	      BWidth = 34
	      BHite = 18
	      Clr = 7
	      Done = 0

	      DO
		CALL PaletteEdit(Palx, Paly, BWidth, BHite, Done)
	      LOOP UNTIL Done

	    CASE 4                            '---- Status Box
	      GPDat(5) = NOT GPDat(5)
	      IF GPDat(5) THEN
		StatBox (1)
	      ELSE
		Measured = -1
		StatBox (2)
	      END IF

	    CASE 5                            '---- Environment
	      CALL SetEnvironment(ShowGrid)
	     ' GOSUB PopDrop

	    CASE 6                            'Set Paths
	      REDIM DMenu$(0)
	      REDIM Resp$(5)
	      REDIM DLog(5)  AS Dialog

	      Resp$(3) = IconPath$
	      Resp$(4) = TilePath$
	      Resp$(5) = FontPath$
	  
	      DLog(0).Heading = "Set Paths"

	      FOR i = 1 TO 2
		DLog(i).DType = 6             'Push Button
		DLog(i).Row = 6 * GPDat(71) + GPDat(71) \ 2 + 3
		DLog(i).NumRows = GPDat(80)
		DLog(i).TextLen = 66
	      NEXT

	      DLog(1).Heading = "OK"
	      DLog(1).Col = 12 * 8 - 2

	      DLog(2).Col = 27 * 8 - 2
	      DLog(2).Heading = "Cancel"
	      DLog(2).HotKey = 1
	      DLog(2).Group = 100

	      FOR i = 3 TO 5
		DLog(i).DType = 3
		DLog(i).Row = i - 2
		DLog(i).Col = 2
		DLog(i).TextLen = 30
	      NEXT

	      DLog(4).Row = 3
	      DLog(3).Heading = "Icon files:<10"
	      DLog(4).Heading = "Tile files:<10"
	      DLog(5).Heading = "Font files:<10"
	      DLog(5).Row = 5

	      CALL GDialog(Choice, DLog(), Resp$(), DMenu$(), Style, 0)

	      IF Choice <> 2 AND Choice <> 27 THEN
		IconPath$ = QPTrim$(Resp$(3))
		TilePath$ = QPTrim$(Resp$(4))
		FontPath$ = QPTrim$(Resp$(5))
	      END IF
		 
	  END SELECT

	CASE 4                                '---- Define fields
	  SELECT CASE MenuChoice

	    CASE 1                            '---- Enter field definitions
	      GOSUB ChkStatus                 'If Status is on, turn it off
	      StatusStatus = GPDat(5)
	      GPDat(5) = 0
	      StatBox 3                       'Reset status box
	      FldNum = Fld(0).Fields + 1
	      CALL GetDefs(FldNum, NumFlds, Calc$(), Help$(), Fld())
	      WaitUp
	      IF StatusStatus THEN
		GPDat(5) = -1
		StatBox 1
	      END IF

	    CASE 2                            '---- Copy Fields
	      FldCopy = -1
	      CALL MoveField(Fld(), FldCopy, PasteBuf(), ANDArray(), Calc$(), Help$())
	      NumFlds = Fld(0).Fields

	    CASE 3
	      FldCopy = 0
	      CALL MoveField(Fld(), FldCopy, PasteBuf(), ANDArray(), Calc$(), Help$())

	    CASE 4                            '---- Arrange fields
	      CALL ArrangeFlds(Fld(), Calc$(), Help$())
	    
	    CASE 5                            '---- Print Field Defs
	      PrintFRM SBName$, Fld(), Calc$(), Help$()

	    CASE 6
	      MakeDemo SBName$, Fld(), Calc$(), Help$()

	    CASE 8                            '---- Try data entry in form
	      Mult = -1
	      REDIM PasteBuf(0): GPDat(86) = 0'Clear PasteBuf, set flag
	      WinImage = 0
	      GOSUB ChkStatus
	      StatBox 3                       'Clear Status Buffer

	      REDIM FData$(Fld(0).Fields, 2)

	      FOR N = 1 TO Fld(0).Fields
		FData$(N, 1) = Help$(N)
		Help$(N) = ""
		FData$(N, 2) = Calc$(N)
		IF LEN(FData$(N, 2)) THEN Tokenize FData$(N, 2), Fld()
		IF Fld(N).FType = MultChAFld THEN Mult = Mult + 1

		IF N = 1 THEN
		  Fld(1).Fields = 1
		ELSE
		  Fld(N).Fields = Fld(N - 1).Fields + Fld(N - 1).StorLen
		END IF
		Fld(0).StorLen = Fld(N - 1).Fields + Fld(N - 1).StorLen - 1
	      NEXT
	      Fld(0).StorLen = Fld(N - 1).Fields + Fld(N - 1).StorLen - 1

	      IF Mult > -1 THEN
		REDIM Choice$(10, Mult)
		Mult = -1
		FOR N = 1 TO Fld(0).Fields
		  IF Fld(N).FType = MultChAFld THEN
		    Mult = Mult + 1
		    Choice$(0, Mult) = STR$(N)
		    FOR R = 1 TO 10
		      Choice$(R, Mult) = CHR$(R + 64) + " Choice "
		    NEXT
		  END IF
		NEXT
	      END IF

	      Frm.FldNo = 1
	      FAction = -1
	      Frm = Blank
	      SRScreen (0)

	      '----- Poll the editor in a loop.
	      DO
		EditFormG FData$(), Fld(), Frm, FAction
	      LOOP UNTIL Frm.KeyCode = 27

	      SRScreen (-1)
	      SRScreen (2)
		  
	      GPDat(76) = 256 * 7             'List color
	      GPDat(78) = 15 + 0 * 256        'Hilite color

	      '---- Restore default settings
	      FOR i = 1 TO Fld(0).Fields
		SELECT CASE Fld(i).FType

		  CASE HScrollFld
		    Fld(i).Value = Fld(i).LowRange
		    Fld(i).RelHandle = Fld(i).LCol + 16

		  CASE VScrollFld
		    Fld(i).Value = Fld(i).LowRange
		    Fld(i).RelHandle = Fld(i).Row + 16

		  CASE MouseFld
		    IF Fld(i).LowRange THEN Fld(i).LowRange = -1

		  CASE ScrollFld
		    Fld(i).ScratchI = 1

		END SELECT

		Help$(i) = FData$(i, 1)
		FData$(i, 1) = ""
		FData$(i, 2) = ""

	      NEXT

	      ERASE FData$

	      IF GPDat(5) THEN StatBox 1

	  END SELECT
      END SELECT
      GPDat(7) = 0                            'Current drawing choice
    END IF
  END IF
  
LOOP

HideCursor                                    'Turn the mouse cursor off
DeInstallKeyDown
SCREEN 0                                      'Return to text mode
COLOR 7, 0
CLS
END
					      'Fini

NotSaved:
 IF GPDat(83) OR GPDat(84) THEN
   SErrCode = 0
   XtButton = 1
   IF GPDat(84) THEN
     Ms$ = "form"
   ELSE
     Ms$ = "drawing"
   END IF
   REDIM Msg$(2)
   Msg$(0) = "Warning"
   Msg$(1) = "The " + Ms$ + " has been modified and"
   Msg$(2) = "has not been saved.  Save it now?"
   Butn = MsgBox(Msg$(), 133)

   SELECT CASE Butn
     CASE 1                                   'OK
       IF UCASE$(SBName$) = "WORKING" THEN
	 SChoice = 1 + Fld(0).Fields
	 CALL SaveFile(SDrive$, SPath$, SBName$, SExt$, SChoice, "Save File", SErrCode, XtButton)
	 CheckExist = -1
       ELSE
	 SErrCode = 0
       END IF
       IF XtButton <> 2 AND XtButton <> 27 THEN GOSUB SavFile
     CASE 2                                   'No
     CASE 3                                   'Cancel
       XtButton = 2
     END SELECT
  END IF
RETURN


SavFile:

  IF SErrCode = 0 THEN                        'If no error code returned
    IF SPath$ = "\" THEN
      BSlash$ = ""
    ELSE
      BSlash$ = "\"
    END IF
    SSpec$ = SDrive$ + ":" + SPath$ + BSlash$ + SBName$ + ".PCX"

    GOSUB ChkStatus                           'See if the status box is on

    Fld(0).Indexed = GPDat(71)

    IF ABS(SChoice) = 1 OR PartialSaved THEN  'Save partial PCX image

      IF NOT PartialSaved THEN                'Already have location
	REDIM A$(2)                           ' so skip this part
	A$(1) = "Using the Mouse or Cursor keys,"
	A$(2) = "draw a box surrounding the image."
	GQSMessage A$(), 20, 0                'Draw the message

	Stt = GPDat(5)
	GPDat(5) = 0
	
	HideCursor
	CALL CursorXY(lasttx, lastty, Button, Ky)

	DO
	  CALL CursorXY(tx, ty, Button, Ky)
	LOOP UNTIL tx <> lasttx OR ty <> lastty OR Ky = 13

	GQSMessage A$(), 20, -1               'Clear the message

	SnapStatus = GPDat(17)
	LastXSnap = GPDat(1)
	LastYSnap = GPDat(6)
	GPDat(7) = 60
	GPDat(1) = 8
	GPDat(2) = 4
	GPDat(6) = 1
	GPDat(10) = 1
	GPDat(17) = -1
	
	DO
	  CALL MouseBox(Partialx1, Partialy1, Partialx2, Partialy2, 0, Button, 15 + 24 * 256)
	LOOP UNTIL Button > 0

	GPDat(5) = Stt
	GPDat(1) = LastXSnap
	GPDat(2) = LastXSnap \ 2
	GPDat(6) = LastYSnap
	GPDat(10) = LastYSnap \ 2
	GPDat(17) = SnapStatus
	IF Button = 2 THEN
	  IF GPDat(5) THEN StatBox 1
	  ShowCursor
	  RETURN
	END IF
      END IF

      Partialx1 = MaxInt(Partialx1, 8)
      Partialx2 = MinInt(Partialx2, 631)

      CALL SAVEPCXREGIONVE(SSpec$, BYVAL Partialx1, BYVAL Partialy1, BYVAL Partialx2, BYVAL Partialy2)
      Fld(0).Row = Partialy1
      Fld(0).LCol = Partialx1 \ 8 + 1
      Fld(0).Decimals = Partialy2 - Partialy1 + 1
      Fld(0).RCol = (Partialx2 - Partialx1) \ 8
      PartialSaved = -1                       'Set PartialPCX Flag
      GPDat(83) = 0
      ShowCursor

    ELSE                                      'Save full screen

      HideCursor
      CALL SavePCXVE(SSpec$)
      PartialSaved = 0
      GPDat(83) = 0
      ShowCursor

      '---- Reset in case they were set by partial save
      Fld(0).Row = 0
      Fld(0).LCol = 0

    END IF

    RowCol$ = CHR$(GPDat(31)) + CHR$(GPDat(85))
    IF PartialSaved THEN RowCol$ = RowCol$ + MKI$(Partialy1) + MKI$(Partialx1 \ 8 + 1)
    
    FOpen SSpec$, Handle

    IF GPDat(31) = 5 THEN
      Pal$ = EPal$
      FSeek Handle, 16&
      FPut Handle, Pal$
      Pal$ = ""
    END IF

    FSeek Handle, 84&                         'Byte 85 in the header
    FPut Handle, RowCol$
    FClose Handle
    Fld(0).Value = GPDat(31)

    IF GPDat(5) THEN
      StatBox 1
    END IF

    SvForm SSpec$, Fld(), Calc$(), Help$()
    IF SChoice > 0 THEN MakeFrmModule SBName$, Fld(), Calc$(), Help$()
  ELSE
    REDIM Msg$(1)
    Msg$(0) = "File Error"
    Msg$(1) = ErrorMsg$(SErrCode)
    Butn = MsgBox(Msg$(), 11)
  END IF

  ExistButton = 0
RETURN


LoadScrn:

  GOSUB ChkStatus                             'See if the status box is on

  RowCol$ = SPACE$(6)
  FOpen TFName$, Handle
  FSeek Handle, 84&
  FGet Handle, RowCol$
  FClose Handle
  
  IF NOT Null(RowCol$) THEN
    ScrnMode = ASC(MID$(RowCol$, 1, 1))
    ScrnRows = ASC(MID$(RowCol$, 2, 1))
    Unknown = -1

    '---- See if its a GQS screen
    IF ScrnMode = 5 OR ScrnMode = 8 THEN
      IF ScrnRows = 25 OR ScrnRows = 30 OR ScrnRows = 43 OR ScrnRows = 60 THEN
	Unknown = 0

	DiffMode = ScrnMode <> GPDat(31)
	DiffRows = ScrnRows <> GPDat(85)
	NewMode = 0


	IF ScrnMode = 8 AND NOT VGA THEN
	  REDIM Msg$(1)
	  Msg$(0) = "Warning"
	  Msg$(1) = "This screen requires a VGA monitor"
	  Msg$(2) = "to display properly. Load anyway?"
	  Button = MsgBox(Msg$(), 32)
	  IF Button = 2 THEN RETURN
	ELSEIF DiffMode OR DiffRows THEN
	  REDIM Msg$(2)
	  Msg$(0) = "Attention"
	  Msg$(1) = "This screen was originally saved"
	  Msg$(2) = "in a different mode. Change Modes?"
	  Button = MsgBox(Msg$(), 132)
	  IF Button = 1 THEN                  'Change Modes
	    GPDat(85) = ScrnRows
	    NewMode = -1
	    GOSUB SetMode
	    FreeMouse
	  END IF
	END IF
      END IF
    END IF
  END IF


  Headr$ = SPACE$(68 + 768)                   'Load the screen
  OK = OpenPCXFile(TFName$, Headr$)

  IF OK THEN

    IF NOT BackUp THEN
      SDrive$ = LDrive$                       'Remember where loaded from to
      SPath$ = LPath$                         ' know where to save to later
      SBName$ = LBName$
      LastSBName$ = LBName$
    END IF

    Ver = ASC(MID$(Headr$, 2, 1))
    XWide = ASC(MID$(Headr$, 14, 1)) * 256 + ASC(MID$(Headr$, 13, 1))
    YHite = ASC(MID$(Headr$, 16, 1)) * 256 + ASC(MID$(Headr$, 15, 1))
    Pal$ = MID$(Headr$, 17, 48)

    FullScreen = XWide = 640 AND (YHite = 350 OR YHite = 480)

    '---- Load only screens that will fit!
    IF XWide <= 640 AND (YHite <= 480) THEN

      IF Ver = 2 OR Ver = 5 THEN              '2 and 5 contain palette info
	N = 1
	i = 1

	IF LEN(QPTrim$(Pal$)) THEN
	  IF YHite = 350 OR (NOT FullScreen AND GPDat(31) = 5) THEN          'EGA SCREEN 9
	    EGAPal$ = ""
	    FOR t = 0 TO 15
	      N = 0

	      A = ASC(MID$(Pal$, i, 1)) \ 64  'Red
	      IF (A AND 1) = 1 THEN N = N + 32
	      IF (A AND 2) = 2 THEN N = N + 4
	      i = i + 1

	      B = ASC(MID$(Pal$, i, 1)) \ 64  'Green
	      IF (B AND 1) = 1 THEN N = N + 16
	      IF (B AND 2) = 2 THEN N = N + 2
	      i = i + 1

	      C = ASC(MID$(Pal$, i, 1)) \ 64  'Blue
	      IF (C AND 1) = 1 THEN N = N + 8
	      IF (C AND 2) = 2 THEN N = N + 1
	      i = i + 1

	      CALL SetPaletteEGA(t, N)
	      EGAPal$ = EGAPal$ + CHR$(N)
	    NEXT

					      'VGA SCREEN 12
	  ELSEIF YHite = 480 OR (NOT FullScreen AND GPDat(31) = 8) THEN
	    FOR i = 0 TO 15                   'Set palette
	      SetPalTripleVGA i, ASC(MID$(Pal$, N, 1)) \ 4, ASC(MID$(Pal$, N + 1, 1)) \ 4, ASC(MID$(Pal$, N + 2, 1)) \ 4
	      N = N + 3
	    NEXT
	  END IF
	END IF
      END IF

    ELSE
      BEEP
      RETURN
    END IF

    IF NOT FullScreen AND NOT Unknown THEN
      PCXRow = CVI(MID$(RowCol$, 3, 2))
      PCXCol = CVI(MID$(RowCol$, 5, 2))
      PositionPCXVE PCXRow, PCXCol
    END IF
    PartialSaved = 0                 '7/6/94
    SChoice = 0                      '7/6/94

    HideCursor
    CALL DispPCXVE(VPage)

    PDQ& = PDQTimer&
    WHILE PDQ& + Delay + 12 > PDQTimer&: WEND
    InitMouse (There)
    Motion (GPDat(4))
    ShowCursor
    ShowCursor

    IF GPDat(5) THEN
      StatBox 1
    END IF

    FrmBName$ = NoExtension$(TFName$) + ".FRM"

    REDIM Form$(MaxFields, 2)
    REDIM Fld(MaxFields) AS FieldInfoG
    REDIM Calc$(MaxFields)
    REDIM Help$(MaxFields)
    FldNum = 1
    NumFlds = 1

    GetFldDef FrmBName$, StartEl, Fld(), Form$()

    IF NOT DOSError THEN
      BName$ = BaseName$(TFName$)
      NumFlds = Fld(0).Fields

      FOR i = 1 TO NumFlds
	Calc$(i) = Form$(i, 2)
	Form$(i, 2) = ""
      NEXT

      FOR i = 1 TO NumFlds
	Help$(i) = Form$(i, 1)
	Form$(i, 1) = ""
      NEXT
      FrmStarted = -1

    END IF

    ERASE Form$

  ELSE
    BEEP
    REDIM Msg$(1)
    Msg$(0) = "File Access Error"
    Msg$(1) = "     File Not Found      "
    Msg$(1) = "Cannot find " + TFName$
    Butn = MsgBox(Msg$(), 11)
  END IF

  SetDrive (Drive$)
  CDir (Path$)
RETURN

ChkStatus:
  IF GPDat(5) THEN                            'If status box on, turn it off
    StatBox 2
  END IF
RETURN


SetMode:
  SELECT CASE GPDat(85)

    CASE 25                                   'Screen 9, 25 lines
      GPDat(31) = 5
      CALL SetScreen
      WIDTH , 25
      Space8 = 0

    CASE 30                                   'Screen 12, 30 lines
      GPDat(31) = 8
      CALL SetScreen
      WIDTH , 30
      Space8 = 0

    CASE 43                                   'Screen 9, 43 lines
      GPDat(31) = 5
      CALL SetScreen
      WIDTH , 43
      GPDat(85) = 43
      GPDat(71) = 8
      GPDat(80) = 18
      GPDat(64) = 2
      Space8 = 2

    CASE 60                                   'Screen 12, 60 lines
      GPDat(31) = 8
      CALL SetScreen
      WIDTH , 60
      GPDat(85) = 60
      GPDat(71) = 8
      GPDat(80) = 18
      GPDat(64) = 4
      Space8 = 2


  END SELECT

  PDQ& = PDQTimer&
  WHILE PDQ& + Delay > PDQTimer&: WEND
  NewScrn = -1

  IF NOT GPDat(14) THEN
    GPDat(1) = 8                         'X Snap spacing
    GPDat(2) = GPDat(1) \ 2              '1/2 X Snap Spacing
    GPDat(6) = GPDat(71)                 'Y Snap spacing
    GPDat(10) = GPDat(6) \ 2             '1/2 Y Snap Spacing
    GPDat(17) = -1                       'Snap On off
  END IF

  GPDat(83) = 0
  GPDat(84) = 0
  FldNum = 1
  NumFlds = 1
  PartialSaved = 0
  InitMouse (There)
  Motion (GPDat(4))
  Palx = 204
  Paly = 100
  TileTLine = 57
  TileULCol = 3
RETURN

FUNCTION EPal$

  FOR i = 1 TO 16

    Clr = ASC(MID$(EGAPal$, i, 1))

    Red = 0
    Blue = 0
    Green = 0

    IF Clr AND 32 THEN Red = 1
    IF Clr AND 4 THEN Red = Red + 2
    Red = Red * 64

    IF Clr AND 16 THEN Green = 1
    IF Clr AND 2 THEN Green = Green + 2
    Green = Green * 64

    IF Clr AND 8 THEN Blue = 1
    IF Clr AND 1 THEN Blue = Blue + 2
    Blue = Blue * 64

    Pal$ = Pal$ + CHR$(Red) + CHR$(Green) + CHR$(Blue)

  NEXT

  EPal$ = Pal$

END FUNCTION

'This subprogram sets the proper screen mode
'
SUB SetScreen STATIC


    IF GPDat(31) = 5 THEN
       SCREEN 9
       GPDat(85) = 25
       GPDat(43) = 640
       GPDat(44) = 350
       GPDat(50) = 16
       GPDat(71) = 14    'Character size is 8x14 pixels
       GPDat(80) = 22
       GPDat(64) = 1

    ELSEIF GPDat(31) = 8 THEN
       SCREEN 12
       GPDat(85) = 30
       GPDat(43) = 640
       GPDat(44) = 480
       GPDat(50) = 16
       GPDat(71) = 16    'Character size is 8x16 pixels
       GPDat(80) = 24    'Button Height
       GPDat(64) = 3

    ELSE

      LOCATE 5, 1
      PRINT "Sorry, this program is for EGA and VGA color monitors only."
      END

    END IF

    GPDat(49) = INT((4 * (GPDat(44) / GPDat(43)) / 3) * 1000)
    GPDat(69) = GPDat(49)           'Polygon aspect ratio

END SUB

