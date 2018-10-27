DEFINT A-Z
'---- ASM subs
DECLARE SUB DispPCXVE (BYVAL VideoPage)
DECLARE SUB FClose (Handle)
DECLARE SUB FGet (Handle, Text$)
DECLARE SUB FGetT (Handle, Source AS ANY, NumBytes)
DECLARE SUB FGetA (Handle, SEG Element AS ANY, NumBytes&)
DECLARE SUB FOpen (FileName$, Handle)
DECLARE SUB FSeek (Handle, Location&)
DECLARE SUB HideCursor ()
DECLARE SUB PCXLibraryFile (BYVAL OffSet&, BYVAL Size&)
DECLARE SUB PositionPCXVE (BYVAL LineNo, BYVAL Column)
DECLARE SUB SetError (Errnum%)
DECLARE SUB SetVideo ()
DECLARE SUB SetPaletteEGA (BYVAL PalReg%, BYVAL Value%)
DECLARE SUB SetPalTripleVGA (BYVAL PalReg%, BYVAL Red%, BYVAL Green%, BYVAL Blue%)
DECLARE SUB ShowCursor ()

'---- ASM functions
DECLARE FUNCTION DOSError ()
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION FLInput$ (Handle%, Buffer$)
DECLARE FUNCTION MaxInt% (A, B)
DECLARE FUNCTION OpenPCXFile (FileName$, Header$)
DECLARE FUNCTION ErrorMsg$ (er)
DECLARE FUNCTION WhichError ()

'---- BASIC subs
DECLARE SUB FindLibFile (LibName$, FileName$, Size&, OffSet&, Handle%, ErrCode%)
DECLARE SUB MoveFRM (Fld() AS ANY, Row, Col)
DECLARE SUB Tokenize (Calc$, Fld() AS ANY)

'---- BASIC functions
DECLARE FUNCTION LibNumFieldsG% (LibName$, FormName$)


'$INCLUDE: 'FldInfo.bi'                 'Defines TYPE for field information
'$INCLUDE: 'Common.Bi'

TYPE Header                             'Defines TYPE for library header
  FilName AS STRING * 12
  Size AS LONG
  OS AS LONG
END TYPE

SUB FindLibFile (LibName$, FileName$, Size&, OffSet&, Handle, ErrCode)

'Finds a specified file within a .GSL library and returns its size and offset
'  within the library
'
'
'ErrCode = 1 - File not found in library
'ErrCode > 1 - Error opening or retrieving a library file

  ErrCode = 0

  Per = INSTR(LibName$, ".")
  IF Per THEN LibName$ = LEFT$(LibName$, Per - 1)
  LibName$ = LibName$ + ".GSL"

  CALL FOpen(LibName$, Handle)
  IF DOSError THEN
    ErrCode = WhichError
    EXIT SUB
  END IF

  DIM FileInfo AS Header

  FileName$ = UCASE$(FileName$)
  Found = 0

  CALL FGetT(Handle, NumScreens, 2)
  FOR i = 1 TO NumScreens
    CALL FGetT(Handle, FileInfo, 20)

    IF DOSError THEN
      CALL FClose(Handle)
      ErrCode = WhichError
      EXIT SUB
    END IF

    IF FileName$ = RTRIM$(FileInfo.FilName) THEN
      Found = -1
      EXIT FOR
    END IF
  NEXT

  IF NOT Found THEN
    CALL FClose(Handle)
    ErrCode = 1
    EXIT SUB
  ELSE
    Size& = FileInfo.Size
    OffSet& = FileInfo.OS
  END IF

END SUB

SUB LibGetFldDefG (LibName$, FrmName$, StartEl, Fld() AS FieldInfoG, Form$(), ErrCode)
                                                         
  CALL FindLibFile(LibName$, FrmName$, Size&, OffSet&, Handle, ErrCode)
  IF ErrCode THEN EXIT SUB

  FSeek Handle, OffSet&
  FGetT Handle, NumFlds, 2
  NumBytes& = (NumFlds + 1) * FrmWdth
  FSeek Handle, OffSet&
  FGetA Handle, Fld(StartEl), NumBytes&

  Byte$ = " "
  FGet Handle, Byte$
  NumBytes& = NumBytes& + 1

  Buffer$ = SPACE$(600)

  DO UNTIL DOSError OR NumBytes& >= Size&
      IF Byte$ = "H" THEN
         FOR N = StartEl + 1 TO StartEl + NumFlds
            Form$(N, 1) = FLInput$(Handle, Buffer$)
            NumBytes& = NumBytes& + LEN(Form$(N, 1))
            IF DOSError THEN EXIT FOR
         NEXT
      ELSE
         FOR N = StartEl + 1 TO StartEl + NumFlds
            Form$(N, 2) = FLInput$(Handle, Buffer$)
            NumBytes& = NumBytes& + LEN(Form$(N, 2))
            IF DOSError THEN EXIT FOR
            IF LEN(Form$(N, 2)) THEN Tokenize Form$(N, 2), Fld()
         NEXT
      END IF

      FGet Handle, Byte$
      NumBytes& = NumBytes& + 1
   LOOP
   Buffer$ = ""
   FClose Handle

   '----- Calculate pointers into whole record buffer
   IF StartEl = 0 THEN
      Fld(StartEl + 1).Fields = 1
   ELSE
      Fld(StartEl + 1).Fields = Fld(StartEl - 1).Fields + Fld(StartEl - 1).StorLen
   END IF

   FOR N = StartEl + 2 TO StartEl + Fld(StartEl).Fields
      Fld(N).Fields = Fld(N - 1).Fields + Fld(N - 1).StorLen
   NEXT

   Fld(0).StorLen = Fld(N - 1).Fields + Fld(N - 1).StorLen - 1

   SetError 0                                   'No errors

END SUB

SUB LibGetGMP (LibName$, GMPName$, Array(), ErrCode)

  CALL FindLibFile(LibName$, GMPName$, Size&, OffSet&, Handle, ErrCode)
  IF ErrCode THEN EXIT SUB

  REDIM Array(1 TO Size& \ 2)
  CALL FSeek(Handle, OffSet&)
  CALL FGetA(Handle, SEG Array(1), Size&)
  IF DOSError THEN
    ErrCode = WhichError
    EXIT SUB
  END IF
  CALL FClose(Handle)

END SUB

FUNCTION LibNumFieldsG (LibName$, FormName$)

  CALL FindLibFile(LibName$, FormName$, Size&, OffSet&, Handle, ErrCode)
  IF ErrCode THEN
    LibNumFieldsG = -ErrCode
    EXIT FUNCTION
  END IF

  CALL FSeek(Handle, OffSet&)
  CALL FGetT(Handle, NumFlds, 2)
  CALL FClose(Handle)

  LibNumFieldsG = NumFlds

END FUNCTION

SUB LibShowForm (LibName$, FileName$, Fld() AS FieldInfoG, Row, Col, VPage, ErrCode) STATIC

  CALL FindLibFile(LibName$, FileName$, Size&, OffSet&, Handle, ErrCode)
  CALL FClose(Handle)

  IF ErrCode THEN EXIT SUB

  '---- ErrCode = 1 - Error opening library file
  '---- ErrCode = 2 - File not found in library
  '---- ErrCode = 3 - NOT an EGA or VGA monitor
  '---- ErrCode = 4 - Error in loading screen
  
  ErrCode = 3                              'Assume an error

  '---- Determine minimum text rows available for monitor type
  SELECT CASE GPDat(31)
    CASE 5                                 'EGA
      Rows = 25
    CASE 8                                 'VGA
      Rows = 30
    CASE ELSE                              'NGA
      EXIT SUB
  END SELECT

  CALL PCXLibraryFile(OffSet&, Size&)

  Header$ = SPACE$(836)                    'Read the PCX header
  IF NOT OpenPCXFile%(LibName$, Header$) THEN
    ErrCode = 4
    Header$ = ""
    EXIT SUB
  END IF

  Pal$ = MID$(Header$, 17, 48)             'Get the palette info

  GQSForm = -1                             'Assume its a GQS form

  SELECT CASE Fld(0).Value                 'Value contains screen mode
    CASE 5                                 'EGA w/ field defs
      GOSUB Scrn9

    CASE 8                                 'VGA w/ field defs
      IF GPDat(31) = 5 THEN
        Header$ = ""                       'Can't display a VGA screen on EGA
        Pal$ = ""
        EXIT SUB
      END IF

      GOSUB Scrn12

    CASE ELSE                              'EGA or VGA w/o field defs
      GQSForm = 0                          'NOT a form

      IF GPDat(31) = 5 THEN                'Set screen mode based on monitor
        IF PreviousRows = 30 OR PreviousRows = 60 THEN PreviousRows = 25
        GOSUB Scrn9                        ' detected.
      ELSE
        IF PreviousRows = 25 OR PreviousRows = 43 THEN PreviousRows = 30
        GOSUB Scrn12
      END IF

  END SELECT

  GPDat(49) = INT((4 * (GPDat(44) / GPDat(43)) / 3) * 1000)

  GPDat(85) = NumRows
  PreviousRows = NumRows

  XMax = CVI(MID$(Header$, 9, 2))
  YMax = CVI(MID$(Header$, 11, 2))

  IF Row + Col THEN
    IF GQSForm THEN MoveFRM Fld(), Row, Col
    IF XMax < (GPDat(43) - 1) OR YMax < (GPDat(44) - 1) THEN PositionPCXVE Row, Col'Partial image
  END IF

  HideCursor
  DispPCXVE (VPage)                         'Show the PCX screen
  ShowCursor

  Header$ = ""
  Pal$ = ""

  ErrCode = 0

EXIT SUB


Scrn9:
  IF Fld(0).Indexed THEN                  'Determine number of rows for WIDTH
    NumRows = 350 \ Fld(0).Indexed
  ELSE
    NumRows = MaxInt(PreviousRows, Rows)
  END IF

  SCREEN 9                                 'Set the screen mode
  IF GPDat(85) <> NumRows THEN WIDTH , NumRows
  GPDat(43) = 640                          'Screen height in pixels
  GPDat(44) = 350                          'Screen width in pixels
  GPDat(50) = 16                           'Colors available
  GPDat(71) = 14                           'Character size is 8x14 pixels
  IF NumRows = 43 THEN GPDat(71) = 8

  GPDat(72) = &HA800                       'Video segment for saving screens

  i = 1

  FOR t = 0 TO 15                          'Set the palette
    N = 0

    A = ASC(MID$(Pal$, i, 1)) \ 64         'Red
    IF (A AND 1) = 1 THEN N = N + 32
    IF (A AND 2) = 2 THEN N = N + 4
    i = i + 1

    B = ASC(MID$(Pal$, i, 1)) \ 64         'Green
    IF (B AND 1) = 1 THEN N = N + 16
    IF (B AND 2) = 2 THEN N = N + 2
    i = i + 1

    C = ASC(MID$(Pal$, i, 1)) \ 64         'Blue
    IF (C AND 1) = 1 THEN N = N + 8
    IF (C AND 2) = 2 THEN N = N + 1
    i = i + 1

    CALL SetPaletteEGA(t, N)

  NEXT

RETURN

Scrn12:

  IF Fld(0).Indexed THEN                  'Determine number of rows for WIDTH
    NumRows = 480 \ Fld(0).Indexed
  ELSE
    NumRows = MaxInt(PreviousRows, Rows)
  END IF

  SCREEN 12                                 'Set the screen mode
  IF GPDat(85) <> NumRows THEN WIDTH , NumRows

  GPDat(43) = 640                           'Screen height in pixels
  GPDat(44) = 480                           'Screen width in pixels
  GPDat(50) = 16                            'Colors available
  GPDat(71) = 16                            'Character size is 8x16 pixels
  IF NumRows = 60 THEN GPDat(71) = 8
  GPDat(72) = &HAA00                        'Video segment for saving screens
  VPage = 0

  N = 1                                     'Set the palette
  FOR i = 0 TO 15
    SetPalTripleVGA i, ASC(MID$(Pal$, N, 1)) \ 4, ASC(MID$(Pal$, N + 1, 1)) \ 4, ASC(MID$(Pal$, N + 2, 1)) \ 4
    N = N + 3
  NEXT

RETURN

END SUB

SUB MoveFRM (Fld() AS FieldInfoG, Row, Col)

 IF (Row = Fld(0).Row) AND (Col = Fld(0).LCol) THEN EXIT SUB

 RowOS = (Fld(0).Row - Row) \ GPDat(71)
 ColOS = Fld(0).LCol - Col

 ColOSxy = ColOS * 8
 RowOSxy = RowOS * GPDat(71)

 FOR i = 1 TO UBOUND(Fld)
   IF Fld(i).FType < MouseFld THEN
     Fld(i).Row = Fld(i).Row - RowOS
     IF Fld(i).FType = NotesFld THEN Fld(i).ScratchI = Fld(i).ScratchI - RowOS
     Fld(i).LCol = Fld(i).LCol - ColOS
     Fld(i).RCol = Fld(i).RCol - ColOS
   ELSE
     Fld(i).Row = Fld(i).Row - RowOSxy
     Fld(i).ScratchI = Fld(i).ScratchI - RowOSxy
     Fld(i).LCol = Fld(i).LCol - ColOSxy
     Fld(i).RCol = Fld(i).RCol - ColOSxy
     IF Fld(i).FType = HScrollFld THEN Fld(i).RelHandle = Fld(i).RelHandle - ColOSxy
     IF Fld(i).FType = VScrollFld THEN Fld(i).RelHandle = Fld(i).RelHandle - RowOSxy
   END IF
 NEXT

 Row = Row + (Fld(0).Row - Row) MOD GPDat(71)

END SUB

