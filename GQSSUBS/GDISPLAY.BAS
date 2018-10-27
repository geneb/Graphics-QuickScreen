DEFINT A-Z
'Displays a 16 color 640x480 res .PCX image

'---- BASIC subs
DECLARE SUB MoveFRM (Fld() AS ANY, Row, Col)

'---- ASM functions
DECLARE FUNCTION MaxInt% (A, B)
DECLARE FUNCTION OpenPCXFile (FileName$, Header$)

'---- ASM subs
DECLARE SUB DispPCXVE (BYVAL VideoPage)
DECLARE SUB HideCursor ()
DECLARE SUB PositionPCXVE (BYVAL LineNo, BYVAL Column)
DECLARE SUB SetVideo ()
DECLARE SUB SetPaletteEGA (BYVAL PalReg%, BYVAL Value%)
DECLARE SUB SetPalTripleVGA (BYVAL PalReg%, BYVAL Red%, BYVAL Green%, BYVAL Blue%)
DECLARE SUB ShowCursor ()

'$INCLUDE: 'FldInfo.Bi'
'$INCLUDE: 'Common.Bi'

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

SUB ShowForm (FileName$, Fld() AS FieldInfoG, Row, Col, VPage, ErrCode) STATIC

  '---- ErrCode = 1  NOT an EGA or VGA monitor
  '---- ErrCode = 2  error in loading screen

  ErrCode = 1                              'Assume an error

  '---- Determine minimum text rows available for monitor type
  SELECT CASE GPDat(31)
    CASE 5                                 'EGA
      Rows = 25
    CASE 8                                 'VGA
      Rows = 30
    CASE ELSE                              'NGA
      EXIT SUB
  END SELECT
  
  Header$ = SPACE$(836)                    'Read the PCX header
  IF NOT OpenPCXFile%(FileName$ + ".PCX", Header$) THEN
    ErrCode = 2
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

