'WINIMAGE.BAS
'
'Written by Phil Cramer 5/93
'
'Loads a Microsoft Windows format .BMP, .ICO, or .CUR image (64k max) into
'an array (or arrays for Icon and Cursor files) that can be displayed with
'BASIC's graphic PUT command.

DEFINT A-Z

DECLARE SUB LoadWindowsImage (FileName$, XORArray%(), ANDArray%(), ErrCode%)

DECLARE SUB BCopy (FromSeg, FromAddress, ToSeg, ToAddress, NumBytes, Dr)

DECLARE FUNCTION DOSError ()
DECLARE FUNCTION WhichError ()
DECLARE FUNCTION QInstrB (Start, Text$, X$)

TYPE IconDir
  Reserved        AS INTEGER
  IDType          AS INTEGER
  IconCount       AS INTEGER
END TYPE

TYPE IconInfo
  IconWidth       AS STRING * 1
  IconHeight      AS STRING * 1
  ColorCnt        AS STRING * 1
  Reserved1       AS STRING * 1
  Planes          AS INTEGER
  BitCount        AS INTEGER
  BytesInRes      AS LONG
  ImageOffSet     AS LONG
END TYPE

TYPE BitMapDir
  bfType          AS INTEGER
  bfSize          AS LONG
  bfReserved1     AS INTEGER
  bfReserved2     AS INTEGER
  bfOffBits       AS LONG
END TYPE

TYPE BitMapInfo
  biSize          AS LONG
  biWidth         AS LONG
  biHeight        AS LONG
  biPlanes        AS INTEGER
  biBitCount      AS INTEGER
  biCompression   AS LONG
  biSizeImage     AS LONG
  biXPelsPerMeter AS LONG
  biYPelsPerMeter AS LONG
  biClrUsed       AS LONG
  biClrImportant  AS LONG
END TYPE

SUB LoadWindowsImage (FileName$, XORArray(), ANDArray(), ErrCode)

  DIM BitMapInfo AS BitMapInfo
  ErrCode = 0

  CALL FOpen(FileName$, Handle)
  IF DOSError THEN
    ErrCode = WhichError
    EXIT SUB
  END IF

  Ext$ = UCASE$(MID$(FileName$, QInstrB(-1, FileName$, ".")))

  SELECT CASE Ext$
    CASE ".ICO", ".CUR"
      DIM IconDir AS IconDir
      DIM IconInfo AS IconInfo
      CALL FGetT(Handle, IconDir, LEN(IconDir))
      CALL FGetT(Handle, IconInfo, LEN(IconInfo))
      CALL FGetT(Handle, BitMapInfo, LEN(BitMapInfo))
      IHeight = ASC(IconInfo.IconHeight)
      IWidth = ASC(IconInfo.IconWidth)
      ByteWidth& = IWidth \ 8
      Arrays = 1

    CASE ".BMP"
      DIM BitMapDir AS BitMapDir
      CALL FGetT(Handle, BitMapDir, LEN(BitMapDir))
      CALL FGetT(Handle, BitMapInfo, LEN(BitMapInfo))
      IHeight = BitMapInfo.biHeight
      IWidth = BitMapInfo.biWidth
      BytesPerLine = (BitMapDir.bfSize - BitMapDir.bfOffBits) \ BitMapInfo.biHeight
      ByteWidth& = BytesPerLine \ BitMapInfo.biBitCount

    CASE ELSE
      ErrCode = -1                             'Not a valid file name

  END SELECT

  SELECT CASE BitMapInfo.biBitCount
    CASE 1
      NumColors = 2
    CASE 4
      NumColors = 16
    CASE ELSE
     ErrCode = -2
     EXIT SUB
  END SELECT

  Clr$ = SPACE$(NumColors * 4)
  CALL FGet(Handle, Clr$)
  Clr$ = ""

  BytesPerPlane& = ByteWidth& * IHeight

  '---- Calc bytes required to hold XOR and AND images
  ImageEls& = (BytesPerPlane& * 4) \ 2

  IF ImageEls& >= 32000 THEN
    ErrCode = -3
    EXIT SUB
  END IF
  
  REDIM XORArray(1 + ImageEls&)
  XORArray(0) = IWidth
  XORArray(1) = IHeight

  IF Arrays THEN
    REDIM ANDArray(1 + ImageEls&)
    ANDArray(0) = IWidth
    ANDArray(1) = IHeight
  END IF

  Clr = 0
  Buf$ = STRING$(ByteWidth& * 4, 0)
  ScanLine$ = STRING$(ByteWidth& * BitMapInfo.biBitCount, 0)

  PalConvert$ = CHR$(0) + CHR$(4) + CHR$(2) + CHR$(6) + CHR$(1) + CHR$(5) + CHR$(3) + CHR$(8) + CHR$(7) + CHR$(12) + CHR$(10) + CHR$(14) + CHR$(9) + CHR$(13) + CHR$(11) + CHR$(15)

  '---- Read the Icon into a Basic PUT array
  FOR Image = BitMapInfo.biBitCount TO BitMapInfo.biBitCount + Arrays

    El = (ImageEls& + 1) - (LEN(Buf$) \ 2) + 1
    Bit = 128
    Char = 1

    FOR Row = 1 TO IHeight

      CALL FGet(Handle, ScanLine$)
      IF DOSError THEN
        ErrCode = WhichError
        EXIT SUB
      END IF


      FOR i = 1 TO LEN(ScanLine$)

        Byte = ASC(MID$(ScanLine$, i, 1))

        IF Image = 4 THEN

          Clr = ASC(MID$(PalConvert$, (Byte \ 16) + 1, 1))   'Upper byte
          IF Clr AND 8 THEN Intensity = Intensity OR Bit
          IF Clr AND 4 THEN Red = Red OR Bit
          IF Clr AND 2 THEN Green = Green OR Bit
          IF Clr AND 1 THEN Blue = Blue OR Bit
          Bit = Bit \ 2

          Clr = ASC(MID$(PalConvert$, (Byte AND 15) + 1, 1)) 'Lower byte
          IF Clr AND 8 THEN Intensity = Intensity OR Bit
          IF Clr AND 4 THEN Red = Red OR Bit
          IF Clr AND 2 THEN Green = Green OR Bit
          IF Clr AND 1 THEN Blue = Blue OR Bit
          Bit = Bit \ 2

        ELSE

          FOR N = 1 TO 8
            IF Byte AND Bit THEN
              Blue = Blue OR Bit
              Green = Green OR Bit
              Red = Red OR Bit
              Intensity = Intensity OR Bit
            END IF
            Bit = Bit \ 2
          NEXT

        END IF

        IF Bit < 1 THEN
          Bit = 128
          MID$(Buf$, Char, 1) = CHR$(Blue)
          MID$(Buf$, Char + ByteWidth&, 1) = CHR$(Green)
          MID$(Buf$, Char + 2 * ByteWidth&, 1) = CHR$(Red)
          MID$(Buf$, Char + 3 * ByteWidth&, 1) = CHR$(Intensity)

          Char = Char + 1
          IF Char > ByteWidth& THEN

            '---- Write bytes to the array
            IF Image = 4 OR Arrays = 0 THEN
              BCopy SSEG(Buf$), SADD(Buf$), VARSEG(XORArray(El)), VARPTR(XORArray(El)), LEN(Buf$), 0
            ELSE
              BCopy SSEG(Buf$), SADD(Buf$), VARSEG(ANDArray(El)), VARPTR(ANDArray(El)), LEN(Buf$), 0
            END IF
            El = El - LEN(Buf$) \ 2

            Buf$ = STRING$(ByteWidth& * 4, 0)
            Char = 1

          END IF

          Blue = 0
          Green = 0
          Red = 0
          Intensity = 0

        END IF

      NEXT

    NEXT

    ScanLine$ = STRING$(ByteWidth&, 0)
    Start = i
    Finish = LEN(Icon$)

  NEXT

  CALL FClose(Handle)

END SUB

