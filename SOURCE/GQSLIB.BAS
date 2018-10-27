'****************************************************************************
' GQSLIB.BAS  file library maker
'
' by Phil Cramer
'
' Copyright (c) 1992, Crescent Software
'
' Creates a library of any combination files from a List file. The List
' file is specified on the command line with or without a path and extension.
' The List file lists each file with a complete path if necessary
' on its own line in an ASCII text file. The library will have the same name
' as the List file but with a .GSL (Graphic Screen Library) extension
'
'
' 1.01 11/2/93 Revised to clear and set read-only attribute for library
'****************************************************************************
DEFINT A-Z

'---- BASIC subs
DECLARE SUB CopyFile (FileName$, Handle%)

'---- ASM subs
DECLARE SUB FClose (Handle)
DECLARE SUB FGetT (Handle, Source AS ANY, NumBytes)
DECLARE SUB FGetA (Handle, SEG Element AS ANY, NumBytes&)
DECLARE SUB FOpen (FileName$, Handle)
DECLARE SUB FSeek (Handle, Location&)
DECLARE SUB KillFile (FileName$)
DECLARE SUB SetAttr (FileName$, Attribute)



'---- ASM functions
DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION DiskRoom& (Drive$)
DECLARE FUNCTION Exist% (FileName$)
DECLARE FUNCTION FLInput$ (Handle, Buf$)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION FLof& (Handle)
DECLARE FUNCTION FEof& (Handle)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION WhichError ()
DECLARE FUNCTION ErrorMsg$ (ErrorNum)


TYPE Header
  Name AS STRING * 12
  Size AS LONG
  OS AS LONG
END TYPE

PRINT "GQSLIB - custom library file maker"
PRINT "Copyright (c) 1992  Crescent Software"
PRINT "Version 1.01"
PRINT


LstFile$ = LTRIM$(RTRIM$(COMMAND$))
IF LEN(LstFile$) = 0 THEN
  BEEP
  PRINT "List file not specified"
  PRINT "Usage: GQSLIB [path]RspFile.LST"
  END
END IF

CALL SplitName(LstFile$, Drive$, Path$, FileName$, Ext$)

IF LEN(Ext$) AND UCASE$(Ext$) <> ".LST" THEN
  PRINT "The List file must have a .LST extension"
  BEEP
  END
END IF

LstFile$ = Drive$ + Path$ + FileName$ + ".LST"
LibFileName$ = FileName$ + ".GSL"


IF Exist(LibFileName$) THEN
  SettAttr LibFileName$, 0      'Clear Read only attribute so we can overwrite
END IF


REDIM LibFile$(500)
Buffer$ = SPACE$(100)

CALL FOpen(LstFile$, Handle)
IF DOSError THEN
  PRINT ErrorMsg$(WhichError)
  END
END IF

NumFiles = 1

DO WHILE NOT FEof(Handle)
  LibFile$(NumFiles) = FLInput$(Handle, Buffer$)
  IF LEN(LTRIM$(RTRIM$(LibFile$(NumFiles)))) THEN NumFiles = NumFiles + 1
LOOP

NumFiles = NumFiles - 1

IF NumFiles < 1 THEN
  PRINT "No files found in List file"
  END
END IF
 
CALL FClose(Handle)

REDIM NameInfo(NumFiles) AS Header
StartPointer& = NumFiles * 20 + 2

FOR i = 1 TO NumFiles
  CurScreen$ = LTRIM$(RTRIM$(LibFile$(i)))
  IF Exist(CurScreen$) THEN
    TotalSize& = TotalSize& + FileSize&(CurScreen$)
  ELSE
    BEEP
    PRINT "Cannot find "; CurScreen$
    END
  END IF
NEXT

Drive$ = CHR$(GetDrive%)
IF TotalSize& > DiskRoom&(Drive$) THEN
  BEEP
  PRINT "Insufficient disk space available for library"
  END
END IF

FOR i = 1 TO NumFiles
  CurScreen$ = LTRIM$(RTRIM$(LibFile$(i)))

  IF Exist(CurScreen$) THEN
    CALL SplitName(CurScreen$, Drive$, Path$, FileName$, Ext$)
    NameInfo(i).Name = UCASE$(FileName$) + UCASE$(Ext$)
    NameInfo(i).Size = FileSize&(CurScreen$)
    NameInfo(i).OS = StartPointer&
    StartPointer& = StartPointer& + NameInfo(i).Size
  ELSE
    PRINT "Cannot find "; CurScreen$
    BEEP
    END
  END IF
NEXT


'---- Create the library file
CALL FCreate(LibFileName$)
CALL FOpen(LibFileName$, Handle)
CALL FPutT(Handle, NumFiles, 2)
CALL FPutA(Handle, SEG NameInfo(1), NumFiles * 20)

IF DOSError THEN
  PRINT ErrorMsg$(WhichError)
  BEEP
  CALL FClose(Handle)
  END
END IF

FOR i = 1 TO NumFiles
  CALL CopyFile(LibFile$(i), Handle)
NEXT

CALL FClose(Handle)

SetAttr LibFileName$, 1         'Make Library read-only (for networks)

PRINT LibFileName$; " created"

SUB CopyFile (FileName$, LibHandle)

  CALL FOpen(FileName$, Handle)
  IF DOSError THEN
    PRINT ErrorMsg$(WhichError)
    BEEP
    CALL FClose(Handle)
    END
  END IF
  
  Size& = FLof(Handle)

  IF Size& < 4096 THEN
    BufSize = Size&
  ELSE
    BufSize = 4096
  END IF

  Buffer$ = SPACE$(BufSize)

  DO
    CALL FGet(Handle, Buffer$)
    CALL FPut(LibHandle, Buffer$)
    IF DOSError THEN
      PRINT ErrorMsg$(WhichError)
      CALL FClose(Handle)
      CALL KillFile(FileName$)
      END
    END IF

    Size& = Size& - BufSize

    IF Size& < BufSize THEN
      BufSize = Size&
      Buffer$ = SPACE$(BufSize)
    END IF

  LOOP UNTIL LEN(Buffer$) = 0

  CALL FClose(Handle)

END SUB

