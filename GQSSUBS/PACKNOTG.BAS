'*** PACKNOTG.BAS ***********************************************************
'Note File Compaction Utility
'
'Copyright (c) 1991, Crescent Software, Inc.
'by Don Malin
'
'This utility can be used to compact abandoned space in "Notes" database
'files that were created using the routines contained in the RANDOMG.BAS
'module.  Portions of a file are abandoned when individual Notes records are
'shortened or lengthened after they were first saved.  While this should
'never cause any speed problems, you may want to compact a file from time to
'time inorder to conserve disk space.
'
'When run, the program will prompt for the name of a Field Definition file.
'Enter the name of the .FRM file.
'
'After prompting for the name of the Field Definition file, the program will
'then create a temporary file named "TEMP.NOT" where it will build the
'compacted version of the Notes file.  Be sure there is enough room on your
'drive as the program will not be able to complete the compaction if there
'isn't.  After transfering all the notes from the original file to the
'temporary one, the original file will be deleted and the temporary file will
'be renamed to the original file's name.
'****************************************************************************
DEFINT A-Z

'$INCLUDE: 'FldInfo.bi'

DECLARE FUNCTION NumFieldsG (FrmName$)

DECLARE SUB GetFldDefG (FrmName$, StartEl%, Fld() AS FieldInfoG, Form$())
DECLARE SUB GetRecG (RecNo&, Form$(), Fld() AS FieldInfoG)
DECLARE SUB OpenFilesG (FormName$, Form$(), Fld() AS FieldInfoG)
DECLARE SUB SaveRecG (RecNo&, Form$(), Fld() AS FieldInfoG)

NoFileMsg$ = "Form not found!"

PRINT "Note File Compaction Utility"
PRINT "Copyright (c) 1991, Crescent Software, Inc."
PRINT
PRINT "Enter the name of the Form Definition file (.FRM)."
INPUT "File Name: ", FileName$
FileName$ = UCASE$(FileName$)

IF LEN(FileName$) = 0 THEN END                  'No file given, end

'----- Find the file's path and extension
FOR N = LEN(FileName$) TO 1 STEP -1             'Look backwards through name
   C$ = MID$(FileName$, N, 1)                   'Examine a character

   IF INSTR(":\", C$) THEN                      'Is it a path character?
      EXIT FOR                                  'Yes, all done
   ELSEIF C$ = "." THEN                         'Is it the extension?
      Extn$ = MID$(FileName$, N)                'Yes, save extension
      FileName$ = LEFT$(FileName$, N - 1)       'Cut extension from name
   END IF
NEXT
Path$ = LEFT$(FileName$, N)                     'Save the path portion

IF LEN(Extn$) = 0 THEN Extn$ = ".FRM"           'Extension not given, default

NumFlds = NumberOfFields(FileName$)             'Find the number of fields
IF NumFlds = 0 THEN                             '0 means file wasn't found
   PRINT NoFileMsg$                             'Bail out.
   END
END IF
REDIM Fld(NumFlds) AS FieldInfoG                'Make a field definition array
REDIM Form$(NumFlds, 2)                         'Array for data

GetFldDefG FileName$, StartEl, Fld(), Form$()

OpenFilesG FileName$, Form$(), Fld()            'Open the main database and
                                                '  Note file (.NOT).
IF LOF(Fld(0).RelHandle) = 0 THEN               'Make sure the database was
   PRINT FileName$; ".DAT "; NoFileMsg$         '  found.
   END
END IF

REDIM Fld2(NumFlds) AS FieldInfoG               'Make a copy of the field
FOR N = 0 TO Fld(0).Fields                      '  definition array for the
   Fld2(N) = Fld(N)                             '  destination (tem.) file.
NEXT

TempName$ = Path$ + "TEMP.NOT"                  'Make the temp. file's name.

Fld2(0).ScratchI = FREEFILE                     'Get a handle
OPEN TempName$ FOR OUTPUT AS Fld2(0).ScratchI   'Truncate any existing file.
CLOSE Fld2(0).ScratchI
OPEN TempName$ FOR BINARY AS Fld2(0).ScratchI   'Open it for binary

'----- Process all records in the main database.
FOR R& = 1 TO LOF(Fld(0).RelHandle) \ Fld(0).StorLen

   GetRec R&, Form$(), Fld()                    'Read main database record
                                                '  and all notes
   FOR N = 1 TO Fld(0).Fields                   'Reset all note field's record
      IF Fld(N).FType = NotesFld THEN           '  pointers to 0 so they will
         MID$(Form$(0, 0), Fld(N).Fields) = MKL$(0)'be realocated below.
      END IF
   NEXT

   SaveRec R&, Form$(), Fld2()                  'Save main database record
NEXT                                            '  and all notes to the temp.
                                                '  file.
CLOSE

FileName$ = FileName$ + ".NOT"                  'Compose note file's name
KILL FileName$                                  'Delete the original file
NAME TempName$ AS FileName$                     'Rename the temp. file to the
                                                '  original name.


