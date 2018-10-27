'****************************************************************************
'DEMODBLG.BAS - sample data entry form
'
'Illustrates using "EditFormG" with AJS Publishing's db/LIB data base
'management software tools.
'
' Copyright (c) 1992 Crescent Software, Inc.
'
' by Donald R. Malin and Phil Cramer
'
'NOTE: You must make a combined Quick Library from both "GForms.Lib" and
'      "DBLib.Lib" inorder to run this demo. Use the "MakeQLB" program to
'      make the smallest possible Quick Library.
'
'      Module - "ListBox.bas" is required for a Multiple Choice field in this
'      demo. If your form doesn't require this feature, load the following
'      "NO" file:
'      "NoMultG.Bas" - Replaces "ListBox.Bas" when not using Multiple Choice
'                      Fields.
'
'      As supplied, this demo uses the "NoNotesG.Bas" module to replace
'      "GQEditS.Bas". Replace this file with "GQEditS.BAS" to see how to
'      implement "Notes" fields (multi-line text fields).
'
'      The "Employee" form does not contain any calculated fields so the
'      "NoCalc.Bas" module has been loaded.  If you plan on using calculated
'      fields in your form, you should instead load the "CACalc.Bas" and
'      "Evaluate.Bas" modules.
'****************************************************************************
DEFINT A-Z

'$INCLUDE: 'FldInfo.bi'        'field information
'$INCLUDE: 'EditForm.bi'       'form editing information

'----- ASM Subs
DECLARE SUB DeInstallKeyDown ()
DECLARE SUB HideCursor ()
DECLARE SUB InitMouse (There)
DECLARE SUB InstallKeyDown ()
DECLARE SUB Motion (Sensitivity)
DECLARE SUB ShowCursor ()

'---- ASM functions
DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION ErrorMsg$ (Er)
DECLARE FUNCTION Exist% (FileSpec$)
DECLARE FUNCTION MultMonitor%
DECLARE FUNCTION WhichError% ()

'----- db/LIB subs
DECLARE SUB AddKEY (NdxFile, Status, KeyDat$, Record#)
DECLARE SUB CloseDBF (DatFile, Status, Mode)
DECLARE SUB CloseDBT (NdxFile, Status, Mode)
DECLARE SUB CloseNDX (NdxFile, Status)
DECLARE SUB CommitSTR (DatFile, Status)
DECLARE SUB DeleteKEY (NdxFile, Status, KeyDat$, Record#)
DECLARE SUB GetKEY (DatFile, Status, KeyDat$, Record#, Mode)
DECLARE SUB GetREC (DatFile, Status, Record#, RecData$)
DECLARE SUB OpenDBF (DatFile, Status, dbFName$, dbType, Mode)
DECLARE SUB OpenDBT (DbtFile, Status, FileName$, Mode)
DECLARE SUB OpenNDX (NdxFile, Status, NdxName$, NdxType, NdxMode, KeyExp$, KeyLen, KeyType, Mode)
DECLARE SUB PutREC (DatFile, Status, Record#, RecData$)
DECLARE SUB StatusDBF (DatFile, Status, DbfType, Flags, TreCords#, TFields, TLength, Update$)
        
'----- BASIC Functions and Subs
DECLARE FUNCTION EndOfForms (Fld() AS ANY)
DECLARE FUNCTION NumFieldsG (FormName$)

DECLARE SUB CloseDatFile (DatFile%, Fld() AS ANY, Status%)
DECLARE SUB db2FormG (First%, Last%, DatFile%, RecData$, Fld() AS FieldInfoG, Form$(), Status%)
DECLARE SUB dbDefineRecG (First%, Last%, DatFile%, Fld() AS FieldInfoG, Status%)
DECLARE SUB EditFormG (Form$(), Fld() AS FieldInfoG, Frm AS FormInfoG, Action)
DECLARE SUB Form2dbG (First%, Last%, DatFile%, RecData$, Fld() AS ANY, Form$(), Status%)
DECLARE SUB GetFLD (DatFile, Status, FldNum, FldName$, FldData$, RecData$)
DECLARE SUB GetFldDefG (FormName$, StartEl, Fld() AS ANY, Form$())
DECLARE SUB Opendb (FormName$, Fld() AS ANY, DatFile%, RecData$)
DECLARE SUB Put2db (DatFile%, Record#, RecData$, Fld() AS ANY, Form$(), Status%)
DECLARE SUB ShowForm (FormName$, Fld() AS FieldInfoG, Row, Col, VPage, ErrCode)


'$INCLUDE: 'SetUp.BAS'                  'Detect monitor type and set default
                                        '  GPDat() values

'----- The following arrays are mandatory
DIM Frm AS FormInfoG                    'TYPE used for editing information.
                                        '  See "FormEdit.bi" for more info.

REDIM Fld(0) AS FieldInfoG              'Holds field information.  See
                                        '  "FldInfo.Bi" for more information
                                        '  REDIMed later in "GetForm" subroutine

REDIM Form$(0, 0)                       'Holds form data, formulas and help
                                        '  messages.  REDIMed later in
                                        '  "GetForm" subroutine

REDIM Choice$(0, 0)                     'Holds Multiple choice selections
                                        '  must dim, even if not using choices


CLS                                     'Clear the screen

'----- Load the field definition file and display the screen
FormName$ = "EMPLOYEV"                  'Specify the screen to use
GOSUB GetForm                           'Go load it
IF ErrorCode THEN END                   'If there was an error, end program

GOSUB EmployeeSetup                     'Do setup for the Employee form

'----- Display the screen
ShowForm FormName$, Fld(), 0, 0, VPage, ErrorCode

SELECT CASE ErrorCode                   'Check for errors displaying screen
   CASE 1
      PRINT "Sorry, this form requires a VGA monitor"
   CASE 2
      PRINT "An error occurred trying to load EMPLOYEV."
   CASE ELSE
END SELECT

IF ErrorCode THEN END

'----- Open the data file and all related index and notes files.
Opendb FormName$, Fld(), DatFile, RecData$
                                        
InitMouse (There)
ShowCursor                              'Turn the mouse cursor on
InstallKeyDown

'----- Set up "Frm" structure variables
Frm.FldNo = 1                           'Start editing field #1
Frm.InsStat = 0                         'Set the insert state (-1 = Insert on)

'----- Set the "Action" flag to force the editor to initialize itself and
'      display the data on the form.
Action = 1

Record# = 0                             'Record# set to 0 will cause the new
                                        '  form to be saved to the end of the
                                        '  file

'----- Poll the editor in a loop.
DO
   EditFormG Form$(), Fld(), Frm, Action

   '----- Check for hot keys
   SELECT CASE Frm.KeyCode

      '----- [F2] - Save the form
      CASE -60
         Put2db DatFile, Record#, RecData$, Fld(), Form$(), Status
         GOSUB NewForm                  'Blank the form

      '----- [F3], [F4] - Get Prev., Next record
      CASE -61, -62
         IF Frm.Edited THEN             'Save the form if it was edited
            Put2db DatFile, Record#, RecData$, Fld(), Form$(), Status
         END IF
                            
         IF Frm.KeyCode = -61 THEN      'If key was [F3],
            Mode = -1                   'Set Mode to -1 (get previous)
         ELSE
            Mode = 1                    'Set Mode to -1 (get next)
         END IF
                                        'Force -2 or 2 if on new form.  Finds
                                        '  first or last record.
         IF Record# = 0 THEN Mode = Mode * -2
                                        'Find the record to retrieve
         GetKEY Fld(1).Indexed, Status, KeyDat$, Record#, Mode
                                        'Did we step off the end of the file?
        
        
         IF Status = 123 THEN
            GOSUB NewForm               'Yes, display a new form
            BEEP
            Status = 0
         ELSE                           'No, get the record
            GetREC DatFile, Status, Record#, RecData$
                                        'Convert the data for the form

           
            db2FormG 1, EndOfForms%(Fld()), DatFile, RecData$, Fld(), Form$(), Status
            Action = 1
        
         END IF

      CASE ELSE
   END SELECT

LOOP UNTIL Frm.KeyCode = 27 OR Status  'Keep editing until the user presses
                                        '  the Escape key.

CloseDatFile DatFile, Fld(), Status     'Close all the files

HideCursor                              'Turn the mouse cursor off
DeInstallKeyDown
END



'****************************************************************************
'*** This subroutine loads and displays the screen file specified in
'*** "FormName$".
'****************************************************************************

GetForm:

    NumFlds = NumFieldsG(FormName$) 'Get the number of fields in form
    IF NumFlds = 0 THEN                 'No fields means file isn't there
       PRINT "Can't find " + FormName$ + ".FRM"
       ErrorCode = -1
       RETURN
    END IF

    REDIM Fld(NumFlds) AS FieldInfoG     'DIM the field information array

    '---- Create the array used for form data, help messages and formulas
    '     Form$(0, 0) holds all data from fields as a contiguous string with
    '                 numbers stored as IEEE formatted strings.
    '     Form$(FieldNum, 0) holds data (formatted numeric string for numbers)
    '     Form$(FieldNum, 1) holds help message string
    '     Form$(FieldNum, 2) holds formulas for calculated fields
    REDIM Form$(NumFlds, 2)             'DIM the form data array

    GetFldDefG FormName$, 0, Fld(), Form$() 'Load the field information
                                        
   
    IF DOSError THEN ErrorCode = -1     'If there was an error, bail out

RETURN



'****************************************************************************
'*** Use this subroutine to clear the contents of fields in order to present
'*** a blank form for editing.
'****************************************************************************
NewForm:
    FOR N = 1 TO Fld(0).fields          'Clear only the field's data, not
        Form$(N, 0) = ""                '  help messages or formulas.
    NEXT
    Action = 1                          'Set Action to initialize the form
    Frm.FldNo = 1
    Record# = 0                         'Set Record to force new data to be
RETURN                                  '  appended to data file.



'****************************************************************************
'*** The following subroutine does setup specificaly for the Employee form **
'****************************************************************************

EmployeeSetup:
    '----- Here's how you set up multiple choice fields
    REDIM Choice$(4, 0)                 '4 choices in 1 menu (menu 0 is used)

    Choice$(0, 0) = "15"                'these choices go with field 15
    Choice$(1, 0) = "Hour"
    Choice$(2, 0) = "Week"
    Choice$(3, 0) = "Month"
    Choice$(4, 0) = "Year"

RETURN

'****************************************************************************
'Closes a dta base file and all associated files such as index and notes
'files.
'****************************************************************************
SUB CloseDatFile (DatFile, Fld() AS FieldInfoG, Status)

    Mode = 0                                    'Use normal close
    CloseDBF DatFile, Status, Mode

    FOR N = 1 TO EndOfForms%(Fld())             'Check each field
        IF Fld(N).Indexed THEN                  'Is this an indexed field?
           CloseNDX Fld(N).Indexed, Status      'Yes, close the NDX file
        END IF

        IF Fld(N).FType = NotesFld THEN         'Is it a Notes field?
           Mode = 0                             'Yes, use normal close on DBT
           CloseDBT Fld(N).RelHandle, Status, Mode
        END IF
    NEXT

END SUB

'****************************************************************************
'Opens a DBF file and all associated files such as NDX and DBT files.
'If the files do not already exist, the routine will create them and define
'the record structure in the DBF file header.
'****************************************************************************
SUB Opendb (FormName$, Fld() AS FieldInfoG, DatFile, RecData$)

    dbFName$ = FormName$ + ".DBF"               'Create the file name
    dbType = 3                                  'Make a DBase III file type

    IF NOT Exist(dbFName$) THEN                 'If the file isn't there,
       Mode = 1                                 '  create it.

       OpenDBF DatFile, Status, dbFName$, dbType, Mode

       IF Status THEN                           'If there was a problem
          Mode = 1                              '  opening it, wipe it out
          CloseDBF DatFile, TempStat, Mode
          KILL dbFName$
          EXIT SUB                              'Bail out
       ELSE
          First = 1                             'Set the first and last fields
          Last = EndOfForms%(Fld())             '  to define.
                                                'Define the record structure
          dbDefineRecG First, Last, DatFile, Fld(), Status
          CommitSTR DatFile, Status
          Mode = 0
          CloseDBF DatFile, Status, Mode        'Save structure to header by
       END IF                                   '  closing the file
    END IF

    Mode = 0                                    'Use normal mode to open file
    OpenDBF DatFile, Status, dbFName$, dbType, Mode
                                                'Get the record length
    StatusDBF DatFile, Status, DbfType, Flags, TreCords#, TFields, TLength, Update$
    RecData$ = SPACE$(TLength)                  'Make a record buffer string

    FOR N = 1 TO EndOfForms%(Fld())             'Examine all fields for
                                                '  associated files
                                                'Is this an indexed field?
        IF (Fld(N).FType < MouseFld) AND (Fld(N).Indexed) THEN
           FileName$ = Fld(N).FldName + ".NDX"  'Make the index file name
         
           IF NOT Exist(FileName$) THEN         'If it isn't there, create it
              Mode = 5                          'Use create mode for open
              NdxType = 3                       'Set DBase III file type
              NdxMode = 0                       'Allow duplicate keys
              KeyExp$ = Fld(N).FldName          'Use field name as expression
                                                'Allow 20 characters maximum
              KeyLen = Fld(N).RCol - Fld(N).LCol + 1
              IF KeyLen > 20 THEN KeyLen = 20
              KeyType = 0                       'Use string stype only
           ELSE                                 'File already exists,
              Mode = 4                          'Use normal open mode
           END IF
                                                'Open the index file
           OpenNDX NdxFile, Status, FileName$, NdxType, NdxMode, KeyExp$, KeyLen, KeyType, Mode
           Fld(N).Indexed = NdxFile             'Save the handle in the "Fld"
        END IF                                  '  structure

        IF Fld(N).FType = NotesFld THEN         'Is this a "Notes" field?
           FileName$ = FormName$ + ".DBT"       'Make the DBT file name

           IF NOT Exist(FileName$) THEN         'If it isn't there, create it
              Mode = 5                          'Use Create mode
           ELSE                                 'Otherwise
              Mode = 4                          'Use normal open mode
           END IF
                                                'Open the file
           OpenDBT DbtFile, Status, FileName$, Mode
           Fld(N).RelHandle = DbtFile           'Save the handle in the "Fld"
        END IF                                  '  structure.
    NEXT

END SUB

'****************************************************************************
'Saves a record from the Form$() array to the DBF file and updates all
'associated indexes.  DBT (Notes) files are also saved.
'****************************************************************************
SUB Put2db (DatFile, Record#, RecData$, Fld() AS FieldInfoG, Form$(), Status) STATIC

    LastFld = EndOfForms%(Fld())
    IF Record# <> 0 THEN                        'If this isn't a new record,
       FOR N = 1 TO LastFld                     '  examine all fields to see
                                                '  if there are any indexes.
           IF (Fld(N).FType < MouseFld) AND (Fld(N).Indexed) THEN
              FldNo = 0                         'Get the fields previous data
              GetFLD DatFile, Status, FldNo, Fld(N).FldName, FldData$, RecData$

              KeyDat$ = LEFT$(FldData$, 20)     'Delete the key from index
              DeleteKEY Fld(N).Indexed, Status, KeyDat$, Record#
           END IF
       NEXT
    END IF
                                                'Convert & copy all fields to
                                                '  record buffer "RecData$"
    Form2dbG 1, LastFld, DatFile, RecData$, Fld(), Form$(), Status
    PutREC DatFile, Status, Record#, RecData$   'Save the record

    FOR N = 1 TO LastFld                        'Check all fields for Indexes
        IF (Fld(N).FType < MouseFld) AND (Fld(N).Indexed > 0) THEN
           FldNo = 0                            'Get the fields contents
           GetFLD DatFile, Status, FldNo, Fld(N).FldName, FldData$, RecData$
       
           KeyDat$ = LEFT$(FldData$, 20)        'Make the key$ and add it
           AddKEY Fld(N).Indexed, Status, KeyDat$, Record#
       
        END IF
    NEXT

END SUB

