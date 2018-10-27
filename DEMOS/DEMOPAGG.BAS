'****************************************************************************
'DEMOPAGG - sample multi-page demo

'Copyright (c) 1992 Crescent Software, Inc.
'Written by Don Malin and Phil Cramer

'Illustrates using "EditFormG" to allow data entry in a multi page form.

'Overview:
'  Loads screens from a .GSL library.
'  Loads and combines the form definitions from each page into the field
'    information arrays.
'  Displays the first page
'  Polls the editing sub program and traps events that cause a page change.
'****************************************************************************

DEFINT A-Z

'$INCLUDE: 'FldInfo.bi'                 'Define the Fld TYPE
'$INCLUDE: 'EditForm.BI'                'Define the Frm TYPE

'----- ASM Functions
DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION WhichError% ()
DECLARE FUNCTION ErrorMsg$ (Er)
DECLARE FUNCTION MultMonitor%

'----- ASM Subs
DECLARE SUB DeInstallKeyDown ()
DECLARE SUB InitMouse (There)
DECLARE SUB InstallKeyDown ()
DECLARE SUB ShowCursor ()
DECLARE SUB HideCursor ()
DECLARE SUB Motion (Sensitivity)

'----- BASIC Subs
DECLARE SUB PrintArray (FirstFld%, LastFld%, Form$(), Fld() AS FieldInfoG)
DECLARE SUB UnPackBuffer (FirstFld%, LastFld%, Form$(), Fld() AS ANY)
DECLARE SUB EditFormG (Form$(), Fld() AS FieldInfoG, Frm AS FormInfoG, Action)
DECLARE SUB LibGetFldDefG (LibName$, FormName$, StartEl, Fld() AS ANY, Form$(), ErrCode)
DECLARE SUB LibShowForm (LibName$, FormName$, Fld() AS FieldInfoG, Row, Col, VPage, ErrCode)
DECLARE SUB Wipes (WipeType%)

'----- BASIC Functions
DECLARE FUNCTION LibNumFieldsG (LibName$, FormName$)

'$INCLUDE: 'SetUp.BAS'                  'Test for monitor type and set
                                        ' default GPDat() values

'----- The following arrays are mandatory
DIM Frm AS FormInfoG                    'TYPE used for editing information.
                                        '  See "FormEdit.bi" for more info.

REDIM Fld(0) AS FieldInfoG              'Holds field information.  See
                                        '  "FieldInf.Bi" for more information
                                        '  REDIMed later

REDIM Form$(0, 0)                       'Holds form data, formulas and help
                                        '  messages.  REDIMed later

REDIM Choice$(0, 0)                     'Holds Multiple choice selections



DIM FormName$(2)                        'Create an array to hold form names
FormName$(1) = "GInvpag1"               'Assign the form names
FormName$(2) = "GInvpag2"               '"      "   "    "
LibName$ = "DemoPagg"

'----- Find the number of fields from both pages (Forms)
NumFlds = -1
FOR Scr = 1 TO 2                        'Get form name from screen number
                                        'Add number of fields to total
   NumFlds = NumFlds + LibNumFieldsG(LibName$, FormName$(Scr) + ".FRM") + 1
   IF NumFlds < 0 THEN                  'Negative value indicates an error
     PRINT "Error reading library file"
     END
   END IF
NEXT



REDIM Fld(NumFlds) AS FieldInfoG        'DIM the field information array to
                                        '  combined size of both forms

'---- Create the array used for form data, help messages and formulas
'     Form$(0, 0) holds all data from fields as a contiguous string with
'                 numbers stored as IEEE formatted strings.  (Data Buffer)
'     Form$(FieldNum, 0) holds data (formatted numeric string for numbers)
'     Form$(FieldNum, 1) holds help message string
'     Form$(FieldNum, 2) holds formulas for calculated fields
REDIM Form$(NumFlds, 2)                 'DIM the form data array


'----- Load the form definitions for both screens
StartEl = 0                             'Load first form to beginning of array
FOR Scr = 1 TO 2                        'Get form name from screen number

  LibGetFldDefG LibName$, FormName$(Scr) + ".FRM", StartEl, Fld(), Form$(), ErrCode

  SELECT CASE ErrCode                     'Check for errors
    CASE 1
      PRINT FormName$(1) + ".FRM"; " not found in library"
      END
    CASE 2
      PRINT "Cannot open"; LibName$; ".GSL"
      END
  END SELECT
                                        'Bump "StartEl" to next available
  StartEl = StartEl + Fld(StartEl).Fields + 1

NEXT

'----- Presets data in fields for this demo
GOSUB InvoiceSetup

'---- Display the form
LibShowForm LibName$, FormName$(1) + ".PCX", Fld(), Fld(0).Row, Fld(0).LCol, 1, ErrCode

SELECT CASE ErrCode                     'Check for errors
  CASE 1
    PRINT "Cannot open "; FormName$(1); ".PCX"
    END
  CASE 2
    PRINT FormName$(1) + ".PCX"; " not found in library"
    END
  CASE 3
    PRINT "Sorry, this form will not run on this monitor."
    END
  CASE 4
    PRINT "An error occurred trying to load " + FormName$(1)
    END
END SELECT

CALL Wipes(20)



InstallKeyDown                          'Install the KeyDown TSR
InitMouse There                         'Initialize the mouse
ShowCursor                              'Turn on the mouse cursor
Motion 5                                'Set the mouse sensitivity


'----- Setup TYPE for setting and reading form editing information.
Frm.FldNo = 1                           'Start editing on field #1
Frm.InsStat = 0                         'Set the insert state (-1 = Insert on)
Frm.StartEl = 0                         'Set form starting element to 0 and
LastStartEl = Frm.StartEl               '  save value for later comparisons
'----- See "FormEdit.bi" for other elements.

FrmName$ = "Invoice.DBF"                'Assign the record data file name

'****************************************************************************
'Open the database file and field "Form$(0, 0)" (the whole record buffer) to
'the length specified by "Fld(0).StorLen".  Fld(0).Storlen shows the record length
'for both forms combined.
'----------------------------------------------------------------------------
OPEN FrmName$ FOR RANDOM AS #1 LEN = Fld(0).StorLen
FIELD #1, Fld(0).StorLen AS Form$(0, 0)
'****************************************************************************


'----- Set the "Action" flag to force the editor to initialize itself and
'      display the data on the form.
Action = 1
Scr = 1

DO                                      'Poll the editing procedure
  EditFormG Form$(), Fld(), Frm, Action


  '----- If the user pressed PgUp or PgDn or moved off the top or bottom of
  '----- the form, "StartEl" will be updated by "EditForm" so we need to
  '----- check it.
  IF Frm.StartEl <> LastStartEl THEN
    IF Frm.StartEl < LastStartEl THEN         'Previous page?
       Scr = Scr - 1                          'Yes set previous page number
       WipeType = 16
    ELSEIF Frm.StartEl > LastStartEl THEN     'Next page?
       Scr = Scr + 1                          'Yes set next page number
       WipeType = 17
    END IF                                    'Display the screen

    LibShowForm LibName$, FormName$(Scr) + ".PCX", Fld(), Fld(0).Row, Fld(0).LCol, 1, ErrCode

    HideCursor
    CALL Wipes(WipeType)
    ShowCursor

    LastStartEl = Frm.StartEl                 'Save the new "StartEl"
    Action = -1                               'Action = -1 redisplays the

  END IF                                      ' form but does not "restore"
                                              ' the previous push button
  '*************************************************************************
  'Check for "Save"-F2 and Retrieve-F3 keys
  '-------------------------------------------------------------------------
  IF Frm.KeyCode = -60 THEN                   'F2 = Save
    PUT #1, 1                                 'Save both pages of the form
    FOR N = 1 TO NumFlds                      'For all fields on both pages
      LSET Form$(N, 0) = ""                   'Clear each field
    NEXT
    Action = 1                                'Set to 1 so form is redisplayed
  
  ELSEIF Frm.KeyCode = -61 THEN               'F3 = Retrieve
    IF LOF(1) THEN
      GET #1, 1                               'Retrieve record #1
      UnPackBuffer 0, 0, Form$(), Fld()       'Unpack Form$(0, 0) into each
                                              'field on the form
      Action = 1                              'Set to 1 so form is redisplayed
    END IF
  END IF
   '*************************************************************************
                                        'Keep editing until the user presses
LOOP UNTIL Frm.KeyCode = 27             '  the Escape key.
                                        
DeInstallKeyDown                        'UnInstall the KeyDown TSR

HideCursor                              'Turn the mouse cursor Off
END


'****************************************************************************
'*** The following subroutine does setup specificaly for the Invoice form ***
'****************************************************************************

InvoiceSetup:
  '----- Here's how you set up multiple choice fields
  REDIM Choice$(7, 1)                 '7 choices in 2 menus (menu 0 is used)

  Choice$(0, 0) = "15"                'these choices go with field 15
  Choice$(1, 0) = "CASH  "
  Choice$(2, 0) = "COD   "
  Choice$(3, 0) = "CREDIT CARD"
  Choice$(4, 0) = "NET-10"
  Choice$(5, 0) = "NET-30"

  Choice$(0, 1) = "16"                'these choices go with field 16
  Choice$(1, 1) = "Don Malin"
  Choice$(2, 1) = "Ethan Winer"
  Choice$(3, 1) = "Nash Bly"
  Choice$(4, 1) = "Paul Passarelli"
  Choice$(5, 1) = "Jay Munro"
  Choice$(6, 1) = "Brian Giedt"
  Choice$(7, 1) = "John Waldman"


  '----- Preset some fields data
  Form$(13, 0) = DATE$                'Invoice date field
  Form$(94, 0) = "8.0"                'Tax rate field

  '----- The following statements create a single long string which will be
  '      formatted into the notes section of the form.  Single strings like
  '      this can be saved as notes fields (variable length fields) in
  '      sequential, "DBase" (db/LIB), "Btrieve" or "ISAM" files.  Note the
  '      CHR$(20) paragraph markers used to specify the end of paragraphs
  '      and blank lines (they will not be displayed on the form).
  '
  Form$(98, 0) = "This is a test of the Notes field type.  We are pre-setting "
  Form$(98, 0) = Form$(98, 0) + "some text in here to show that it will be un-wrapped "
  Form$(98, 0) = Form$(98, 0) + "and displayed properly when the form is first entered."
  Form$(98, 0) = Form$(98, 0) + CHR$(20) + CHR$(20) + "This is the start of another "
  Form$(98, 0) = Form$(98, 0) + "paragraph which should be on the second line after "
  Form$(98, 0) = Form$(98, 0) + "the above paragraph." + CHR$(20)

RETURN



