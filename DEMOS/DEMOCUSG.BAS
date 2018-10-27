'****************************************************************************
'DEMOCUSG.BAS - sample data entry form

'Copyright (c) 1992 Crescent Software, Inc.
'by Phil Cramer & Don Malin

'Illustrates using "EditFormG" to allow data entry in a form.  Also
'demonstrates random access file techniques.  See also the "InputBoxG$" and
'"MsgBoxG%" functions that demonstrate popping a form on top of an existing
'form to get additional information from or display information to the user.
'****************************************************************************

DEFINT A-Z

'$INCLUDE: 'FldInfo.bi'                'Define the Fld TYPE
'$INCLUDE: 'EditForm.BI'               'Define the Frm TYPE

'---- ASM subs
DECLARE SUB BCopy (FromSeg%, FromAddr%, ToSeg%, ToAddr%, NumBytes%, Direction%)
DECLARE SUB DeInstallKeyDown ()
DECLARE SUB HideCursor ()
DECLARE SUB InitMouse (There)
DECLARE SUB InstallKeyDown ()
DECLARE SUB Motion (Sensitivity)
DECLARE SUB ShowCursor ()
DECLARE SUB Split ()
DECLARE SUB SplitHorizontal (BYVAL ULCol, BYVAL ULLine, BYVAL LRCol, BYVAL LRLine, BYVAL DestCol, BYVAL DestLine)

'---- ASM functions
DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION ErrorMsg$ (Er)
DECLARE FUNCTION MultMonitor%
DECLARE FUNCTION WhichError% ()

'---- BASIC subs
DECLARE SUB EditFormG (Form$(), Fld() AS FieldInfoG, Frm AS FormInfoG, Action)
DECLARE SUB GetFldDefG (FormName$, StartEl, Fld() AS ANY, Form$())
DECLARE SUB ShowForm (FormName$, Fld() AS FieldInfoG, Row, Col, VPage, ErrCode)
DECLARE SUB Split ()
'-- Procedures from our Main Module
DECLARE FUNCTION InputBoxG$ (FormName$)
DECLARE FUNCTION MsgBoxG% (FormName$, Msg$)
'-- Procedures from RANDOMIO.BAS
DECLARE SUB GetRecG (RecNo&, Form$(), Fld() AS ANY)
DECLARE SUB OpenFilesG (FormName$, Form$(), Fld() AS ANY)
DECLARE SUB SaveRecG (RecNo&, Form$(), Fld() AS ANY)
'-- Procedures from FORMEDIT.BAS
DECLARE SUB UnPackBuffer (FirstFld%, LastFld%, Form$(), Fld() AS ANY)

'---- BASIC functions
DECLARE FUNCTION NumFieldsG (FormName$)

'----- Define the key codes we will use.
CONST F2Key = -60
CONST F3Key = -61
CONST F4KEY = -62
CONST ESC = 27

'$INCLUDE: 'SetUp.BAS'                  'Determine monitor type, set default
                                        ' GPDat() settings

FormName$ = "CUSTOMG"                   'Assign the form name

NumFields = NumFieldsG(FormName$)       'Determine the number of fields

'---- Define mandatory arrays
DIM Frm AS FormInfoG                    'See 'EditForm.BI' for more info.
REDIM Form$(NumFields, 2)               'Holds the data for the form
REDIM Fld(NumFields) AS FieldInfoG      'Holds information about the form
REDIM Choice$(0, 0)                     'Used to hold multiple choices

'---- Load the form definitions from disk
GetFldDefG FormName$, StartEl, Fld(), Form$()
IF DOSError THEN
  PRINT ErrorMsg$(WhichError)
  END
END IF

ShowForm FormName$, Fld(), 0, 0, 1, ErrorCode  'Display the form
SELECT CASE ErrorCode                          'Check for Errors
  CASE 1
    PRINT "Sorry, this form requires an EGA or VGA monitor."
    END
  CASE 2
    PRINT "An error occurred trying to load CUSTOMG.PCX"
    END
END SELECT

CALL Split                              'Display using the Split wipe type

InstallKeyDown                          'Install the KeyDown TSR
InitMouse There                         'Initialize the mouse
Motion 5                                'Set the mouse sensitivity
ShowCursor                              'Turn on the mouse cursor

RecNo& = 1                              'Start on the first record
Action = 1

'----- Open/Field the data file as well as the notes file.
OpenFilesG FormName$, Form$(), Fld()
                                        'Find the last record number
LastRec& = LOF(Fld(0).RelHandle) \ (Fld(0).StorLen)

'----- Set the "Action" flag to force the editor to initialize itself and
'      display the data on the form.
Action = -1

'----- Clears the form, puts date in field, and sets record pointer
GOSUB ResetForm


'----- The following is our form editing loop
DO

  EditFormG Form$(), Fld(), Frm, Action

  SELECT CASE Frm.KeyCode               'Check the user's key presses
    CASE F2Key
      GOSUB ResetForm                   'Clear the form
      GPDat(93) = 0                     'Save to video memory

    CASE F3Key
      GOSUB StoreRec                    'Save the form
      GPDat(93) = 0                     'Save to video memory

    CASE F4KEY
      EditFormG Form$(), Fld(), Frm, -2
      GOSUB RetrieveRec                 'Get a previously saved record

    CASE ELSE                           'Ignor all other keys including ""
  END SELECT

LOOP UNTIL Frm.KeyCode = ESC            'Keep editing until the user presses
                                        '  the Escape key.
HideCursor                              'Turn the mouse cursor Off
DeInstallKeyDown                        'Remove the KeyDown TSR

END


'****************************************************************************
'************ Subroutines used to save and get BASIC records ****************
'****************************************************************************

'----- Clears the form and sets the record pointer to the end of the file.
ResetForm:

  GOSUB CheckSave                      'Prompt user to save if record changed

  FOR N = 1 TO Fld(0).Fields           'Clear all fields
    Form$(N, 0) = ""
  NEXT
  Form$(2, 0) = DATE$                  'Put today's date on the form
  RecNo& = LastRec& + 1                'Set current record # to end of file

  IF Action <> -1 THEN Action = 1      'Force a redisplay of the form

RETURN


'----- Prompts the user to save the current record if it has changed
CheckSave:

  IF Frm.Edited THEN                   'If the form has changed, prompt save.
    IF MsgBoxG("MsgBoxG", "This record has changed. Save now?") = 1 THEN
      GOSUB StoreRec                   'Save if "Y" was pressed "Y" = 1,
    END IF                             '  "N" = 2, and Esc = 0
    Action = -1
  END IF

RETURN


'----- Saves the current record
StoreRec:

  IF RecNo& = 0 THEN RecNo& = 1
  SaveRecG RecNo&, Form$(), Fld()       'Save the currently displayed record
  IF RecNo& > LastRec& THEN LastRec& = RecNo&
  Frm.Edited = 0                        'Reset "Edited" flag

RETURN


'----- Get a previously saved record
RetrieveRec:

  GOSUB CheckSave                     'Prompt user to save if record changed

  IF LastRec& THEN                    'If there are records to retrieve,
                                      '  ask the user for the record number
    DO
      RecNo& = VAL(InputBoxG$("InputBxG"))

      IF RecNo& > LastRec& THEN       'Is record past the end?
        Temp = MsgBoxG("OKMsg", "    Record" + STR$(RecNo&) + " does not exist!")
      ELSE
        EXIT DO
      END IF
    LOOP

    IF RecNo& THEN                   'If a record was specified,

      GetRecG RecNo&, Form$(), Fld() 'Retrieve the record from disk

      '----- The following line of code can be used to copy the
      '      Form$(0, 0) file buffer to our TYPE record "CustomG".
      '      This is completly optional.
      'BCopy VARSEG(Form$(0, 0)), SADD(Form$(0, 0)), VARSEG(CustomG), VARPTR(CustomG), LEN(Customer), 0
             '^
             'ÀÄSSEG for BASIC 7.x

                                     'Copy "Form" buffer to individual
                                     '  field elements
        UnPackBuffer 1, Fld(0).Fields, Form$(), Fld()
    END IF
  ELSE
                                     'Tell the user that there are no records
    Temp = MsgBoxG("OKMsg", "      No records to retrieve!")
  END IF
  
  Action = -1                        'Since the calls to InputBoxG$ &
                                     '  MsgBoxG$ above used EditForm, we
                                     '  need to re-initialize the customer
                                     '  form.
RETURN

'****************************************************************************
'Allows the user to enter a string of text into a field of a specified form.
'
'Inputs:
'  LibName$ - Name of the screen/form library file containing the input form.
'  FormName$ - Name of the input form.
'
'Returns:
'  The text of the field if enter was pressed, otherwise returns a nul string.
'
'Notes:
'  The underlying screen is automaticaly saved on entry and restored on exit.
'  The input form should have one entry field (any type) and two button
'  fields.  The first button should return an Enter key press (13) and the
'  second should return an Escape key (27).
'****************************************************************************
'
FUNCTION InputBoxG$ (FormName$)



NumFields = NumFieldsG(FormName$)

'---- Define mandatory arrays
DIM Frm AS FormInfoG                    'See 'EditForm.BI' for more info.
REDIM Form$(NumFields, 2)               'Holds the data for the form
REDIM Fld(NumFields) AS FieldInfoG      'Holds information about the form
REDIM Choice$(0, 0)                     'Used to hold multiple choices

GPDat(93) = -1                          'Save to conventional memory

   '---- Load the form definitions from disk
   GetFldDefG FormName$, StartEl, Fld(), Form$()

   HideCursor
   PCOPY 0, 1             'Save the form the page 1 (EGA only)
   ShowCursor

   '---- Display the form
   ShowForm FormName$, Fld(), Fld(0).Row, Fld(0).LCol, 0, ErrorCode
   IF ErrorCode THEN
     BEEP
     EXIT FUNCTION
   END IF

   Action = -1                             'Action -1 prevents the previous
                                           ' button from "restoring" onto
                                           ' the new form

   DO                                      'Allow user to edit the form
      EditFormG Form$(), Fld(), Frm, Action
   LOOP UNTIL Frm.KeyCode = 13 OR Frm.KeyCode = 27

   HideCursor
   PCOPY 1, 0                              'Restore original form (EGA only)
   ShowCursor

   IF Frm.KeyCode = 13 THEN                'If user pressed Enter,
      InputBoxG$ = Form$(1, 0)             '  return field 1's contents
   ELSE                                    'otherwise,
      InputBoxG$ = ""                      '  return nothing.
   END IF

END FUNCTION

'****************************************************************************
'Displays a form containing a message and waits for the user to press a key
'or button to acknowledge the message.
'
'Inputs:
'  LibName$ - Name of the screen/form library file containing the input form.
'  FormName$ - Name of the message form.
'
'Returns:
'  The number of the button used to acknowledge the message.
'
'Notes:
'  The underlying screen is automaticaly saved on entry and restored on exit.
'  The Message form should contain button fields only.  If you defined 3
'  buttons, where the first returned 65 ("A"bort), the second 82 ("R"etry)
'  and the third 73 ("I"gnore), and the user pressed "R" or clicked on the
'  second button, this function would return 2.
'****************************************************************************
'
FUNCTION MsgBoxG% (FormName$, Msg$)



NumFields = NumFieldsG(FormName$)

'---- Define mandatory arrays
DIM Frm AS FormInfoG                    'See 'EditForm.BI' for more info.
REDIM Form$(NumFields, 2)               'Holds the data for the form
REDIM Fld(NumFields) AS FieldInfoG      'Holds information about the form
REDIM Choice$(0, 0)                     'Used to hold multiple choices

GPDat(93) = -1                          'Save to conventional memory

'---- Load the form definitions from disk
GetFldDefG FormName$, StartEl, Fld(), Form$()

   HideCursor
   PCOPY 0, 1                 'Save the screen to background page 1 (EGA only)
   ShowCursor


   ShowForm FormName$, Fld(), Fld(0).Row, Fld(0).LCol, 0, ErrorCode  'Display the form

   Action = -1
   Form$(1, 0) = Msg$

   DO

      EditFormG Form$(), Fld(), Frm, Action

      SELECT CASE Frm.KeyCode
        CASE 13
          B = 1
          EXIT DO
        CASE 27
          B = 2
          EXIT DO
      END SELECT

   LOOP

   HideCursor
   PCOPY 1, 0                 'Restore the form
   ShowCursor

   MsgBoxG% = B

END FUNCTION

SUB Split

  SplitHorizontal 1, 0, 80, 349, 1, 0

END SUB

