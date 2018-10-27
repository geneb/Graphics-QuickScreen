'***************  DemoAnyG.Bas  -  Demo Data Entry on ANY Form

' Copyright (c) 1990, 1991 Crescent Software
' Written by Nash Bly and Don Malin

' Use this demo with any form you have created using Graphics QuickScreen.
' You will need your .PCX and .FRM file to see how they work.
' This is a bare bones example of loading and displaying screens,
' loading form definitions and using the EditForm sub for data entry.
' All of the "No" stub files are used in this demo since you may not
' be able to load all of the working modules into memory.
'
'****************************************************************************
DEFINT A-Z

'$INCLUDE: 'FldInfo.bi'
'$INCLUDE: 'EditForm.BI'

'---- ASM subs
DECLARE SUB DeInstallKeyDown ()
DECLARE SUB InitMouse (There)
DECLARE SUB InstallKeyDown ()
DECLARE SUB ShowCursor ()
DECLARE SUB HideCursor ()
DECLARE SUB Motion (Sensitivity)

'---- ASM functions
DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION WhichError% ()
DECLARE FUNCTION ErrorMsg$ (Er)
DECLARE FUNCTION MultMonitor%

'---- BASIC subs
DECLARE SUB EditFormG (Form$(), Fld() AS FieldInfoG, Frm AS FormInfoG, Action)
DECLARE SUB GetFldDefG (FormName$, StartEl, Fld() AS ANY, Form$())
DECLARE FUNCTION NumFieldsG (FormName$)
DECLARE SUB ShowForm (FormName$, Fld() AS FieldInfoG, Row, Col, VPage, ErrCode)

'$INCLUDE: 'SetUp.BAS'


CLS
PRINT "Enter the name of the form (You may specify a path but no extension)"
PRINT "Press Escape to end the demo"
INPUT "Formname: "; FormName$


NumFields = NumFieldsG(FormName$)

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


'---- Look for multiple choice fields
FOR i = 1 TO NumFields
  IF Fld(i).FType = MultChAFld THEN
    ChoiceField$ = LTRIM$(STR$(i)) + ", "
  END IF
NEXT

'---- Set up some choices if required
L = LEN(ChoiceField$)
IF L THEN
  REDIM Choice$(9, 0)
  Choice$(0, 0) = LEFT$(ChoiceField$, L - 2)
  Choice$(1, 0) = "Choice 1 "
  Choice$(2, 0) = "Choice 2 "
  Choice$(3, 0) = "Choice 3 "
  Choice$(4, 0) = "Choice 4 "
  Choice$(5, 0) = "Choice 5 "
  Choice$(6, 0) = "Choice 6 "
  Choice$(7, 0) = "Choice 7 "
  Choice$(8, 0) = "Choice 8 "
  Choice$(9, 0) = "Choice 9 "
END IF

'---- Display the form
ShowForm FormName$, Fld(), Fld(0).Row, Fld(0).LCol, 0, ErrorCode

SELECT CASE ErrorCode
  CASE 1
    PRINT "Sorry, this form will not run on this monitor."
    END
  CASE 2
    PRINT "An error occurred trying to load DEMOALL.PCX"
    END
END SELECT


InstallKeyDown                          'Install the KeyDown TSR
InitMouse There                         'Initialize the mouse
ShowCursor                              'Turn on the mouse cursor
Motion 5                                'Set the mouse sensitivity

Action = 1

'---- The following is our form editing loop
DO
  EditFormG Form$(), Fld(), Frm, Action
LOOP UNTIL Frm.KeyCode = 27

DeInstallKeyDown                        'UnInstall the KeyDown TSR

