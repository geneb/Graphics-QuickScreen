'****************************************************************************
'DEMOALL.BAS - sample data entry form

'Copyright (c) 1992 Crescent Software
'Written by Phil Cramer

'This demo shows each of the GQS field types.
'The NONOTESG.BAS stub file is used to replace the GQEditS.BAS multi-line
'text editor to reduce the amount of memory required for this demo. As such,
'the memo field will not allow you enter text. If you use BASIC PDS, and
'have expanded memory and an EMS driver, you should be able to replace
'NONOTESG.BAS stub file with the GQEditS.BAS module to enable the memo field
'****************************************************************************

'---- This demo will display and edit ANY GQS form... just change the
'     FormName$ below to the base name of your .PCX screen/form and modify
'     the form specific code in the EditFormG loop

DEFINT A-Z

'$INCLUDE: 'FldInfo.bi'                 'Define the FieldInfoG TYPE
'$INCLUDE: 'EditForm.BI'                'Define the Frm TYPE

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
DECLARE SUB PrintArray (FirstFld%, LastFld%, Form$(), Fld() AS ANY)
DECLARE SUB ShowForm (FormName$, Fld() AS FieldInfoG, Row, Col, VPage, ErrCode)

DECLARE FUNCTION NumFieldsG (FormName$)

'$INCLUDE: 'SetUp.BAS'                  'Determine monitor type and sets up
                                        ' default GPDat() variables

FormName$ = "DEMOALL"                   'Assign the form name

NumFields = NumFieldsG(FormName$)       'Determine the number of fields in
                                        ' the form
'---- Define mandatory arrays
DIM Frm AS FormInfoG                    'See 'EditForm.BI' for more info.
REDIM Form$(NumFields, 2)               'Holds the data for the form
REDIM Fld(NumFields) AS FieldInfoG      'Holds information about the form
REDIM Choice$(0, 0)                     'Used to hold multiple choices

'---- Load the form definitions from disk
GetFldDefG FormName$, StartEl, Fld(), Form$()
IF DOSError THEN                        'See if an error occured loading
  PRINT ErrorMsg$(WhichError)           ' the file, display an appropriate
  END                                   ' error message and end the program
END IF
                                        'Define choices for mult-choice fields
REDIM Choice$(9, 0)                     'Assign choices to fields 25 and 26
Choice$(0, 0) = "25, 26"
Choice$(1, 0) = "Choice 1 "
Choice$(2, 0) = "Choice 2 "
Choice$(3, 0) = "Choice 3 "
Choice$(4, 0) = "Choice 4 "
Choice$(5, 0) = "Choice 5 "
Choice$(6, 0) = "Choice 6 "
Choice$(7, 0) = "Choice 7 "
Choice$(8, 0) = "Choice 8 "
Choice$(9, 0) = "Choice 9 "

ShowForm FormName$, Fld(), 0, 0, 0, ErrorCode  'Display the form
SELECT CASE ErrorCode                            'Check for Errors
  CASE 1
    PRINT "Sorry, this form requires a VGA monitor."
    END
  CASE 2
    PRINT "An error occurred trying to load DEMOALL.PCX"
    END
END SELECT


InstallKeyDown                          'Install the KeyDown TSR
InitMouse There                         'Initialize the mouse
ShowCursor                              'Turn on the mouse cursor
Motion 5                                'Set the mouse sensitivity (1 - 100)

Action = 1
'---- The following is our form editing loop
DO
  EditFormG Form$(), Fld(), Frm, Action

    SELECT CASE Frm.FldNo

      CASE 28
        '---- Assign new scrollbar large change highlight color
        GPDat(100) = 4
       'Fld(28).Value returns the scroll bar value
      CASE 41
        '---- Assign new scrollbar large change highlight color
        GPDat(100) = 5
       'Fld(41).Value returns the scroll bar value
      CASE 42
        '---- Assign new scrollbar large change highlight color
        GPDat(100) = 6
       'Fld(42).Value returns the scroll bar value
      CASE 43
        '---- Assign new scrollbar large change highlight color
        GPDat(100) = 2
       'Fld(43).Value returns the scroll bar value

      CASE 29                           'Select mouse fields
        IF Frm.KeyCode = 83 THEN
          FOR i = 34 TO 39
            Form$(i, 0) = "X"
          NEXT
          CALL PrintArray(34, 39, Form$(), Fld())
        END IF

      CASE 30                           'Clear mouse fields
        IF Frm.KeyCode = 67 THEN
          FOR i = 34 TO 39
            Form$(i, 0) = " "
          NEXT
          CALL PrintArray(34, 39, Form$(), Fld())
        END IF

    END SELECT

LOOP UNTIL Frm.KeyCode = 27

DeInstallKeyDown                        'UnInstall the KeyDown TSR

