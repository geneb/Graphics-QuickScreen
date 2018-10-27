'****************************************************************************
'DEMOWIPE.BAS 
'
'Copyright (c) 1992 Crescent Software, Inc.
'by Phil Cramer & Don Malin
'
'This program demonstrates the 21 EGA wipe types available for displaying
'EGA full screen images. Since it unlikely that a single program would use
'all of these wipe types, it is recommended that you copy and paste the
'desired wipe types into your source code and call them directly rather than
'call the Wipes subroutine as shown here.
'****************************************************************************
DEFINT A-Z

'$INCLUDE: 'FldInfo.bi'

'---- BASIC subs
DECLARE SUB ShowForm (FormName$, Fld() AS FieldInfoG, Row, Col, VPage, ErrCode)
DECLARE SUB Wipes (WipeType%)
DECLARE FUNCTION MultMonitor% ()

'$INCLUDE: 'SetUp.BAS'              'Detect monitor type, set up GPDat() array

REDIM EGAScreen$(3)                 'Create an array to hold screen names
EGAScreen$(0) = "GInvPag1"
EGAScreen$(1) = "CustomG"
EGAScreen$(2) = "GInvPag2"
EGAScreen$(3) = "GInvoice"

DIM Fld(1) AS FieldInfoG
Fld(0).Indexed = 14                 'Use 25 line mode (14 pixels per char)
Fld(0).Value = 5                    'Force SCREEN 9


'---- Display the screens with ShowForm
FOR i = 1 TO 21

  ShowForm EGAScreen$(i MOD 4), Fld(), 0, 0, 1, ErrorCode  'Display the form
  SELECT CASE ErrorCode                                    'Check for Errors
    CASE 1
      PRINT "Sorry, this form requires an EGA or VGA monitor."
      END
    CASE 2
      PRINT "An error occurred trying to load " + EGAScreen$(N)
      END
  END SELECT

CALL Wipes(i)                                    'Wipe image onto the screen

NEXT

