'****************************************************************************
'DEMOINVG.BAS - sample data entry form

'Copyright (c) 1992 Crescent Software, Inc.
'by Don Malin and Phil Cramer

'Illustrates using "EditFormG" with a complex data entry form containing
'"Multiple Choice", "Calculated" and "Notes" fields.  Also shows how to
'modify the contents of fields on the fly based on the user's input.  See
'"CalcDiscountRate" and "FillInLineItem".

'This demo uses the "NONOTESG.BAS" stub file to reduce the amount of memory
'required by the demo. If you use BASIC PDS and have an EMS driver, you may
'be able to replace NONOTESG.BAS with GQEDITS.BAS to have a fully functional
'demo.
'****************************************************************************

DEFINT A-Z

'$INCLUDE: 'FldInfo.bi'              'Define the Fld TYPE
'$INCLUDE: 'EditForm.BI'             'Define the Frm TYPE

'---- ASM subs
DECLARE SUB HideCursor ()
DECLARE SUB InitMouse (There)
DECLARE SUB Motion (Sensitivity)
DECLARE SUB ShowCursor ()
DECLARE SUB SlideDown (BYVAL ULCol, BYVAL ULLine, BYVAL LRCol, BYVAL LRCol, BYVAL DestCol, BYVAL DestLine)
DECLARE SUB SlideUp (BYVAL ULCol, BYVAL ULLine, BYVAL LRCol, BYVAL LRCol, BYVAL DestCol, BYVAL DestLine)
DECLARE SUB SlideRight (BYVAL ULCol, BYVAL ULLine, BYVAL LRCol, BYVAL LRCol, BYVAL DestCol, BYVAL DestLine)
DECLARE SUB SlideLeft (BYVAL ULCol, BYVAL ULLine, BYVAL LRCol, BYVAL LRCol, BYVAL DestCol, BYVAL DestLine)

'---- ASM functions
DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION WhichError% ()
DECLARE FUNCTION ErrorMsg$ (Er)
DECLARE FUNCTION MultMonitor%

'---- BASIC subs
DECLARE SUB CalcDiscountRate (Form$(), Fld() AS ANY)
DECLARE SUB CalcFields (StartOfForm%, FldNo%, Form$(), Fld() AS ANY)
DECLARE SUB EditFormG (Form$(), Fld() AS FieldInfoG, Frm AS FormInfoG, Action)
DECLARE SUB FillInLineItem (FldNo%, Form$(), Fld() AS ANY)
DECLARE SUB GetFldDefG (FormName$, StartEl, Fld() AS ANY, Form$())
DECLARE SUB PaintFieldG (Fld AS ANY, Clr)
DECLARE SUB PrintArray (FirstFld%, LastFld%, Form$(), Fld() AS ANY)
DECLARE SUB SaveField (FldNo%, Form$(), Fld() AS ANY, BadFld%)
DECLARE SUB ShowForm (FormName$, Fld() AS FieldInfoG, Row, Col, VPage, ErrCode)
DECLARE SUB SlideFan ()
DECLARE SUB Wipes (WipeType%)

'---- BASIC functions
DECLARE FUNCTION FldNum% (N$, Fld() AS ANY)
DECLARE FUNCTION NumFieldsG (FormName$)

'$INCLUDE: 'SetUp.BAS'

'----- Reserve stack space for recursive subs (calculated fields).
'      Note: If you get an "Out of stack space" error, first check that your
'            form doesn't have any "Circular" references in any calculated
'            fields.  If it does not then un-rem and/or increase the value
'            below.
'CLEAR , , 2000


'----- This demo makes references to certain fields for calculations and
'      since we might add, delete, or move fields around, it is a good
'      idea to use field number variables to refer to them.  See the calls
'      to "FldNum%" in the "InvoiceSetup:" subroutine to see how these are
'      assigned.
DIM SHARED CurDate                      'Field # of the date field
DIM SHARED Descr1                       'Field # of Description of 1st item
DIM SHARED DescrDif                     '# of fields between description fields
DIM SHARED DiscountRate                 'Field # of Discount rate field
DIM SHARED TaxRate                      'Field # of the tax rate field
DIM SHARED Notes                        'Field # of the notes field.


FormName$ = "GINVOICE"                  'Assign the Form name

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

GOSUB InvoiceSetUp

ShowForm FormName$, Fld(), 0, 0, 1, ErrorCode  'Display the form
SELECT CASE ErrorCode                       'Check for Errors
  CASE 1
    PRINT "Sorry, this form requires an EGA or VGA monitor."
    END
  CASE 2
    PRINT "An error occurred trying to load GINVOICE.PCX"
    END
END SELECT

CALL SlideFan                           'Use the slide fan wipe type
                                        ' from EGAWIPES.BAS
InitMouse There                         'Initialize the mouse
ShowCursor                              'Turn on the mouse cursor
Motion 5                                'Set the mouse sensitivity

GPDat(90) = 0                           'Use the following list box colors:
GPDat(76) = 0 + 15 * 256                'ListBox text color
GPDat(78) = 0 + 11 * 256                'ListBox highlight color
GPDat(99) = 15                          'Maximum number of items to display
                                        ' in each ListBox

Action = 1

'---- The following is our form editing loop
DO
  EditFormG Form$(), Fld(), Frm, Action

   '----- Are we moving to a new field?
   IF Frm.FldNo <> Frm.PrevFld THEN

      '----- Did they enter/edit anything?
      IF Frm.FldEdited THEN

         '----- Where we within the range of line items?
         IF Frm.PrevFld >= Descr1 AND Frm.PrevFld <= LastDescr THEN

            '----- See which field column we where on
            SELECT CASE (Frm.PrevFld - Descr1) MOD DescrDif

               '----- Was it a description field?
               CASE 0
                  '----- Fill in the rest of the line (Units, Quantity, Price)
                  FillInLineItem Frm.PrevFld, Form$(), Fld()
                  '----- Calculate the discount rate if it applies
                  CalcDiscountRate Form$(), Fld()
                  '----- Put the cursor on the next line item description
                  Frm.FldNo = Frm.PrevFld + DescrDif

               '----- Was it a quantity field?
               CASE 2
                  '----- Calculate the discount rate if it applies
                  CalcDiscountRate Form$(), Fld()

               CASE ELSE
            END SELECT
         END IF
      END IF

   END IF

LOOP UNTIL Frm.KeyCode = 27
END

'****************************************************************************
'*** The following subroutine does setup specificaly for the Invoice form ***
'****************************************************************************

InvoiceSetUp:

    '----- Find field numbers from their names
    CurDate = FldNum%("INVDATE", Fld())
    Descr1 = FldNum%("DESCR1", Fld())
    DescrDif = FldNum%("DESCR2", Fld()) - Descr1
    DiscountRate = FldNum%("DISCRATE", Fld())
    TaxRate = FldNum%("TAXRATE", Fld())
    Notes = FldNum%("NOTES", Fld())

    LastDescr = 10 * DescrDif + Descr1


    '----- Here's how you set up multiple choice fields
    REDIM Choice$(13, 2)                '13 choices in 3 menus (menu 0 is used)

    Choice$(0, 0) = STR$(FldNum%("TERMS", Fld()))
    Choice$(1, 0) = "CASH"
    Choice$(1, 0) = "CHECK"
    Choice$(2, 0) = "COD"
    Choice$(3, 0) = "AMEX"
    Choice$(4, 0) = "VISA"
    Choice$(5, 0) = "Master Card"

    Choice$(0, 1) = STR$(FldNum%("SALESMAN", Fld()))
    Choice$(1, 1) = "Don Malin"
    Choice$(2, 1) = "Ethan Winer"
    Choice$(3, 1) = "Tami Schwartzkopf"
    Choice$(4, 1) = "Paul Passarelli"
    Choice$(5, 1) = "Jay Munro"
    Choice$(6, 1) = "Brian Giedt"
    Choice$(7, 1) = "Jon Waldman"
    Choice$(8, 1) = "Phil Cramer"
    Choice$(9, 1) = "Nash Bly"

    '----- Define the stock items.
    Choice$(1, 2) = "0110   QuickPak Professional"
    Choice$(2, 2) = "1000   P.D.Q."
    Choice$(3, 2) = "0270   Quick Screen"
    Choice$(4, 2) = "0410   GraphPak Professional"
    Choice$(5, 2) = "1200   Graphics Workshop"
    Choice$(6, 2) = "0600   LaserPak"
    Choice$(7, 2) = "0700   QuickPak Scientific"
    Choice$(8, 2) = "1001   P.D.Q. COM"
    Choice$(9, 2) = "1100   XREF Cross Reference"
    Choice$(10, 2) = "0500   QuickMenu"
    Choice$(11, 2) = "2001   Auto-Upgrade plan for QuickPak"
    Choice$(12, 2) = "2000   Auto-Upgrade plan for P.D.Q."
    Choice$(13, 2) = "X                          "
    '----- The above choices go with each of the 10 description fields so we
    '      need to put the numbers of all ten in the first element seperated
    '      by comas.
    Choice$(0, 2) = STR$(Descr1)
    FOR N = 1 TO 9
      FldNo$ = STR$(N * DescrDif + Descr1)
      Choice$(0, 2) = Choice$(0, 2) + "," + FldNo$
    NEXT


    '----- Preset some fields data
    Form$(CurDate, 0) = DATE$           'Invoice date field
    Form$(TaxRate, 0) = "8"             'Tax rate field

    '----- The following statements create a single long string which will be
    '      formatted into the notes section of the form.  Single strings like
    '      this can be saved as notes fields (variable length fields) in
    '      sequential, "DBase" (db/LIB), "Btrieve" or "ISAM" files.  Note the
    '      CHR$(20) paragraph markers used to specify the end of paragraphs
    '      and blank lines.
    '
    Form$(Notes, 0) = "This is a test of the Notes field type.  We are presetting "
    Form$(Notes, 0) = Form$(Notes, 0) + "some text in here to show that it will be unwrapped "
    Form$(Notes, 0) = Form$(Notes, 0) + "and displayed properly when the form is first entered."
    Form$(Notes, 0) = Form$(Notes, 0) + CHR$(20) + CHR$(20) + "This is the start of another "
    Form$(Notes, 0) = Form$(Notes, 0) + "paragraph which should be on the second line after "
    Form$(Notes, 0) = Form$(Notes, 0) + "the above paragraph." + CHR$(20)

RETURN

'****************************************************************************
'Calculates a discount rate based on the number of products purchased.
'While this procedure is only for use with this demo., it shows how a
'program can modify the contents of fields and force calculations.
'****************************************************************************
'
SUB CalcDiscountRate (Form$(), Fld() AS FieldInfoG)

   FOR L = 0 TO 9
      QuantFld = L * DescrDif + Descr1 + 2      'Find Quantity field's number
      '----------------------------------------------------------------------
      'The following line retrieves the Quantity field's value from
      'the field buffer "Form$(0, 0)".  Since numeric fields are
      'stored here as IEEE strings, we can use CV? to quickly extract
      'a field's value.  While we could accomplish the same thing by
      'using a statement such as:
      '  Quant = VAL(Form$(QuantFld, 0))
      'or to guard against dollar signs and commas in number strings:
      '  Quant = Value#(Form$(QuantFld, 0))
      'While these two examples may seem simpler, they are
      'considerably slower.

      Quant = CVI(MID$(Form$(0, 0), Fld(QuantFld).Fields, 2))
      '                ^ buffer     ^ Offset into buffer  ^ 2 bytes for Int.s
      '----------------------------------------------------------------------

      '----- Blank numbers are stored as the lowest possible value for the
      '      type of number being represented.  ie. a blank integer field
      '      would hold the value: -32767.
      IF Quant > 0 THEN
         NumItems = NumItems + Quant
      END IF
   NEXT

   '----- Determine the discount rate based on the number of products.
   '      Notice that we use LSET to assign the Form$() array element.
   '      This insures that the length of the field does not change.
   SELECT CASE NumItems
      CASE 2
         LSET Form$(DiscountRate, 0) = "5"
      CASE 3
         LSET Form$(DiscountRate, 0) = "10"
      CASE IS >= 4
         LSET Form$(DiscountRate, 0) = "15"
      CASE ELSE
   END SELECT

   '----- Format and save the field's value into the Form$(0, 0) buffer
   SaveField DiscountRate, Form$(), Fld(), BadFld

   '----- Display the field's formatted contents on the screen.
   PrintArray DiscountRate, DiscountRate, Form$(), Fld()

   '----- Force the discount amount to be calculated from the
   '      discount rate field we just updated.
   CalcFields 0, DiscountRate, Form$(), Fld()
   '             ^ Point to the field we just changed
END SUB

'****************************************************************************
'Fills in line item fields (Units, Quantity, and Unit Price) based on the
'product selected from a Description field's menu.
'
'While this procedure is only for use with this demo., it shows how a
'program can modify the contents of fields and force calculations.
'****************************************************************************
'
SUB FillInLineItem (FldNo, Form$(), Fld() AS FieldInfoG)

   Quant$ = "1"

   '----- Find the unit price based on the product number
   SELECT CASE LEFT$(Form$(FldNo, 0), 4)
      CASE "0110"
         Um$ = "EA"
         Unit$ = "199"
      CASE "1000"
         Um$ = "EA"
         Unit$ = "129"
      CASE "0270"
         Um$ = "EA"
         Unit$ = "149"
      CASE "0410"
         Um$ = "EA"
         Unit$ = "149"
      CASE "1200"
         Um$ = "EA"
         Unit$ = "149"
      CASE "0600"
         Um$ = "EA"
         Unit$ = "149"
      CASE "0700"
         Um$ = "EA"
         Unit$ = "149"
      CASE "1001"
         Um$ = "EA"
         Unit$ = "99"
      CASE "1100"
         Um$ = "EA"
         Unit$ = "59"
      CASE "0500"
         Um$ = "EA"
         Unit$ = "59"
      CASE "2000"
         Um$ = "YEAR"
         Unit$ = "69"
      CASE "2001"
         Um$ = "YEAR"
         Unit$ = "69"
      CASE ELSE
         Quant$ = ""
   END SELECT

   '----- Assign the Units of measure, Quantity, and Unit Price
   '      Notice that we use LSET to assign the Form$() array element.
   '      This insures that the length of the field does not change.
   LSET Form$(FldNo + 1, 0) = Um$
   LSET Form$(FldNo + 2, 0) = Quant$
   LSET Form$(FldNo + 3, 0) = Unit$

   '----- Format and save each field's value into the Form$(0, 0) buffer
   FOR N = FldNo + 1 TO FldNo + 3
      SaveField N, Form$(), Fld(), BadFld
   NEXT

   '----- Display the field's formatted contents on the screen.
   PrintArray FldNo + 1, FldNo + 3, Form$(), Fld()

   '----- Force the total item cost as well as the total fields to be
   '      updated.
   CalcFields 0, FldNo + 2, Form$(), Fld()
   '             ^ Point to the quantity field we just changed

END SUB

'****************************************************************************
'This procedure can be used to size and load the field data (Form$()) and
'information (Fld()) arrays from a form file (.FRM).
'****************************************************************************
'
SUB LoadFormDef (FormName$, Form$(), Fld() AS FieldInfoG)

    NumFlds = NumFieldsG(FormName$) 'Get the number of fields in form
    IF DOSError THEN EXIT SUB

    REDIM Fld(NumFlds) AS FieldInfoG    'DIM the field information array

    '---- Create the array used for form data, help messages and formulas
    '     Form$(0, 0) holds all data from fields as a contiguous string with
    '                 numbers stored as IEEE formatted strings.
    '     Form$(FieldNum, 0) holds data (formatted numeric string for numbers)
    '     Form$(FieldNum, 1) holds help message string
    '     Form$(FieldNum, 2) holds formulas for calculated fields
    REDIM Form$(NumFlds, 2)             'DIM the form data array

    GetFldDefG FormName$, 0, Fld(), Form$() 'Load the field information

END SUB

SUB SlideFan

   SlideUp 1, 0, 40, 175, 1, 0
   SlideRight 41, 0, 80, 175, 41, 0
   SlideDown 41, 176, 80, 349, 41, 176
   SlideLeft 1, 176, 40, 349, 1, 176

END SUB

