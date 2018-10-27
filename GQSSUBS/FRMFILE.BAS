'****************************************************************************
'Module used to load individual Form Definition files (.FRM).
'
'by Donald R. Malin,  Copyright (c) 1990 Crescent Software, Inc.
'
'This module should be used with programs that load field definitions from
'individual Form (.FRM) Files.  There are two procedures contained in this
'module:
'
'  "NumFieldsG"      - returns the number of fields contained in the form.
'                     Use this function to determine the size of the Field
'                     definition array (Fld()) and the form's data array
'                     (Form$()) prior to loading the file with "GetFldDefG".
'
'  "GetFldDefG"     - Loads field definitions, help strings and formulas
'                     into the arrays mentioned above.
'****************************************************************************

DEFINT A-Z

'$INCLUDE: 'FldInfo.bi'                 'Defines TYPE for field information

'----- Define functions
DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION FLInput$ (Handle%, Buffer$)
DECLARE FUNCTION NumFieldsG% (FrmName$)

'----- Define subs
DECLARE SUB FClose (Handle%)
DECLARE SUB FGet (Handle, Text$)
DECLARE SUB FGetA (Handle%, SEG Element AS ANY, NumBytes&)
DECLARE SUB FGetT (Handle, Addr AS ANY, Var AS ANY)
DECLARE SUB FOpen (FileName$, Handle%)
DECLARE SUB FSeek (Handle, Offset&)
DECLARE SUB SetError (Errnum%)
DECLARE SUB Tokenize (Calc$, Fld() AS ANY)

'****************************************************************************
'Retrieves Form information from file and puts it in a structure for later
'reference by other routines.  Also loads formulas and help messages into
'the "Form$()" data array.
'
'FrmName$ is the name of the form definition file (.FRM).
'
'StartEl is the element of the arrays below to begin loading the file into.
'
'Fld() is the field information TYPE array that the file will be loaded into.
'  It must have been DIMensioned to at least the number of fields in the form.
'  See "NumberOfFields" function to determine the number of fields in a form.
'
'Form$() is the field data array that the Help and Calc files will be loaded
'  into.  It must have been DIMensioned to the number of fields by 2 ie.
'  [REDIM Form$(NumFields, 2)].  The Help messages will be loaded into the
'  second column [Form$(1, 1)] and the formulas will be loaded into the third
'  [Form$(1, 2).
'****************************************************************************
'
SUB GetFldDefG (FrmName$, StartEl, Fld() AS FieldInfoG, Form$()) STATIC

   Per = INSTR(FrmName$, ".")                   'Look for a period in name
   IF Per THEN                                  'If there is one,
      Temp$ = FrmName$                          '  use the name as is
   ELSE                                         'Otherwise add the extension
      Temp$ = FrmName$ + ".Frm"
   END IF

   FOpen Temp$, Handle                          'Open the file
   Temp$ = ""                                   'Don't need this any more
   IF DOSError THEN EXIT SUB                    'Bail out if we got an error

   FGetT Handle, NumFlds, 2
   NumBytes& = (NumFlds + 1) * FrmWdth
   FSeek Handle, 0&
   FGetA Handle, Fld(StartEl), NumBytes&

   Byte$ = " "
   FGet Handle, Byte$

   Buffer$ = SPACE$(400)
  
   DO UNTIL DOSError
      IF Byte$ = "H" THEN
         FOR N = StartEl + 1 TO StartEl + NumFlds
            Form$(N, 1) = FLInput$(Handle, Buffer$)
            IF DOSError THEN EXIT FOR
         NEXT
      ELSE
         FOR N = StartEl + 1 TO StartEl + NumFlds
            Form$(N, 2) = FLInput$(Handle, Buffer$)
            IF DOSError THEN EXIT FOR
            IF LEN(Form$(N, 2)) THEN Tokenize Form$(N, 2), Fld()
         NEXT
      END IF

      FGet Handle, Byte$
   LOOP
   Buffer$ = ""
   FClose Handle
  
   '----- Calculate pointers into whole record buffer
   IF StartEl = 0 THEN
      Fld(StartEl + 1).Fields = 1
   ELSE
      Fld(StartEl + 1).Fields = Fld(StartEl - 1).Fields + Fld(StartEl - 1).StorLen
   END IF
                                               
   FOR N = StartEl + 2 TO StartEl + Fld(StartEl).Fields
      Fld(N).Fields = Fld(N - 1).Fields + Fld(N - 1).StorLen
   NEXT

   Fld(0).StorLen = Fld(N - 1).Fields + Fld(N - 1).StorLen - 1

   SetError 0                                   'No errors

END SUB

'****************************************************************************
'Function returns the number of fields contained in the form "FrmName$".
'Use this routine to determine the size of the arrays used to hold field
'information prior to loading them with the SUB - "GetFldDef".
'****************************************************************************
'
FUNCTION NumFieldsG (FrmName$) STATIC

   Per = INSTR(FrmName$, ".")                   'Look for a period in name
   IF Per THEN                                  'If there is one,
      Temp$ = FrmName$                          '  use the name as is
   ELSE                                         'Otherwise add the extension
      Temp$ = FrmName$ + ".Frm"
   END IF

   FOpen Temp$, Handle                          'Open the file
   Temp$ = ""                                   'Don't need this any more
   IF NOT DOSError THEN                         'If no error, get the number
      FGetT Handle, NumFlds, 2                  '  of fields from first word
   ELSE                                         'Otherwise make it 0
      NumFlds = 0
   END IF
   FClose Handle

   NumFieldsG = NumFlds                          'Assign the function

END FUNCTION

