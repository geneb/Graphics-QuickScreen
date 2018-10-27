'***** GQSCalc.Bas *************************************************************
'      Module used to evaluate calculated fields in a form

'      See NoCalc.Bas to link out this code for forms that don't use
'      calculated fields

'Copyright (c) 1989 Crescent Software, Inc.
'by Don Malin


'NOTE: Calculations are done by converting numeric strings
'      into double precision numbers and then returning the
'      result as a double precision number.  Actual calculations
'      are done by "Evaluate.Bas".
'      All of BASIC's math operators are supported as well as
'      the following constants and math functions.

'Constants:
'  "PI" = 3.14159265358979323846
'  "@TODAY" = Todays date as number of days since 12-31-1979

'Functions:
'  "ARCSINH" - Inverse Hyperbolic Sine
'  "ARCCOSH" - Inverse Hyperbolic Cosine
'  "ARCTANH" - Inverse Hyperbolic Tangent
'  "ARCSECH" - Inverse Hyperbolic Secant
'  "ARCCSCH" - Inverse Hyperbolic Cosecant
'  "ARCCOTH" - Inverse Hyperbolic Cotangent
'  "ARCSIN"  - Inverse Sine
'  "ARCCOS"  - Inverse Cosine
'  "ARCSEC"  - Inverse Secant
'  "ARCCSC"  - Inverse Cosecant
'  "ARCCOT"  - Inverse Cotangent
'  "SINH"    - Hyperbolic Sine
'  "TANH"    - Hyperbolic Tangent
'  "SECH"    - Hyperbolic Secant
'  "CSCH"    - Hyperbolic Cosecant
'  "COTH"    - Hyperbolic Cotangent
'  "CSC"     - Cosecant
'  "COT"     - Cotangent
'  "SEC"     - Secant
'  "SIN"     - Sine
'  "COS"     - Cosine
'  "TAN"     - Tangent
'  "ATN"     - Inverse Tangent
'  "LOG"     - Natural Log
'  "EXP"     - Exponent
'  "SQR"     - Square Root
'  "CLG"     - Common Log
'  "!"       - Factorial ie. 123!  (0! to 170!)
'  "ABS"     - Absolute Value

'Math operators:
'  ^ * / \ MOD + -

'Relational operators:
'  = > <

'Logical Operators:
'  NOT AND OR

'Note also that these should not be used as field names unless you do
'not need them as functions.
'****************************************************************************

DEFINT A-Z

'$INCLUDE: 'FldInfo.bi'                'Defines a TYPE for field information


CONST BlankInt% = -32767                        'Specifies blank integer
CONST BlankLng& = -2147483647                   '       "        long int.
CONST BlankSng! = -3.402823E+38                 '       "        single
CONST BlankDbl# = -1.79769313486231D+308        '       "        double

'----- ASM Functions
DECLARE FUNCTION ASCII% (Strng$)
DECLARE FUNCTION Blanks% (Text$)
DECLARE FUNCTION Date2Num% (Dat$)
DECLARE FUNCTION EDate2Num% (Dat$)
DECLARE FUNCTION ENum2Date$ (Days)
DECLARE FUNCTION MaxInt% (a, B)
DECLARE FUNCTION MinInt% (a, B)
DECLARE FUNCTION Null% (Text$)
DECLARE FUNCTION Num2Date$ (Days)

'----- BASIC Function
DECLARE FUNCTION EndOfForms% (Fld() AS FieldInfoG)
DECLARE FUNCTION Evaluate# (Equation$)
DECLARE FUNCTION WholeWordIn% (Text$, Word$)

'----- BASIC Subs
DECLARE SUB PrintArray (FirstFld%, LastFld%, Form$(), Fld() AS FieldInfoG)
DECLARE SUB SaveField (FldNo%, Form$(), Fld() AS FieldInfoG, BadFld%)
DECLARE SUB UnPackBuffer (FirstFld%, LastFld%, Form$(), Fld() AS ANY)

'----- Recursive procedure used to calculate all fields dependant on an
'      input field
'
SUB CalcFields (StartOfForm, FldNo, Form$(), Fld() AS FieldInfoG)

    '----- The following variables can be preserved between calls
    STATIC Temp$, Calc$, Result$, FPos, Fd, Concated
    STATIC FType, MainType, X, I, X&, X!, X#

    FldName$ = "F" + STR$(FldNo) + " "          'Make a field name token to
                                                '  search for in formulas
    IF LEN(Temp$) = 0 THEN Temp$ = SPACE$(23)   'Make a buffer to hold numbers
   
    FOR N = 1 TO EndOfForms(Fld())              'Check all formulas for
        IF Fld(N).FType = 0 THEN N = N + 1      '  references to the incomming
                                                '  field
        IF INSTR(Form$(N, 2), FldName$) THEN
                                                'If we found a reference,
           Calc$ = Form$(N, 2)                  '  make a copy of the formula

           MainType = Fld(N).FType              'Get type of this field
           IF MainType = Relational THEN MainType = Fld(N).ScratchI

           Result$ = ""                         'Clear previous result string
                                                '  for string concatinations
           Concated = 0                         'Flag for concatinated field

           DO
              FPos = INSTR(Calc$, "F ")         'Look for field references
              IF FPos THEN                      'If we found one, get field #
                 Fd = VAL(MID$(Calc$, FPos + 1))
                                                
                 '----- Process according to the result field's type
                 SELECT CASE MainType
                                                'Numeric fields
                    CASE IntFld, DateFld, EuroDateFld, LongIntFld, SngFld, DblFld, MoneyFld, HScrollFld, VScrollFld
                   
                       FType = Fld(Fd).FType    'Get type of referenced field
                       IF FType = Relational THEN FType = Fld(Fd).ScratchI
                 
                       SELECT CASE FType        'Process acording to type
                                                'Integer types
                          CASE IntFld, DateFld, EuroDateFld, HScrollFld, VScrollFld
                                                'Get value from record buffer
                             X = CVI(MID$(Form$(0, 0), Fld(Fd).Fields, 2))
                                                'Put 0 in blank fields
                             IF X = BlankInt THEN X = 0
                                                'Put number $ in the buffer
                             LSET Temp$ = STR$(X)

                          CASE LongIntFld       'Long integer
                             X& = CVL(MID$(Form$(0, 0), Fld(Fd).Fields, 4))
                             IF X& = BlankLng& THEN X& = 0
                             LSET Temp$ = STR$(X&)

                          CASE SngFld           'Single precision
                             X! = CVS(MID$(Form$(0, 0), Fld(Fd).Fields, 4))
                             IF X! = BlankSng! THEN X! = 0
                             LSET Temp$ = STR$(X!)

                          CASE DblFld, MoneyFld 'Double precision and currency
                             X# = CVD(MID$(Form$(0, 0), Fld(Fd).Fields, 8))
                             IF X# <= BlankDbl# THEN X# = 0
                             LSET Temp$ = STR$(X#)

                          CASE ELSE             'String field type
                                                'Look to left for non blank
                             FOR I = FPos - 1 TO 2 STEP -1
                                IF MID$(Calc$, I, 1) <> " " THEN EXIT FOR
                             NEXT
                                                'Is character a relational operator?
                             SELECT CASE MID$(Calc$, MaxInt%(1, I), 1)
                                                'Yes, put quotes around field $
                                CASE "=", ">", "<"
                                   LSET Temp$ = CHR$(34) + Form$(Fd, 0) + CHR$(34)
                                CASE ELSE       'Otherwise, look to the right
                                                '  for non-blank
                                   I = Blanks%(MID$(Calc$, FPos + 23)) + FPos + 23
                                                'Is character a relational operator?
                                   SELECT CASE MID$(Calc$, MaxInt%(1, I), 1)
                                                'Yes, put quotes around field $
                                      CASE "=", ">", "<"
                                         LSET Temp$ = CHR$(34) + Form$(Fd, 0) + CHR$(34)
                                      CASE ELSE 'Otherwise, just use field $
                                         LSET Temp$ = Form$(Fd, 0)
                                   END SELECT
                             END SELECT

                       END SELECT
                                                'Put the number $ in formula
                       MID$(Calc$, FPos, 23) = Temp$

                    CASE ELSE                   'Result field is a string type
                                                'Are we concatinating?
                       IF LEN(Result$) THEN Concated = -1
                       Result$ = Result$ + RTRIM$(Form$(Fd, 0))
                                                'Look for sting constant in
                                                '  formula (Quoted string)
                       X = INSTR(FPos, Calc$, CHR$(34))
                       IF X THEN
                          I = INSTR(X + 1, Calc$, CHR$(34))
                          Result$ = Result$ + MID$(Calc$, X + 1, I - X - 1)
                       END IF

                       MID$(Calc$, FPos, 23) = Temp$

                 END SELECT
              END IF

           LOOP WHILE FPos > 0                  'Look for next field reference

           '----- If this is a string formula or a copy, the result is
           '      already done so just save the result
           IF LEN(Result$) THEN
                                                
              SELECT CASE MainType
                 CASE DateFld, EuroDateFld
                    IF Concated OR CVI(MID$(Form$(0, 0), Fld(N).Fields, 2)) = BlankInt THEN
                       LSET Form$(N, 0) = Result$
                    END IF
                 CASE ELSE
                    IF Concated OR Null%(MID$(Form$(0, 0), Fld(N).Fields, Fld(N).StorLen)) THEN
                       LSET Form$(N, 0) = Result$
                    END IF
              END SELECT

              SaveField N, Form$(), Fld(), 0

           '----- If this is a numeric calculation, evaluate the formula
           '      and save the result
           ELSE

              X# = Evaluate#(Calc$)             'Evaluate the formula

              IF X# < Fld(N).LowRange OR X# > Fld(N).HiRange THEN X# = 0

              SELECT CASE MainType              'Set the buffer to IEEE format
                 CASE IntFld, DateFld, EuroDateFld
                    MID$(Form$(0, 0), Fld(N).Fields) = MKI$(INT(X#))
                 CASE LongIntFld
                    MID$(Form$(0, 0), Fld(N).Fields) = MKL$(CLNG(X#))
                 CASE SngFld
                    MID$(Form$(0, 0), Fld(N).Fields) = MKS$(CSNG(X#))
                 CASE DblFld, MoneyFld
                    MID$(Form$(0, 0), Fld(N).Fields) = MKD$(X#)
              END SELECT

              UnPackBuffer N, N, Form$(), Fld()

           END IF

           IF N > StartOfForm AND N <= StartOfForm + Fld(StartOfForm).Fields THEN
              PrintArray N, N, Form$(), Fld()   'Print the fields new value
           END IF

           '----- Call ourself to look for formulas that depend on this field
           CalcFields StartOfForm, (N), Form$(), Fld()

        END IF

    NEXT

END SUB

'----- Replaces field names with a padded (23 character) field number token.
'      ie. "F ###                  "
'
SUB Tokenize (Calc$, Fld() AS FieldInfoG) STATIC

    DIM FldPad AS STRING * 23                   'Make a 23 character fixed
                                                '  length string buffer
                                                
    FOR N = 1 TO EndOfForms(Fld())              'Check all field names

        FldName$ = RTRIM$(Fld(N).FldName)       'Trim the field name
        DO
           Start = WholeWordIn%(Calc$, FldName$) 'Do a whole word search for
                                                 '  it in formula "Calc$"
           IF Start THEN                        'If it was found, put field #
              LSET FldPad$ = "F" + STR$(N)      '  in our buffer
                                                'Replace field name with the
                                                '  buffer
              Calc$ = LEFT$(Calc$, Start - 1) + FldPad$ + MID$(Calc$, Start + LEN(FldName$))
           END IF

        LOOP WHILE Start                        'Look for another occurance
                                                '  of the same field name.
    NEXT

END SUB

'----- Does a "Whole Word" search for a sub-string within a string.
'      Considers math operators to be delimiters such that "WORD" will be
'      found in "WORD1+WORD" at position 7
'
FUNCTION WholeWordIn% (Text$, Word$) STATIC

    Ps = 0                                      'Initialize position variable
    DO
       Ok = -1                                  'Assume we will find it
       Ps = INSTR(Ps + 1, Text$, Word$)         'Let BASIC find it initially

       IF Ps THEN                               'If it was found
          X = 0
          Temp = Ps - 1                         'Check left of word
          DO
             IF Temp THEN Temp = ASCII(MID$(Text$, Temp, 1))
             SELECT CASE Temp
                CASE 48 TO 59, 63 TO 255        'If its not a delimiter, set
                   Ok = 0                       '  flag to show its not Ok
                   EXIT DO
                CASE ELSE
             END SELECT
             X = X + 1
             Temp = Ps + LEN(Word$)             'Check right of word
          LOOP WHILE X < 2 AND Temp <= LEN(Text$)
       END IF
    LOOP UNTIL Ok

    WholeWordIn% = Ps                           'Assign the match position

END FUNCTION

