'----- Define the Field Information TYPE
TYPE FieldInfoG
     Fields    AS INTEGER              '0
     Row       AS INTEGER              '1
     LCol      AS INTEGER              '2
     RCol      AS INTEGER              '3
     StorLen   AS INTEGER              '4
     FType     AS INTEGER              '5
     RelFile   AS STRING * 8           '6
     RelFld    AS INTEGER              '10
     Indexed   AS INTEGER              '11
     FldName   AS STRING * 8           '12
     Decimals  AS INTEGER              '16
     RelHandle AS INTEGER              '17
     Protected AS INTEGER              '18
     ScratchI  AS INTEGER
     LowRange  AS DOUBLE
     HiRange   AS DOUBLE
     ScratchS  AS STRING * 8
     Value     AS INTEGER
END TYPE

CONST FrmWdth = 66
CONST MaxFields = 200

'----- Define Field Type constants
CONST StrFld = 1
CONST PropStrFld = 2       '23
CONST UCaseStrFld = 3      '2
CONST NumericStrFld = 4    '16
CONST NotesFld = 5         '17
CONST ScrollFld = 6

CONST LogicalFld = 7       '14
CONST IntFld = 8           '3
CONST LongIntFld = 9       '18
CONST SngFld = 10          '4
CONST DblFld = 11          '5

CONST MoneyFld = 12        '10
CONST DateFld = 13         '6
CONST EuroDateFld = 14     '19
CONST PhoneFld = 15        '7
CONST ZipFld = 16          '9
CONST SoSecFld = 17        '8

CONST Relational = 18      '11
CONST MultChAFld = 19      '22
CONST MouseFld = 20        '25
CONST PButton = 21         '24
CONST HScrollFld = 22
CONST VScrollFld = 23


CONST VirtualFld = 12      '12
CONST RecNoFld = 16        '20
CONST TotRecsFld = 21      '21
CONST SequFld = 15         '15
CONST MultChFld = 19       '13

