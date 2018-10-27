TYPE FormInfoG
   StartEl AS INTEGER          'Starting (base) element of the current form
   FldNo AS INTEGER            'Current field number
   PrevFld AS INTEGER          'Previous field (different from above only
                               '  when first moving to a new field.
   FldEdited AS INTEGER        'Specifies that a field has been changed (-1)
   KeyCode AS INTEGER          'ASCII value of the last key pressed. Negative
                               '  for extended keys ie. F1 = -59
   TxtPos AS INTEGER           'Cursor position relative to current field.
   InsStat AS INTEGER          'Current insertion mode status (-1) insert ON)
   Presses AS INTEGER          'Number of mouse presses since last press.
   MRow AS INTEGER             'Mouse row # at last press
   MCol AS INTEGER             'Mouse column # at last press
   Mx AS INTEGER               'Mouse x
   My AS INTEGER               'Mouse y
   Button AS INTEGER           'Current mouse button
   DoingMult AS INTEGER        'True if handling a multiple choice field
   Edited AS INTEGER           'True if anything on the form has changed.
END TYPE

CONST UCase = 1          '1
CONST Intgr = 2          '2
CONST FloatP = 3         '3
CONST Money = 4          '4
CONST MultFld = 5        '5
CONST Propr = 6          '6
CONST MFld = 20
CONST ButtonFld = 21     '7
CONST ScrollBarFld = 22

