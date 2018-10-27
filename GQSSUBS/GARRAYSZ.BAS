DEFINT A-Z

'by Phil Cramer
'Copyright (c) 1992 Crescent Software

DECLARE FUNCTION EGAMem ()
DECLARE FUNCTION GArraySize& (x1, y1, x2, y2)

FUNCTION GArraySize& (x1, y1, x2, y2)

'Determines the number of bytes required to hold a graphics image for
'BASIC's GET statement in ANY graphics screen mode

CALL GetVMode(Mode, Page, PageSize&, Rows, Columns)

BPPPP = 1                                'Bits-per-pixel-per-plane
Planes = 1                               'Screens 2 & 11

SELECT CASE Mode                         'Set values according to screen mode
 
  CASE 4                                 'Screen 1
    BPPPP = 2
  CASE 13                                'Screen 7
    Planes = 4
  CASE 14                                'Screen 8
    Planes = 4
  CASE 15                                'Screen 10
    Planes = 2
  CASE 16                                'Screen 9
    Planes = 2
    IF EGAMem > 1 THEN Planes = 4
  CASE 18                                'Screen 12
    Planes = 4
  CASE 19                                'Screen 13
    BPPPP = 8

END SELECT
GArraySize& = 4 + INT(((x2 - x1 + 1) * (BPPPP) + 7) / 8) * Planes * ((y2 - y1) + 1)

END FUNCTION

