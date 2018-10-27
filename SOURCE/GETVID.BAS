'********* GetVid.Bas - determines monitor type, declares Shared variables

'Copyright (c) 1990 Brian Giedt

DEFINT A-Z

'$INCLUDE: 'Common.GQS'

REDIM SHARED Font$(0, 0), FontWidth%(0, 0), FontHeight%(0)
REDIM SHARED Tile$(0), AltTile$(0), GPDat%(100), Choice$(0, 0)
REDIM SHARED Font$(95, 1), FontWidth%(95, 1), FontHeight%(1)


GPMonitor% = MultMonitor%
IF GPMonitor% AND &H200 THEN        'Capable of 8514A, use VGA 16 color
  GPDat%(31) = 8
ELSEIF GPMonitor% AND &H20 THEN     'Capable of VGA 16 color
  GPDat%(31) = 8
ELSEIF GPMonitor% AND &H10 THEN     'Capable of VGA mono
  GPDat%(31) = 7
ELSEIF GPMonitor% AND &H8 THEN      'Capable of EGA 16 color
  GPDat%(31) = 5
ELSEIF GPMonitor% AND &H4 THEN      'Capable of EGA mono
  GPDat%(31) = 1
ELSEIF GPMonitor% AND &H2 THEN      'Capable of CGA (Set to 2 color)
  GPDat%(31) = 6
ELSEIF GPMonitor% AND &H1 THEN      'Capable of HERC mono
  CLS
  PRINT "This program will not run on a Hercules monitor!"
  PRINT "Press any key to end.";
  WHILE INKEY$ = "": WEND
  END
END IF

'---- Setup only those elements of GPDat% we need set
GPDat%(23) = 0                      'Boolean to set boldfacing
GPDat%(24) = 90                     'Angle for Italicizing of Text
GPDat%(33) = 0                      'Number of GraphPak Fonts
GPDat%(47) = 2                      'Vertical Spacing for GraphPak Fonts
GPDat%(48) = 3                      'Horizontal Spacing for GraphPak Fonts

GPDat%(57) = 0                      'Background Color (Used by GPaintBox)

GPDat%(71) = 14                     'Lines used by a character font (EGA mode)
GPDat%(72) = &HAA00                 'Default location to save images when
				    'using the GMove2VE routine.

'Note: elements 73 through 80 are assigned below for use with the
'      pulldown and vertical menu systems.

GPDat%(81) = 0                      'Graphics Workshop Fonts available
GPDat%(82) = 0                      'Graphics Workshop Font Active
GPDat%(83) = 6                      'Graphics Workshop Horizontal Font Spacing
GPDat%(84) = 5                      'Graphics Workshop Vertical Font Spacing
GPDat%(85) = 0                      'Graphics Workshop Overall Font Height

GPDat(87) = 7                       'List box scroll bar colors
GPDat(88) = 15
GPDat(89) = 8

CALL InitMouse(GPDat%(73))

