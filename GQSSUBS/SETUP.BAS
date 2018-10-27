'********* Setup.Bas - determines monitor type, declares Shared variables

'$INCLUDE: 'Common.Bi'

  REDIM SHARED GPDat%(100), Choice$(0, 0)

  MonType% = MultMonitor%

  IF MonType AND &H200 THEN       'Capable of 8514A, use VGA 16 color
    GPDat%(31) = 8
  ELSEIF MonType% AND &H20 THEN   'Capable of VGA 16 color
    GPDat%(31) = 8
  ELSEIF MonType% AND &H10 THEN   'Capable of VGA mono
    GPDat%(31) = 7
  ELSEIF MonType% AND &H8 THEN    'Capable of EGA 16 color
    GPDat%(31) = 5
  ELSEIF MonType% AND &H4 THEN    'Capable of EGA mono
    GPDat%(31) = 1
  ELSEIF MonType% AND &H2 THEN    'Capable of CGA (Set to 2 color)
    GPDat%(31) = 6
  ELSEIF MonType% AND &H1 THEN    'Capable of HERC mono
    GPDat%(31) = 2
  END IF

  GPDat%(76) = 0 + 7 * 256        'ListBox text color
  GPDat%(78) = 15 + 0 * 256       'ListBox highlight color
                                  'ListBox scrollbar colors:
  GPDat%(87) = 7                  'Scrollbar button color
  GPDat%(88) = 15                 'Scrollbar highlight
  GPDat%(89) = 8                  'Scrollbar shaded portion
  GPDat%(90) = -1                 'Use field colors for multiple-choice fields
  GPDat%(94) = 0                  'Message box text color
  GPDat%(95) = 7                  'Message box background color
  GPDat%(96) = 15                 'Message box highlight color
  GPDat%(97) = 8                  'Message box shade color
  GPDat%(98) = 7                  'Scrollbar, sliding portion
  GPDat%(99) = 7                  'Number of list items before scrollbar

  CALL InitMouse(GPDat%(73))

