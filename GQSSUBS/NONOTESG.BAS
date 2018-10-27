DEFINT A-Z
DECLARE SUB GQEditCursor (X%, Y%, CursorLength%, CursorOn%, CursorTime%, Clr%)

'$INCLUDE: 'QEditype.BI'

'$INCLUDE: 'Common.BI'

SUB GAPrint0 (Array$(), Start, NumEls, FirstChar, NumChars, Colr)
END SUB

SUB QEdit (Array$(), X$, Action%, Ed AS EditInfo) STATIC

  X$ = INKEY$
  CALL ButtonPress(1, Stat, Ed.Presses, X, Y)
  Ed.MCol = X \ 8 + 1
  Ed.MRow = Y \ GPDat(71) + 1

END SUB

SUB TextCursor (X, Y, CursorLength, CursorOn, CursorTime, Clr) STATIC
END SUB

