DEFINT A-Z

'GetGMP        Loads a .GMP bit mapped image into a PUT array

'example:
'         REDIM Array(0)
'         CALL GetGMP(FileName$, Array(), ErrCode)
'         PUT (x, y), Array
      
'---- ASM subs
DECLARE SUB FClose (Handle)
DECLARE SUB FGetA (Handle, SEG Element AS ANY, Bytes&)
DECLARE SUB FOpen (FileName$, Handle)

'---- ASM functions
DECLARE FUNCTION DOSError ()
DECLARE FUNCTION FileSize& (FileName$)

SUB GetGMP (FileName$, Array(), ErrCode)

  GMPBytes& = FileSize&(FileName$)
  IF GMPBytes& = -1 THEN
    ErrCode = -1
    EXIT SUB
  END IF

  REDIM Array(1 TO GMPBytes& \ 2)
  FOpen FileName$, Handle
  IF DOSError THEN
    ErrCode = -1
    EXIT SUB
  ELSE
    FGetA Handle, SEG Array(1), GMPBytes&
    FClose Handle
  END IF

END SUB

