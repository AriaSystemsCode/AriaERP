LCFILE = ALIAS()
IF  .NOT. USED('NOTEPAD')
   SELECT 0
   USE (GCDATADIR+'NOTEPAD')
ENDIF
SELECT NOTEPAD
SET ORDER TO NOTEPAD
IF SEEK('T'+'BANK')
store 'T' to lcnotepad
store mnotes to lmmnotes
   SELECT &lcFile
   RETURN .T.
ELSE
store 'F' to lcnotepad
store '' to lmmnotes
   SELECT &lcFile
   RETURN .F.
ENDIF
RETURN
*
