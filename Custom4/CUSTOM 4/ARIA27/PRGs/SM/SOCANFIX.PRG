*:**************************************************************************
*: Program file  : SOCANFIX
*: Program desc. : Fix program to fill the new added field (Price) in OrdCanLn form OrdLine.
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 06/24/2001
*: Reference     : B604500
*:**************************************************************************
*: Procedures    : lpCreatFil
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*
PARAMETERS lcDataDir

PRIVATE lcDataDir , llEmptySo , lnLength
lcDataDir = ""
lcDataDir = GETDIR('','Select data directory')

IF EMPTY(lcDataDir)
  RETURN
ENDIF

llEmptySo = .F.
llEmptySo = FILE(lcDataDir+'ORDCANLN.DBF') AND FILE(lcDataDir+'ORDLINE.DBF')

lnLength = LEN(lcDataDir)
IF !SUBSTR (lcDataDir,lnLength)='\'
  lcDataDir=lcDataDir+'\'
ENDIF
  
*--Open ORDCANLN.
IF USED('ORDCANLN')
  USE IN ORDCANLN
ENDIF  
USE (lcDataDir+'ORDCANLN') IN 0 ORDER TAG ORDCANLN

*--Start updating
llEmptySo = llEmptySo AND SEEK('O')

*--Update Sales Orders
WAIT WINDOW 'Start updating....' NOWAIT
IF llEmptySo
  DO lpUpdSo
ENDIF

*--Close files
IF USED('ORDCANLN')
  USE IN ORDCANLN
ENDIF
IF USED('ORDLINE')
  USE IN ORDLINE
ENDIF
WAIT CLEAR
*--End of SOCANFIX

*!**************************************************************************
*! Name      : lpUpdSo
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/24/2001
*! Purpose   : Fill the new added field (Price) in OrdCanLn from OrdLine.
*!**************************************************************************
*! Example   : DO lpUpdSo
*!**************************************************************************
*
PROCEDURE lpUpdSO

IF !USED('ORDLINE')
  USE (lcDataDir+'ORDLINE') IN 0 ORDER TAG ORDLINE
ENDIF

lcOldDel = SET('DELETE')
SET DELETE OFF
SELECT ORDCANLN
=SEEK('O')
SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' FOR !DELETED() AND EMPTY(PRICE)
  lcKey = cordtype+order+STR(lineno,6)
  IF SEEK(lcKey,'ORDLINE')
     WAIT WINDOW 'Updating Sales order # : ' +ORDER NOWAIT
     REPLACE OrdCanLn.Price WITH OrdLine.Price
  ENDIF
ENDSCAN

*--Restore delete status
SET DELETE &lcOldDel
*--End of lpUpdSo
