*!********************************************************************************************
*: Program file  : SMUPFLDS.PRG
*: Program desc. : Fix program to fill the new added fileds (Style,Account,store,dyelot) in 
*:               : ORDCANLN file from their originam files.
*: NOTE          : As these fileds are newly added, they are empty for all Caneclled Sales
*:               : orders , Purchase Orders and Cutink Tickets, so we need to update them.
*:         System: ARIA APPAREL SERIES
*:         Module: SM
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           Date: 11/16/2000
*!********************************************************************************************
*! Refer to (B604019)
*!********************************************************************************************

PARAMETERS lcDataDir

PRIVATE lcDataDir
IF TYPE('lcDataDir') # 'C' OR EMPTY(lcDataDir)
  lcDataDir = ""
  lcDataDir = GETDIR('','Select data directory')
ENDIF

STORE .F. TO llEmptySo,llEmptyPo,llEmptyCt
llEmptySo = FILE(lcDataDir+'ORDCANLN.DBF') AND FILE(lcDataDir+'ORDLINE.DBF')
llEmptyPo = FILE(lcDataDir+'POSLN.DBF')
llEmptyCt = FILE(lcDataDir+'CUTTKTL.DBF')

IF EMPTY(lcDataDir) OR !llEmptySo 
  RETURN
ENDIF

lnLen=LEN(lcDataDir)
IF !SUBSTR (lcDataDir,lnLen)='\'
  lcDataDir=lcDataDir+'\'
ENDIF  
*--Open ORDCANLN.
IF USED('ORDCANLN')
  USE IN ORDCANLN
ENDIF  
USE (lcDataDir+'ORDCANLN') IN 0 ORDER TAG ORDCANLN
SELECT ORDCANLN
DIME laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cordtype+order+STR(lineno,6)'
laIndex[1,2] = 'OrdCanLn'
=AFIELDS(laFields)
lnFields = ALEN(laFields,1)
IF ASCAN(laFields,'STYLE') = 0
  DIMENSION laFields[lnFields+1,4]
  laFields[lnFields+1,1]='STYLE'
  laFields[lnFields+1,2]='C'
  laFields[lnFields+1,3]=19
  laFields[lnFields+1,4]=0
ENDIF
IF ASCAN(laFields,'STORE') = 0
  lnFields = ALEN(laFields,1)
  DIMENSION laFields[lnFields+1,4]
  laFields[lnFields+1,1]='STORE'
  laFields[lnFields+1,2]='C'
  laFields[lnFields+1,3]=8
  laFields[lnFields+1,4]=0
ENDIF
IF ASCAN(laFields,'ACCOUNT') = 0
  lnFields = ALEN(laFields,1)
  DIMENSION laFields[lnFields+1,4]
  laFields[lnFields+1,1]='ACCOUNT'
  laFields[lnFields+1,2]='C'
  laFields[lnFields+1,3]=5
  laFields[lnFields+1,4]=0
ENDIF
IF ASCAN(laFields,'DYELOT') = 0
  lnFields = ALEN(laFields,1)
  DIMENSION laFields[lnFields+1,4]
  laFields[lnFields+1,1]='DYELOT'
  laFields[lnFields+1,2]='C'
  laFields[lnFields+1,3]=10
  laFields[lnFields+1,4]=0
ENDIF
lcTmpName = ("X"+SUBSTR(SYS(2015),4))
CREATE TABLE (lcDataDir+lcTmpName) FROM ARRAY  laFields 
SELECT ORDCANLN 
SCAN
  WAIT WINDOW 'Transaction # '+ORDER NOWAIT
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTmpName) FROM MEMVAR
ENDSCAN
WAIT CLEAR 
USE IN (lcTmpName)
USE (lcDataDir+lcTmpName) IN 0 EXCLUSIVE 
SELECT  (lcTmpName) 
INDEX ON &laIndex[1,1] TAG &laIndex[1,2] ADDITIVE
REINDEX  

USE IN (lcTmpName)
USE IN ORDCANLN
ERASE  (lcDataDir+'ORDCANLN.DBF')
ERASE  (lcDataDir+'ORDCANLN.CDX')
RENAME (lcDataDir+lcTmpName+'.DBF') TO (lcDataDir+'ORDCANLN.DBF')
RENAME (lcDataDir+lcTmpName+'.CDX') TO (lcDataDir+'ORDCANLN.CDX')
USE lcDataDir+"ORDCANLN.dbf" IN 0 ORDER ORDCANLN
SELECT ORDCANLN

*--Start updating
llEmptySo = llEmptySo AND SEEK('O')
llEmptyPo = llEmptyPo AND SEEK('P')
llEmptyCT = llEmptyCt AND SEEK('T')

*==Update missing fields==
*--Update Sales Orders
WAIT WINDOW 'Start updating....' NOWAIT
IF llEmptySo
  DO lpUpdSo
ENDIF
*--Update Cutting Tickets.
IF llEmptyCt
  DO lpUpdCT
ENDIF
*--Update Purchased Orders.
IF llEmptyPo
  DO lpUpdPo
ENDIF
*--Close files
IF USED('ORDCANLN')
  USE IN ORDCANLN
ENDIF
IF USED('ORDLINE')
  USE IN ORDLINE
ENDIF
IF USED('CUTTKTL')
  USE IN CUTTKTL
ENDIF
IF USED('POSLN')
  USE IN POSLN
ENDIF

WAIT CLEAR
*--End of SMUPFLDS

*!********************************************************************************************
*: Program file  : lpUpdSo.PRG
*: Program desc. : Fill the new added fileds (Style,Account,store,dyelot) in 
*:               : ORDCANLN file from ORDLINE.
*: NOTE          : As these fileds are newly added, they are empty for all Caneclled Sales
*:               : orders , Purchase Orders and Cutink Tickets, so we need to update them.
*:         System: ARIA APPAREL SERIES
*:         Module: SM
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           Date: 11/16/2000
*!********************************************************************************************
*! Refer to (B604019)
*!********************************************************************************************
PROCEDURE lpUpdSO

IF !USED('ORDLINE')
  USE (lcDataDir+'ORDLINE') IN 0 ORDER TAG ORDLINE
ENDIF

lcOldDel = SET('DELETE')
SET DELETE OFF
SELECT ORDCANLN
=SEEK('O')
SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' FOR !DELETED() AND EMPTY(STYLE)
  lcKey = cordtype+order+STR(lineno,6)
  IF SEEK(lcKey,'ORDLINE')
     WAIT WINDOW 'Updating Sales order # : ' +ORDER NOWAIT
     REPLACE ORDCANLN.STYLE   WITH ORDLINE.STYLE,;
             ORDCANLN.ACCOUNT WITH ORDLINE.ACCOUNT,;
             ORDCANLN.STORE   WITH ORDLINE.STORE,;
             ORDCANLN.DYELOT  WITH ORDLINE.DYELOT
  ENDIF
ENDSCAN
*--Restore delete status
SET DELETE &lcOldDel

*--End of lpUpdSo

*!********************************************************************************************
*: Program file  : lpUpdCT.PRG
*: Program desc. : Fill the new added fileds (Style,dyelot) in 
*:               : ORDCANLN file from CUTTKTL.
*: NOTE          : As these fileds are newly added, they are empty for all Caneclled Sales
*:               : orders , Purchase Orders and Cutink Tickets, so we need to update them.
*:         System: ARIA APPAREL SERIES
*:         Module: SM
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           Date: 11/16/2000
*!********************************************************************************************
*! Refer to (B604019)
*!********************************************************************************************
PROCEDURE lpUpdCT

IF !USED('CUTTKTL')
  USE (lcDataDir+'CUTTKTL') IN 0 ORDER TAG Cutlin
ENDIF

SELECT ORDCANLN
=SEEK('T')
SCAN REST WHILE cordtype+order+STR(lineno,6) = 'T' FOR !DELETED() AND EMPTY(STYLE)
  lcKey = order
  IF SEEK(lcKey,'CUTTKTL')
    SELECT CUTTKTL 
    LOCATE REST WHILE cuttkt+style+STR(lineno,6)+trancd = lcKey FOR STR(lineno,6) = STR(ORDCANLN.LINENO,6)
    IF FOUND()
      WAIT WINDOW 'Updating Cutting Ticket # : ' +CUTTKT NOWAIT
      REPLACE ORDCANLN.STYLE   WITH CUTTKTL.STYLE,;
              ORDCANLN.DYELOT  WITH CUTTKTL.DYELOT
    ENDIF           
    SELECT ORDCANLN     
  ENDIF
ENDSCAN
*--End of lpUpdCt

*!********************************************************************************************
*: Program file  : lpUpdPo.PRG
*: Program desc. : Fill the new added fileds (Style,dyelot) in 
*:               : ORDCANLN file from POSLN.
*: NOTE          : As these fileds are newly added, they are empty for all Caneclled Sales
*:               : orders , Purchase Orders and Cutink Tickets, so we need to update them.
*:         System: ARIA APPAREL SERIES
*:         Module: SM
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           Date: 11/16/2000
*!********************************************************************************************
*! Refer to (B604019)
*!********************************************************************************************
PROCEDURE lpUpdPo

IF !USED('POSLN')
  USE (lcDataDir+'POSLN') IN 0 ORDER TAG POSLN
ENDIF

SELECT ORDCANLN
=SEEK('P')
SCAN REST WHILE cordtype+order+STR(lineno,6) = 'P' FOR !DELETED() AND EMPTY(STYLE)
  lcKey = cordtype+order
  IF SEEK(lcKey,'POSLN')
    SELECT POSLN
    LOCATE REST WHILE cstytype+po+style+STR(lineno,6)+trancd = lcKey FOR STR(lineno,6) = STR(ORDCANLN.LINENO,6)
    IF FOUND()
      WAIT WINDOW 'Updating Purchase Order # : ' +PO NOWAIT
      REPLACE ORDCANLN.STYLE   WITH POSLN.STYLE,;
              ORDCANLN.DYELOT  WITH POSLN.DYELOT
    ENDIF           
    SELECT ORDCANLN     
  ENDIF
ENDSCAN
*--End of lpUpdPo