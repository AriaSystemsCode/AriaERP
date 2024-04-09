*:****************************************************************************************
*: Program file  : chgcucod.PRG
*: Program desc. : Main Program.
*: System        : Aria Apparel System (A27).
*: Developer     : ALBERT RAIF - (ALB) Due to B607287,1
*: Date          : 05/22/2003
*: Purpose       : Custom  Program .
*:****************************************************************************************
*: Parameters:
*:****************************************************************************************
*: Returns   : Logical value.       
*:****************************************************************************************
*: B607287,1
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************
*

lcDataDir = GETDIR('','Select Data Directory')
lcSysDir  = SUBSTR(lcDataDir,1,ATC('DBFS',lcDataDir)-1)+'SYSFILES'

DIMENSION laCustStru[2,4]
laCustStru[1,1] = 'COLDACCT'
laCustStru[1,2] = 'C'
laCustStru[1,3] = 5
laCustStru[1,4] = 0

laCustStru[2,1] = 'CNEWACCT'
laCustStru[2,2] = 'C'
laCustStru[2,3] = 5
laCustStru[2,4] = 0
  
DIMENSION laSeqStru[2,4]
laSeqStru[1,1] = 'CAcctCode'
laSeqStru[1,2] = 'C'
laSeqStru[1,3] = 3
laSeqStru[1,4] = 0
  
laSeqStru[2,1] = 'NRECNO'
laSeqStru[2,2] = 'N'
laSeqStru[2,3] = 2
laSeqStru[2,4] = 0

STORE '' TO lcTmpCust,lcSeqCust,lcOldAlias
STORE .F. TO llCuseUse,llSysUse
lcTmpCust = 'CUST'
lcSeqCust = 'SEQUAN'
lcOldAlias = ALIAS()

CREATE CURSOR &lcTmpCust FROM ARRAY laCustStru
INDEX ON COLDACCT+CNEWACCT TAG (lcTmpCust)

CREATE CURSOR &lcSeqCust FROM ARRAY laSeqStru
INDEX ON CAcctCode TAG (lcSeqCust)

lcFileName = lcDataDir+'\CUSTOMER.dbf'
lcSysName = lcSysDir +'\sydflfld.dbf'

IF !FILE(lcFileName) OR !FILE(lcSysName)
  WAIT 'INCORRECT DIRECTORY' WINDOW
  RETURN
ENDIF

IF !USED('CUSTOMER')
  USE &lcFileName. SHARED IN 0
  llCuseUse = .T.
ENDIF

IF !USED('sydflfld')
  llSysUse = .T.
  USE &lcSysName. SHARED IN 0
ENDIF

SELECT CUSTOMER
SET ORDER TO TAG Customer

SEEK('M')
SCAN REST WHILE TYPE = 'M'
  *lcSeek = SUBSTR(ALLTRIM(BTNAME),1,3)
  lcSeek = lfGetCus(IIF(SUBSTR(ALLTRIM(BTNAME),1,4)='THE ',SUBSTR(ALLTRIM(BTNAME),5),BTNAME))
  SELECT (lcSeqCust)
  IF !SEEK(lcSeek)
    APPEND BLANK
    REPLACE CAcctCode WITH lcSeek ,;
            NRECNO    WITH 0
  ELSE
    REPLACE NRECNO WITH NRECNO + 1
  ENDIF
  lcNewAcct = lcSeek + PADL(ALLTRIM(STR(NRECNO)),2,'0')
  INSERT INTO (lcTmpCust) VALUES (CUSTOMER.ACCOUNT,lcNewAcct)
  
  SELECT CUSTOMER
ENDSCAN



SELECT sydflfld
SET ORDER TO TAG Cfld_name
SEEK('ACCOUNT')
llUsed = .F.

SCAN REST WHILE CFLD_NAME = 'ACCOUNT'
  lcAlias = ALLTRIM(sydflfld.cfile_nam)
  IF sydflfld.cUpGrdLvl = 'S'
    LOOP
  ELSE
    lcFileName = lcDataDir  +'\' + lcAlias +'.DBF'
  ENDIF
  IF FILE(lcFileName)
    WAIT lcFileName WINDOW NOWAIT
    IF !USED(lcAlias)
      USE &lcFileName. SHARED IN 0
      llUsed = .T.
    ENDIF
    SELECT (lcAlias)
    REPLACE ALL ACCOUNT WITH &lcTmpCust..CNEWACCT FOR;
            SEEK(ACCOUNT,lcTmpCust)
    IF llUsed
      llUsed = .F.
      SELECT (lcAlias)
      USE
    ENDIF
  ENDIF
ENDSCAN

SELECT (lcTmpCust)
USE
SELECT (lcSeqCust)
USE

IF llCuseUse
  SELECT CUSTOMER
  USE
ENDIF

IF llSysUse
  SELECT sydflfld
  USE 
ENDIF
IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF


*!*************************************************************
*! Name      : lfGetCus
*! Developer : Albert Raif 
*! Date      : 05/22/2003
*! Purpose   : Function to Get the firest 3 char. from customer Name
*!           : excluding spaces and special char.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  lfGetCus()
*!*************************************************************

FUNCTION lfGetCus
PARAMETER lcCustName
PRIVATE lcReturn,lnCharPos,lcChar
STORE '' TO lcReturn,lcChar
STORE 1 TO lnCharPos

DO WHILE LEN(lcReturn) < 3 AND lnCharPos <= LEN(lcCustName)
  lcChar = SUBSTR(lcCustName,lnCharPos,1)
  IF  BETWEEN(ASC(lcChar),ASC('A'),ASC('Z')) OR BETWEEN(ASC(lcChar),ASC('A'),ASC('Z'));
      OR lcChar = '&'
    lcReturn = lcReturn + lcChar
  ENDIF
  lnCharPos = lnCharPos + 1
ENDDO

RETURN IIF(EMPTY(lcReturn),SUBSTR(lcCustName,1,3),lcReturn)
