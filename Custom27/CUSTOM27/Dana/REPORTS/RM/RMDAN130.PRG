*!***************************************************************************
*: Program file  : RMDAN130.PRG
*: Program desc. : CUSTOMIZED CREDIT MEMO LOG REPORT 
*: 				         The program is a conversion from 2.6 ==> 2.7
*: Date          : 19/03/2000
*: System        : Aria Advantage Series.
*: Module        : Return Merchandise (RM)
*: Developer     : Wael Mohammed Anwar Hamed (WMA)
*: Reference     : C101787
*:
*!***************************************************************************
*: Calls         : 						
*:    Functions  : lfsrRet
*!***************************************************************************
*: Passed Parameters  : lcParm
*!***************************************************************************
*: Notes   : ....
*!***************************************************************************


*--Variables Declarations & Set Settings 
lnFlag=0

lcSetCen = SET('CENTURY')
SET CENTURY ON
*--END Variables Declarations & Set Settings 

*ahmed find the date location in the fixed filter
*--The Date Variables
lnFltPos=ASCAN(laOgFxflt,'RETLINE.CRDATE')
IF lnFltPos<>0
  lnFltRow=ASUBSCRIPT(laOgFxflt,lnFltPos,1)
  IF !EMPTY(laOgFxflt[lnFltRow,6])
    lnDate   = ASUBSCRIPT(laOgFxflt,ASCAN(laOgFxflt,'RETLINE.CRDATE'),1)
    lcFDate  = ALLTRIM(SUBSTR(LAOGFXFLT[lnDate,6],1 , 10))
    lcTDate  = ALLTRIM(SUBSTR(LAOGFXFLT[lnDate,6],12, 20))
    IF lcFDate='/  /' AND lcTDate='/  /'
      lnFlag=1
    ELSE
      IF lcFDate='/  /'
        lnFlag=2
      ENDIF
    ENDIF
  ELSE
    lnFlag=1
  ENDIF  
ELSE
  lnFlag=1
ENDIF
*--END The Date Variables

*--Relations
SELECT Retline 
SET ORDER TO RETLINES 
SET RELATION TO Crmemo INTO Rethdr 
SET RELATION TO STYLE INTO STYLE ADDITIVE
SELECT STYLE
SET RELATION TO cstygroup INTO Danaconv ADDITIVE
*--END Relations


*--Calculate Total Returns Amount
CREATE TABLE (gcWorkDir+lcRpTmp) (Crmemo C(6))
INDEX ON Crmemo TAG (lcRpTmp)

lnTotAmn=0
SELECT Retline
SCAN For &lcRpExp
  SELECT RetHDR 
  SCAN WHILE RETHDR.CRMEMO=RETLINE.CRMEMO
    SELECT (lcRpTmp)
    =SEEK(RETHDR.CRMEMO,lcRpTmp)
    IF !FOUND()
      SELECT RETHDR   
      lnTotAmn=lnTotAmn+RetHdr.TotCredit
      SELECT (lcRpTmp)
      APPEND BLANK
      REPLACE &lcRpTmp..Crmemo WITH RETHDR.CRMEMO
    ENDIF
  ENDSCAN 
ENDSCAN

SELECT (lcRpTmp)
USE
IF FILE(gcWorkDir+lcRpTmp)
  ERASE (gcWorkDir + lcRpTmp + '.DBF')
  ERASE (gcWorkDir + lcRpTmp + '.CDX')
  ERASE (gcWorkDir + lcRpTmp + '.FPT')
ENDIF

SELECT Retline
*--END Calculate Total Returns Amount


*--Run Report
DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRpExp
*--END Run Report

*--
SET CENTURY &lcSetCen
*--

*-------------------------
*   End  RMDAN130.PRG
*-------------------------

*!****************************************************************************
*: Name      : lfsrRet
*: Developer : Wael Mohammed Anwar Hamed (WMA)
*: Date      : 19/03/2000
*: Purpose   : To Set Order to Indexes When Enter In Range 
*!****************************************************************************
*: Calls     : None
*!****************************************************************************
*: Passed Parameters  : lcParm
*!****************************************************************************
*: Returns            : None 
*!****************************************************************************
*: Example            :  lfsrRet()
*!****************************************************************************
FUNCTION lfsrRet
PARAMETERS lcParm
IF lcParm = 'S'
  SET ORDER TO TAG RETHDR IN RETHDR
  SET ORDER TO TAG CUSTOMER IN CUSTOMER
  GO TOP IN Rethdr  
ENDIF