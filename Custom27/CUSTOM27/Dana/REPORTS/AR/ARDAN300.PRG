*!***************************************************************************
*: Program file  : ARDAN300.PRG
*: Program desc. : CUSTOMIZED SHIPPING LOG REPORT 
*: 				   The program is a conversion from 2.6 ==> 2.7
*: Date          : 14/03/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Wael Mohammed Anwar Hamed (WMA)
*: Reference     : C101777
*:
*!***************************************************************************
*: Calls         : 						
*:    Functions  : lfsrInv
*!***************************************************************************
*: Passed Parameters  : lcParm
*!***************************************************************************
*: Notes   : ....
*!***************************************************************************
*B804534,1 11/18/2001 SSE Fix bug in Frx where Grand Total always resets at end of page.
*B804534,1                All modifications is in FRX 

*--Variables Declarations & Set Settings 
lnFlag=0
lcSetCen = SET('CENTURY')
SET CENTURY ON
*--END Variables Declarations & Set Settings 


*--The Date Variables
lnFltPos=ASCAN(laOgFxflt,'INVLINE.INVDATE')
IF lnFltPos<>0
  lnFltRow=ASUBSCRIPT(laOgFxflt,lnFltPos,1)
  IF !EMPTY(laOgFxflt[lnFltRow,6])
    lnDate   = ASUBSCRIPT(laOgFxflt,ASCAN(laOgFxflt,'INVLINE.INVDATE'),1)
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


*--Fixed Filter When Invoice Status Is VALID
lcRpExp = IIF(EMPTY(lcRpExp), ".T." , lcRpExp) + ".AND. InvHdr.Status <> 'V' "
*--END Fixed Filter When Invoice Status Is VALID


*--Relations
SELECT Invline
SET ORDER TO INVLINES
SET RELATION TO invoice INTO Invhdr
SET RELATION TO STYLE INTO STYLE ADDITIVE
SELECT STYLE
SET RELATION TO cstygroup INTO Danaconv ADDITIVE
*--END Relations


*--Calculate Total Invoices Amount
CREATE TABLE (gcWorkDir+lcRpTmp) (INVOICE C(6))
INDEX ON INVOICE TAG (lcRpTmp)

lnTotAmn=0
SELECT Invline
SCAN For &lcRpExp
  SELECT INVHDR 
  SCAN WHILE INVHDR.INVOICE=INVLINE.INVOICE
    SELECT (lcRpTmp)
    =SEEK(INVHDR.INVOICE,lcRpTmp)
    IF !FOUND()
      SELECT INVHDR   
      lnTotAmn=lnTotAmn+InvHdr.Totalchg 
      SELECT (lcRpTmp)
      APPEND BLANK
      REPLACE &lcRpTmp..INVOICE WITH INVHDR.INVOICE
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

SELECT Invline
*--END Calculate Total Invoices Amount


*--Run Report
DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRpExp
*--END Run Report


*--
SET CENTURY &lcSetCen
*--


*-------------------------
*   End  ARDAN300.PRG
*-------------------------


*!****************************************************************************
*: Name      : lfsrInv
*: Developer : Wael Mohammed Anwar Hamed (WMA)
*: Date      : 14/03/2000
*: Purpose   : To Set Order to Indexes When Enter In Range 
*!****************************************************************************
*: Calls     : None
*!****************************************************************************
*: Passed Parameters  : lcParm
*!****************************************************************************
*: Returns            : None 
*!****************************************************************************
*: Example            :  lfsrInv()
*!****************************************************************************
FUNCTION lfsrInv
PARAMETERS lcParm
  IF lcParm = 'S'
    SET ORDER TO TAG INVHDR IN INVHDR
    SET ORDER TO TAG CUSTOMER IN CUSTOMER
    GO TOP IN Invhdr  
  ENDIF
