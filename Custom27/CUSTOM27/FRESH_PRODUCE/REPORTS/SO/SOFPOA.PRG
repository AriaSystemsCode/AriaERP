****************************************************************************
*: Program file      : SOFPOA.PRG
*: Program desc.     : FRESH PRODUCE AUTOMATIC ORDER APPROVAL
*: System            : Aria Apparel System (A27).
*: Module            : (SO)
*: Developer         : TAK
*: Date              : 10/27/2000
*:**************************************************************************

WAIT WINDOW 'Start Collecting Orders...' NOWAIT

*--Date filter.
lcDateFiltr = ".T."
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ENTERED'),1)
IF lnPosition > 0 AND !EMPTY(SUBSTR(laOGFxFlt[lnPosition,6],1,2))
  =ACOPY(laOGFxFlt,laOrdFlt,1,7)
  DIME laOrdFlt[1,7]
  lcDateFiltr = gfGenFlt('laOrdFlt',.T.)
  lcDateFiltr = IIF(EMPTY(lcDateFiltr),".T.",lcDateFiltr)

  IF OCCURS('ORDHDR.ENTERED',lcDateFiltr) <> 0
    IF lcDateType <> 'E'
      lcDateFiltr = STRTRAN(lcRpExp,'ORDHDR.ENTERED','ORDHDR.'+IIF(lcDateType = 'C','COMPLETE','START'))
    ENDIF
  ENDIF
ENDIF


*-- Check for Accounts was selected.
llActSel  = .F.
lcActFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ACCOUNT'),1)
IF lnPosition > 0
  lcActFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcActFile) AND USED(lcActFile)
    SELECT (lcActFile)
    GOTO TOP
    llActSel = !EOF()
  ENDIF  
ENDIF


*-- Check for orders was selected.
llOrdtSel  = .F.
lcOrdFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ORDER'),1)
IF lnPosition > 0
  lcOrdFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcOrdFile) AND USED(lcOrdFile)
    SELECT (lcOrdFile)
    GOTO TOP
    llOrdtSel = !EOF()
  ENDIF  
ENDIF



lcTmpFile = gfTempName()
CREATE TABLE(gcWorkDir+lcTmpFile);
    (ORDER C(6),ACCOUNT C(5),NOTE1 C(30),ENTERED D(8),START D(8),COMPLETE D(8),OPEN N(8), OPENAMT N(13,2) )
INDEX ON Order+ACCOUNT TAG (lcTmpFile)


lcMainFlt = "ORDHDR.Status = 'H' AND CUSTOMER.Status = 'A'"

SELECT ORDHDR
SET RELATION TO 'M'+Account INTO CUSTOMER

*--Collecting Data.
IF !llActSel AND !llOrdtSel
  SELECT ORDHDR
  GO TOP
  SCAN FOR &lcMainFlt AND &lcDateFiltr
    WAIT WINDOW 'Order : '+Order NOWAIT
    =lfUpdTmp()
  ENDSCAN
ELSE
  IF llActSel
    lcOrNmFlt = IIF(llOrdtSel,"SEEK(Order,lcOrdFile)",".T.")
    SELECT ORDHDR
    SET ORDER TO TAG Ordacct
    SELECT (lcActFile)  
    SCAN
      SELECT ORDHDR
      =SEEK(&lcActFile..Account)
      SCAN WHILE ACCOUNT = &lcActFile..Account FOR &lcMainFlt AND &lcDateFiltr AND &lcOrNmFlt
        WAIT WINDOW 'Order : '+Order NOWAIT
        =lfUpdTmp()
      ENDSCAN
    ENDSCAN
    SELECT ORDHDR
    SET ORDER TO TAG Ordhdr
  ELSE
    SELECT (lcOrdFile)
    SCAN
      SELECT ORDHDR
      =SEEK('O'+&lcActFile..Order)
      SCAN WHILE cOrdType+Order = 'O'+&lcActFile..Order FOR &lcMainFlt AND &lcDateFiltr
        WAIT WINDOW 'Order : '+Order NOWAIT
        =lfUpdTmp()
      ENDSCAN
    ENDSCAN
  ENDIF
ENDIF
WAIT CLEAR


SELECT (lcTmpFile)
GO TOP
IF !EOF()

  = gfDispRe(lcRpForm)

  *--Are you sure you want to Approve the printed Order(s)? [Y/N]
  IF gfModalGen("QRM32900B32000",'DIALOG') = 1
    SET DEVI TO SCREEN
    =lfApprove()
  ENDIF

ELSE
  *--No records was selected, Nothing to print!!
  SET DEVI TO SCREEN
  =gfModalGen('TRM00052B00000','DIALOG')
ENDIF

SELECT ORDHDR
SET RELATION TO

IF USED(lcTmpFile)
  SELECT (lcTmpFile)
  USE
  ERASE(gcWorkDir+lcTmpFile+'.DBF')
  ERASE(gcWorkDir+lcTmpFile+'.CDX')
ENDIF

RETURN

********************************************
FUNCTION lfUpdTmp

SELECT (lcTmpFile)
APPEND BLANK
REPLACE ORDER    WITH ORDHDR.Order,; 
        ACCOUNT  WITH ORDHDR.Account,;
        NOTE1    WITH CUSTOMER.BTNAME,;
        ENTERED  WITH ORDHDR.Entered,;
        START    WITH ORDHDR.START,;
        COMPLETE WITH ORDHDR.COMPLETE,;
        OPEN     WITH ORDHDR.Open,;
        OPENAMT  WITH ORDHDR.OpenAmt
RETURN


********************************************
FUNCTION lfApprove

=gfOpenFile(gcDataDir+'PikTkt','PikTkt','SH')
=gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')

llApproved = .F.
SELECT (lcTmpFile)
SCAN
  WAIT WINDOW 'Approving Order :'+Order NOWAIT
  IF SEEK('O'+Order,'ORDHDR') AND ORDHDR.Status='H'

    llApproved = .T.
    lcUntSin = ' '
    lcExRSin = gfGetExSin(@lcUntSin,OrdHdr.cCurrCode)
    lnOpen = CEILING(ORDHDR.OPENAMT) &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
    SELECT Customer
    REPLACE CrAvail WITH CrAvail - lnOpen
    SELECT ORDHDR
    REPLACE STATUS     WITH 'O'           ,;
            DECL_CODE  WITH SPACE(2)      ,;
            APPRAMT    WITH CEILING(OPENAMT)
  

    = gfDoTriger('SOORAPR',PADR('APR_OPN',10))

  ENDIF
ENDSCAN

IF llApproved
  =gfModalGen("QRM32901B00000",'DIALOG')
ENDIF

WAIT CLEAR
RETURN
