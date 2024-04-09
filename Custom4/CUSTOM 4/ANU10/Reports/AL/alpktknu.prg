*:******************************************************************************************
*:Program file : alpktknu.PRG
*:Program desc. : Pick Ticket Form for A'nue
*:System : Sales order allocation
*:Developer : Tarek Mohamed Ibrahim
*:Date : 06/21/2007
*:Check ticket # : T20070112.0051
*: Tracking # 200803
*:******************************************************************************************

PRIVATE lnPageLns
lnPageLns = 9

STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4,;
            lcCustPoVl

lcHdrInfo = loOgScroll.gfTempName()
lcSbTotTtl = SPACE(10)+'C O N T I N U E D ...'+SPACE(17)+'S U B T O T A L :    '

STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

DECLARE  laBtAddr[3,1] , laStAddr[3,1]
STORE "" TO laBtAddr , laStAddr

SELECT (lcTmpOrdL)
INDEX ON PIKTKT+ORDER+CGRUPDETAL+STYLE TAG (lcTmpOrdL+'X')

LOCATE
IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
ELSE

  SELECT (lcTmpOrdL)
  DELETE FOR cGrupDetal = 'H'
  
  *- SET PAGE WITH 9 LINES EACH
  =lfSetPages()
  
  =lfEvalSegs()
  llNoRec = .F.

  llPrinter = (SET('DEVICE')=='PRINT')

  SELECT (lcTmpOrdL)
  DO gfDispRe WITH EVAL('lcFormName')
  
  *- set the print flag to true if printing
ENDIF

SET RELATION OFF INTO CUSTOMER
*-- end of main report code.


*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 06/11/2000
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs
PARAMETER lcReturn

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF
*-- end of lfEvalSegs.



*:**************************************************************************
*:* Name        : lfShipTo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/2007
*:* Purpose     : Get shipping address
*:***************************************************************************
*:* Called from : frx
*:***************************************************************************
FUNCTION lfShipTo
LOCAL lnSlct
lnSlct = SELECT()

  SELECT OrdHdr
  IF Alt_ShpTo
  	XSTNAME  = OrdHdr.STName   
    XSTADDR1 = OrdHdr.cAddress1
    XSTADDR2 = OrdHdr.cAddress2
    XSTADDR3 = TRIM(OrdHdr.cAddress3) + ' ' + TRIM(OrdHdr.cAddress4) + ' ' + TRIM(OrdHdr.cAddress5)
    
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT Customer
    lnRecNo = RECNO()       && Variable to hold the record number.
    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    = gfGetAdr('CUSTOMER' , '' , '' , '' , @laStAddr)
    XSTADDR1 = laStAddr[1]
    XSTADDR2 = laStAddr[2]
    XSTADDR3 = laStAddr[3]

    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF

  =gfGetAdr('CUSTOMER' , '' , '' , '' , @laBtAddr , '2')
  IF LEN(TRIM(laBtAddr[2])) =0
    laBtAddr[2] = laBtAddr[3]
    laBtAddr[3] = ''
  ENDIF

  lcCustPoVl = IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO)
  
SELECT (lnSlct)
RETURN ''
*-- end of lfShipTo.


*:**************************************************************************
*:* Name        : lfSetPages
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2007
*:* Purpose     : set pages that is , 9 lines per page
*:***************************************************************************
FUNCTION lfSetPages
LOCAL lnSlct,lcPageCnt,lcOrder,lnRepl
lnSlct = SELECT()
lcPageCnt = loOgScroll.gfTempName()

SELECT (lcTmpOrdL)
lcOrder = ORDER()
SET ORDER TO &lcOrder DESCENDING

SELECT PIKTKT,COUNT(*) AS LNCNT FROM (lcTmpOrdL) GROUP BY PIKTKT INTO CURSOR &lcPageCnt
SELECT &lcPageCnt
SCAN 
  SELECT (lcTmpOrdL)
  =SEEK(&lcPageCnt..PIKTKT)
  lnRepl = MOD(&lcPageCnt..LNCNT,lnPageLns)
  lnRepl = IIF( lnRepl = 0 , lnPageLns , lnRepl )
  REPLACE LLASTPAGE WITH .T. NEXT lnRepl
ENDSCAN
USE IN &lcPageCnt

*- Only 9 lines in each page
SELECT (lcTmpOrdL)
SET ORDER TO &lcOrder
LOCATE
lcKey = KEY()
DO WHILE !EOF()
  lcPiktkt = PIKTKT
  lnPage = 0
  DO WHILE PIKTKT = lcPiktkt
    lnPage = lnPage + 1
    lnRec = 0
    SCAN REST WHILE PIKTKT = lcPiktkt
      lnRec = lnRec + 1
      REPLACE PAGENO WITH lnPage
      IF lnRec = lnPageLns
        SKIP
        EXIT
      ENDIF
    ENDSCAN
  ENDDO
ENDDO

SELECT (lcTmpOrdL)
SET RELATION TO IIF(EMPTY(STORE),'M','S')+ACCOUNT+STORE INTO CUSTOMER 
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE  
SET RELATION TO STYLE INTO STYLE ADDITIVE
LOCATE
SELECT (lnSlct)
*-- end of lfSetPages.




