*:***************************************************************************************************************************
*: Program file  : SOIMRGR.PRG
*: Program desc. : IMPORTING ORDERS FROM MORELY TO GOURDINI
*: Module        : SO
*: System        : Aria4XP
*: Developer     : Tarek Mohammed Ibrahim
*! Ticket #      : C201328
*! Tracking #    : T20101109.0013
*!***************************************************************************************************************************
*: Passed Parameters  : None
*!***************************************************************************************************************************
*! Modifications:
*C201328,3 TMI 07/06/2011 if the store is empty use the main account [T20101109.0013 ]
*C201328,4 TMI 07/10/2011 There is only one END segment in the whole file as per Alaa [T20101109.0013 ]
*B609822,1 MMT 02/06/2012 Custom Import SO program does not save SOs[T20120119.0025]
*B609822,2 MMT 02/09/2012 use Alltrim with store field to remove spaces if it has[T20120208.0012]
*E303093,1 HIA 03/26/2012 INCREATE STORE LENGTH TO BE 8 CHAR(S), and get currency code from customer table [T20120309.0037]
*E303100,1 HIA 03/29/2012 Get style price depend on currency [T20120309.0037]
*!***************************************************************************************************************************

*- Previewing The OG
lcExpr = gfOpGrid('SOIMRGR',.T.)

*- Just closing the OG
IF EMPTY(&lcExpr)
  RETURN
ENDIF

*- Get the value of the variable lcRpFlPath
lcRpFlPath = ''
IF FILE (oAriaApplication.DataDir+'SOIMRGR.MEM')
  RESTORE FROM (oAriaApplication.DataDir+'SOIMRGR.MEM') ADDITIVE
ENDIF
*B609822,1 MMT 02/06/2012 Custom Import SO program does not save SOs[T20120119.0025][Start]
*IF EMPTY(lcRpFlPath)
IF EMPTY(lcRpFlPath) OR !FILE(lcRpFlPath)
  *B609822,1 MMT 02/06/2012 Custom Import SO program does not save SOs[T20120119.0025][END]
  gfModalGen('INM00000B00000',.F.,.F.,.F.,'No file was selected.')
  RETURN
ENDIF

*--Open Needed files
=lfOpenFls()

*- Create the needed temp files for the current session
lcTmpImprt = gfTempName()
lcLogFile = gfTempName()
lcOrdHdr = gfTempName()
lcOrdLine = gfTempName()
lcT_BomVar = gfTempName()
=lfCratTemp()

*- Get the data from the temp file
SELECT &lcTmpImprt
ZAP
APPEND FROM (lcRpFlPath) SDF
DELETE FOR EMPTY(TYPE)

IF !lfVerify()
  =lfClear()
  RETURN
ENDIF

*--lnOrdCount : to hold the numbers of orders that have been generated
*--laOrders   : array to hold the orders numbers.
DIMENSION laOrders[1]
STORE '' TO laOrders
lnOrdCount = 0

llTRDDISCL = gfGetMemVar('M_TRDDISCL')
llTRDDISCL = IIF(EMPTY(llTRDDISCL),.F.,llTRDDISCL)

SELECT WAREHOUS
LOCATE FOR LDEFWARE
IF !FOUND()
  LOCATE
ENDIF

SELECT &lcTmpImprt
LOCATE
DO WHILE !EOF(lcTmpImprt)

  SELECT (lcOrdHdr)
  SCATTER MEMVAR MEMO BLANK

  m.Lineno = 0

  SELECT &lcTmpImprt
  m.CCONTREF  = &lcTmpImprt..MRLORDER
  m.CustPo    = &lcTmpImprt..BUYERPO
  m.Start     = lfGetDt('START')
  m.Complete  = lfGetDt('COMPLETE')
  m.Entered   = lfGetDt('ENTERED')
  m.CWARECODE = WAREHOUS.CWARECODE

  m.Account   = &lcTmpImprt..Account
  *B609822,2 MMT 02/09/2012 use Alltrim with store field to remove spaces if it has[T20120208.0012][Start]
  *m.Store     = &lcTmpImprt..STORE
  m.Store     = ALLTRIM(&lcTmpImprt..STORE)
  *B609822,2 MMT 02/09/2012 use Alltrim with store field to remove spaces if it has[T20120208.0012][End]
  =SEEK('M'+m.Account,'CUSTOMER')
  m.ShipVia   = CUSTOMER.ShipVia
  m.Note2     = &lcTmpImprt..SHIPBILL

  m.NCURRUNIT = 1
  m.NEXRATE   = 1
  *E303093,1 HIA 03/26/2012 GET CURRENCY FROM CUSTOMER [BEGIN]
  *m.CCURRCODE = oAriaApplication.BaseCurrency

  m.CCURRCODE = CUSTOMER.CCURRCODE
  *-- Get Currency, Exchange rate, and unit
  lcCurrCode = IIF(EMPTY(m.CCURRCODE),oAriaApplication.BaseCurrency,m.CCURRCODE)
  STORE 1 TO lnExRate, lnCurrUnit
  IF lcCurrCode <> oAriaApplication.BaseCurrency
    DECLARE laSetups[1,2]
    laSetups[1,1] = 'LLEDITEXRA'    && Edit Exchange Rate
    =gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyID)
    llEditExRt   = laSetups[1,2]
    lnExRate = gfChkRate('lnCurrUnit',lcCurrCode,m.Entered,.T.,.F.,.F.,llEditExRt)
    *lnExRate = gfChkRate('lnCurrUnit',lcCurrCode,oAriaApplication.SystemDate,.T.,.F.,.F.,llEditExRt)
    IF lnExRate = 0
      IF llEditExRt
        *-- Message : 00262
        *-- A valid xxx to xxx exchange rate could not be found on xxx.
        *-- Button : 00000
        *-- Ok
        =gfModalGen('INM04156B00000','ALERT',ALLTRIM(lcCurrCode)+'|'+ALLTRIM(oAriaApplication.BaseCurrency)+'|'+DTOC(oAriaApplication.SystemDate))
      ELSE
        lcCurrCode = oAriaApplication.BaseCurrency
        STORE 1 TO lnExRate, lnCurrUnit
      ENDIF
    ENDIF
  ENDIF
  m.CCURRCODE = lcCurrCode
  m.NEXRATE   = lnExRate
  m.NCURRUNIT = lnCurrUnit

  *E303093,1 HIA 03/26/2012 INCREATE STORE LENGTH TO BE 8 CHAR(S) [END]

  m.CORDTYPE  = 'O'
  m.STATUS    = 'O'
  m.Flag      = 'N'
  m.Bulk      = 'N'
  m.DirectInv = .F.
  m.LContract = .F.
  m.CREORDER  = 'N'
  m.MULTI     = 'N'
  m.Cinsur    = 'Y'
  m.Buyer     = CUSTOMER.Buyer
  m.Phone     = CUSTOMER.Phone1
  m.Disc      = CUSTOMER.Disc
  m.REP1      = CUSTOMER.SalesRep
  m.Comm1     = CUSTOMER.COMM
  m.REP2      = CUSTOMER.REP2
  m.Comm2     = CUSTOMER.Comm2
  m.Priority  = CUSTOMER.Priority
  m.CADDRESS1 = CUSTOMER.CADDRESS1
  m.CADDRESS2 = CUSTOMER.CADDRESS2
  m.CADDRESS3 = CUSTOMER.CADDRESS3
  m.CADDRESS4 = CUSTOMER.CADDRESS4
  m.CADDRESS5 = CUSTOMER.CADDRESS5
  m.Link_Code = IIF(EMPTY(CUSTOMER.Link_Code),'DEFDEF',CUSTOMER.Link_Code)
  m.GL_Sales  = IIF(EMPTY(CUSTOMER.cSlsGlLink),'DEF',CUSTOMER.cSlsGlLink)
  m.CTERMCODE = CUSTOMER.CTERMCODE
  m.SPCINST   = CUSTOMER.SPCINST
  m.LMORELY    = .T.

  *- check styles , if they are related to several seasons the update season as '*'
  *- get the cDivision from the related style division
  *- if styles are related to more than one division then show an error message
  SKIP
  IF TYPE = 'PRM'
    m.NOTE1  = MRLORDER
    SKIP
  ENDIF
  SCAN REST WHILE TYPE = 'DET'
    lcUPC = ALLTRIM(Account+STORE+CLPBILL)
    =SEEK(lcUPC,'STYLEUPC')
    =SEEK(STYLEUPC.STYLE,'STYLE')
    lcSz = STYLEUPC.SIZE

    IF !SEEK(STYLE.STYLE,lcOrdLine)
      m.Lineno = m.Lineno + 1
      m.LASTLINE = m.Lineno
      m.STYLE = STYLE.STYLE
      m.DESC1 = STYLE.DESC1
      m.SCALE = STYLE.SCALE
      m.SEASON = STYLE.SEASON
      m.CDIVISION = STYLE.CDIVISION
      *-if style.cdefware differ for cwarecode then add this style to the new warehouse
      IF m.CWARECODE <> STYLE.CDEFWARE  AND !SEEK(STYLE.STYLE+m.CWARECODE,'STYDYE')
        INSERT INTO STYDYE (STYLE,CWARECODE,DESC,GL_LINK,AVE_COST) ;
          VALUES (STYLE.STYLE,m.CWARECODE,STYLE.DESC,STYLE.Link_Code,STYLE.AVE_COST)
        =lfAddUsrDt('STYDYE')
      ENDIF

      WAIT WINDOW NOWAIT 'Saving the Morely order # '+ m.NOTE1
      *- updating the lcordline temp file
      SELECT &lcOrdLine
      APPEND BLANK
      GATHER MEMVAR    &&  the fields that would be filled using this GATHER command are the following
      && ACCOUNT,COMM1,COMM2,COMPLETE,CORDTYPE,CUSTPO,CWARECODE,FLAG,GL_SALES,ORDER,SEASON,START,STORE

      *- update audit data
      =lfAddUsrDt(lcOrdLine)

    ENDIF

    SELECT &lcOrdLine
    REPLACE QTY&lcSz  WITH QTY&lcSz + VAL(SUBSTR(&lcTmpImprt..MRLORDER,4)) ;
      TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
      BOOK&lcSz WITH QTY&lcSz ;
      TOTBOOK   WITH BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8
    lcLevel = lfGetLvl()
    
    *E303100,1 HIA 03/29/2012 Get style price depend on currency [BEGIN]
    *REPLACE PRICE      WITH STYLE.PRICE&lcLevel ;
    *  GROS_PRICE WITH STYLE.PRICE&lcLevel ;
    *  TRDE_DISC  WITH IIF(llTRDDISCL,STYLE.trd_disc&lcLevel.,0)
    
    lcGros_Price = gfGetprice(&lcOrdLine..STYLE ,lcLevel ,&lcOrdLine..TotQty,lcCurrCode)
    REPLACE PRICE  WITH lcGros_Price ;
        GROS_PRICE WITH lcGros_Price ;
        TRDE_DISC  WITH IIF(llTRDDISCL,STYLE.trd_disc&lcLevel.,0)

    *E303100,1 HIA 03/29/2012 Get style price depend on currency [END]
  ENDSCAN

  SELECT &lcTmpImprt
  *C201328,4 TMI [Start] only one end per file
  *SKIP  && The END line
  *C201328,4 TMI [End  ] only one end per file

  SELECT (lcOrdLine)
  LOCATE
  SCAN
    m.BOOK      = m.BOOK + TOTBOOK
    m.BookAmt   = m.BookAmt + (PRICE * TOTQTY)
    m.OpenAmt   = m.BookAmt
    m.CancelAmt = 0
    m.OPEN      = m.BOOK
  ENDSCAN

  IF (m.BOOK > 0 OR m.OPEN > 0)

    *- select if styles are related to multiple seasons
    DIMENSION laSeason[1]
    laSeason = ' '
    SELECT DISTINCT SEASON FROM (lcOrdLine) INTO ARRAY laSeason
    IF ALEN(laSeason) > 1
      SELECT (lcOrdLine)
      LOCATE
      REPLACE SEASON WITH '*' ALL
      LOCATE

      *C201328,3,1 TMI 04/13/2011 [Start]
      *SELECT (lcOrdHdr)
      *LOCATE
      *REPLACE SEASON WITH '*' ALL
      *LOCATE
      m.SEASON = '*'
      *C201328,3,1 TMI 04/13/2011 [End  ]
    ENDIF

    INSERT INTO (lcOrdHdr) FROM MEMVAR
    =lfAddUsrDt(lcOrdHdr)
    SELECT (lcOrdHdr)
    LOCATE

    SELECT (lcOrdLine)
    SET ORDER TO ORDLINE
    LOCATE

    =lfSaveData()

    SET ORDER TO ORDLINES IN (lcOrdLine)
  ENDIF

  SELECT &lcTmpImprt
  *C201328,4 TMI 07/10/2011 [Start] if end of file, exit
  IF TYPE='END'
    EXIT
  ENDIF
  *C201328,4 TMI 07/10/2011 [End  ]
ENDDO

DO CASE
CASE lnOrdCount=1
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is saved as ' + laOrders[1])
CASE lnOrdCount > 1
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Orders from ' + laOrders[1] + ' to ' + laOrders[lnOrdCount]+ ' have been generated successfully')
CASE lnOrdCount = 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No orders saved.')
ENDCASE

*- clear
=lfClear()
RETURN

*!**************************************************************************
*!* Name        : lfvSelFile
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Save the selected file path to import orders from
*!***************************************************************************
FUNCTION lfvSelFile
LOCAL o
o = _SCREEN.ACTIVEFORM.ACTIVECONTROL
IF ALLTRIM(o.VALUE) == ALLTRIM(o.OldValue)
  RETURN
ENDIF

IF !EMPTY(lcRpFlPath)
  lcRpFlPath = ALLT(lcRpFlPath)
  IF !FILE(lcRpFlPath)
    lcRpFlPath = GETFILE('TXT')
  ENDIF
ENDIF
SAVE TO (oAriaApplication.DataDir+'SOIMRGR.MEM') ALL LIKE lcRpFlPath
*- End of FUNCTION lfvSelFile

*!**************************************************************************
*!* Name        : lfwRepWhen
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : When function of the OG
*!***************************************************************************
FUNCTION lfwRepWhen
IF FILE (oAriaApplication.DataDir+'SOIMRGR.MEM')
  RESTORE FROM (oAriaApplication.DataDir+'SOIMRGR.MEM') ADDITIVE
ENDIF
*- End of FUNCTION lfwRepWhen

*!**************************************************************************
*!* Name        : lfOpenFls
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Open needed files
*!***************************************************************************
FUNCTION lfOpenFls
=gfOpenTable(oAriaApplication.DataDir+'BomVar',oAriaApplication.DataDir+'BomVar','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH')
=gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
=gfOpenTable(oAriaApplication.DataDir+'WAREHOUS','WAREHOUS','SH')
=gfOpenTable(oAriaApplication.DataDir+'CUSTOMER','CUSTOMER','SH')
=gfOpenTable(oAriaApplication.DataDir+'SALESREP','SALESREP','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDHDR','ORDHDR','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDLINE','ORDLINE','SH')
=gfOpenTable(oAriaApplication.DataDir+'UnCmSess',oAriaApplication.DataDir+'TRANS','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
=gfOpenTable(oAriaApplication.DataDir+'NOTEPAD','NOTEPAD','SH')

=gfOpenTable(oAriaApplication.SysPath+'SycCurr','cCurrCode','SH')
=gfOpenTable(oAriaApplication.SysPath+'SYCEXCH','CURRENCY','SH')
=gfOpenTable(oAriaApplication.SysPath+'SYCINT','CCONTCODE','SH')

=gfOpenTable(oAriaApplication.SysPath+'STYLEUPC','STYUPCN','SH')
SET ORDER TO TAG CURRENCY DESC IN 'SYCEXCH'
*- End of FUNCTION lfOpenFls

*!**************************************************************************
*!* Name        : lfCratTemp
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose   : Create temp files
*!***************************************************************************
FUNCTION lfCratTemp
PRIVATE laFileStru,lnFileStru,laIndex,lnAlias
lnAlias = SELECT()
SELECT ORDHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'LERRORS'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0

FOR lnLoop = 1 TO  2
  STORE ' ' TO  laFileStru[lnFileStru +lnLoop,7],laFileStru[lnFileStru +lnLoop,8],;
    laFileStru[lnFileStru +lnLoop,9],laFileStru[lnFileStru +lnLoop,10],;
    laFileStru[lnFileStru +lnLoop,11],laFileStru[lnFileStru +lnLoop,12],;
    laFileStru[lnFileStru +lnLoop,13],laFileStru[lnFileStru +lnLoop,14],;
    laFileStru[lnFileStru +lnLoop,15],laFileStru[lnFileStru +lnLoop,16]
  STORE 0 TO    laFileStru[lnFileStru +lnLoop,17] ,laFileStru[lnFileStru +lnLoop,18]
ENDFOR

DECLARE laIndex[2,2]
laIndex[1,1] = 'ACCOUNT+CUSTPO'
laIndex[1,2] = 'CUSTPO'
laIndex[2,1] = 'cOrdType+ORDER'
laIndex[2,2] = '&lcOrdHdr'
=gfCrtTmp(lcOrdHdr,@laFileStru,@laIndex)

SELECT(lcOrdHdr)
INDEX ON 'cOrdType+ORDER' TAG (lcOrdHdr)
SET ORDER TO TAG 'CUSTPO' IN (lcOrdHdr)

*----------------
SELECT ORDLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lContract'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0

FOR lnLoop = 1 TO  2
  STORE ' ' TO  laFileStru[lnFileStru +lnLoop,7],laFileStru[lnFileStru +lnLoop,8],;
    laFileStru[lnFileStru +lnLoop,9],laFileStru[lnFileStru +lnLoop,10],;
    laFileStru[lnFileStru +lnLoop,11],laFileStru[lnFileStru +lnLoop,12],;
    laFileStru[lnFileStru +lnLoop,13],laFileStru[lnFileStru +lnLoop,14],;
    laFileStru[lnFileStru +lnLoop,15],laFileStru[lnFileStru +lnLoop,16]
  STORE 0 TO    laFileStru[lnFileStru +lnLoop,17] ,laFileStru[lnFileStru +lnLoop,18]
ENDFOR
=gfCrtTmp(lcOrdLine,@laFileStru,'STYLE+STORE','ORDLINES',.F.)

SELECT(lcOrdLine)
USE IN (lcOrdLine)
=gfOpenTable(oAriaApplication.WorkDir+lcOrdLine,'ORDLINES','EX')

SELECT(lcOrdLine)
INDEX ON CORDTYPE+ORDER+STR(LINENO,6) TAG ORDLINE
SET ORDER TO ORDLINES

*----------------
SELECT BomVar
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]


laFileStru[lnFileStru+1,1] = 'nRecno'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'cStatus'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
FOR lnLoop = 1 TO  2
  STORE ' ' TO  laFileStru[lnFileStru+lnLoop,7],laFileStru[lnFileStru+lnLoop,8],;
    laFileStru[lnFileStru+lnLoop,9],laFileStru[lnFileStru+lnLoop,10],;
    laFileStru[lnFileStru+lnLoop,11],laFileStru[lnFileStru+lnLoop,12],;
    laFileStru[lnFileStru+lnLoop,13],laFileStru[lnFileStru+lnLoop,14],;
    laFileStru[lnFileStru+lnLoop,15],laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO    laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
ENDFOR
IF USED(lcT_BomVar)
  ZAP IN (lcT_BomVar)
ELSE
  =gfCrtTmp(lcT_BomVar,@laFileStru,[cIdType+cCost_Id+STR(LineNo,6)],lcT_BomVar)
ENDIF

* Create a temp table to append HDR lines into, It will be used also for the PRM promo ( with no problems )
* To access data in sequence I'll use the same file for the DET
* Mapped fields would be as follows
*  Line Sequence :: substr(MRLORDER,1,3)
*  Quantity      :: substr(MRLORDER,4,4)
*  Gordini UPC   :: ACCOUNT+STORE+CLPBILL
DIMENSION laFileStru[12,4]
lnI = 0

lnI = lnI + 1
laFileStru[lnI,1] = 'TYPE'  && to hold HDR,PRM and DET descriptions
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = lnI + 1
laFileStru[lnI,1] = 'MRLORDER'   &&Morley Order Number
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

lnI = lnI + 1
laFileStru[lnI,1] = 'ACCOUNT' &&Gordini Bill to Acct
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = lnI + 1
laFileStru[lnI,1] = 'STORE'  && Gordini Ship To Store Number
laFileStru[lnI,2] = 'C'
*E303093,1 HIA 03/26/2012 INCREATE STORE LENGTH TO BE 8 CHAR(S) [BEGIN]
*laFileStru[lnI,3] = 5
laFileStru[lnI,3] = 8
*E303093,1 HIA 03/26/2012 INCREATE STORE LENGTH TO BE 8 CHAR(S) [END]
laFileStru[lnI,4] = 0

lnI = lnI + 1
laFileStru[lnI,1] = 'CLPBILL'  &&CLP BilltoAcct
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = lnI + 1
laFileStru[lnI,1] = 'CLPSHIP'  &&CLP Shipto Acct
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = lnI + 1
laFileStru[lnI,1] = 'ENTERED'  && Entered Date
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = lnI + 1
laFileStru[lnI,1] = 'COMPLETE'  &&Cancel By Date
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = lnI + 1
laFileStru[lnI,1] = 'START'  &&Ship Start Date
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = lnI + 1
laFileStru[lnI,1] = 'BUYERPO'  &&Buyers PO
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 15
laFileStru[lnI,4] = 0

lnI = lnI + 1
laFileStru[lnI,1] = 'SHIPVIA'  &&Ship Via Carrier Name
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 15
laFileStru[lnI,4] = 0

lnI = lnI + 1
laFileStru[lnI,1] = 'SHIPBILL'&&Ship Bill 3rd Party Acct
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 20
laFileStru[lnI,4] = 0

*=gfCrtTmp(lcTmpImprt,@laFileStru,'')
CREATE CURSOR &lcTmpImprt FROM ARRAY laFileStru

SELECT(lnAlias)
*- End of FUNCTION lfCratTemp

*!**************************************************************************
*!* Name        : lfAddUsrDt
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Add user id ,date  and time to files08/01/2002
*!***************************************************************************
FUNCTION lfAddUsrDt
PARAMETERS lcAlias
PRIVATE lcCurrAls
lcCurrAls = SELECT()
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
REPLACE cAdd_User WITH oAriaApplication.User_ID,;
  dAdd_Date WITH oAriaApplication.SystemDate,;
  cAdd_Time WITH TIME()

SELECT (lcCurrAls)
*- End of FUNCTION lfAddUsrDt

*!**************************************************************************
*!* Name        : lfSaveData
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Save temp file to Actual orders
*!***************************************************************************
FUNCTION lfSaveData

PRIVATE llOrdSaved
lnAlias = SELECT()

STORE 'O'  TO lcOrdType
DECLARE laWareHouses[1,2]  , laSetups[6,2] , laKeyField[2,4] , laSeasons[1,2] ,;
  laCodes[1,10]      , laSource[1,2] , laOrdStatus [1]
STORE ''   TO laWareHouses , lcODefWare    , lcWareHouse     , lcScFields ,;
  laSeasons     , laCodes       , lcODefSes       , lcSeason   ,laOrdStatus
STORE .F.  TO laSetups     , llFound1      , llFound2        , llBrowse
STORE 1    TO lnWareHouse  , lnOrdStatus   , lnSeason
STORE m.Stname TO lcShipName
STORE m.CADDRESS1 TO lcShipAdd1
STORE m.CADDRESS2 TO lcShipAdd2
STORE PADR(m.CADDRESS3,15,' ')+','+m.CADDRESS4+','+m.CADDRESS5 TO lcShipAdd3
STORE ' ' TO lcShipAdd4
STORE ' ' TO lcShipAdd5

STORE 0    TO lnBook    , lnOpen
STORE 0.00 TO lnOpenAmt , lnBookAmt , lnTotAmt

*-- variables of SOUPDATE.PRG [Start]
DECLARE laVariables[6] , laScrMode[4]
STORE .F. TO llContinue , llBomVarnt , llCDPerL
STORE ''  TO lcFlToUpd  , lcSession  , lcFiles   , laVariables , lcGlYear , lcGlPeriod ,;
  lcExRsin   , lcUntSin   , lcODefDiv , lcScrMode   , lcCurrOrd
STORE {}  TO ldDefOrdDate
lcFlToUpd = gfTempName()
*-- variables of SOUPDATE.PRG [End]

laSetups[1,1]  = 'M_PACK'           && System has been steup to use packs
laSetups[2,1]  = 'M_STY_COM'        && Edit sales reps commissions at style level
laSetups[3,1]  = 'M_OR_NOTE'        && Edit order lines notepad
laSetups[4,1]  = 'M_LINK_GL'        && System has been linked to GL
laSetups[5,1]  = 'M_WareHouse'      && System has been steup to use multiple warehouses
laSetups[6,1]  = 'M_GenOrNum'       && Generate order number manually
=gfGetMemVar(@laSetups,gcAct_Comp)

*-- variables of SOUPDATE.PRG [Start]
laVariables[1] = 'ldDefOrdDate'
laVariables[2] = 'lcODefSes'
laVariables[3] = 'lcODefDiv'
laVariables[4] = 'lcODefWare'
laVariables[5] = 'lcScrMode'
laVariables[6] = 'lcCurrOrd'
*-- variables of SOUPDATE.PRG [End]

lcScFields = 'ORDER,ACCOUNT,STORE,CUSTPO,STATUS,MULTI,MULTIPO,ENTERED,START,'+;
  'COMPLETE,cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DISC,DEPT,'+;
  'NOTE1,NOTE2,BUYER,PHONE,CINSUR,BULK,CREORDER,PRIORITY,CFACCODE,'+;
  'REP1,COMM1,REP2,COMM2,CWARECODE,LINK_CODE,CCURRCODE,NEXRATE,BOOK,BOOKAMT,'+;
  'SHIP,SHIPAMT,CANCEL,CANCELAMT,OPEN,OPENAMT,CFROMORDER,'+;
  'CANCELLED,DECL_DATE,DECL_CODE,CCANCRESON,APPROVAL,APPRAMT,'+;
  'NCURRUNIT,Alt_ShpTo,CORDERCAT,GL_SALES,INT_VEND,EVENT_COD,'+;
  'BILLNO,MERC_TYPE,BLANK_ORD,DISTRB_NO,CCLASS,LFROMWEB'

*- This field is added specially to differentiate the imported morely orders
lcScFields = lcScFields +',LMORELY'

SELECT WAREHOUS
SELECT cDesc,CWARECODE FROM WAREHOUS INTO ARRAY laWareHouses

lnWareHouse = ASCAN(laWareHouses,lcODefWare)
lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))

STORE .F. TO llMFDsPrc , llPoDsPrc
llOrdSaved = .F.
*--Loop throght the temp file lcOrdHdr to save data
SELECT (lcOrdHdr)
SET ORDER TO TAG &lcOrdHdr

llOrdSaved = .T.
SCATTER FIELDS &lcScFields MEMVAR

SELECT (lcOrdLine)
LOCATE
SELECT (lcOrdHdr)
llContinue = .T.

loFormSet = CREATEOBJECT('Custom')
loFormSet.ADDPROPERTY('laEvntTrig[1]','SOIMPRO   ')
loFormSet.ADDPROPERTY('lcdeposittemp',gfTempName())
loFormSet.ADDPROPERTY('Activemode','A')
loFormSet.ADDPROPERTY('llUpdate',.F.)


DO lfSavScr IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') ;
  WITH .F., 'A', lcOrdHdr,lcOrdLine,.F.,.F.,lcT_BomVar ,loFormSet

lnOrdCount = lnOrdCount + 1
DIMENSION laOrders[lnOrdCount]
laOrders[lnOrdCount] = ORDHDR.ORDER
REPLACE FLAG WITH ' ' IN ORDHDR
*B609822,1 MMT 02/06/2012 Custom Import SO program does not save SOs[T20120119.0025][Start]
*loFormSet.AddProperty('DataSessionId',oAriaApplication.laRemoteTable[1].lnDataSession)
loFormSet.ADDPROPERTY('DataSessionId',SET("Datasession"))
*B609822,1 MMT 02/06/2012 Custom Import SO program does not save SOs[T20120119.0025][END]
=lfSavefiles()


*--Delele Records of the immediately saved order
SELECT (lcOrdLine)
DELETE ALL
PACK

SELECT (lcOrdHdr)
DELETE ALL
PACK

*- End of FUNCTION lfSaveData

*!**************************************************************************
*!* Name        : lfSavefiles
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : commit updated data
*!***************************************************************************
FUNCTION lfSavefiles
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  RETURN .F.
ENDIF
lnUpdated = 0
lnAryLen = ALEN(oAriaApplication.laRemoteTable)
FOR lnCounter=1 TO lnAryLen
  IF oAriaApplication.laRemoteTable[lnCounter].lnDataSession == loFormSet.DATASESSIONID
    IF !oAriaApplication.laRemoteTable[lnCounter].TABLEUPDATE(lcTranCode)
      lnUpdated=lnCounter
      EXIT
    ENDIF
  ENDIF
NEXT
IF lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  MESSAGEBOX('Saving Process is Rolled Back')
  THISFORMSET.UNDO()
  RETURN
ELSE
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ENDIF
*- End of FUNCTION lfSavefiles

*!**************************************************************************
*!* Name        : lfVerify
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Check that the imported flat file is ok
*!***************************************************************************
FUNCTION lfVerify
LOCAL lnSlct
lnSlct = SELECT(0)
lcCr = CHR(13)
lcErrStr = ''
SELECT (lcTmpImprt)
*- Check if a HDR has no details
LOCATE
DO WHILE !EOF()
  IF TYPE<>'HDR'
    lfErr('First line of an order does not has an HDR type')
  ENDIF
  LOCATE REST FOR TYPE='DET'
  SCAN REST WHILE TYPE = 'DET'
  ENDSCAN
  *C201328,4 TMI 07/10/2011 [Start] comment this part
  *IF TYPE<>'END'
  *  lfErr('Last line of an order does not has an END type')
  *ELSE
  *  SKIP
  *ENDIF
  IF TYPE = 'END'  && only one end in the whole file
    EXIT
  ENDIF
  *C201328,4 TMI 07/10/2011 [End  ]
ENDDO

*- Check if some HDR does not has a DET
lcMRLORDER = ' '
LOCATE FOR TYPE='HDR'
DO WHILE FOUND()
  lcMRLORDER  = MRLORDER
  SKIP
  IF TYPE = 'PRM'
    SKIP
  ENDIF
  IF TYPE = 'DET'
    SCAN REST WHILE TYPE='DET'
    ENDSCAN
  ELSE
    lfErr('The HDR order # &lcMRLORDER does not has a DET part')
  ENDIF
  CONTINUE
ENDDO

*- check for Accounts and Stores
LOCATE
SCAN FOR TYPE = 'HDR'
  *C201328,3 TMI 07/06/2011 [Start] null stores are equivalent to main account
  IF ALLTRIM(&lcTmpImprt..STORE)='NULL'
    REPLACE STORE WITH ''
  ENDIF
  *C201328,3 TMI 07/06/2011 [End  ]
  lcAccnt = &lcTmpImprt..Account
  *B609822,2 MMT 02/09/2012 use Alltrim with store field to remove spaces if it has[T20120208.0012][Start]
  *lcStore = &lcTmpImprt..STORE
  lcStore = ALLTRIM(&lcTmpImprt..STORE)
  *B609822,2 MMT 02/09/2012 use Alltrim with store field to remove spaces if it has[T20120208.0012][End]
  IF !SEEK('M'+Account,'CUSTOMER')
    lfErr('Account &lcAccnt. is not found in the system')
  ELSE
    IF CUSTOMER.STATUS <> 'A'
      lfErr('Account &lcAccnt. is not active')
    ENDIF
  ENDIF
  *C201328,3 TMI 07/06/2011 [Start] check only for stores not as empty
  IF !EMPTY(&lcTmpImprt..STORE)
    *C201328,3 TMI 07/06/2011 [End  ]
    *B609822,2 MMT 02/09/2012 use Alltrim with store field to remove spaces if it has[T20120208.0012][Start]
    *IF !SEEK('S'+ACCOUNT+PADR(STORE,8),'CUSTOMER')
    IF !SEEK('S'+Account+PADR(ALLTRIM(STORE),8),'CUSTOMER')
      *B609822,2 MMT 02/09/2012 use Alltrim with store field to remove spaces if it has[T20120208.0012][End]
      lfErr('The Store &lcStore of the account &lcAccnt is not found in the system')
    ENDIF
    *C201328,3 TMI 07/06/2011 [Start]
  ENDIF
  *C201328,3 TMI 07/06/2011 [End  ]

ENDSCAN

PRIVATE lcDiv
STORE ''  TO lcErrUPC, lcErrUpcQty, lcErrDiv, lcErrSes

lcDiv = '  '
lcErrDiv = lcCr
LOCATE
SCAN FOR TYPE = 'DET'
  *- check for upc's
  lcUPC = ALLTRIM(Account+STORE+CLPBILL)
  IF !SEEK(lcUPC,'STYLEUPC')
    lcErrUPC = lcErrUPC + lcUPC + lcCr
  ELSE
    *- check upc qty
    =SEEK(STYLEUPC.STYLE,'STYLE')
    IF VAL(SUBSTR(MRLORDER,4))<=0
      lcErrUpcQty = lcErrUpcQty + lcUPC + lcCr
    ENDIF

    *C201328,3,1 TMI 04/13/2011 [Start] extra line of code added by mistake
    *=SEEK(STYLEUPC.STYLE,'STYLE')
    *C201328,3,1 TMI 04/13/2011 [End  ]
    *- Check division
    IF EMPTY(lcDiv )
      lcDiv = STYLE.CDIVISION
    ELSE
      IF lcDiv <> STYLE.CDIVISION AND !lcMRLORDER $ lcErrDiv
        lcErrDiv = 'Order # &lcMRLORDER. styles are related to more than one division'+lcCr
      ENDIF
    ENDIF

    *- Check empty seasons
    IF EMPTY(STYLE.SEASON) AND !STYLE.STYLE$lcErrSes
      lcErrSes = lcErrSes + STYLE.STYLE + lcCr
    ENDIF

  ENDIF
ENDSCAN

IF !EMPTY(lcErrUPC)
  lfErr('The following UPCs are not found in the system:')
  lfErr(lcErrUPC)
ENDIF

IF !EMPTY(lcErrUpcQty)
  lfErr('The following UPCs have zero qty:')
  lfErr(lcErrUpcQty)
ENDIF

IF !EMPTY(lcErrSes)
  lfErr('The following Styles are not assigned to a season:')
  lfErr(lcErrSes)
ENDIF

IF !EMPTY(lcErrDiv)
  lfErr(lcErrDiv)
ENDIF


IF !EMPT(lcErrStr)
  lcAdd2Msg = ''
  lcAdd2Msg = lcAdd2Msg + REPL('-',70)+ lcCr
  lcAdd2Msg = lcAdd2Msg + 'Data imported from file '+lcRpFlPath+' at Date : '+DTOC(DATE())+' Time : '+TIME() + lcCr
  lcAdd2Msg = lcAdd2Msg + REPL('-',70)+ lcCr
  lcAdd2Msg = lcAdd2Msg + 'Import Failed,..'

  lcErrStr = lcAdd2Msg + lcCr + lcErrStr
  LOCAL oObject,o
  o = CREATEOBJECT('TextBox')
  o.VALUE = lcErrStr
  o.READONLY = .T.

  oObject = CREATEOBJECT("AriaZoom",o)
  oObject.CAPTION = 'Importing Orders from Morely Error Log file'
  oObject.VISIBLE = .T.
  oObject.SHOW()
ENDIF
SELECT (lnSlct)
RETURN EMPTY(lcErrStr)
*- End of FUNCTION lfVerify

*!**************************************************************************
*!* Name        : lfErr
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Add a line to the Error string
*!***************************************************************************
FUNCTION lfErr
PARAMETERS lcErr
lcErrStr = lcErrStr + lcErr + CHR(13)
*- End of FUNCTION lfErr

*!**************************************************************************
*!* Name        : lfClear
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Erase temp files , close opened files
*!***************************************************************************
FUNCTION lfClear
=lfErase(lcTmpImprt )
=lfErase(lcLogFile )
=lfErase(lcOrdHdr )
=lfErase(lcOrdLine )
=lfErase(lcT_BomVar )

*- Close files
=gfCloseTable('BomVar')
=gfCloseTable('STYLE')
=gfCloseTable('SCALE')
=gfCloseTable('WAREHOUS')
=gfCloseTable('CUSTOMER')
=gfCloseTable('SALESREP')
=gfCloseTable('ORDHDR')
=gfCloseTable('ORDLINE')
=gfCloseTable('UnCmSess')
=gfCloseTable('STYDYE')
=gfCloseTable('NOTEPAD')
=gfCloseTable('SycCurr')
=gfCloseTable('SYCEXCH')
=gfCloseTable('SYCINT')
=gfCloseTable('STYLEUPC')
*- End of FUNCTION lfClear

*!**************************************************************************
*!* Name        : lfErase
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Erase temp files
*!***************************************************************************
FUNCTION lfErase
LPARAMETERS lcAlias
LOCAL lnSlct,lcDbf
lnSlct = SELECT(0)
IF USED(lcAlias)
  lcDbf = STRTRAN( DBF(lcAlias) , '.DBF' , '.*' )
  USE IN (lcAlias)
  ERASE (lcDbf)
ENDIF
SELECT (lnSlct)
*- End of FUNCTION lfErase

*!**************************************************************************
*!* Name        : lfGetDt
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : convert string to date using DATE func.
*!***************************************************************************
FUNCTION lfGetDt
PARAMETERS lcFld
RETURN DATE(VAL(SUBSTR(&lcTmpImprt..&lcFld,1,4)),VAL(SUBSTR(&lcTmpImprt..&lcFld,6,2)),VAL(SUBSTR(&lcTmpImprt..&lcFld,9,2)))
*- End of FUNCTION lfGetDt

*!**************************************************************************
*!* Name        : lfGetLvl
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : Get customer's level
*!***************************************************************************
FUNCTION lfGetLvl

IF CUSTOMER.PRICELVL = 'Q'
  DO CASE
  CASE STYLE.nAtQtyC > 0 AND &lcOrdLine..TOTQTY > STYLE.nAtQtyC
    lcLevel = 'C'
  CASE STYLE.nAtQtyB > 0 AND &lcOrdLine..TOTQTY > STYLE.nAtQtyB
    lcLevel = 'B'
  OTHERWISE
    lcLevel = 'A'
  ENDCASE
ELSE
  lcLevel=IIF(INLIST(CUSTOMER.PRICELVL,'A','B','C'),CUSTOMER.PRICELVL,'A')
ENDIF
RETURN lcLevel
*- End of FUNCTION lfGetLvl

*!**************************************************************************
*!* Name        : lfCustMsg
*!* Developer   : TMI - TAREK MOHAMMED IBRAHIM
*!* Date        : 04/11/2011
*!* Purpose     : just added to prevent a message to emits up from the SOUPDATE
*                 that a sales orders # is saved
*!***************************************************************************
FUNCTION lfCustMsg
**
*- End of FUNCTION lfCustMsg
