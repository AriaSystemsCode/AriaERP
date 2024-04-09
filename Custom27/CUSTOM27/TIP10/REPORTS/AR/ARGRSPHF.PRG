*:***************************************************************************
*: Program file  : ARGRSPHF.PRG
*: Program desc. : GROSS PROFIT BY DIVISION FOR HIGH FASHION UK LTD.
*: Date          : 07/19/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C200214
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARGRSPHF
*:***************************************************************************
*: Modifications :
*: B604963,1 BWA 26/09/2001 Fix the bug in the summary report which is the subtotal part doesn't print.
*: B605116,1 SSE 11/12/2001 Fix bug in Summary type of not clearing the division totals after printing each division.
*: B605319,1 SSE 01/22/2001 Fix bug of not printing correct data when sorting by Account using summary option.
***************************************************************************
*-- ldRPPLDate Variable that hold From date value
*-- ldRPPHDate Variable that hold to   date value
*-- lcRPFrSty  Variable that hold From style value
*-- lcRPToSty  Variable that hold To   style value
*-- lcRPFrAcc  Variable that hold From Account value
*-- lcRPToAcc  Variable that hold To   Account value
*-- lcRPSeason Variable that hold Season
*-- lcRPDiv    Variable that hold Division

STORE SPACE(0) TO lcTrue , lcFalse
ldLDate    = ldRPPLDate
ldHDate    = ldRPPHDate
lnMajorLen = LEN(gfItemMask("PM"))
XLSTYLE    = PADR(lcRPFrSty,lnMajorLen)
XHSTYLE    = PADR(lcRPToSty,lnMajorLen)
XLACCOUNT  = lcRPFrAcc
XHACCOUNT  = lcRPToAcc
lcSeason   = lcRPSeason
lcDiv      = lcRPDiv

*B604963,1 BWA 26/09/2001 Fix the bug in the summary report which is the subtotal part doesn't print.[START]
lcSumBrk   = ".F."
SumBrk     = &lcSumBrk
*B604963,1 BWA 26/09/2001.[END]

*-- Getting class. postion in Fixed filtter array to get all classes that
*-- user selected in option grid
*-- XTCS variable that hold selected classes
lnClassPo = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CLASS'),1)
XTCS = laOGFxFlt[lnClassPo,6]

*-- lcSlsRep  variable that hold sales rep.
*-- XDT       variable that showes how to print invoice (by Ship date OR Inv. date)
*-- lcReport  variable that showes if print report in summary or detail
*-- lcIncRet  variable that showse if returns is included or not
*-- lcSort    variable that showes the report sort
*-- XGrdTotO  variable that showes printing grand total or not
*-- lcOTitle  variable that hold the optional title
*-- lcSubGros variable showes deducting returns from gross sales or not

lcSlsRep  = lcRPSalRep
XDT       = lcRPInvBy
lcReport  = lcRPSumDet
lcIncRet  = IIF(llRPIncRet,'Y','N')
lcSort    = lcRPSortBy
XGrdTotO  = IIF(llRPGrnTot,'Y','N')
llInclRet = llRPIncRet
lcOTitle  = lcRPTitle
lcSubGros = IIF(llRPSubRet,'Y','N')
lcNamSrep = SPACE(0)

DO lpFilesRela

*--initilize filters on file.
XFILTER = "INVHDR.Status<>'V'"
IF llInclRet
  XRFILTER= "RETHDR.Status<>'V'"
ENDIF

*--Add the posting date to the filter.
=lfvPostDat()

*--Set and build the filters due to grid var.
DO lpSetFilter
lcSFilt=' '

*--Creat a temp file.
DO lpCreateTmp

WAIT WINDOW 'Building the invoices records...' NOWAIT
*-- Build the invoice records.
SELECT INVLINE
IF ! EMPTY(XLSTYLE)
  lcSetNear = SET('NEAR')
  SET NEAR ON
  SEEK XLSTYLE
  IF EOF()
    WAIT CLEAR
    =gfModalGen('TRM00052B00000','DIALOG')
    SET DEVICE TO SCREEN
    SET NEAR &lcSetNear
    RETURN
  ENDIF
  SET NEAR &lcSetNear
  lcSFilt = 'REST WHILE STYLE<=XHSTYLE '
ELSE
  IF !EMPTY(XHSTYLE)
    lcSFilt = 'REST WHILE STYLE<=XHSTYLE '
  ENDIF
ENDIF

IF EMPTY(XLSTYLE) AND EMPTY(XHSTYLE)
  SET ORDER TO Invline
ENDIF

PRIVATE llCallHF
llCallHF = EMPTY(laOGFxFlt[2,6]) OR (!EMPTY(laOGFxFlt[2,6]) AND lcRpCurr <> 'F')

IF !llRPRtOnly          &&llRPRtOnly > to check the selection of the user.
  SCAN &lcSFilt FOR &XFILTER
    WAIT WINDOW 'Collecting invoice # : '+Invoice NOWAIT
    SCATTER FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,GROS_PRICE,TOTQTY,COST TO laInvRec

    SELECT (INVLTEMP)
    APPE BLANK
    GATHER FROM laInvRec FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,GROS_PRICE,TOTQTY,COST
    REPLACE TYPE      WITH 'INV'            ,;
            CODE      WITH INVLINE.INVOICE  ,;
            DATE      WITH INVLINE.INVDATE  ,;
            NAME      WITH CUSTOMER.BTNAME  ,;
            CLASS     WITH CUSTOMER.CLASS   ,;
            DISCPCNT  WITH INVHDR.DISCPCNT  ,;
            TRDE_DISC WITH INVHDR.TRDE_DISC ,;
            Ccurrcode WITH INVHDR.Ccurrcode ,;
            Nexrate   WITH INVHDR.Nexrate   ,;
            Ncurrunit WITH INVHDR.Ncurrunit ,;
            REP1      WITH INVHDR.REP1      ,;
            cdivision WITH INVHDR.cdivision ,;
            Season    WITH INVHDR.SEASON    ,;
            llSeason  WITH .F.              ,;
            Discount  WITH INVLINE.TOTQTY * IIF(llCallHF , gfAmntDisp(INVLINE.PRICE, lcRpCurr , ldRpExDate , lcRpTmpNam),INVLINE.PRICE) * INVHDR.DISCPCNT/100

  ENDSCAN
ENDIF
SELECT (INVLTEMP)

IF llRetModu AND llInclRet
  WAIT WINDOW 'Building the returns records...' NOWAIT
  *-- Build the returns records.
  INDEX ON Invoice+STR(RECNO(),7) TAG &INVLTEMP
  SELECT RETLINE
  IF ! EMPTY(XLSTYLE)
    lcOldNear = SET("NEAR")
    SET NEAR ON
    =SEEK(XLSTYLE)
    SET NEAR &lcOldNear
  ENDIF

  XRFILTER = STRTRAN(XRFILTER,"INVHDR.cCurrcode","RETHDR.cCurrcode")
  SCAN &lcSFilt FOR &XRFILTER
    WAIT WINDOW 'Collecting credit memo # : '+Crmemo NOWAIT
    lcInvoice = Invoice
    DIMENSION laInvRec[6]
    SCATTER FIELDS ACCOUNT,STYLE,LINENO,GROS_PRICE,PRICE,TOTQTY,COST TO laInvRec
    SELECT (INVLTEMP)

    IF ! SEEK(lcInvoice)
      lcInvoice = 'ZZZZZZ'      && Returns without invoice or for invoices
                                && not in date range are holding the ZZZZZZ.
    ENDIF
    =SEEK(RETLINE.ACCOUNT+RETLINE.CRMEMO,'RETHDR')
    APPEND BLANK
    GATHER FROM laInvRec FIELDS ACCOUNT,STYLE,LINENO,GROS_PRICE,PRICE,TOTQTY,COST
    REPLACE TYPE      WITH 'RET'            ,;
            INVOICE   WITH lcInvoice        ,;
            CODE      WITH RETLINE.CRMEMO   ,;
            DATE      WITH RETLINE.CRDATE   ,;
            NAME      WITH CUSTOMER.BTNAME  ,;
            CLASS     WITH CUSTOMER.CLASS   ,;
            Ccurrcode WITH RETHDR.Ccurrcode ,;
            Nexrate   WITH RETHDR.Nexrate   ,;
            Ncurrunit WITH RETHDR.Ncurrunit ,;
            REP1      WITH RETHDR.SalesRep1 ,;
            cdivision WITH RETHDR.cdivision ,;
            llSeason  WITH .F.

    *B604963,1 BWA 26/09/2001 Fix the bug in the summary report which is the subtotal part doesn't print.[START]
    REPLACE Season    WITH STYLE.SEASON
    *B604963,1 BWA 26/09/2001.[END]
  ENDSCAN
ENDIF

SELECT (INVLTEMP)
IF RECCOUNT() = 0
  WAIT CLEAR
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*--Clear relations
SELECT INVLINE
SET RELA TO

IF llRetModu
  SELECT RETLINE
  SET RELA TO
ENDIF

*-- Indexing file.
Z = RECCOUNT()
WAIT WINDOW 'Sorting '+STR(Z,6)+' records for the report ' NOWAIT

SELECT (INVLTEMP)
DO CASE
  CASE lcSort = 'A'
    INDEX ON cDivision+Season+Account+Invoice+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    IF lcReport = 'D'
      lcHBreak = 'Season+cDivision'
    ELSE
      lcSumBrk = 'Season+cDivision'
      lcHBreak  = 'ACCOUNT'
    ENDIF
    lcTitle  = 'ACCOUNT'

  CASE lcSort = 'S'
    INDEX ON cDivision+Season+Style+Invoice+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    IF lcReport = 'D'
      lcHBreak = 'Season+cDivision'
    ELSE
      lcSumBrk = 'Season+cDivision'
      lcHBreak  = 'STYLE'
    ENDIF
    lcTitle  = 'STYLE'

  CASE lcSort = 'C'
    INDEX ON cDivision+Season+Class+Invoice+Account+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    IF lcReport = 'D'
      lcHBreak = 'Season+cDivision'
    ELSE
      lcSumBrk = 'Season+cDivision'
      lcHBreak  = 'CLASS'
    ENDIF
    lcTitle  = 'CLASS'

  CASE lcSort = 'R'
    INDEX ON cDivision+Season+REP1+Invoice+Account+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    IF lcReport = 'D'
      lcHBreak = 'Season+cDivision'
    ELSE
      lcSumBrk = 'Season+cDivision'
      lcHBreak  = 'REP1'
    ENDIF
    lcTitle  = "Sales Rep"

  CASE lcSort = 'I'
    INDEX ON cDivision+Season+Invoice+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    IF lcReport = 'D'
      lcHBreak = 'Season+cDivision'
    ELSE
      lcSumBrk = 'Season+cDivision'
      lcHBreak  = 'INVOICE'
    ENDIF
    lcTitle  = 'INVOICE'

ENDCASE

STORE SPACE(0) TO lcSeaDiv
SELECT (INVLTEMP)
SCAN
  REPLACE llSeason WITH IIF(lcSeaDiv # cdivision , .T. , .F. ) ,;
          DivDesc  WITH IIF(lcSeaDiv # cdivision , gfCodDes(CDIVISION,'CDIVISION') , "")
  lcSeaDiv  = cdivision
ENDSCAN

WAIT CLEAR
R_WIDTH = 'XW'
SET DEVICE TO PRINT
DO lpPrint
DO ENDREPORT
SET DEVICE TO SCREEN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpFilesRela
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Create the relation between the files.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpFilesRela
*!*************************************************************
PROCEDURE lpFilesRela

SELECT INVLINE
SET RELATION TO INVOICE INTO INVHDR,'M'+ACCOUNT INTO CUSTOMER
SET RELATION TO STYLE INTO STYLE ADDITIVE

IF llRetModu
  SELECT RETLINE
  SET RELATION TO ACCOUNT+CRMEMO INTO RETHDR,'M'+ACCOUNT INTO CUSTOMER
  SET RELATION TO STYLE INTO STYLE ADDITIVE
ENDIF

*--End of lpFilesRela.
*!*************************************************************
*! Name      : lpSetFilter
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : reconstruct filter expression
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpSetFilter
*!*************************************************************
PROCEDURE lpSetFilter

lcTrue  = IIF(XDT='S' , ".AND. INVHDR.SHIPDATE <=ldHDate" , ".AND. INVHDR.INVDATE <=ldHDate" )
lcFalse = IIF(XDT='S', ".AND. BETWEEN(INVHDR.SHIPDATE,ldLDate,ldHDate)", ".AND. BETWEEN(INVHDR.INVDATE,ldLDate,ldHDate)")

IF !EMPTY(ldHDate)
  XFILTER=XFILTER+ IIF(EMPTY(ldLDate),lcTrue,lcFalse)
  IF llInclRet
    XRFILTER = XRFILTER+ ".AND. BETWEEN(CRDATE,ldLDate,ldHDate)"
  ENDIF
ENDIF

IF ! EMPTY(XLACCOUNT)
  XFILTER = XFILTER + ".AND. BETWEEN(ACCOUNT,XLACCOUNT,XHACCOUNT)"
  IF llInclRet
    XRFILTER = XRFILTER + ".AND. BETWEEN(ACCOUNT,XLACCOUNT,XHACCOUNT)"
  ENDIF
ENDIF

IF ! EMPTY(lcSeason)
  XFILTER = XFILTER + " .AND.INVHDR.SEASON = lcSeason"
  IF llInclRet
    XRFILTER = XRFILTER + ".AND. Style.Season = lcSeason"
  ENDIF
ENDIF

IF !EMPTY(lcSlsRep)
  XFILTER = XFILTER + ".AND. (INVHDR.REP1 = lcSlsRep .OR. INVHDR.REP2 = lcSlsRep) "
  IF llRetModu AND llInclRet
    XRFILTER = XRFILTER+ ".AND. (RETHDR.SALESREP1 = lcSlsRep .OR. RETHDR.SALESREP2 = lcSlsRep )"
  ENDIF
ENDIF

IF ! EMPTY(lcDiv)
  XFILTER = XFILTER + ".AND.INVHDR.cDIVISION = lcDiv"
  IF llRetModu AND llInclRet
    XRFILTER = XRFILTER + ".AND. RETHDR.cDIVISION = lcDiv"
  ENDIF
ENDIF

IF ! EMPTY(ALLTRIM(XTCS))
  XFILTER = XFILTER + ".AND.TRIM(CUSTOMER.CLASS)$XTCS"
  IF llInclRet
    XRFILTER = XRFILTER + ".AND.TRIM(CUSTOMER.CLASS)$XTCS"
  ENDIF
ENDIF

IF ! EMPTY(laOGFxFlt[2,6])
  XFILTER = XFILTER + IIF(EMPTY(XFILTER), "" , " .AND. " ) + "INVHDR.cCurrcode = ALLTRIM(laOGFxFlt[2,6])"
  IF llRetModu AND llInclRet
    XRFILTER = XRFILTER + IIF(EMPTY(XRFILTER), "" , " .AND. " ) + " INVHDR.cCurrcode = ALLTRIM(laOGFxFlt[2,6]) "
  ENDIF
ENDIF

IF !EMPTY(lcStyGrp)
  XFILTER = XFILTER + ".AND. STYLE.cStyGroup = lcStyGrp"
  IF llInclRet
    XRFILTER = XRFILTER + " .AND. STYLE.cStyGroup = lcStyGrp"
  ENDIF
ENDIF

*--End of lpSetFilter.
*!*************************************************************
*! Name      : lpCreateTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Craete temp. file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCreateTmp
*!*************************************************************
PROCEDURE lpCreateTmp

CREATE TABLE &gcWorkDir.&INVLTEMP ;
  (TYPE  C(3)      , DATE D           , CODE  C(6)         , ACCOUNT C(5)   ,;
   NAME C(30)      , CLASS C(6)       , INVOICE C(6)       , STYLE C(19)    ,;
   LINENO N(6)     , PRICE N(12,2)    , GROS_PRICE N(12,2) , TOTQTY N(8)    , COST N(10,2)     ,;
   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , CCURRCODE  C(3)    , Nexrate N(9,4) , Ncurrunit N(4,0) ,;
   REP1 C(3)       , cdivision C(6)   , Season C(6)        , llSeason L(1)  , Discount N(13,2) ,;
   DivDesc C(30))
*--End of lpCreateTmp.
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : To get the style major segement title.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*--End of lfMajTtGet.
*!*************************************************************
*! Name      : lfNonMaj
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : To get the style nonmajor segement structure.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*--End of lfNonMaj.
*!*************************************************************
*! Name      : lfchkRet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Check existence of RM module.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfchkRet()
*!*************************************************************
FUNCTION lfchkRet

RETURN ('RM' $ gcCMpModules)

*--End of lfchkRet.
*!*************************************************************
*! Name      : lfvIncRet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Validate include returns option.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvIncRet()
*!*************************************************************
FUNCTION lfvIncRet

lnSubRetPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPSubRet'),1)
laOGObjCnt[lnSubRetPo] = llRPIncRet
llRPSubRet = IIF(llRPIncRet,llRPSubRet,.F.)
= lfOGShowGet('llRPSubRet')

lnRetOnly = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPRtOnly'),1)
laOGObjCnt[lnRetOnly] = llRPIncRet
llRPRtOnly = IIF(llRPIncRet,llRPRtOnly,.F.)
= lfOGShowGet('llRPRtOnly')

*--End of lfvIncRet.
*!*************************************************************
*! Name      : lfvSumDet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Validate Print summary or Detail option.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSumDet()
*!*************************************************************
FUNCTION lfvSumDet

lnGrnTotPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPGrnTot'),1)
laOGObjCnt[lnGrnTotPo] = lcRPSumDet = 'S'
llRPGrnTot = .F.
= lfOGShowGet('llRPGrnTot')

*--End of lfvSumDet.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Get major segment picture.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*--End of lfMajPic.
*!*************************************************************
*! Name      : lfvSalesRep
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Validate sales rep.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSalesRep()
*!*************************************************************
FUNCTION lfvSalesRep

lcRepCode = VARREAD()

IF LASTKEY() = 13 AND !MDOWN()
  IF !EMPTY(&lcRepCode) AND  !SEEK(&lcRepCode, "SALESREP")
    XREPCODE = &lcRepCode
    DO REPCHK WITH XREPCODE, .T.
    &lcRepCode = XREPCODE
  ENDIF
ELSE
  &lcRepCode = ''
ENDIF

*--End of lfvSalesRep.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng

PRIVATE ldFrom,ldTo
ldFrom = ldRPPLDate
ldTo   = ldRPPHDate
lcTitle = 'Invoice Date'
DO DateRng.Spx

ldRPPLDate = ldFrom
ldRPPHDate = ldTo

*-- End of lfvDateRng.
*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbDateOk()
*!*************************************************************
FUNCTION lfvpbDateOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF

*-- End of lfvpbDateOk.
*!*************************************************************
*! Name      : lfvObjRng
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Showes range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvObjRng()
*!*************************************************************
FUNCTION lfvObjRng
PARAMETERS lcObj

lcTitle = lcObj

DO CASE
  CASE UPPER(lcObj) = "ACCOUNT"
    STORE lcRPFrAcc  TO lcFrom,lcOldFrm
    STORE lcRPToAcc  TO lcTo,lcOldTo
    STORE "@! XXXXX" TO lcPict

  CASE UPPER(lcObj) = "STYLE"
    STORE lcRPFrSty  TO lcFrom,lcOldFrm
    STORE lcRPToSty  TO lcTo,lcOldTo
    STORE lcMajPic   TO lcPict

ENDCASE

DO ObjRng.Spx

DO CASE
  CASE UPPER(lcObj) = "ACCOUNT"
    STORE lcFrom TO lcRPFrAcc
    STORE lcTo   TO lcRPToAcc

  CASE UPPER(lcObj) = "STYLE"
    STORE lcFrom TO lcRPFrSty
    STORE lcTo   TO lcRPToSty
ENDCASE

*-- End of lfvObjRng.
*!*************************************************************
*! Name      : lfvObj
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Validate Account OR style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvObj()
*!*************************************************************
FUNCTION lfvObj

*-- this is the validation of from_item
lcRngVarNa = VARREAD()
lcRngVarVa = &lcRngVarNa.

IF !EMPTY(lcRngVarVa)
  DO CASE
    CASE UPPER(lcObj) = "ACCOUNT"
      &lcRngVarNa = lfvAccount()
    CASE UPPER(lcObj) = "STYLE"
      &lcRngVarNa = lfvStyle()
  ENDCASE
ENDIF

SHOW GET lcRngVarNa

*-- End of lfvObj.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

lcStyle = lcRngVarNa
lcTag   = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style')
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ENDIF

SET ORDER TO lcTag IN STYLE

RETURN &lcStyle

*-- End of lfvStyle.
*!*************************************************************
*! Name      : lfOldVal
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Saving old value
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOldVal()
*!*************************************************************
FUNCTION lfOldVal

lcObjNam = VARREAD()

DO CASE
  CASE UPPER(lcObjNam) = "LCFROM"
    lcOldFrm = &lcObjNam
  CASE UPPER(lcObjNam) = "LCTO"
    lcOldTo = &lcObjNam
ENDCASE

*-- End of lfOldVal.
*!*************************************************************
*! Name      : lfvpbObjOk
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Validate range screen's ok button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbObjOk()
*!*************************************************************
FUNCTION lfvpbObjOk

*-- this is the validation of to_item
IF !EMPTY(lcFrom) .OR. !EMPTY(lcTo)
  IF EMPTY(lcTo)
    lcTo = lcFrom
  ENDIF
  IF lcFrom > lcTo
    WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
   _CUROBJ = OBJNUM(lcFrom)
  ELSE
    DO CASE
      CASE UPPER(lcObj) = "ACCOUNT"
        lcRPFrAcc = lcFrom
        lcRPToAcc = lcTo
      CASE UPPER(lcObj) = "STYLE"
        lcRPFrSty = lcFrom
        lcRPToSty = lcTo
    ENDCASE
    CLEAR READ
  ENDIF
ELSE
  CLEAR READ
ENDIF

*-- End of lfvpbObjOk.
*!*************************************************************
*! Name      : lfvpbObjCa
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Validate range screen's Cancel button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbObjCa()
*!*************************************************************
FUNCTION lfvpbObjCa

*-- this is the validation of to_item
STORE lcOldFrm TO lcFrom
STORE lcOldTo  TO lcTo

DO CASE
  CASE UPPER(lcObj) = "ACCOUNT"
    lcRPFrAcc = lcFrom
    lcRPToAcc = lcTo
  CASE UPPER(lcObj) = "STYLE"
    lcRPFrSty = lcFrom
    lcRPToSty = lcTo
ENDCASE
CLEAR READ

*-- End of lfvpbObjCa.
*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Print report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************
PROCEDURE lpPrint

PRIVATE llCallGfam
llCallGfam = EMPTY(laOGFxFlt[2,6]) OR (!EMPTY(laOGFxFlt[2,6]) AND lcRpCurr <> 'F')

SELECT (INVLTEMP)
GO TOP
ROW     = 99
PAGENO  = 0
R_TITLE = 'GROSS PROFIT BY ' + lcTitle
BREAK   = &lcHBreak
IF lcReport = 'S'
  SumBrk  = &lcSumBrk
ENDIF

*-- Variables Initialization.
*XTOT8--> for the Merch discount
*XTOT9--> for the Trade discount

STORE 0 TO XTOT1  , XTOT2  , XTOT3  , XTOT4  , XTOT5  , XTOT8  , XTOT9            &&Subtotals
STORE 0 TO XGTOT1 , XGTOT2 , XGTOT3 , XGTOT4 , XGTOT5 , XGTOT8 , XGTOT9           &&Grandtotlals
STORE 0 TO XDTOT1 , XDTOT2 , XDTOT3 , XDTOT4 , XDTOT5 , XDTOT6 , XDTOT8 , XDTOT9  &&Totals for division.

STORE 0 TO XGTOTSUM1 , XGTOTSUM2 , XGTOTSUM3 , XGTOTSUM4 , XGTOTSUM5 , XGTOTSUM8 , XGTOTSUM9 &&Grandtotlals for the summery report.

XTITLE  = SPACE(1)
XTIME   = TIME()
XAcName = ' '

CLEAR TYPEAHEAD

DO WHILE .T.
   WAIT WINDOW 'Printing Transaction # : '+ &INVLTEMP..Invoice NOWAIT
   IF ROW >= 53
     PAGENO = PAGENO+1
     DO RPT_HDR WITH 'ARGRSPHF' + lcReport,lcOTitle,R_WIDTH
     IF lcReport='S' .OR. ! llInclRet
        IF lcRPProfit = "N"
          @ 05,00 SAY 'ACC. ...... NAME ......... INVOICE ' + PADR(lcMajTtl,20) + ' PIECES    S. PRICE     GROSS AMT MERCH DISC SETTL DISC      NET AMT       COST      PROFIT     PROF.%'
        ELSE
          @ 05,00 SAY 'ACC. ...... NAME ......... INVOICE ' + PADR(lcMajTtl,20) + ' PIECES    S. PRICE     GROSS AMT MERCH DISC SETTL DISC      NET AMT       COST  MERCH PROFIT MERCH    PROF%'
        ENDIF
     ELSE
       IF lcRPProfit = "N"
          @ 05,00 SAY 'ACC.  ...... NAME ..... TYP INV/CRD ' + PADR(lcMajTtl,20) + 'PIECES   S. PRICE     GROSS AMT MERCH DISC SETTL DISC      NET AMT       COST       PROFIT     PROF.%'
       ELSE
          @ 05,00 SAY 'ACC.  ...... NAME ..... TYP INV/CRD ' + PADR(lcMajTtl,20) + 'PIECES   S. PRICE     GROSS AMT MERCH DISC SETTL DISC      NET AMT       COST   MERCH PROFIT MERCH    PROF%'
       ENDIF
     ENDIF
     @ 06,00 SAY REPLICATE('=',177)
     ROW = 07
   ENDIF

   DO lpSubTotals

   STORE 0 TO XRTOT1,XRTOT2,XRTOT3,XRTOT4,XRTOT5,xRTot8,xRTot9    &&RetSubTot

   IF EOF()
     EXIT
   ENDIF

   IF ROW >=53
     ROW = 99
     LOOP
   ENDIF

   IF !(llInclRet .AND. lcReport='D' .AND. lcSort <> 'I' .AND. Invoice='ZZZZZZ')
     IF lcReport = 'D'
       DO lpDetail
     ELSE
       DO lpSummary
     ENDIF
   ENDIF

   *--Print returns without invoice.
   IF llInclRet .AND. lcSort <> 'I' .AND. Invoice = 'ZZZZZZ'
     DO lpRetNOInv
   ENDIF

   xTot1 = xTot1 + xRTot1
   xTot2 = xTot2 + xRTot2
   xTot3 = xTot3 + xRTot3
   xTot4 = xTot4 + xRTot4
   XTOt8 = XTOt8 + xRTot8
   XTOt9 = XTOt9 + xRTot9

ENDDO

*--Print returns without invoice case of invoice sort.
IF llInclRet .AND. lcSort = 'I' .AND. Invoice = 'ZZZZZZ'
  DO lpRetNOInv
  xGTot1 = xGTot1 + xRTot1
  xGTot2 = xGTot2 + xRTot2
  xGTot3 = xGTot3 + xRTot3
  xGTot4 = xGTot4 + xRTot4
  xGTot8 = xGTot8 + xRTot8
  xGTot9 = xGTot9 + xRTot9

ENDIF
*---- End main report loop ---

IF xTot1 > 0
  XGTOT1 = XGTOT1 + XTOT1
  XGTOT2 = XGTOT2 + XTOT2
  XGTOT3 = XGTOT3 + XTOT3
  XGTOT4 = XGTOT4 + XTOT4
  XGTOT8 = XGTOT8 + XTOT8
  XGTOT9 = XGTOT9 + XTOT9

  STORE 0 TO XTOT1 , XTOT2 , XTOT3 , XTOT4 , XTOT5 , XTOT8 , XTOT9
ENDIF

*-- GRAND TOTAL part.
ROW=ROW+1
@ ROW,00 SAY REPLICATE('=',177)
ROW = ROW+1
@ ROW,000 SAY '* GRAND TOTAL *'
@ ROW,054 SAY XGTOT1  PICTURE '99999999'
@ ROW,074 SAY XGTOT2  PICTURE '9999999999.99'
@ ROW,087 SAY XGTOT8  PICTURE '99999999.99'
@ ROW,098 SAY XGTOT9  PICTURE '99999999.99'
@ ROW,110 SAY XGTOT3  PICTURE '999999999.99'
@ ROW,122 SAY XGTOT4  PICTURE '99999999.99'

XGTOT5=XGTOT3-XGTOT4
XGTOT6=ROUND(IIF(XGTOT3<>0,(XGTOT5/XGTOT3)*100,0),2)
XGTOT6=IIF((XGTOT5<0 .AND. XGTOT3<0),-XGTOT6,XGTOT6)
IF lcRPProfit = "N"
  @ ROW,134 SAY XGTOT5  PICTURE '999999999.99'
  @ ROW,148 SAY XGTOT6 PICTURE '99999.99%'
ELSE
  @ ROW,136 SAY XGTOT5  PICTURE '999999999.99'
  @ ROW,154 SAY XGTOT6 PICTURE '99999.99%'
ENDIF

ROW = ROW+1
@ ROW,00 SAY REPLICATE('=',177)

WAIT CLEAR

*-- End of lpPrint.
*!*************************************************************
*! Name      : lpDetail
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : printing report in detail format.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpDetail
*!*************************************************************
PROCEDURE lpDetail

llRetExst = .F.      &&Flag for Existing of the returns.
DO WHILE .T.
  *--Calculate report fields.
  lnQty    = IIF(Type='INV',TotQty,-(TotQty))
  lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
  lnMdisc  = DISCOUNT
  lnSetTrd = lnGross * (1 - (DiscPcnt/100)) * IIF(Trde_Disc <= 0 , 0 , (Trde_Disc/100))
  lnNet    = lnGross * (1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))
  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost)

  IF lcRPProfit = "N"
    lnProfit = lnNet - lnCost
    lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)
  ELSE
    lnProfit = lnGross - lnMdisc - lnCost
    lnProPer = IIF( (lnGross - lnMdisc) <> 	0 , lnProfit/(lnGross - lnMdisc), 0)
  ENDIF

  IF !llRetExst
    IF llSeason
      @ ROW,00 SAY 'DIVISION:' + SUBSTR(BREAK,7,6) + &INVLTEMP..DIVDESC
      ROW = ROW + 2
    ENDIF
      @ ROW,000 SAY Account
      @ ROW,006 SAY SUBSTR(Name,1,IIF(llInclRet,17,21))
  ENDIF
  IF llInclRet
    @ ROW,024 SAY Type
  ENDIF

  @ ROW,028 SAY Code
  @ ROW,036 SAY Style
  @ ROW,056 SAY lnQty PICTURE '999999'
  @ ROW,063 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '9999999.99'

  IF lcSubGros = 'Y'
    @ ROW,074 SAY lnGross PICTURE '9999999999.99'
  ELSE
    IF TYPE='INV'
      @ ROW,074 SAY lnGross PICTURE '9999999999.99'
    ENDIF
  ENDIF

  @ ROW,088 SAY lnMdisc         PICTURE '9999999.99'
  @ ROW,099 SAY lnSetTrd        PICTURE '9999999.99'
  @ ROW,110 SAY lnNet           PICTURE '999999999.99'
  @ ROW,123 SAY lnCost          PICTURE '9999999.99'

  IF lcRPProfit = "N"
    @ ROW,134 SAY lnProfit        PICTURE '999999999.99'
    lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
    lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)
    @ ROW,148 SAY lnPercnt PICTURE '99999.99%'
  ELSE
    @ ROW,136 SAY lnProfit        PICTURE '999999999.99'
    lnPercnt = ROUND(IIF( (lnGross - lnMdisc) <> 0, lnProPer*100 ,0),2)
    lnPercnt = IIF(( lnProfit < 0 .AND. (lnGross - lnMdisc) < 0),-lnPercnt,lnPercnt)
    @ ROW,154 SAY lnPercnt PICTURE '99999.99%'
  ENDIF

  XRTOT1 = XRTOT1 + lnQty
  IF lcSubGros = 'Y'
    XRTOT2 = XRTOT2 + lnGross
  ELSE
    IF TYPE = 'INV'
      XRTOT2 = XRTOT2 + lnGross
    ENDIF
  ENDIF

  XRTOT8 = XRTOT8 + lnMdisc
  XRTOT9 = XRTOT9 + lnSetTrd
  XRTOT3 = XRTOT3 + lnNet
  XRTOT4 = XRTOT4 + lnCost

  XAcName = Name
  BREAK   = &lcHBreak
  SKIP

  IF BREAK # &lcHBreak
    EXIT
  ENDIF

  IF llInclRet
    IF Type = 'RET' .AND. Invoice <> 'ZZZZZZ'
      llRetExst = .T.
      ROW = ROW + 1
    ELSE
      EXIT
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO

IF llRetExst
  ROW = ROW + 1
  @ ROW,060 SAY 'NET AMOUNT AFTER RETURNS :'
  @ ROW,110 SAY XRTOT3   PICTURE '999999999.99'
  @ ROW,123 SAY XRTOT4   PICTURE '9999999.99'

  XRTOT5 = XRTOT3 - XRTOT4
  @ ROW,133 SAY XRTOT5  PICTURE '999999999.99'
  XRTOT6 = ROUND(IIF(XRTOT3<>0,(XRTOT5/XRTOT3)*100,0),2)
  XRTOT6 = IIF((XRTOT5<0 .AND. XRTOT3<0),-XRTOT6,XRTOT6)
  @ ROW,147 SAY XRTOT6 PICTURE '999.99%'
ENDIF
ROW = ROW+1

*-- End of lpDetail.
*!*************************************************************
*! Name      : lpSummary
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : printing report in summary format
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpSummary
*!*************************************************************
PROCEDURE lpSummary

DO WHILE .T.
  *--Calculate report fields.
  lnQty    = IIF(Type='INV',TotQty,-(TotQty))
  lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
  lnMdisc  = DISCOUNT
  lnSetTrd = lnGross * (1 - (DiscPcnt/100)) * IIF(Trde_Disc <= 0 , 0 , (Trde_Disc/100))
  lnNet    = lnGross * (1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))
  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost)

  IF lcRPProfit = "N"
    lnProfit = lnNet - lnCost
    lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)
  ELSE
    lnProfit = lnGross - lnMdisc - lnCost
    lnProPer = IIF( (lnGross - lnMdisc) <> 	0 , lnProfit/(lnGross - lnMdisc), 0)
  ENDIF

  XRTOT1 = XRTOT1 + lnQty

  IF lcSubGros = 'Y'
    XRTOT2 = XRTOT2 + lnGross
  ELSE
    IF TYPE = 'INV'
      XRTOT2 = XRTOT2 + lnGross
    ENDIF
  ENDIF

  XRTOT8 = XRTOT8 + lnMdisc
  XRTOT9 = XRTOT9 + lnSetTrd
  XRTOT3 = XRTOT3 + lnNet
  XRTOT4 = XRTOT4 + lnCost

  XAcName= Name
  BREAK  = &lcHBreak
  SKIP

  IF SumBrk # &lcSumBrk  
    EXIT
  ENDIF

  *B605319,1 Fix the break in case of sort by invoice. [Begin]
  IF lcSort = "I" AND (SumBrk # &lcSumBrk OR BREAK # &lcHBreak)
    EXIT
  ENDIF
  *B605319,1 Fix the break in case of sort by invoice. [End]

  IF llInclRet
    IF !(Type='RET' .AND. Invoice <> 'ZZZZZZ' )
      EXIT
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO

*-- End of lpSummary.
*!*************************************************************
*! Name      : lpSubTotals
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : printing Sub totals.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpSubTotals
*!*************************************************************
PROCEDURE lpSubTotals

*-- Begin Subtotals Loop --
DO WHILE LEN(TRIM(lcHBreak)) <> 0

  IF lcReport = 'S'
    IF &lcHBreak = BREAK
      IF LEFT(SumBrk,6) = LEFT(EVAL(lcSumBrk),6) AND SUBSTR(SumBrk,7,6) = SUBSTR(EVAL(lcSumBrk),7,6)
        EXIT
      ENDIF
    ENDIF
  ELSE
    IF &lcHBreak = BREAK
      EXIT
    ENDIF
  ENDIF

  IF lcReport = 'D'
    DO CASE
      CASE lcSort = 'A'
        lcSubTitl = LEFT(BREAK,6) + " " + SUBSTR(BREAK,7,6)
      CASE lcSort = 'C'
        lcSubTitl = LEFT(BREAK,6) + " " + SUBSTR(BREAK,7,6)
        SELECT (INVLTEMP)
      CASE lcSort = 'R'
        IF SEEK(BREAK , 'SalesRep')
          lcNamSrep = Salesrep.Name
        ENDIF
        lcSubTitl = LEFT(BREAK,6) + " " + SUBSTR(BREAK,7,6) +'  ' + lcNamSrep
      CASE lcSort = 'E'
        lcSubTitl = SUBSTR(BREAK,1,6)
      OTHERWISE      
        lcSubTitl = LEFT(BREAK,6) + " " + SUBSTR(BREAK,7,6)
    ENDCASE
  ELSE
    DO CASE
      CASE lcSort = 'A'
        lcSubTitl = BREAK + '   ' +IIF(lcReport='D',SUBSTR(XAcName,1,26),XAcName)
      CASE lcSort = 'C'
        lcSubTitl = BREAK+'   '+gfCodDes(BREAK,'CLASS')
        SELECT (INVLTEMP)
      CASE lcSort = 'R'
        lcNamSrep = SPACE(0)
        IF SEEK(IIF(EMPTY(lcRPSalRep) , BREAK , lcRPSalRep) , 'SalesRep')
          lcNamSrep = Salesrep.Name
        ENDIF
        lcSubTitl = IIF(EMPTY(lcRPSalRep) , BREAK , lcRPSalRep) +'  ' + lcNamSrep
      OTHERWISE
        lcSubTitl = BREAK
    ENDCASE  
  ENDIF

  IF lcReport = 'D'
    @ ROW,00 SAY REPLICATE('-',177)
    ROW = ROW+1
    @ ROW,000 SAY '* SUB TOTAL *  ' + lcSubTitl
  ELSE
    IF XGrdTotO='N'

     *B605319,1 Print the word 'return' instead of ZZZZZZ. [Begin]
     *@ ROW,IIF(lcSort = 'S',35,IIF(lcSort = 'I', 27 , 0)) SAY ;
     *       IIF(lcSort='C',' CLASS : ',IIF(lcSort='R',' Sales Rep : ','')) + lcSubTitl
     @ ROW,IIF(lcSort = 'S',35,IIF(lcSort = 'I', 27 , 0)) SAY ;
            IIF(lcSort='C',' CLASS : ',IIF(lcSort='R',' Sales Rep : ','')) + ;
            IIF(lcSort="I",IIF(Break="ZZZZZZ" ,"Return",lcSubTitl),lcSubTitl)
     *B605319,1 Print the word 'return' instead of ZZZZZZ. [End]
            
    ENDIF
  ENDIF

  *--SUB TOTAL
  IF XGrdTotO='N'
    @ ROW,054 SAY XTOT1  PICTURE '99999999'
    @ ROW,074 SAY XTOT2  PICTURE '9999999999.99'
    @ ROW,088 SAY XTOT8  PICTURE '9999999.99'
    @ ROW,099 SAY XTOT9  PICTURE '9999999.99'
    @ ROW,110 SAY XTOT3  PICTURE '999999999.99'
    @ ROW,122 SAY XTOT4  PICTURE '99999999.99'

    XTOT5 = XTOT3 - XTOT4
    XTOT6 = ROUND(IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0),2)
    XTOT6 = IIF((XTOT5<0 .AND. XTOT3<0),-XTOT6,XTOT6)

    IF lcRPProfit = "N"
      @ ROW,134 SAY XTOT5  PICTURE '999999999.99'
      @ ROW,148 SAY XTOT6 PICTURE '99999.99%'
    ELSE
      @ ROW,136 SAY XTOT5  PICTURE '999999999.99'
      @ ROW,154 SAY XTOT6 PICTURE '99999.99%'
    ENDIF
  ENDIF

  IF lcReport = 'D'
    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('-',177)
  ENDIF

  IF XGrdTotO='N'
    ROW = ROW+1
  ENDIF

  XGTOT1 = XGTOT1 + XTOT1
  XGTOT2 = XGTOT2 + XTOT2
  XGTOT3 = XGTOT3 + XTOT3
  XGTOT4 = XGTOT4 + XTOT4
  XGTOT8 = XGTOT8 + XTOT8
  XGTOT9 = XGTOT9 + XTOT9

  XGTOTSUM1 = XGTOTSUM1 + XTOT1
  XGTOTSUM2 = XGTOTSUM2 + XTOT2
  XGTOTSUM3 = XGTOTSUM3 + XTOT3
  XGTOTSUM4 = XGTOTSUM4 + XTOT4
  XGTOTSUM8 = XGTOTSUM8 + XTOT8
  XGTOTSUM9 = XGTOTSUM9 + XTOT9

  XDTOT1 = XDTOT1 + XTOT1
  XDTOT2 = XDTOT2 + XTOT2
  XDTOT3 = XDTOT3 + XTOT3
  XDTOT4 = XDTOT4 + XTOT4
  XDTOT8 = XDTOT8 + XTOT8
  XDTOT9 = XDTOT9 + XTOT9

  IF lcReport = 'D'
    IF SUBSTR(BREAK,7,6) # SUBSTR(EVAL(lcHBreak),7,6)
      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('=',177)
      ROW = ROW + 1
      @ ROW,000 SAY 'TOTAL FOR DIVISION :' + SUBSTR(BREAK,7,6)
      @ ROW,054 SAY XDTOT1  PICTURE '99999999'
      @ ROW,074 SAY XDTOT2  PICTURE '9999999999.99'
      @ ROW,088 SAY XDTOT8  PICTURE '9999999.99'
      @ ROW,099 SAY XDTOT9  PICTURE '9999999.99'
      @ ROW,110 SAY XDTOT3  PICTURE '999999999.99'
      @ ROW,122 SAY XDTOT4  PICTURE '99999999.99'

      XDTOT5 = XDTOT3 - XDTOT4
      XDTOT6 = ROUND(IIF(XDTOT3<>0 , (XDTOT5/XDTOT3)*100,0) , 2 )
      XDTOT6 = IIF((XDTOT5<0 .AND. XDTOT3<0), -XDTOT6 , XDTOT6)

      IF lcRPProfit = "N"
        @ ROW,134 SAY XDTOT5  PICTURE '999999999.99'
        @ ROW,148 SAY XDTOT6  PICTURE '99999.99%'
      ELSE
        @ ROW,136 SAY XDTOT5  PICTURE '999999999.99'
        @ ROW,154 SAY XDTOT6  PICTURE '99999.99%'
      ENDIF

      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('=',177)
      ROW = ROW + 2

      STORE 0 TO XDTOT1 , XDTOT2 , XDTOT3 , XDTOT4 , XDTOT8 , XDTOT9
    ENDIF
  ELSE
    IF LEFT(SumBrk,6) # LEFT(EVAL(lcSumBrk),6) AND SUBSTR(SumBrk,7,6) = SUBSTR(EVAL(lcSumBrk),7,6)
      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('-',177)
      ROW = ROW + 1
      @ ROW,000 SAY '* SUB TOTAL *  ' + LEFT(SumBrk,6) + " " + SUBSTR(SumBrk,7,6)
      @ ROW,054 SAY XDTOT1  PICTURE '99999999'
      @ ROW,074 SAY XDTOT2  PICTURE '9999999999.99'
      @ ROW,088 SAY XDTOT8  PICTURE '9999999.99'
      @ ROW,099 SAY XDTOT9  PICTURE '9999999.99'
      @ ROW,110 SAY XDTOT3  PICTURE '999999999.99'
      @ ROW,122 SAY XDTOT4  PICTURE '99999999.99'

      XDTOT5 = XDTOT3 - XDTOT4
      XDTOT6 = ROUND(IIF(XDTOT3<>0 , (XDTOT5/XDTOT3)*100,0) , 2 )
      XDTOT6 = IIF((XDTOT5<0 .AND. XDTOT3<0), -XDTOT6 , XDTOT6)

      IF lcRPProfit = "N"
        @ ROW,134 SAY XDTOT5  PICTURE '999999999.99'
        @ ROW,148 SAY XDTOT6  PICTURE '99999.99%'
      ELSE
        @ ROW,136 SAY XDTOT5  PICTURE '999999999.99'
        @ ROW,154 SAY XDTOT6  PICTURE '99999.99%'
      ENDIF

      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('-',177)
      ROW = ROW + 2
      
      STORE 0 TO XDTOT1 , XDTOT2 , XDTOT3 , XDTOT4 , XDTOT8 , XDTOT9
    
    ENDIF  

  ENDIF

  IF lcReport = 'S'

    *B604963,1 BWA 26/09/2001 Fix the bug in the summary report which is the subtotal part doesn't print.[START]
    *IF LEFT(SumBrk,6) # LEFT(EVAL(lcSumBrk),6) AND SUBSTR(SumBrk,7,6) # SUBSTR(EVAL(lcSumBrk),7,6)
    IF (LEFT(SumBrk,6) # LEFT(EVAL(lcSumBrk),6) AND SUBSTR(SumBrk,7,6) # SUBSTR(EVAL(lcSumBrk),7,6))  OR;
      (LEFT(SumBrk,6) = LEFT(EVAL(lcSumBrk),6) AND SUBSTR(SumBrk,7,6) # SUBSTR(EVAL(lcSumBrk),7,6))
    *B604963,1 BWA 26/09/2001.[END]

      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('-',177)
      ROW = ROW + 1
      @ ROW,000 SAY '* SUB TOTAL *  ' + LEFT(SumBrk,6) + " " + SUBSTR(SumBrk,7,6)
      @ ROW,054 SAY XDTOT1  PICTURE '99999999'
      @ ROW,074 SAY XDTOT2  PICTURE '9999999999.99'
      @ ROW,088 SAY XDTOT8  PICTURE '9999999.99'
      @ ROW,099 SAY XDTOT9  PICTURE '9999999.99'
      @ ROW,110 SAY XDTOT3  PICTURE '999999999.99'
      @ ROW,122 SAY XDTOT4  PICTURE '99999999.99'

      XDTOT5 = XDTOT3 - XDTOT4
      XDTOT6 = ROUND(IIF(XDTOT3<>0 , (XDTOT5/XDTOT3)*100,0) , 2 )
      XDTOT6 = IIF((XDTOT5<0 .AND. XDTOT3<0), -XDTOT6 , XDTOT6)

      IF lcRPProfit = "N"
        @ ROW,134 SAY XDTOT5  PICTURE '999999999.99'
        @ ROW,148 SAY XDTOT6  PICTURE '99999.99%'
      ELSE
        @ ROW,136 SAY XDTOT5  PICTURE '999999999.99'
        @ ROW,154 SAY XDTOT6  PICTURE '99999.99%'
      ENDIF

      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('-',177)
      ROW = ROW + 2
      
      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('=',177)
      ROW = ROW + 1
      @ ROW,000 SAY 'TOTAL FOR DIVISION :' + SUBSTR(SumBrk,7,6)
      @ ROW,054 SAY XGTOTSUM1  PICTURE '99999999'
      @ ROW,074 SAY XGTOTSUM2  PICTURE '9999999999.99'
      @ ROW,088 SAY XGTOTSUM8  PICTURE '9999999.99'
      @ ROW,099 SAY XGTOTSUM9  PICTURE '9999999.99'
      @ ROW,110 SAY XGTOTSUM3  PICTURE '999999999.99'
      @ ROW,122 SAY XGTOTSUM4  PICTURE '99999999.99'

      *B605116,1 Correct the variables names. [Begin]
      *XGTOT5 = XGTOT3 - XGTOT4
      *XGTOT6 = ROUND(IIF(XGTOT3<>0 , (XGTOT5/XGTOT3)*100,0) , 2 )
      *XGTOT6 = IIF((XGTOT5 < 0 .AND. XGTOT3 < 0), -XGTOT6 , XGTOT6)
      *IF lcRPProfit = "N"
      *  @ ROW,134 SAY XGTOT5  PICTURE '999999999.99'
      *  @ ROW,148 SAY XGTOT6  PICTURE '99999.99%'
      *ELSE
      *  @ ROW,136 SAY XGTOT5  PICTURE '999999999.99'
      *  @ ROW,154 SAY XGTOT6  PICTURE '99999.99%'
      *ENDIF

      XGTOTSUM5 = XGTOTSUM3 - XGTOTSUM4
      XGTOTSUM6 = ROUND(IIF(XGTOTSUM3 <> 0 , (XGTOTSUM5/XGTOTSUM3)*100,0) , 2 )
      XGTOTSUM6 = IIF((XGTOTSUM5 < 0 .AND. XGTOTSUM3 < 0), -XGTOTSUM6 , XGTOTSUM6)
      IF lcRPProfit = "N"
        @ ROW,134 SAY XGTOTSUM5  PICTURE '999999999.99'
        @ ROW,148 SAY XGTOTSUM6  PICTURE '99999.99%'
      ELSE
        @ ROW,136 SAY XGTOTSUM5  PICTURE '999999999.99'
        @ ROW,154 SAY XGTOTSUM6  PICTURE '99999.99%'
      ENDIF
      *B605116,1 Correct the variables names. [End]

      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('=',177)
      ROW = ROW + 2

      STORE 0 TO XDTOT1 , XDTOT2 , XDTOT3 , XDTOT4 , XDTOT8 , XDTOT9
      STORE 0 TO XGTOTSUM1 , XGTOTSUM2 , XGTOTSUM3 , XGTOTSUM4 , XGTOTSUM5 , XGTOTSUM8 , XGTOTSUM9 &&Grandtotlals for the summery report.

    ENDIF
  ENDIF

  STORE 0 TO XTOT1 , XTOT2 , XTOT3 , XTOT4 , XTOT5 , XTOT8 , XTOT9
  IF lcReport = 'S'
    SumBrk = &lcSumBrk
    BREAK = &lcHBreak
  ELSE
    BREAK = &lcHBreak
  ENDIF

  EXIT
ENDDO

*-- End of lpSubTotals.
*!*************************************************************
*! Name      : lpRetNOInv
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : IF include returns and printing by invoice date
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpRetNOInv
*!*************************************************************
PROCEDURE lpRetNOInv

IF lcReport = 'D' .AND. IIF(lcSort <> 'I', &lcHBreak = BREAK,.T.)
  IF llSeason
    @ ROW,00 SAY 'DIVISION:' + SUBSTR(BREAK,7,6) + &INVLTEMP..DIVDESC
    ROW = ROW + 2
  ELSE
    ROW = ROW + 1
  ENDIF
  @ ROW,00 SAY 'RETURNS WITHOUT INVOICES :'
ENDIF

*B604963,1 BWA 26/09/2001 Fix the bug in the summary report which is the subtotal part doesn't print.[START]
*DO WHILE INVOICE = 'ZZZZZZ' .AND. IIF(lcSort <> 'I', &lcHBreak = BREAK ,.T.)

*B605319,1 Fix the displaying of data. [Begin]
*DO WHILE INVOICE = 'ZZZZZZ' .AND. IIF(lcSort <> 'I', IIF(lcReport = 'D' , &lcHBreak = BREAK , &lcSumBrk = SumBrk),.T.)
DO WHILE INVOICE = 'ZZZZZZ' .AND. IIF(lcSort <> 'I', IIF(lcReport = 'D' , &lcHBreak = BREAK , &lcSumBrk = SumBrk AND &lcHBreak = BREAK),.T.)
*B605319,1 Fix the displaying of data. [End]

*B604963,1 BWA 26/09/2001.[END]


  lnQty    = IIF(TYPE='INV',TOTQTY,-(TOTQTY))
  lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
  lnMdisc  = DISCOUNT
  lnSetTrd = lnGross * (1 - (DiscPcnt/100)) * IIF(Trde_Disc <= 0 , 0 , (Trde_Disc/100))
  lnNet    = lnGross * (1 - (DISCPCNT/100)) * (1 - (TRDE_DISC/100))
  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost)

  IF lcRPProfit = "N"
    lnProfit = lnNet - lnCost
    lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)
  ELSE
    lnProfit = lnGross - lnMdisc - lnCost
    lnProPer = IIF( (lnGross - lnMdisc) <> 	0 , lnProfit/(lnGross - lnMdisc), 0)
  ENDIF

  IF lcReport = 'D'
    ROW = ROW+1
    @ ROW,000 SAY Account
    @ ROW,06 SAY SUBSTR(Name,1,17)
    @ ROW,024 SAY Type
    @ ROW,028 SAY Code
    @ ROW,036 SAY Style
    @ ROW,056 SAY lnQty PICTURE '999999'
    @ ROW,063 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '9999999.99'

    IF lcSubGros = 'Y'
      @ ROW,074 SAY lnGross PICTURE '9999999999.99'
    ELSE
      IF TYPE='INV'
        @ ROW,074 SAY lnGross PICTURE '9999999999.99'
      ENDIF
    ENDIF
    @ ROW,088 SAY lnMdisc         PICTURE '9999999.99'
    @ ROW,099 SAY lnSetTrd        PICTURE '9999999.99'
    @ ROW,112 SAY lnNet           PICTURE '9999999.99'
    @ ROW,123 SAY lnCost          PICTURE '9999999.99'

    IF lcRPProfit = "N"
      @ ROW,134 SAY lnProfit        PICTURE '999999999.99'
      lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
      lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)
      @ ROW,148 SAY lnPercnt PICTURE '99999.99%'
    ELSE
      @ ROW,136 SAY lnProfit        PICTURE '999999999.99'
      lnPercnt = ROUND(IIF( (lnGross - lnMdisc) <> 0, lnProPer*100 ,0),2)
      lnPercnt = IIF(( lnProfit < 0 .AND. (lnGross - lnMdisc) < 0),-lnPercnt,lnPercnt)
      @ ROW,154 SAY lnPercnt PICTURE '99999.99%'
    ENDIF
  ENDIF

  xRTot1 = xRTot1 + lnQty
  IF lcSubGros = 'Y'
    xRTot2 = xRTot2 + lnGross
  ELSE
    IF TYPE = 'INV'
      xRTot2 = xRTot2 + lnGross
    ENDIF
  ENDIF

  XRTOT8 = XRTOT8 + lnMdisc
  XRTOT9 = XRTOT9 + lnSetTrd
  xRTot3 = xRTot3 + lnNet
  xRTot4 = xRTot4 + lnCost
  xRTot5 = xRTOT3 - XRTOT4
  xRTot6 = ROUND(IIF(XRTOT3<>0,(XRTOT5/XRTOT3)*100,0),2)
  xRTot6 = IIF((XRTOT5<0 .AND. XRTOT3<0),-XRTOT6,XRTOT6)
  
  BREAK = &lcHBreak
  SKIP
  
ENDDO 

ROW = ROW + IIF(lcReport='D',1,0)
IF (xRTot1+xRTot3<>0 .AND. lcSort = 'I'.AND. lcReport='S')
  ROW = ROW + 1
  @ ROW,00 SAY 'RETURNS WITHOUT INVOICES :'
  @ ROW,078 SAY xRTot1  PICTURE '999999'
  @ ROW,096 SAY xRTot2  PICTURE '999999999.99'
  @ ROW,110 SAY xRTot3  PICTURE '999999999.99'
  @ ROW,123 SAY xRTot4  PICTURE '999999999.99'
  @ ROW,133 SAY xRTot5  PICTURE '99999999.99'
  @ ROW,147 SAY xRTot6  PICTURE '999.99%'
ENDIF

*-- End of lpRetNOInv.
*!*************************************************************
*! Name      : lfwCurCode
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :
*!*************************************************************
FUNCTION lfwCurCode

lcOldCurr = laOGFxFlt[2,6]

*-- End of lfwCurCode.
*!*************************************************************
*! Name      : lfvCurCode
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :
*!*************************************************************
FUNCTION lfvCurCode

lnAlias = SELECT(0)
*-- Allow blank curreny in case user have no selected forign currency.
IF EMPTY(laOGFxFlt[2,6]) .AND. lcRpCurr <> 'F' 
  RETURN
ENDIF

IF !SEEK(laOGFxFlt[2,6],'SYCCURR') .OR. ATC("?",laOGFxFlt[2,6]) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    laOGFxFlt[2,6] = lcOldCurr
  ELSE
    laOGFxFlt[2,6] = laTemp[1]
  ENDIF
ENDIF

SHOW GET laOGFxFlt[2,6]

SELECT(lnAlias)

*-- End of lfvCurCode.
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : This function called from the currency Display
*!             field to display currency options screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :
*!*************************************************************
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- If forgin currency you must be sure invoice currency has value.
IF lcRpCurr = 'F' .AND. EMPTY(laOGFxFlt[2,6])
  laOGFxFlt[2,6]= gcBaseCurr
ENDIF
SHOW GET laOGFxFlt[2,6]

*-- End of lfvCurDisp.
*!*************************************************************
*! Name      : lfRepShow
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Show the currency filter.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :
*!*************************************************************
FUNCTION lfRepShow

laOGFxFlt[2,6]= gcBaseCurr
=lfOGShowGet("lnRepCurr")

*-- End of lfRepShow.
*!********************************************************************
*! Name      : lfGetFCurr
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Return the Foreign amount From Base currency.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : lcRpDispCur  && The way to display the amount.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcTmepFile   && The temp file name that hold the temp.
*!           :                 exchange rates.
*!           : llAprvCurr   && If you are using the Approved currency.
*!********************************************************************
*! Call      : From all the AP reports that is using the currency display
*!           : feature.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
*! Example   : lfGetFCurr(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.).
*!********************************************************************
FUNCTION lfGetFCurr
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile

PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)
IF lcRpDispCur = 'F'
  lnExRate   = 0
  lnUnit     = 0

  IF EMPTY(lcGetFile)
    lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
  ELSE
    lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
  ENDIF

  IF lcCurrCode = gcBaseCurr
    lnExRate = 1
    lnUnit   = 1
  ELSE
    ldExRateDt = DATE
    lnExRate = &INVLTEMP..Nexrate
    lnUnit   = &INVLTEMP..Ncurrunit
  ENDIF

  lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
  lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
  lcExSin1 = IIF(lcExSin1 = '/','*','/')
  lcExSin2 = IIF(lcExSin2 = '*','/','*')
  lnAmount = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
ENDIF

SELECT (lnSavAlias)
RETURN lnAmount

*-- End of lfGetFCurr.
*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPbOk()
*!*************************************************************
FUNCTION lfvPbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  ldRPPLDate = ldFrom
  ldRPPHDate = ldTo
  CLEAR READ
ENDIF

*-- End of lfvPbOk.
*!*************************************************************
*! Name      : lfvPostDat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Add the posting date filter to the filter if selected.
*!*************************************************************
*! Called from : ARGRSPHF.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPostDat()
*!*************************************************************
FUNCTION lfvPostDat
PRIVATE lcDatPost

STORE  "" TO lcDatPost

lnDataPos = ASCAN(laOgVrFlt,'Post Date')
IF lnDataPos > 0
  lnDataPos  = ASUBSCRIPT(laOgVrFlt,lnDataPos,1)
  DO CASE
    CASE AT('Like',laOgVrFlt[lnDataPos,5])             <> 0       && Like
      lnDatPost = AT('(DTOS(Post Date)',LCRPEXP)

    CASE AT('Greater Than',laOgVrFlt[lnDataPos,5])     <> 0       && Greater Than
      lnDatPost = AT('(DTOS(Post Date)',LCRPEXP)

    CASE AT('Less Than',laOgVrFlt[lnDataPos,5])        <> 0       && Less Than
      lnDatPost = AT('(DTOS(Post Date)',LCRPEXP)

    CASE AT('Greater Or Equal',laOgVrFlt[lnDataPos,5]) <> 0       && Greater Or Equal
      lnDatPost = AT('(DTOS(Post Date)',LCRPEXP)

    CASE AT('Less Or Equal',laOgVrFlt[lnDataPos,5])    <> 0       && Less Or Equal
      lnDatPost = AT('(DTOS(Post Date)',LCRPEXP)

    CASE AT('Between',laOgVrFlt[lnDataPos,5])          <> 0       && Between
      lnDatPost = AT('(BETWEEN(DTOS(Post Date)',LCRPEXP)

    CASE AT('In List',laOgVrFlt[lnDataPos,5])          <> 0       && In List
      lnDatPost = AT('(INLIST(DTOS(Post Date)',LCRPEXP)
  ENDCASE

  IF lnDatPost > 0
    lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnDatPost))
    IF lnPos1 > 0
      lcDatPost = SUBSTR(lcRpExp ,lnDatPost , lnPos1-1)
    ELSE
      lcDatPost = SUBSTR(lcRpExp ,lnDatPost)
    ENDIF
  ENDIF

  *--Case invhdr file
  lcDatPost = STRTRAN(lcDatPost,"Post Date" , "INVHDR.DPOSTDATE")
  IF !EMPTY(lcDatPost)
    XFILTER  = XFILTER  + IIF(EMPTY(lcDatPost) ,""," .AND. "  ) + lcDatPost
  ENDIF

  lcDatPost = STRTRAN(lcDatPost,"INVHDR." , "RETHDR.")
  IF !EMPTY(lcDatPost)
    XRFILTER = XRFILTER + IIF(EMPTY(lcDatPost) ,""," .AND. "  ) + lcDatPost
  ENDIF
ENDIF

*-- End of lfvPostDat.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

SET ORDER TO CODES    IN CODES
SET ORDER TO CUSTOMER IN CUSTOMER
SET ORDER TO SALESREP IN SALESREP
SET ORDER TO STYLE    IN STYLE
SET ORDER TO INVHDR   IN INVHDR
SET ORDER TO INVLINES IN INVLINE

IF llRetModu
  SET ORDER TO RETHDRA  IN RETHDR
  SET ORDER TO RETLINES IN RETLINE
ENDIF

IF EMPTY(laOgVrFlt)
  SHOW GET pbAdvanced DISABLE
ENDIF

= lfvSumDet()

*-- End of lfwRepWhen.
*!*************************************************************
*! Name      : lfvAccount
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/19/2001
*! Purpose   : Validate function for the Account.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvAccount()
*!*************************************************************
FUNCTION lfvAccount

lcAccount = lcRngVarNa
IF LASTKEY() = 13 AND !MDOWN()
  XACCOUNT = &lcAccount
  IF !SEEK('M'+XACCOUNT,'Customer'))
    DO CUSBROWM WITH XACCOUNT
    &lcAccount = XACCOUNT
  ENDIF
ENDIF

RETURN &lcAccount

*--End of lfvAccount.