*:***************************************************************************
*: Program file  : ARVIT20
*: Program desc. : Custom Gross profit for Vital
*: System        : Aria Advantage Series.
*: Module        : Account receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Notes : This report is an exact copy of the standard Gross Profit Report 
*:         We've removed bugs , enhancements and customs.     
*:***************************************************************************
*: Example : DO ARVIT20
*:***************************************************************************
*C102473,1 SSE 03/05/2002 Custom modifications adding Royality code to option grid.

*C102473,1 Define a variable for Royality. [Begin]
PRIVATE lcRoyalExp
lcRoyalExp = ''
*C102473,1 Define a variable for Royality. [End]

*-- ldRPPLDate Variable that hold From date value
*-- ldRPPHDate Variable that hold to   date value
*-- lcRPFrSty  Variable that hold From style value
*-- lcRPToSty  Variable that hold To   style value
*-- lcRPFrAcc  Variable that hold From Account value
*-- lcRPToAcc  Variable that hold To   Account value
*-- lcRPSeason Variable that hold Season
*-- lcRPDiv    Variable that hold Division

STORE SPACE(0) TO lcTrue , lcFalse

ldLDate   = ldRPPLDate
ldHDate   = ldRPPHDate
lnMajorLen = LEN(gfItemMask("PM"))
XLSTYLE   = PADR(lcRPFrSty,lnMajorLen)
XHSTYLE   = PADR(lcRPToSty,lnMajorLen)
XLACCOUNT = lcRPFrAcc
XHACCOUNT = lcRPToAcc
lcSeason  = lcRPSeason
lcDiv     = lcRPDiv

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
XFILTER = "INVHDR->Status<>'V'"
IF llInclRet 
  XRFILTER= "RETHDR.Status<>'V'"
ENDIF

=lfvPostDat()

*--Set and build the filters due to grid var.
DO lpSetFilter
lcSFilt=' '

*--Creat a temp file.
INVLTEMP = gfTEMPNAME()
DO lpCreateTmp

WAIT WINDOW 'Building the invoices records...' NOWAIT
*-- Build the invoice records.
SELECT INVLINE
IF !EMPTY(XLSTYLE)
  lcSetNear=SET('NEAR')
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
  lcSFilt='REST WHILE STYLE<=XHSTYLE '
ELSE
  IF !EMPTY(XHSTYLE)  
    lcSFilt='REST WHILE STYLE<=XHSTYLE '
  ENDIF
ENDIF

IF EMPTY(XLSTYLE) AND EMPTY(XHSTYLE)
  SET ORDER TO Invline
ENDIF

SCAN &lcSFilt FOR &XFILTER
  WAIT WINDOW 'Collecting invoice # : '+Invoice NOWAIT

  SCATTER FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,GROS_PRICE,TOTQTY,COST TO laInvRec

  SELECT &INVLTEMP
  APPEND BLANK  
  GATHER FROM laInvRec FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,GROS_PRICE,TOTQTY,COST
  
  REPLACE TYPE  WITH 'INV',;
          CODE  WITH INVLINE->INVOICE,;
          DATE  WITH INVLINE->INVDATE,;             
          NAME  WITH CUSTOMER->BTNAME,;
          CLASS WITH CUSTOMER->CLASS,;
          DISCPCNT  WITH INVHDR->DISCPCNT,;
          TRDE_DISC WITH INVHDR->TRDE_DISC ,;          
          Ccurrcode WITH INVHDR.Ccurrcode  ,;
          Nexrate   WITH INVHDR.Nexrate     ,;
          Ncurrunit WITH INVHDR.Ncurrunit
          
  *C102473,1 Update the Royality code from the style file. [Begin]
  *REPLACE REP1  WITH INVHDR.REP1
  REPLACE Rep1 WITH InvHdr.Rep1 , Royalty WITH Style.Royalty
  *C102473,1 Update the Royality code from the style file. [End]
  
ENDSCAN

SELECT &INVLTEMP 

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
    lcInvoice=Invoice
    DIMENSION laInvRec[6]

    SCATTER FIELDS ACCOUNT,STYLE,LINENO,GROS_PRICE,PRICE,TOTQTY,COST TO laInvRec
    SELECT &INVLTEMP 
    IF ! SEEK(lcInvoice)
      lcInvoice='ZZZZZZ'      && Returns without invoice or for invoices
                              && not in date range are holding the ZZZZZZ.
    ENDIF

    =SEEK(RETLINE.ACCOUNT+RETLINE.CRMEMO,'RETHDR')
    APPEND BLANK
    GATHER FROM laInvRec FIELDS ACCOUNT,STYLE,LINENO,GROS_PRICE,PRICE,TOTQTY,COST
            
    REPLACE TYPE WITH 'RET',;
            INVOICE WITH lcInvoice,;
            CODE  WITH RETLINE->CRMEMO      ,;
            DATE  WITH RETLINE->CRDATE      ,;             
            NAME  WITH CUSTOMER->BTNAME     ,;
            CLASS WITH CUSTOMER->CLASS      ,;
            Ccurrcode WITH RETHDR.Ccurrcode ,;
            Nexrate   WITH RETHDR.Nexrate   ,;
            Ncurrunit WITH RETHDR.Ncurrunit
              
    *C102473,1 Update the Royality code from the style file. [Begin]
    *REPLACE REP1  WITH RetHdr.SalesRep1
    REPLACE Rep1 WITH RetHdr.SalesRep1 , Royalty WITH Style.Royalty
    *C102473,1 Update the Royality code from the style file. [End]
  
  ENDSCAN
ENDIF

SELECT &INVLTEMP 
IF RECCOUNT() = 0
  WAIT CLEAR
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*--Clear relations
SELECT INVLINE
SET RELATION TO

IF llRetModu
  SELECT RETLINE
  SET RELATION TO
ENDIF

*-- Indexing file.
Z = RECCOUNT()
WAIT WINDOW 'Sorting '+STR(Z,6)+' records for the report ' NOWAIT

SELECT &INVLTEMP 
DO CASE
  CASE lcSort='A'
    INDEX ON Account+Invoice+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    lcHBreak='ACCOUNT'
  CASE lcSort='I'
    INDEX ON Invoice+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    lcHBreak='INVOICE'
  CASE lcSort='S'
    INDEX ON Style+Invoice+Type+Code+STR(RECNO(),7) TAG &INVLTEMP    
    lcHBreak='STYLE'
  CASE lcSort='C'
    INDEX ON Class+Invoice+Account+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    lcHBreak='CLASS'    
  CASE lcSort='R'
    INDEX ON REP1+Invoice+Account+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    lcHBreak='REP1'
    
  *C102473,1 Add the new sort by Royality. [Begin]
  CASE lcSort='O'
    INDEX ON Royalty+Invoice+Account+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    lcHBreak = 'ROYALTY'
  *C102473,1 Add the new sort by Royality. [End]

ENDCASE

WAIT CLEAR
R_WIDTH = 'W'

SET DEVICE TO PRINT
DO lpPrint
DO ENDREPORT
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lpFilesRela
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Create files relation
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

*!*************************************************************
*! Name      : lpSetFilter
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
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

*C102473,1 Generate the inlist for Account and Style. [Begin]
IF ALLTRIM(laOGFxFlt[lnRoyalPos,1]) $ lcRpExp
  DO lpGenExpr
ENDIF
*C102473,1 Generate the inlist for Account and Style. [End]

lcTrue = IIF(XDT='S' , ".AND. INVHDR.SHIPDATE <=ldHDate" , ".AND. INVHDR.INVDATE <=ldHDate" )
lcFalse = IIF(XDT='S', ".AND. BETWEEN(INVHDR.SHIPDATE,ldLDate,ldHDate)", ".AND. BETWEEN(INVHDR.INVDATE,ldLDate,ldHDate)")

IF !EMPTY(ldHDate)
  
  XFILTER=XFILTER+ IIF(EMPTY(ldLDate),lcTrue,lcFalse)

  IF llInclRet
    XRFILTER=XRFILTER+ ".AND. BETWEEN(CRDATE,ldLDate,ldHDate)"
  ENDIF 
ENDIF

IF ! EMPTY(XLACCOUNT)
  XFILTER = XFILTER + ".AND. BETWEEN(ACCOUNT,XLACCOUNT,XHACCOUNT)"
  IF llInclRet
    XRFILTER= XRFILTER + ".AND. BETWEEN(ACCOUNT,XLACCOUNT,XHACCOUNT)"
  ENDIF
ENDIF

IF ! EMPTY(lcSeason)
  XFILTER = XFILTER + " .AND.INVHDR->SEASON=lcSeason"
  IF llInclRet
    XRFILTER= XRFILTER + ".AND. Style.Season=lcSeason"
  ENDIF
ENDIF

IF ! EMPTY(lcSlsRep)
  XFILTER = XFILTER + ".AND.INVHDR->REP1=lcSlsRep"
  IF llRetModu AND llInclRet  
    XRFILTER= XRFILTER+ ".AND.RETHDR->SALESREP1=lcSlsRep"  
  ENDIF
ENDIF

IF ! EMPTY(lcDiv)
  XFILTER = XFILTER + ".AND.INVHDR->cDIVISION=lcDiv"
  IF llRetModu AND llInclRet  
    XRFILTER= XRFILTER + ".AND. RETHDR->cDIVISION=lcDiv"
  ENDIF
ENDIF

IF ! EMPTY(ALLTRIM(XTCS)) 
  XFILTER = XFILTER + ".AND.TRIM(CUSTOMER->CLASS)$XTCS"
  IF llInclRet  
    XRFILTER= XRFILTER + ".AND.TRIM(CUSTOMER->CLASS)$XTCS" 
  ENDIF
ENDIF

IF ! EMPTY(laOGFxFlt[2,6])
  XFILTER = XFILTER + IIF(EMPTY(XFILTER), "" , " .AND. " ) + "INVHDR.cCurrcode = ALLTRIM(laOGFxFlt[2,6])"
  IF llRetModu AND llInclRet  
    XRFILTER= XRFILTER + IIF(EMPTY(XRFILTER), "" , " .AND. " ) + " INVHDR.cCurrcode = ALLTRIM(laOGFxFlt[2,6]) "
  ENDIF
ENDIF

IF !EMPTY(lcStyGrp)
  XFILTER = XFILTER + ".AND. STYLE.cStyGroup=lcStyGrp"
  IF llInclRet  
    XRFILTER= XRFILTER + " .AND. STYLE.cStyGroup=lcStyGrp"
  ENDIF  
ENDIF

*C102473,1 Add the new inlist style group expression. [Begin]
IF !EMPTY(lcRoyalExp)
  XFILTER = XFILTER + [ AND ] + lcRoyalExp
  IF llInclRet
    XRFILTER = XRFILTER + [ AND ] + lcRoyalExp
  ENDIF
ENDIF
*C102473,1 Add the new inlist style group expression. [End]

*!*************************************************************
*! Name      : lpCreateTmp
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
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

*C102473,1 Add the Royality field to the temp file. [Begin]
*CREATE TABLE &gcWorkDir.&INVLTEMP ;
*  (TYPE  C(3)  , DATE D       , CODE  C(6)   , ACCOUNT C(5) ,;
*   NAME C(30)  , CLASS C(6)   , INVOICE C(6) , STYLE C(19)  ,;
*   LINENO N(6) , PRICE N(12,2) , GROS_PRICE N(12,2) , TOTQTY N(8)  , COST N(10,2) ,;
*   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , CCURRCODE  C(3) , Nexrate N(9,4) , Ncurrunit N(4,0),;
*   REP1 C(3))

CREATE TABLE &gcWorkDir.&INVLTEMP ;
  (TYPE  C(3)  , DATE D       , CODE  C(6)   , ACCOUNT C(5) ,;
   NAME C(30)  , CLASS C(6)   , INVOICE C(6) , STYLE C(19)  ,;
   LINENO N(6) , PRICE N(12,2) , GROS_PRICE N(12,2) , TOTQTY N(8)  , COST N(10,2) ,;
   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , CCURRCODE  C(3) , Nexrate N(9,4) , Ncurrunit N(4,0),;
   REP1 C(3) , ROYALTY C(6))
*C102473,1 Add the Royality field to the temp file. [End]

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style major segement title
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

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style nonmajor segement structure
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

*!*************************************************************
*! Name      : lfchkRet
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Check existence of RM module
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfchkRet()
*!*************************************************************

FUNCTION lfchkRet

RETURN ('RM' $ gcCMpModules)

*!*************************************************************
*! Name      : lfvIncRet
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate include returns option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
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

*!*************************************************************
*! Name      : lfvSumDet
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate Print summary or Detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
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

*!*************************************************************
*! Name      : lfMajPic
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
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

*!*************************************************************
*! Name      : lfvSalesRep
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate sales rep.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSalesRep()
*!*************************************************************

FUNCTION lfvSalesRep

lcRepCode = VARREAD()

IF '?' $ &lcRepCode OR  !EMPTY(&lcRepCode) AND  !SEEK(&lcRepCode, "SALESREP")  
  XREPCODE = &lcRepCode
  DO REPCHK WITH XREPCODE, .T.
  &lcRepCode = XREPCODE
ENDIF

*!*************************************************************
*! Name      : lfvDateRng
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
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
*-- end of lfvDateRng.

*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

*!*************************************************************
*! Name      : lfvObjRng
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
 
*!*************************************************************
*! Name      : lfvObj
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

*!*************************************************************
*! Name      : lfvStyle
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

lcTag = ORDER('STYLE')

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

*!*************************************************************
*! Name      : lfOldVal
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

*!*************************************************************
*! Name      : lfvpbObjOk
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

IF !EMPTY(lcFrom) OR !EMPTY(lcTo)
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

*!*************************************************************
*! Name      : lfvpbObjCa
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

*!*************************************************************
*! Name      : lpPrint
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
llCallGfam=EMPTY(laOGFxFlt[2,6]) OR (!EMPTY(laOGFxFlt[2,6]) AND lcRpCurr <> 'F')

SELECT &INVLTEMP
GO TOP

ROW     = 99
PAGENO  = 0
R_TITLE = 'GROSS PROFIT BY ' + lcHBreak
BREAK   = &lcHBreak

*C102473,1 Define 2 variables to hold old Royality code and long name. [Begin]
PRIVATE lcRoyalCod , lcRoyalDes
STORE '' TO lcRoyalCod , lcRoyalDes
*C102473,1 Define 2 variables to hold old Royality code and long name. [End]

*-- Variables Initialization.
STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5        &&Subtotals
STORE 0 TO XGTOT1,XGTOT2,XGTOT3,XGTOT4,XGTOT5   &&Grandtotlals

XTITLE = SPACE(1)
XTIME  = TIME()
PAGENO = 0
ROW    = 99
XAcName= ' '

CLEAR TYPEAHEAD

DO WHILE .T.

   WAIT WINDOW 'Printing Transaction # : '+ &INVLTEMP..Invoice NOWAIT

   IF ROW >= 53
     PAGENO = PAGENO+1
     DO RPT_HDR WITH 'ARVIT20'+lcReport,lcOTitle,R_WIDTH
     IF lcReport='S' OR ! llInclRet
     
       *C102473,1 Add the royality title. [Begin]       
       *@ 05,00 SAY 'ACC. ...... NAME ......... INVOICE ' + PADR(lcMajTtl,20) + ' PIECES    S. PRICE     GROSS AMT      NET AMT       COST      PROFIT  PROF.%'
       IF lcSort = 'O'
         @ 05,00 SAY 'ROYALTY ...... NAME ...... INVOICE ' + PADR(lcMajTtl,20) + ' PIECES   ' + IIF(lcReport='S',SPACE(1),SPACE(0)) + 'S. PRICE    ' + IIF(lcReport='S',SPACE(0),SPACE(1)) + 'GROSS AMT      NET AMT       COST      ' + + IIF(lcReport='S',SPACE(1),SPACE(0)) + 'PROFIT  ' + IIF(lcReport='S',SPACE(0),SPACE(1)) + 'PROF.%'
       ELSE
         @ 05,00 SAY 'ACC. ...... NAME ......... INVOICE ' + PADR(lcMajTtl,20) + ' PIECES   ' + IIF(lcReport='S',SPACE(1),SPACE(0)) + 'S. PRICE    ' + IIF(lcReport='S',SPACE(0),SPACE(1)) + 'GROSS AMT      NET AMT       COST      ' + + IIF(lcReport='S',SPACE(1),SPACE(0)) + 'PROFIT  ' + IIF(lcReport='S',SPACE(0),SPACE(1)) + 'PROF.%'
       ENDIF
       *C102473,1 Add the royality title. [End]

     ELSE

       *C102473,1 Add the royality title. [Begin]       
       *@ 05,00 SAY 'ACC.  ...... NAME ..... TYP INV/CRD ' + PADR(lcMajTtl,20) + 'PIECES   S. PRICE     GROSS AMT      NET AMT       COST      PROFIT   PROF.%'
       IF lcSort = 'O'
         @ 05,00 SAY 'ROYALTY ..... NAME .... TYP INV/CRD ' + PADR(lcMajTtl,20) + 'PIECES   S. PRICE     GROSS AMT      NET AMT       COST      PROFIT   PROF.%'
       ELSE
         @ 05,00 SAY 'ACC.  ...... NAME ..... TYP INV/CRD ' + PADR(lcMajTtl,20) + 'PIECES   S. PRICE     GROSS AMT      NET AMT       COST      PROFIT   PROF.%'
       ENDIF  
       *C102473,1 Add the royality title. [End]

     ENDIF
     @ 06,00 SAY REPLICATE('=',132)
     ROW = 07
   ENDIF

   DO lpSubTotals   &&SubTotals

   STORE 0 TO XRTOT1,XRTOT2,XRTOT3,XRTOT4,XRTOT5    &&RetSubTot

   IF EOF() OR (llInclRet .AND. lcSort='I' .AND. Invoice='ZZZZZZ')
     EXIT  
   ENDIF

   IF ROW >=53
     ROW = 99
     LOOP
   ENDIF
    
   IF !(llInclRet .AND. lcReport='D' .AND. lcSort<>'I' .AND. Invoice='ZZZZZZ')
     IF lcReport = 'D'
       DO lpDetail
     ELSE
       DO lpSummary
     ENDIF
   ENDIF

   *--Print returns without invoice.
   IF llInclRet .AND. lcSort<>'I' .AND. Invoice='ZZZZZZ' 
     DO lpRetNOInv
   ENDIF

   xTot1 = xTot1 + xRTot1 
   xTot2 = xTot2 + xRTot2 
   xTot3 = xTot3 + xRTot3 
   xTot4 = xTot4 + xRTot4 

ENDDO

*--Print returns without invoice case of invoice sort.
IF llInclRet .AND. lcSort='I' .AND. Invoice='ZZZZZZ'
  DO lpRetNOInv
  xGTot1 = xGTot1 + xRTot1 
  xGTot2 = xGTot2 + xRTot2
  xGTot3 = xGTot3 + xRTot3 
  xGTot4 = xGTot4 + xRTot4
ENDIF
*---- End main report loop ---

IF xTot1 > 0
  XGTOT1 = XGTOT1 + XTOT1
  XGTOT2 = XGTOT2 + XTOT2
  XGTOT3 = XGTOT3 + XTOT3
  XGTOT4 = XGTOT4 + XTOT4
  STORE 0 TO XTOT1 , XTOT2 , XTOT3 , XTOT4 , XTOT5
ENDIF

*-- GRAND TOTAL part.
ROW=ROW+1
@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW+1
@ ROW,000 SAY '* GRAND TOTAL *'

@ ROW,054 SAY XGTOT1  PICTURE '99999999'
@ ROW,074 SAY XGTOT2  PICTURE '9999999999.99'
@ ROW,088 SAY XGTOT3  PICTURE '999999999.99'
@ ROW,100 SAY XGTOT4  PICTURE '99999999.99'

XGTOT5=XGTOT3-XGTOT4 

*C102473,1 Shift Grand Profit one character to the left. [Begin]
*@ ROW,112 SAY XGTOT5  PICTURE '999999999.99'
@ ROW,111 SAY XGTOT5 PICTURE '999999999.99'
*C102473,1 Shift Grand Profit one character to the left. [End]

XGTOT6=ROUND(IIF(XGTOT3<>0,(XGTOT5/XGTOT3)*100,0),2)
XGTOT6=IIF((XGTOT5<0 .AND. XGTOT3<0),-XGTOT6,XGTOT6)

*@ ROW,123 SAY XGTOT6 PICTURE '99999.99 %'
@ ROW,125 SAY XGTOT6 PICTURE '999.99%'

ROW = ROW+1
@ ROW,00 SAY REPLICATE('=',132)
WAIT CLEAR

*!*************************************************************
*! Name      : lpDetail
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : printing report in detail format
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

llRetExst=.F.      &&Flag for Existing of the returns.
DO WHILE .T.
  *--Calculate report fields.
  lnQty    = IIF(Type='INV',TotQty,-(TotQty))
  lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
  lnNet    = lnGross * (1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))  
  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost) 
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

  IF ! llRetExst
    
    *C102473,1 Print Royality code and description. [Begin]
    *@ ROW,000 SAY Account    
    *@ ROW,006 SAY SUBSTR(Name,1,IIF(llInclRet,17,21))
    IF lcSort = 'O'
      IF lcRoyalCod <> Royalty
        lcRoyalCod = Royalty
        lcRoyalDes = gfCodDes(Royalty,'ROYALTY')
      ENDIF
      
      @ ROW,000 SAY Royalty
      @ ROW,008 SAY SUBSTR(lcRoyalDes,1,IIF(llInclRet,15,18))
    ELSE
      @ ROW,000 SAY Account
      @ ROW,006 SAY SUBSTR(Name,1,IIF(llInclRet,17,21))
    ENDIF  
    *C102473,1 Print Royality code and description. [End]

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
  @ ROW,088 SAY lnNet PICTURE '999999999.99'    
  @ ROW,101 SAY lnCost          PICTURE '9999999.99'
  @ ROW,111 SAY lnNet - lnCost  PICTURE '999999999.99'
 
  lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
  lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)
  @ ROW,125 SAY lnPercnt PICTURE '999.99%'

  llGMA = IIF(TYPE('llGMA') = "U", .F. , llGMA )
  IF llGMA
    IF TYPE='INV' .AND. SEEK(Style,'STYLE') .AND. !EMPTY(Style.Desc1)
      ROW=ROW+1
      @ ROW,038 SAY Style.Desc1
    ENDIF
  ENDIF

  XRTOT1 = XRTOT1 + lnQty
  
  IF lcSubGros = 'Y'
    XRTOT2 = XRTOT2 + lnGross       
  ELSE
    IF TYPE='INV'     
      XRTOT2 = XRTOT2 + lnGross       
    ENDIF
  ENDIF

  XRTOT3 = XRTOT3 + lnNet
  XRTOT4 = XRTOT4 + lnCost
 
  XAcName=Name
  BREAK  =&lcHBreak
  SKIP

  IF llInclRet
    IF Type='RET' .AND. Invoice<>'ZZZZZZ'  &&Return exist for invoice.
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
  @ ROW,088 SAY XRTOT3   PICTURE '999999999.99'
  @ ROW,101 SAY XRTOT4   PICTURE '9999999.99'

  XRTOT5=XRTOT3-XRTOT4 
  @ ROW,111 SAY XRTOT5  PICTURE '999999999.99'
  XRTOT6=ROUND(IIF(XRTOT3<>0,(XRTOT5/XRTOT3)*100,0),2)
  XRTOT6=IIF((XRTOT5<0 .AND. XRTOT3<0),-XRTOT6,XRTOT6)
  
  @ ROW,125 SAY XRTOT6 PICTURE '999.99%'
ENDIF
ROW=ROW+1


*!*************************************************************
*! Name      : lpSummary
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
  lnNet    = lnGross * (1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))
  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost) 
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)
  
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

  XRTOT1 = XRTOT1 + lnQty
  
  IF lcSubGros = 'Y'
    XRTOT2 = XRTOT2 + lnGross       
  ELSE
    IF TYPE='INV'     
      XRTOT2 = XRTOT2 + lnGross       
    ENDIF
  ENDIF

  XRTOT3 = XRTOT3 + lnNet
  XRTOT4 = XRTOT4 + lnCost

  XAcName=Name
  BREAK  =&lcHBreak
  SKIP

  IF llInclRet
    IF ! (Type='RET' .AND. Invoice<>'ZZZZZZ')
      EXIT
    ENDIF
  ELSE
    EXIT 
  ENDIF  
ENDDO

*!*************************************************************
*! Name      : lpSubTotals
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : printing Sub totals
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

  IF &lcHBreak = BREAK
     EXIT
  ENDIF

  DO CASE
    CASE lcSort='A'
      lcSubTitl=BREAK+'   '+IIF(lcReport='D',SUBSTR(XAcName,1,26),XAcName)
    CASE lcSort='C'
      lcSubTitl=BREAK+'   '+gfCodDes(BREAK,'CLASS')      
      SELECT &INVLTEMP
    
    *C102473,1 Add the Royality title. [Begin]
    CASE lcSort='O'
      lcSubTitl = BREAK+'   '+gfCodDes(BREAK,'ROYALTY')      
      SELECT &INVLTEMP
    *C102473,1 Add the Royality title. [End]
    
    CASE lcSort='R'
      IF SEEK(BREAK , 'SalesRep')
        lcNamSrep = Salesrep.Name
      ENDIF
      lcSubTitl=BREAK +'  ' + lcNamSrep
    
    OTHE
      lcSubTitl=BREAK
  ENDCASE  

  IF lcReport='D'
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW+1
    @ ROW,000 SAY '* SUB TOTAL *  '+lcSubTitl
  ELSE
    IF XGrdTotO='N'
     
     *C102473,1 Add the Royality title. [Begin]
     *@ ROW,IIF(lcSort = 'S',35,IIF(lcSort = 'I', 27 , 0)) SAY ;
     *       IIF(lcSort='C',' CLASS : ',IIF(lcSort='R',' Sales Rep : ',''))+lcSubTitl
     @ ROW,IIF(lcSort = 'S',35,IIF(lcSort = 'I', 27 , 0)) SAY ;
            IIF(lcSort='C',' CLASS : ',IIF(lcSort='O',' ROYALTY : ',IIF(lcSort='R',' Sales Rep : ','')))+lcSubTitl
     *C102473,1 Add the Royality title. [End]

    ENDIF
  ENDIF

  IF XGrdTotO='N'
    @ ROW,054 SAY XTOT1  PICTURE '99999999'
    @ ROW,074 SAY XTOT2  PICTURE '9999999999.99'
    @ ROW,088 SAY XTOT3  PICTURE '999999999.99'
    @ ROW,100 SAY XTOT4  PICTURE '99999999.99'

    XTOT5=XTOT3-XTOT4
    
    *C102473,1 Shift Profit one character to the left. [Begin]
    *@ ROW,112 SAY XTOT5  PICTURE '999999999.99'
    @ ROW,111 SAY XTOT5  PICTURE '999999999.99'
    *C102473,1 Shift Profit one character to the left. [End]

    XTOT6=ROUND(IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0),2)
    XTOT6=IIF((XTOT5<0 .AND. XTOT3<0),-XTOT6,XTOT6)
    
    @ ROW,125 SAY XTOT6 PICTURE '999.99%'
  ENDIF

  IF lcReport='D'
    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('-',132)
  ENDIF 

  IF XGrdTotO='N'
    ROW = ROW+1
  ENDIF

  XGTOT1 = XGTOT1 + XTOT1
  XGTOT2 = XGTOT2 + XTOT2
  XGTOT3 = XGTOT3 + XTOT3
  XGTOT4 = XGTOT4 + XTOT4
  STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5

  BREAK=&lcHBreak

  EXIT
ENDDO
*-- End Subtotals Loop --

*!*************************************************************
*! Name      : lpRetNOInv
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

IF lcReport='D' .AND. IIF(lcSort<>'I',&lcHBreak=BREAK,.T.)
  ROW = ROW + 1
  @ ROW,00 SAY 'RETURNS WITHOUT INVOICES :'       
ENDIF

DO WHILE INVOICE='ZZZZZZ' .AND. IIF(lcSort<>'I',&lcHBreak=BREAK,.T.)

  lnQty    = IIF(TYPE='INV',TOTQTY,-(TOTQTY))
  lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
  lnNet    = lnGross * (1 - (DISCPCNT/100)) * (1 - (TRDE_DISC/100))
  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost) 
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

  IF lcReport='D'
    ROW=ROW+1

    *C102473,1 Print Royality code and description. [Begin]
    *@ ROW,000 SAY Account
    *@ ROW,06 SAY SUBSTR(Name,1,17)    
    IF lcSort = 'O'
      IF lcRoyalCod <> Royalty
        lcRoyalCod = Royalty
        lcRoyalDes = gfCodDes(Royalty,'ROYALTY')
      ENDIF
      
      @ ROW,000 SAY Royalty
      @ ROW,008 SAY SUBSTR(lcRoyalDes,1,IIF(llInclRet,15,18))
    ELSE
      @ ROW,000 SAY Account
      @ ROW,06 SAY SUBSTR(Name,1,17)    
    ENDIF  
    *C102473,1 Print Royality code and description. [End]

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
    
    *C102473,1 Increase picture. [Begin]
    *@ ROW,088  SAY lnNet           PICTURE '9999999.99'
    @ ROW,088  SAY lnNet PICTURE '999999999.99'
    *C102473,1 Increase picture. [End]
    
    @ ROW,101 SAY lnCost          PICTURE '9999999.99'   
    @ ROW,111 SAY lnNet - lnCost  PICTURE '999999999.99'
    lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
    lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)
    
    @ ROW,125 SAY lnPercnt PICTURE '999.99%'
    
    llGMA = IIF(TYPE('llGMA') = "U", .F. , llGMA )
    IF llGMA
      IF SEEK(Style ,'STYLE') .AND. !EMPTY(Style.Desc1)
        ROW=ROW+1
        @ ROW,038 SAY Style.Desc1
      ENDIF
    ENDIF
    
  ENDIF
    
  xRTot1 = xRTot1 + lnQty
  IF lcSubGros = 'Y'
    xRTot2 = xRTot2 + lnGross
  ELSE
    IF TYPE='INV'     
      xRTot2 = xRTot2 + lnGross
    ENDIF
  ENDIF
  xRTot3 = xRTot3 + lnNet
  xRTot4 = xRTot4 + lnCost
  
  xRTot5 = xRTOT3 - XRTOT4
  xRTot6 = ROUND(IIF(XRTOT3<>0,(XRTOT5/XRTOT3)*100,0),2)
  xRTot6 = IIF((XRTOT5<0 .AND. XRTOT3<0),-XRTOT6,XRTOT6)
  
  BREAK=&lcHBreak

 SKIP
ENDDO  

ROW = ROW + IIF(lcReport='D',1,0)
IF (xRTot1+xRTot3<>0 .AND. lcSort='I'.AND. lcReport='S')
  ROW = ROW + 1
  @ ROW,00 SAY 'RETURNS WITHOUT INVOICES :'       

  @ ROW,056 SAY xRTot1 PICTURE '999999'
  @ ROW,074 SAY xRTot2 PICTURE '999999999.99'
  @ ROW,088 SAY xRTot3 PICTURE '999999999.99'
  @ ROW,101  SAY xRTot4 PICTURE'999999999.99'
  @ ROW,111 SAY xRTot5 PICTURE  '99999999.99'
  @ ROW,125 SAY xRTot6 PICTURE      '999.99%'
ENDIF

*!*************************************************************
*! Name      : lfwCurCode
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
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

*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
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
lnAlias=SELECT(0)


*-- Allow blank curreny in case user have no selected forign currency.
IF EMPTY(laOGFxFlt[2,6]) .AND. lcRpCurr <> 'F' 
  RETURN
ENDIF

IF !SEEK(laOGFxFlt[2,6],'SYCCURR') OR ATC("?",laOGFxFlt[2,6]) > 0
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

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : This function called from the currency Display
*!             field to display currency options screen .
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)


*-- If forgin currency you must be sure invoice currency has value.
IF lcRpCurr = 'F' .AND. EMPTY(laOGFxFlt[2,6])
  laOGFxFlt[2,6]= gcBaseCurr
ENDIF
SHOW GET laOGFxFlt[2,6]


*!*************************************************************
*! Name      : lfRepShow
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*
FUNCTION lfRepShow

laOGFxFlt[2,6]= gcBaseCurr
=lfOGShowGet("lnRepCurr")

*!********************************************************************
*! Name      : lfGetFCurr
*! Developer : Sameh Aldesouki (SAM)
*! Date      : 01/16/2000
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
    
    ldExRateDt=DATE
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
*-- end of lfGetFCurr.

*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 03/03/2000
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

*!*************************************************************
*! Name      : lfvPostDat
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/26/2000
*! Purpose   : Add the posting date filter to the filter if selected.
*!*************************************************************
*! Called from : ARVIT20.PRG
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

*-- End of lfvPostDat

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

*C102473,1 Get the position of both Account and Style. [Begin]
lnRoyalPos = lfItmPos('STYLE.ROYALTY')     && get Royality Fixed filter Position.
*C102473,1 Get the position of both Account and Style. [End]

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

IF ALLTRIM(gfGetMemVar('M_LINK_GL')) <>  'Y'
  SHOW GET pbAdvanced DISABLE
ENDIF

= lfvSumDet()

*!*************************************************************
*! Name      : lfvAccount
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/2001
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

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/05/2002
*! Purpose   : Generate Expression.
*!***************************************************************************
*! Example   : DO lpGenExpr
*!***************************************************************************
*C102473,1 Remove Royality In list.
PROCEDURE lpGenExpr
PRIVATE lcAlias , lnX
lcAlias = ALIAS()

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1] , laBrTmpFlt[1]
=ACOPY(laOGFxFlt,laTempExpr)         && Copy Fixed Filter Array to Temp Array.

*-- If user selected Royality.
IF ALLTRIM(laTempExpr[lnRoyalPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy only Royal expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnRoyalPos,1),7)
  
  *-- Generate expression for Royal.
  lcRoyalExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- we need to convert Roayl in filter Expression to true
  lcRpExp = STRTRAN(lcRpExp,lcRoyalExp,".T.")
ENDIF  
*-- EndIf of user selected Royality.

*-- Restore original laOGFxFlt.
DIMENSION laOGFxFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)         && Restore Fixed Filter Array to Temp Array.

SELECT (lcAlias)
*-- End of lpGenExpr.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/03/2002
*! Purpose   : To get the position of the fixed filter in OG.
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*C102473,1
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

