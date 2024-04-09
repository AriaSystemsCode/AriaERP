*:***************************************************************************
*: Program file  : ARGRSPDP.PRG
*: Program desc. : CUSTOMIZED GROSS PROFIT BY DEPARTEMENT FOR GMA.
*: Date          : 09/11/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102439
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARGRSPDP
*:***************************************************************************
*: Modifications :
*:C102514,1 SSE 01/03/2002 Custom add new report layout.
***************************************************************************
*-- ldRPPLDate Variable that hold From date value
*-- ldRPPHDate Variable that hold to   date value
*-- lcRPFrSty  Variable that hold From style value
*-- lcRPToSty  Variable that hold To   style value
*-- lcRPFrAcc  Variable that hold From Account value
*-- lcRPToAcc  Variable that hold To   Account value
*-- lcRPSeason Variable that hold Season
*-- lcRPDiv    Variable that hold Division

*C102514,1 Define 2 variables for Account and style expression [Begin]
IF lcRpReport = "D" AND EMPTY(lcRpOutPut)
  =lfTextFile()
  IF EMPTY(lcRpOutPut)
    *=gfModalGen("TRM000000B00000","DIALOG",'','','You have to select a text file.')
    SET DEVICE TO SCREEN
    RETURN
  ENDIF   
ENDIF

PRIVATE lcStyleExp , lcAcctExp , lcStyGrp , lcDeptExp , lcSumBrk , SumBrk
STORE '' TO lcStyleExp , lcAcctExp , lcStyGrp , lcDeptExp
lcSumBrk = ".F."
SumBrk   = &lcSumBrk
*C102514,1 Define 2 variables for Account and style expression [End]

STORE SPACE(0) TO lcTrue , lcFalse , lcRturnDec

ldLDate    = ldRPPLDate
ldHDate    = ldRPPHDate
lnMajorLen = LEN(gfItemMask("PM"))
XLSTYLE    = PADR(lcRPFrSty,lnMajorLen)
XHSTYLE    = PADR(lcRPToSty,lnMajorLen)
XLACCOUNT  = lcRPFrAcc
XHACCOUNT  = lcRPToAcc
lcSeason   = lcRPSeason
lcDiv      = lcRPDiv

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
  XRFILTER = "RETHDR.Status<>'V'"
ENDIF

*C102514,1 Add Customer department to lcRpExp. [Begin]
*IF AT('INLIST(CUSTDEPT.DEPT',LCRPEXP) > 0 AND lcRPFrAcc = lcRPToAcc
IF AT('INLIST(CUSTDEPT.DEPT',LCRPEXP) > 0 AND USED(laOGFxFlt[lnAcctPos,6]) AND RECCOUNT(laOGFxFlt[lnAcctPos,6]) = 1
*C102514,1 Add Customer department to lcRpExp. [End]

  =lfvDept()
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

*C102514,1 SSE Commented Out. [Begin]
*IF ! EMPTY(XLSTYLE)
*  lcSetNear=SET('NEAR')
*  SET NEAR ON
*  SEEK XLSTYLE
*  IF EOF()
*    WAIT CLEAR
*    =gfModalGen('TRM00052B00000','DIALOG')
*    SET DEVICE TO SCREEN
*    SET NEAR &lcSetNear
*    RETURN
*  ENDIF
*  SET NEAR &lcSetNear
*  lcSFilt = 'REST WHILE STYLE<=XHSTYLE '
*ELSE
*  IF !EMPTY(XHSTYLE)
*    lcSFilt = 'REST WHILE STYLE<=XHSTYLE '
*  ENDIF
*ENDIF

*IF EMPTY(XLSTYLE) AND EMPTY(XHSTYLE)
*  SET ORDER TO Invline
*ENDIF
*C102514,1 SSE Commented Out. [Begin]

*C102514,1 Add the style expression to XFILTER. [Begin]
*SCAN &lcSFilt FOR &XFILTER
SCAN FOR &XFILTER
*C102514,1 Add the style expression to XFILTER. [End]

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
          Dept      WITH INVHDR.DEPT
          
  *C102514,1 Add the style group code. [Begin]
  REPLACE cStyGroup WITH Style.cStyGroup
  *C102514,1 Add the style group code. [End]
ENDSCAN

SELECT (INVLTEMP)
lcRturnRet = SPACE(0)

IF llRetModu AND llInclRet
  WAIT WINDOW 'Building the returns records...' NOWAIT
  *-- Build the returns records.
  INDEX ON Invoice + STR(RECNO(),7) TAG (INVLTEMP)
  SELECT RETLINE
  
  *C102514,1 SSE Commented Out. [Begin]
  *IF ! EMPTY(XLSTYLE)
  *  lcOldNear = SET("NEAR")
  *  SET NEAR ON
  *  =SEEK(XLSTYLE)
  *  SET NEAR &lcOldNear
  *ENDIF
  *C102514,1 SSE Commented Out. [End]

  XRFILTER = STRTRAN(XRFILTER,"INVHDR.cCurrcode","RETHDR.cCurrcode")

  *C102514,1 Add the style expression to XFILTER. [Begin]
  *SCAN &lcSFilt FOR &XRFILTER
  SCAN FOR &XRFILTER
  *C102514,1 Add the style expression to XFILTER. [End]
  
    WAIT WINDOW 'Collecting credit memo # : '+Crmemo NOWAIT
    lcInvoice = Invoice
    DIMENSION laInvRec[6]
    SCATTER FIELDS ACCOUNT,STYLE,LINENO,GROS_PRICE,PRICE,TOTQTY,COST TO laInvRec
    SELECT (INVLTEMP)
    IF !SEEK(lcInvoice)
      lcInvoice = 'ZZZZZZ'      && Returns without invoice or for invoices
                                && not in date range are holding the ZZZZZZ.
    ENDIF
    llInsert = IIF(EMPTY(lcDeptVal) , .T. , lfDeptRet())
    IF llInsert
      =SEEK(RETLINE.ACCOUNT+RETLINE.CRMEMO,'RETHDR')
      APPE BLANK
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
              REP1      WITH RetHdr.SalesRep1 ,;
              Dept      WITH IIF(EMPTY(lcDeptVal) , lfDeptAll() , lcRturnRet)

      *C102514,1 Add the style group code. [Begin]
      REPLACE cStyGroup WITH Style.cStyGroup
      *C102514,1 Add the style group code. [End]
              
    ENDIF
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
SET RELATION TO

IF llRetModu
  SELECT RETLINE
  SET RELATION TO
ENDIF

*-- Indexing file.
Z = RECCOUNT()
WAIT WINDOW 'Sorting '+STR(Z,6)+' records for the report ' NOWAIT

SELECT (INVLTEMP)

*C102514,1 Add the report layout to the index. [Begin]
*DO CASE
*  CASE lcSort = 'A'
*    INDEX ON Account+Invoice+Style+Type+Code+STR(RECNO(),7) TAG (INVLTEMP)
*    lcHBreak = 'ACCOUNT'
*  CASE lcSort = 'T'
*    INDEX ON Account+DEPT+Invoice+Style+Type+Code+STR(RECNO(),7) TAG (INVLTEMP)
*    lcHBreak = 'ACCOUNT+DEPT'
*  CASE lcSort = 'I'
*    INDEX ON Invoice+Style+Type+Code+STR(RECNO(),7) TAG (INVLTEMP)
*    lcHBreak = 'INVOICE'
*  CASE lcSort = 'S'
*    INDEX ON Style+Invoice+Type+Code+STR(RECNO(),7) TAG (INVLTEMP)
*    lcHBreak = 'STYLE'
*  CASE lcSort = 'C'
*    INDEX ON Class+Invoice+Account+Style+Type+Code+STR(RECNO(),7) TAG (INVLTEMP)
*    lcHBreak = 'CLASS'
*  CASE lcSort = 'R'
*    INDEX ON REP1+Invoice+Account+Style+Type+Code+STR(RECNO(),7) TAG (INVLTEMP)
*    lcHBreak = 'REP1'
*ENDCASE

DO CASE
  CASE lcRpReport = "S"
    DO CASE
      CASE lcSort = 'A'
        INDEX ON Account+Invoice+Style+Type+Code+STR(RECNO(),7) TAG (INVLTEMP)
        lcHBreak = 'ACCOUNT'
      CASE lcSort = 'T'
        INDEX ON Account+DEPT+Invoice+Style+Type+Code+STR(RECNO(),7) TAG (INVLTEMP)
        lcHBreak = 'ACCOUNT+DEPT'
      CASE lcSort = 'I'
        INDEX ON Invoice+Style+Type+Code+STR(RECNO(),7) TAG (INVLTEMP)
        lcHBreak = 'INVOICE'
      CASE lcSort = 'S'
        INDEX ON Style+Invoice+Type+Code+STR(RECNO(),7) TAG (INVLTEMP)
        lcHBreak = 'STYLE'
      CASE lcSort = 'C'
        INDEX ON Class+Invoice+Account+Style+Type+Code+STR(RECNO(),7) TAG (INVLTEMP)
        lcHBreak = 'CLASS'
      CASE lcSort = 'R'
        INDEX ON REP1+Invoice+Account+Style+Type+Code+STR(RECNO(),7) TAG (INVLTEMP)
        lcHBreak = 'REP1'
    ENDCASE
  CASE lcRpReport = "Y"
    INDEX ON Account+DEPT+cStyGroup TAG (INVLTEMP)
    lcHBreak = 'ACCOUNT+DEPT+CSTYGROUP'
    lcSumBrk = 'ACCOUNT+DEPT'
  CASE lcRpReport = "D"
    INDEX ON Account+DEPT+Style TAG (INVLTEMP)
    lcHBreak = 'ACCOUNT+DEPT+STYLE'
    lcSumBrk = 'ACCOUNT+DEPT'    
ENDCASE      
*C102514,1 Add the report layout to the index. [End]

WAIT CLEAR
R_WIDTH = 'W'
SET DEVICE TO PRINT

*C102514,1 Change printing according to Report Layout. [Begin]
*DO lpPrint
*DO ENDREPORT

DO CASE
  CASE lcRpReport = "S"
    DO lpPrint
    DO ENDREPORT
  CASE lcRpReport = "Y"  
    DO lpDeptPrnt
    DO ENDREPORT
  CASE lcRpReport = "D"
    PRIVATE lnHandle
    lnHandle = FCREATE(lcRpOutput)
    IF lnHandle < 0  
      =gfModalGen("TRM000000B00000","DIALOG",'','','Cannot open output file.')
      RETURN
    ENDIF
    
    DO lpDownLPrt
    =FCLOSE(lnHandle)
ENDCASE
*C102514,1 Change printing according to Report Layout. [End]

SET DEVICE TO SCREEN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpFilesRela
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Create files relation.
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

*C102514,1 Check if user selected Style Dept. [Begin]
IF USED(laOGFxFlt[lnStyDepPo,6])
  IF RECCOUNT(laOGFxFlt[lnStyDepPo,6]) > 0
    SELECT IcDeptDt
    SET RELATION TO Dept INTO laOGFxFlt[lnStyDepPo,6] ADDITIVE
  
    SELECT InvLine
    SET RELATION TO Style INTO IcDeptDt ADDITIVE
  ENDIF
ENDIF  
*C102514,1 Check if user selected Style Dept. [End]

SELECT INVHDR
SET RELATION TO Invhdr.ACCOUNT INTO CUSTDEPT ADDITIVE

IF llRetModu
  SELECT RETLINE
  SET RELATION TO ACCOUNT+CRMEMO INTO RETHDR,'M'+ACCOUNT INTO CUSTOMER
  SET RELATION TO STYLE INTO STYLE ADDITIVE

  *C102514,1 Check if user selected Style Dept. [Begin]
  IF USED(laOGFxFlt[lnStyDepPo,6])
    IF RECCOUNT(laOGFxFlt[lnStyDepPo,6]) > 0
      SET RELATION TO Style INTO IcDeptDt ADDITIVE
    ENDIF
  ENDIF  
  *C102514,1 Check if user selected Style Dept. [End]

ENDIF

*--End of lpFilesRela.
*!*************************************************************
*! Name      : lpSetFilter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : reconstruct filter expression.
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

lcTrue = IIF(XDT='S' , ".AND. INVHDR.SHIPDATE <=ldHDate" , ".AND. INVHDR.INVDATE <=ldHDate" )
lcFalse = IIF(XDT='S', ".AND. BETWEEN(INVHDR.SHIPDATE,ldLDate,ldHDate)", ".AND. BETWEEN(INVHDR.INVDATE,ldLDate,ldHDate)")

*C102514,1 Generate the inlist for Account and Style. [Begin]
IF ALLTRIM(laOGFxFlt[lnAcctPos,1]) $ lcRpExp OR ALLTRIM(laOGFxFlt[lnStylePos,1]) $ lcRpExp  OR ;
  ALLTRIM(laOGFxFlt[lnStyGrpPo,1]) $ lcRpExp OR ALLTRIM(laOGFxFlt[lnStyDepPo,1]) $ lcRpExp
  DO lpGenExpr
ENDIF
*C102514,1 Generate the inlist for Account and Style. [End]

IF !EMPTY(ldHDate)
  XFILTER = XFILTER+ IIF(EMPTY(ldLDate),lcTrue,lcFalse)
  IF llInclRet
    XRFILTER = XRFILTER + ".AND. BETWEEN(CRDATE,ldLDate,ldHDate)"
  ENDIF
ENDIF

*C102514,1 Add the new inlist account and style expression. [Begin]
*IF !EMPTY(XLACCOUNT)
*  XFILTER = XFILTER + ".AND. BETWEEN(ACCOUNT,XLACCOUNT,XHACCOUNT)"
*  IF llInclRet
*    XRFILTER = XRFILTER + ".AND. BETWEEN(ACCOUNT,XLACCOUNT,XHACCOUNT)"
*  ENDIF
*ENDIF
IF !EMPTY(lcAcctExp)
  XFILTER = XFILTER + [ AND ] + lcAcctExp
  IF llInclRet
    XRFILTER = XRFILTER + [ AND ] + STRTRAN(lcAcctExp,'INVLINE.','RETLINE.')
  ENDIF
ENDIF
IF !EMPTY(lcStyleExp)
  XFILTER = XFILTER + [ AND ] + lcStyleExp
  IF llInclRet
    XRFILTER = XRFILTER + [ AND ] + STRTRAN(lcStyleExp,'INVLINE.STYLE+INVLINE.INVOICE+STR(INVLINE.LINENO,6)','RETLINE.STYLE')
  ENDIF
ENDIF
IF !EMPTY(lcDeptExp)
  XFILTER = XFILTER + [ AND ] + "!EOF(laOGFxFlt[lnStyDepPo,6])"
  IF llInclRet
    XRFILTER = XRFILTER + [ AND ] + "!EOF(laOGFxFlt[lnStyDepPo,6])"
  ENDIF
ENDIF
*C102514,1 Add the new inlist account and style expression. [End]

IF !EMPTY(lcSeason)
  XFILTER = XFILTER + " .AND.INVHDR.SEASON = lcSeason"
  IF llInclRet
    XRFILTER = XRFILTER + ".AND. Style.Season = lcSeason"
  ENDIF
ENDIF

IF ! EMPTY(lcSlsRep)
  XFILTER = XFILTER + ".AND.INVHDR.REP1 = lcSlsRep"
  IF llRetModu AND llInclRet
    XRFILTER = XRFILTER+ ".AND.RETHDR.SALESREP1 = lcSlsRep"
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

IF !EMPTY(laOGFxFlt[2,6])
  XFILTER = XFILTER + IIF(EMPTY(XFILTER), "" , " .AND. " ) + "INVHDR.cCurrcode = ALLTRIM(laOGFxFlt[2,6])"
  IF llRetModu AND llInclRet
    XRFILTER = XRFILTER + IIF(EMPTY(XRFILTER), "" , " .AND. " ) + " INVHDR.cCurrcode = ALLTRIM(laOGFxFlt[2,6]) "
  ENDIF
ENDIF

*C102514,1 Add the new inlist style group expression. [Begin]
*IF !EMPTY(lcStyGrp)
*  XFILTER = XFILTER + ".AND. STYLE.cStyGroup = lcStyGrp"
*  IF llInclRet
*    XRFILTER = XRFILTER + " .AND. STYLE.cStyGroup = lcStyGrp"
*  ENDIF
*ENDIF
IF !EMPTY(lcStyGrp)
  XFILTER = XFILTER + [ AND ] + lcStyGrp
  IF llInclRet
    XRFILTER = XRFILTER + [ AND ] + lcStyGrp
  ENDIF
ENDIF
*C102514,1 Add the new inlist style group expression. [End]

*--End of lpSetFilter.
*!*************************************************************
*! Name      : lpCreateTmp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Craete temp. file.
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

*C102514,1 Add the Style Group code and description. [Begin] 
*CREATE TABLE &gcWorkDir.&INVLTEMP ;
*  (TYPE  C(3)      , DATE D           , CODE  C(6)         , ACCOUNT C(5)                      ,;
*   NAME C(30)      , CLASS C(6)       , INVOICE C(6)       , STYLE C(19)                       ,;
*   LINENO N(6)     , PRICE N(12,2)    , GROS_PRICE N(12,2) , TOTQTY N(8)    , COST N(10,2)     ,;
*   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , CCURRCODE  C(3)    , Nexrate N(9,4) , Ncurrunit N(4,0) ,;
*   REP1 C(3)       , Dept C(5))
CREATE TABLE &gcWorkDir.&INVLTEMP ;
  (TYPE  C(3)      , DATE D           , CODE  C(6)         , ACCOUNT C(5)                      ,;
   NAME C(30)      , CLASS C(6)       , INVOICE C(6)       , STYLE C(19)                       ,;
   LINENO N(6)     , PRICE N(12,2)    , GROS_PRICE N(12,2) , TOTQTY N(8)    , COST N(10,2)     ,;
   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , CCURRCODE  C(3)    , Nexrate N(9,4) , Ncurrunit N(4,0) ,;
   REP1 C(3)       , Dept C(5) , cStyGroup C(6))
*C102514,1 Add the Style Group code and description. [End] 

*--End of lpCreateTmp.
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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

*--End of lfvIncRet.
*!*************************************************************
*! Name      : lfvSumDet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Showes date range screen.
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
*
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo

ldFrom = ldRPPLDate
ldTo   = ldRPPHDate
lcTitle = 'Invoice Date'

DO DateRng.Spx

ldRPPLDate = ldFrom
ldRPPHDate = ldTo

*--End of lfvDateRng.
*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Validate date range screen's OK button.
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
*
FUNCTION lfvpbDateOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF

*--End of lfvpbDateOk.
*!*************************************************************
*! Name      : lfvObjRng
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Showes range screen.
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
*CSSESSE
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

        IF lnDisDept > 0
          IF lcRPFrAcc = lcRPToAcc AND !EMPTY(lcRPFrAcc)
            laOGObjCnt[lnDisDept] = .T.
            = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnItmPos)) + ',6]')
          ELSE
            laOGObjCnt[lnDisDept] = .F.
            = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnItmPos)) + ',6]')
          ENDIF
        ENDIF

  CASE UPPER(lcObj) = "STYLE"
    STORE lcFrom TO lcRPFrSty
    STORE lcTo   TO lcRPToSty
ENDCASE

*--End of lfvObjRng.
*!*************************************************************
*! Name      : lfvObj
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Validate Account OR style.
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
*C102514,1 This function is not used anymore.
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

*--End of lfvObj.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Validate style.
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
*C102514,1 This function is not used anymore.
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

*--End of lfvStyle.
*!*************************************************************
*! Name      : lfOldVal
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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
*C102514,1 This function is not used anymore.
FUNCTION lfOldVal

lcObjNam = VARREAD()
DO CASE
  CASE UPPER(lcObjNam) = "LCFROM"
    lcOldFrm = &lcObjNam
  CASE UPPER(lcObjNam) = "LCTO"
    lcOldTo  = &lcObjNam
ENDCASE

*--End of lfOldVal.
*!*************************************************************
*! Name      : lfvpbObjOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Validate range screen's ok button.
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
*C102514,1 This function is not used anymore.
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

*--End of lfvpbObjOk.
*!*************************************************************
*! Name      : lfvpbObjCa
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Validate range screen's Cancel button.
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
*C102514,1 This function is not used anymore.

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

*--End of lfvpbObjCa.
*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Print report.
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
R_TITLE = 'GROSS PROFIT BY ' + lcHBreak
BREAK   = &lcHBreak

*-- Variables Initialization.
STORE 0 TO XTOT1  , XTOT2  , XTOT3  , XTOT4  , XTOT5 , XTOT6
STORE 0 TO XGTOT1 , XGTOT2 , XGTOT3 , XGTOT4 , XGTOT5 , XGTOT6     &&Grandtotlals
STORE 0 TO XDTOT1 , XDTOT2 , XDTOT3 , XDTOT4                       &&Totals for division.

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
     DO RPT_HDR WITH 'ARGRSPDP'+lcReport,lcOTitle,R_WIDTH
     IF lcReport = 'S' .OR. ! llInclRet
        @ 05,00 SAY 'ACC. ...... NAME ......... INVOICE ' + PADR(lcMajTtl,20) + ' PIECES    S. PRICE     GROSS AMT      NET AMT       COST      PROFIT  PROF.%'
     ELSE
        @ 05,00 SAY 'ACC.  ...... NAME ..... TYP INV/CRD ' + PADR(lcMajTtl,20) + 'PIECES   S. PRICE     GROSS AMT      NET AMT       COST      PROFIT   PROF.%'
     ENDIF
     @ 06,00 SAY REPLICATE('=',132)
     ROW = 07
   ENDIF
   
   DO lpSubTotals   &&SubTotals   

   STORE 0 TO XRTOT1,XRTOT2,XRTOT3,XRTOT4,XRTOT5    &&RetSubTot

   IF EOF() .OR. (llInclRet .AND. lcSort='I' .AND. Invoice='ZZZZZZ')
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
  STORE 0 TO XTOT1 , XTOT2 , XTOT3 , XTOT4 , XTOT5 , XTOT6
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

XGTOT5 = XGTOT3 - XGTOT4

@ ROW,112 SAY XGTOT5  PICTURE '999999999.99'
XGTOT6 = ROUND(IIF(XGTOT3<>0,(XGTOT5/XGTOT3)*100,0),2)
XGTOT6 = IIF((XGTOT5<0 .AND. XGTOT3<0),-XGTOT6,XGTOT6)
@ ROW,125 SAY XGTOT6 PICTURE '999.99%'
ROW = ROW+1
@ ROW,00 SAY REPLICATE('=',132)

WAIT CLEAR

*--End of lpPrint.
*!*************************************************************
*! Name      : lpDetail
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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
  @ ROW,088 SAY lnNet           PICTURE '999999999.99'
  @ ROW,101 SAY lnCost          PICTURE '9999999.99'
  @ ROW,112 SAY lnNet - lnCost  PICTURE '999999999.99'

  lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
  lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)
  @ ROW,125 SAY lnPercnt PICTURE '999.99%'
  XRTOT1 = XRTOT1 + lnQty

  IF lcSubGros = 'Y'
    XRTOT2 = XRTOT2 + lnGross
  ELSE
    IF TYPE = 'INV'
      XRTOT2 = XRTOT2 + lnGross
    ENDIF
  ENDIF

  XRTOT3 = XRTOT3 + lnNet
  XRTOT4 = XRTOT4 + lnCost

  XAcName = Name
  BREAK   = &lcHBreak
  SKIP

  IF llInclRet
    IF Type = 'RET' .AND. Invoice<>'ZZZZZZ'  &&Return exist for invoice.
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

  XRTOT5 = XRTOT3-XRTOT4
  @ ROW,111 SAY XRTOT5  PICTURE '999999999.99'
  XRTOT6 = ROUND(IIF(XRTOT3<>0,(XRTOT5/XRTOT3)*100,0),2)
  XRTOT6 = IIF((XRTOT5<0 .AND. XRTOT3<0),-XRTOT6,XRTOT6)

  @ ROW,125 SAY XRTOT6 PICTURE '999.99%'
ENDIF
ROW = ROW+1

*--End of lpDetail.
*!*************************************************************
*! Name      : lpSummary
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : printing report in summary format.
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

  XRTOT1 = XRTOT1 + lnQty

  IF lcSubGros = 'Y'
    XRTOT2 = XRTOT2 + lnGross
  ELSE
    IF TYPE = 'INV'
      XRTOT2 = XRTOT2 + lnGross
    ENDIF
  ENDIF

  XRTOT3 = XRTOT3 + lnNet
  XRTOT4 = XRTOT4 + lnCost

  XAcName = Name
  BREAK   = &lcHBreak
  SKIP

  IF llInclRet
    IF ! (Type='RET' .AND. Invoice<>'ZZZZZZ')
      EXIT
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO

*--End of lpSummary.
*!*************************************************************
*! Name      : lpSubTotals
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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
  IF &lcHBreak = BREAK
     EXIT
  ENDIF

  DO CASE
    CASE lcSort = 'A'
      lcSubTitl = BREAK+'   '+IIF(lcReport = 'D', SUBSTR(XAcName,1,26) , XAcName)
    CASE lcSort = 'C'
      lcSubTitl = BREAK+'   ' + gfCodDes(BREAK,'CLASS')
      SELECT (INVLTEMP)
    CASE lcSort = 'R'
      IF SEEK(BREAK , 'SalesRep')
        lcNamSrep = Salesrep.Name
      ENDIF
      lcSubTitl = BREAK +'  ' + lcNamSrep
    CASE lcSort = 'T'
      lcSubTitl = LEFT(BREAK,5) + ' ' + SUBSTR(BREAK,6,5) + IIF(!EMPTY(SUBSTR(BREAK,6,5)) , " " + lfDptDsc() , "")
    OTHERWISE
      lcSubTitl = BREAK
  ENDCASE

  IF lcReport='D'
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW+1
    @ ROW,000 SAY '* SUB TOTAL *  ' + lcSubTitl
  ELSE
    IF XGrdTotO='N'
     @ ROW,IIF(lcSort = 'S', 35 , IIF(lcSort = 'I', 27 , 0)) SAY ;
           IIF(lcSort = 'C',' CLASS : ',IIF(lcSort='R',' Sales Rep : ','')) + lcSubTitl
    ENDIF
  ENDIF

  IF XGrdTotO = 'N'
    @ ROW,054 SAY XTOT1  PICTURE '99999999'
    @ ROW,074 SAY XTOT2  PICTURE '9999999999.99'
    @ ROW,088 SAY XTOT3  PICTURE '999999999.99'
    @ ROW,100 SAY XTOT4  PICTURE '99999999.99'

    XTOT5 = XTOT3-XTOT4
    @ ROW,112 SAY XTOT5  PICTURE '999999999.99'

    XTOT6 = ROUND(IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0),2)
    XTOT6 = IIF((XTOT5<0 .AND. XTOT3<0),-XTOT6,XTOT6)
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

  BREAK = &lcHBreak
  EXIT
ENDDO

*-- End of lpSubTotals.
*!*************************************************************
*! Name      : lpRetNOInv
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : IF include returns and printing by invoice date.
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

IF lcReport = 'D' .AND. IIF(lcSort<>'I', &lcHBreak = BREAK,.T.)
  ROW = ROW + 1
  @ ROW,00 SAY 'RETURNS WITHOUT INVOICES :'
ENDIF

DO WHILE INVOICE = 'ZZZZZZ' .AND. IIF(lcSort <> 'I', &lcHBreak = BREAK,.T.)
  lnQty    = IIF(TYPE = 'INV',TOTQTY , -(TOTQTY))
  lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
  lnNet    = lnGross * (1 - (DISCPCNT/100)) * (1 - (TRDE_DISC/100))
  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost)
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

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
      IF TYPE = 'INV'
        @ ROW,074 SAY lnGross PICTURE '9999999999.99'
      ENDIF
    ENDIF
    @ ROW,088 SAY lnNet           PICTURE '9999999.99'
    @ ROW,101 SAY lnCost          PICTURE '9999999.99'
    @ ROW,111 SAY lnNet - lnCost  PICTURE '999999999.99'
    lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
    lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)
    @ ROW,125 SAY lnPercnt PICTURE '999.99%'
  ENDIF

  xRTot1 = xRTot1 + lnQty
  IF lcSubGros = 'Y'
    xRTot2 = xRTot2 + lnGross
  ELSE
    IF TYPE = 'INV'
      xRTot2 = xRTot2 + lnGross
    ENDIF
  ENDIF
  xRTot3 = xRTot3 + lnNet
  xRTot4 = xRTot4 + lnCost
  xRTot5 = xRTOT3 - XRTOT4
  xRTot6 = ROUND(IIF(XRTOT3<>0,(XRTOT5/XRTOT3)*100,0),2)
  xRTot6 = IIF((XRTOT5<0 .AND. XRTOT3<0),-XRTOT6,XRTOT6)

  BREAK = &lcHBreak
 SKIP
ENDDO

ROW = ROW + IIF(lcReport = 'D' , 1 , 0 )
IF (xRTot1+xRTot3<>0 .AND. lcSort = 'I' .AND. lcReport = 'S' )
  ROW = ROW + 1
  @ ROW,00  SAY 'RETURNS WITHOUT INVOICES :'
  @ ROW,056 SAY xRTot1 PICTURE '999999'
  @ ROW,074 SAY xRTot2 PICTURE '999999999.99'
  @ ROW,088 SAY xRTot3 PICTURE '999999999.99'
  @ ROW,101 SAY xRTot4 PICTURE '999999999.99'
  @ ROW,111 SAY xRTot5 PICTURE  '99999999.99'
  @ ROW,125 SAY xRTot6 PICTURE      '999.99%'
ENDIF

*--End of lpRetNOInv.
*!*************************************************************
*! Name      : lfwCurCode
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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

*--End of lfwCurCode.
*!*************************************************************
*! Name      : lfvCurCode
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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
PRIVATE lnAlias
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

*--End of lfvCurCode.
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*-- If forgin currency you must be sure invoice currency has value.
IF lcRpCurr = 'F' .AND. EMPTY(laOGFxFlt[2,6])
  laOGFxFlt[2,6] = gcBaseCurr
ENDIF
SHOW GET laOGFxFlt[2,6]

*--End of lfvCurDisp.
*!*************************************************************
*! Name      : lfRepShow
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Enable or disable the currency field.
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

*--End of lfRepShow.
*!********************************************************************
*! Name      : lfGetFCurr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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

*--End of lfGetFCurr.
*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Validate date range screen's OK button.
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
*
FUNCTION lfvPbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  g = ldFrom
  ldRPPHDate = ldTo
  CLEAR READ
ENDIF

*--End of lfvPbOk.
*!*************************************************************
*! Name      : lfvPostDat
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Add the posting date filter to the filter if selected.
*!*************************************************************
*! Called from : ARGRSPDP.PRG
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

*-- End of lfvPostDat
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Option Grid When function.
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

*C102514,1 Get the position of both Account and Style. [Begin]
lnAcctPos  = lfItmPos('INVLINE.ACCOUNT') && get Account Fixed filter Position.
lnStylePos = lfItmPos('INVLINE.STYLE')  && get Style Fixed filter Position.
lnStyGrpPo = lfItmPos('STYLE.CSTYGROUP')  && get Style Group Fixed filter Position.
lnStyDepPo = lfItmPos('ICDEPTHD.DEPT')  && get Dept Fixed filter Position.

IF lcRpReport = "D"
  SHOW GET pbOGPrevie DISABLE
ELSE
  SHOW GET pbOGPrevie ENABLE
ENDIF
*C102514,1 Get the position of both Account and Style. [End]

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

lnItmPos = ASCAN(laOGFxFlt,'CUSTDEPT.DEPT')
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF

IF lnItmPos > 0
  lnDisDept   = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
               ALLTRIM(STR(lnItmPos)) + ",6]"),1)
               
  *C102514,1 Check if user changed report type. [Begin]
  *laOGObjCnt[lnDisDept] = .F.
  *= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnItmPos)) + ',6]')
  laOGObjCnt[lnDisDept] = .F.
  IF USED(laOGFxFlt[lnAcctPos,6]) AND RECCOUNT(laOGFxFlt[lnAcctPos,6]) = 1
    laOGObjCnt[lnDisDept] = .T.
  ENDIF  
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnItmPos)) + ',6]')
  *C102514,1 Check if user changed report type. [End]
  
ENDIF

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfvAccount
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
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
*C102514,1 This function is not used anymore.
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
*!*************************************************************
*! Name      : lfvDept
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Add the departement selection to the filter if selected.
*!*************************************************************
*! Called from : ARGRSPDP.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDept()
*!*************************************************************
FUNCTION lfvDept
PRIVATE lcAlias

lcAlias = SELECT(0)
lnDataPos = ASCAN(laOgFXFlt,'CUSTDEPT.DEPT')
IF lnDataPos > 0
  lnDataPos  = ASUBSCRIPT(laOgFXFlt,lnDataPos,1)
  SELECT LAOGFXFLT[lnDataPos,6]
  lcDeptVal = SPACE(0)
  SCAN
    IF !(&LAOGFXFLT[lnDataPos,6]..DEPT $ lcDeptVal)
      lcDeptVal = lcDeptVal + IIF(EMPTY(lcDeptVal) , "" , ",") + &LAOGFXFLT[lnDataPos,6]..DEPT
    ENDIF
  ENDSCAN
ENDIF

XFILTER = XFILTER + IIF(EMPTY(lcDeptVal) , "" , " .AND. " + "INVHDR.DEPT $ lcDeptVal")

SELECT(lcAlias)
*--End of lfvDept.
*!*************************************************************
*! Name      : lfvDept
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Add the departement selection to the filter if selected.
*!*************************************************************
*! Called from : ARGRSPDP.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDept()
*!*************************************************************
FUNCTION lfDptDsc
PRIVATE lcAlias

lcAlias = SELECT(0)
SELECT(lcAlias)
lcRturnDec = SPACE(0)
IF SEEK(LEFT(BREAK,5) + SUBSTR(BREAK,6,5) , 'CUSTDEPT')
 lcRturnDec = LEFT(CUSTDEPT.CDEPTDESC,25)
ENDIF

SELECT(lcAlias)
RETURN lcRturnDec

*--End of lfDptDsc.
*!*************************************************************
*! Name      : lfDeptRet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Add the department value for the return in the temp. file.
*!*************************************************************
*! Called from : ARGRSPDP.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfDeptRet()
*!*************************************************************
FUNCTION lfDeptRet
PRIVATE lcAlias

lcAlias = SELECT(0)
lcRturnRet = SPACE(0)
llRturnRet = .F.

IF SEEK(RETLINE.INVOICE , 'INVHDR')
  IF INVHDR.DEPT $ lcDeptVal
    IF SEEK(INVHDR.ACCOUNT + INVHDR.DEPT , 'CUSTDEPT')
     lcRturnRet = CUSTDEPT.DEPT
     llRturnRet = .T.
    ENDIF
  ENDIF
ENDIF

SELECT(lcAlias)
RETURN llRturnRet

*--End of lfDeptRet.
*!*************************************************************
*! Name      : lfDeptAll
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Add the department value in case user select all.
*!*************************************************************
*! Called from : ARGRSPDP.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfDeptAll()
*!*************************************************************
FUNCTION lfDeptAll
PRIVATE lcAlias

lcAlias = SELECT(0)
lcRturnRet = SPACE(0)

IF SEEK(RETLINE.INVOICE , 'INVHDR')
  IF SEEK(INVHDR.ACCOUNT + INVHDR.DEPT , 'CUSTDEPT')
    lcRturnRet = CUSTDEPT.DEPT
  ELSE
    IF !EMPTY(INVHDR.DEPT)
      lcRturnRet = INVHDR.DEPT
    ENDIF
  ENDIF
ENDIF

SELECT(lcAlias)
RETURN lcRturnRet

*--End of lfDeptAll.
*!*************************************************************
*! Name      : lfDepRet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/11/2001
*! Purpose   : Check if the department value match the criteria or not.
*!*************************************************************
*! Called from : ARGRSPDP.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfDepRet()
*!*************************************************************
FUNCTION lfDepRet
PRIVATE lcAlias

lcAlias = SELECT(0)
llRetLogc = .F.
IF SEEK(lcInvoice , 'INVHDR')
  IF SEEK(INVHDR.ACCOUNT + INVHDR.DEPT , 'CUSTDEPT') AND INVHDR.DEPT $ lcDeptVal
   llRetLogc = .T.
  ENDIF
ENDIF

SELECT(lcAlias)
RETURN llRetLogc

*--End of lfDepRet.

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 31/12/2001
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrvSty()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
*C102514,1
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT Style
    SET ORDER TO TAG Cstyle
    SET RELATION TO Style.Style INTO STYLE_X
    GO TOP IN Style
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT Style
    SET ORDER TO TAG Style
ENDCASE
*-- End of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 31/12/2001
*! Purpose   : Sum a specific field for the current style in style file
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Returns   : Calculated field value.
*!**************************************************************************
*! Example   : =lfStySum()
*!**************************************************************************
*C102514,1
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfStySum.

*!**************************************************************************
*! Name      : lfsrAcc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 31/12/2001
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
*C102514,1
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    IF lnDisDept > 0
      IF USED(laOGFxFlt[lnAcctPos,6]) AND RECCOUNT(laOGFxFlt[lnAcctPos,6]) = 1
        laOGObjCnt[lnDisDept] = .T.
        = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnItmPos)) + ',6]')
        
        IF lcRPFrAcc <> EVALUATE(laOGFxFlt[lnAcctPos,6]+'.Account')
          lcRPFrAcc = EVALUATE(laOGFxFlt[lnAcctPos,6]+'.Account')
          IF USED(laOGFxFlt[lnItmPos,6])
            PRIVATE lcAlias
            lcAlias = ALIAS()
            SELECT (laOGFxFlt[lnItmPos,6])
            ZAP
            SELECT (lcAlias)
          ENDIF    
        ENDIF
      ELSE
        laOGObjCnt[lnDisDept] = .F.
        = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnItmPos)) + ',6]')

        IF USED(laOGFxFlt[lnAcctPos,6]) AND RECCOUNT(laOGFxFlt[lnAcctPos,6]) > 1
          IF USED(laOGFxFlt[lnItmPos,6])
            PRIVATE lcAlias
            lcAlias = ALIAS()
            SELECT (laOGFxFlt[lnItmPos,6])
            ZAP
            SELECT (lcAlias)
          ENDIF    
        ENDIF
        
      ENDIF
    ENDIF  
ENDCASE
*-- End of lfsrAcc.

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/03/2002
*! Purpose   : Generate Expression.
*!***************************************************************************
*! Example   : DO lpGenExpr
*!***************************************************************************
*C102514,1 Remove the Account and Style In list.
PROCEDURE lpGenExpr
PRIVATE lcAlias , lnX
lcAlias = ALIAS()

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1] , laBrTmpFlt[1]
=ACOPY(laOGFxFlt,laTempExpr)         && Copy Fixed Filter Array to Temp Array.
=ACOPY(laBrFldFlt,laBrTmpFlt)        && Copy Browse Filter Array to Temp Array.

*-- If user selected Style Group.
IF ALLTRIM(laTempExpr[lnStyGrpPo,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  laOGFxFlt = ""          

  *-- Copy only Account expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnStyGrpPo,1),7)
  
  *-- Generate expression for Account.
  lcStyGrp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- we need to convert Account in filter Expression to true
  lcRpExp = STRTRAN(lcRpExp,lcStyGrp,".T.")
ENDIF  
*-- EndIf of user selected Style Group.

*-- If user selected Style Dept.
IF ALLTRIM(laTempExpr[lnStyDepPo,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7] , laBrFldFlt[1,5]
  laOGFxFlt = ""          

  *-- Convert numeric position to string
  PRIVATE lcBrowPos
  lcBrowPos = ALLTRIM(STR(lnStyDepPo))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = "laOGFxFlt:1"

  *-- Copy only Account expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnStyDepPo,1),7)
  
  *-- Generate expression for Account.
  lcDeptExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- we need to convert Account in filter Expression to true
  lcRpExp = STRTRAN(lcRpExp,lcDeptExp,".T.")
ENDIF  
*-- EndIf of user selected Style Dept.

*-- If user selected Account.
IF ALLTRIM(laTempExpr[lnAcctPos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7] , laBrFldFlt[1,5]
  laOGFxFlt = ""          

  *-- Convert numeric position to string
  PRIVATE lcBrowPos
  lcBrowPos = ALLTRIM(STR(lnAcctPos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = "laOGFxFlt:1"

  *-- Copy only Account expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnAcctPos,1),7)
  
  *-- Generate expression for Account.
  lcAcctExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- we need to convert Account in filter Expression to true
  lcRpExp = STRTRAN(lcRpExp,lcAcctExp,".T.")
ENDIF  
*-- EndIf of user selected Account.

*-- If user selected Style.
IF ALLTRIM(laTempExpr[lnStylePos,1]) $ lcRpExp 

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7] , laBrFldFlt[1,5]
  laOGFxFlt = ""          

  *-- Convert numeric position to string
  PRIVATE lcBrowPos
  lcBrowPos = ALLTRIM(STR(lnAcctPos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = "laOGFxFlt:1"

  *-- Copy only Month Range expression to laOGFxFlt.
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnStylePos,1),7)
  
  *-- Generate expression for Month Range.
  lcStyleExp = gfGenFlt('laOGFxFlt',.T.,.T.)

  *-- Remove Style Group from lcRpExp.
  lcRpExp = STRTRAN(lcRpExp,lcStyleExp,".T.")
  
  *-- Only style major are allowed
  lcStyleExp = STRTRAN(lcStyleExp,SPACE(19-lnMajorLen),"")
  lcStyleExp = STRTRAN(lcStyleExp,"SEEK(INVLINE.STYLE","SEEK(SUBSTR(INVLINE.STYLE,1,lnMajorLen)")
ENDIF  
*-- If user selected Style.

*-- Restore original laOGFxFlt.
DIMENSION laOGFxFlt[1] , laBrFldFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)         && Restore Fixed Filter Array to Temp Array.
=ACOPY(laBrTmpFlt,laBrFldFlt)        && Restore Browse Filter Array to Temp Array.

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
*C102514,1
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lfvReport
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/03/2002
*! Purpose   : Validation for Report Layout option.
*!**************************************************************************
*! Example   : = lfvReport()
*!**************************************************************************
*C102514,1
FUNCTION lfvReport
CLEAR READ
*-- End of lfvReport.

*!**************************************************************************
*! Name      : lpDeptPrnt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/03/2002
*! Purpose   : printing report for Report Layout "Summary by Dept."
*!**************************************************************************
*! Example   : DO lpDeptPrnt
*!**************************************************************************
*C102514,1
PROCEDURE lpDeptPrnt
PRIVATE llCallGfam , lcStyGroup

llCallGfam = EMPTY(laOGFxFlt[2,6]) OR (!EMPTY(laOGFxFlt[2,6]) AND lcRpCurr <> 'F')

SELECT (INVLTEMP)
GO TOP

ROW     = 99
PAGENO  = 0
R_TITLE = 'GROSS PROFIT BY ACCOUNT+DEPT'
BREAK   = &lcHBreak
SumBrk  = &lcSumBrk
STORE ' ' TO XAcName , lcStyGroup

*-- Variables Initialization.
STORE 0 TO XTOT1  , XTOT2  , XTOT3  , XTOT4  , XTOT5
STORE 0 TO XGTOT1 , XGTOT2 , XGTOT3 , XGTOT4 , XGTOT5 , XGTOT6     &&Grandtotlals
STORE 0 TO XDTOT1 , XDTOT2 , XDTOT3 , XDTOT4                       &&Totals for division.
STORE 0 TO XGTOTSUM1 , XGTOTSUM2 , XGTOTSUM3 , XGTOTSUM4           &&Grandtotlals for the summery report.

XTITLE = SPACE(1)
XTIME  = TIME()
PAGENO = 0
ROW    = 99

CLEAR TYPEAHEAD

DO WHILE .T.
  WAIT WINDOW 'Printing Transaction # : '+ &INVLTEMP..Invoice NOWAIT

  IF ROW >= 53
    PAGENO = PAGENO+1
    DO RPT_HDR WITH 'ARGRSPDP'+lcReport,lcOTitle,R_WIDTH
    @ 05,00 SAY 'ACC.. DEPT. ..... NAME ..... ...... STYLE GROUP ......  ' + 'PIECES    S. PRICE    GROSS AMT      NET AMT       COST       PROFIT  PROF.%'     
    @ 06,00 SAY REPLICATE('=',132)
    ROW = 07
  ENDIF
   
  DO lpDeptTots

  STORE 0 TO XRTOT1,XRTOT2,XRTOT3,XRTOT4,XRTOT5    &&RetSubTot

  IF EOF()
    EXIT
  ENDIF

  IF ROW >=53
    ROW = 99
    LOOP
  ENDIF

  DO lpDeptSumy

  xTot1 = xTot1 + xRTot1
  xTot2 = xTot2 + xRTot2
  xTot3 = xTot3 + xRTot3
  xTot4 = xTot4 + xRTot4

ENDDO

IF xTot1 > 0
  XGTOT1 = XGTOT1 + XTOT1
  XGTOT2 = XGTOT2 + XTOT2
  XGTOT3 = XGTOT3 + XTOT3
  XGTOT4 = XGTOT4 + XTOT4
  STORE 0 TO XTOT1 , XTOT2 , XTOT3 , XTOT4 , XTOT5 , XTOT6
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

XGTOT5 = XGTOT3 - XGTOT4

@ ROW,112 SAY XGTOT5  PICTURE '999999999.99'
XGTOT6 = ROUND(IIF(XGTOT3<>0,(XGTOT5/XGTOT3)*100,0),2)
XGTOT6 = IIF((XGTOT5<0 .AND. XGTOT3<0),-XGTOT6,XGTOT6)
@ ROW,125 SAY XGTOT6 PICTURE '999.99%'
ROW = ROW+1
@ ROW,00 SAY REPLICATE('=',132)

WAIT CLEAR
*--End of lpDeptPrnt.

*!**************************************************************************
*! Name      : lpDeptTots
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/03/2002
*! Purpose   : printing Sub totals for Report Layout "Summary by Dept."
*!**************************************************************************
*! Example   : DO lpDeptTots
*!**************************************************************************
*C102514,1
PROCEDURE lpDeptTots

*-- Begin Subtotals Loop --
DO WHILE LEN(TRIM(lcHBreak)) <> 0
  IF &lcHBreak = BREAK
    IF LEFT(SumBrk,5) = LEFT(EVAL(lcSumBrk),5) AND SUBSTR(SumBrk,6,5) = SUBSTR(EVAL(lcSumBrk),6,5)
      EXIT
    ENDIF
  ENDIF

  lcSubTitl = SUBSTR(BREAK,1,5)+ ' ' + SUBSTR(BREAK,6,5) + ' ' + ;
              IIF(SEEK(SUBSTR(Break,1,10),'CustDept'),SUBSTR(CustDept.cDeptDesc,1,16),SPACE(16)) + ;
              ' ' + lcStyGroup + ' ' + SUBSTR(gfCodDes(SUBSTR(BREAK,11),'CSTYGROUP'),1,16)

  IF XGrdTotO='N'
    @ ROW,0 SAY lcSubTitl
  ENDIF

  *--SUB TOTAL
  IF XGrdTotO='N'
    @ ROW,054 SAY XTOT1  PICTURE '99999999'
    @ ROW,074 SAY XTOT2  PICTURE '9999999999.99'
    @ ROW,088 SAY XTOT3  PICTURE '999999999.99'
    @ ROW,101 SAY XTOT4  PICTURE '9999999.99'
    XTOT5 = XTOT3-XTOT4
    @ ROW,112 SAY XTOT5  PICTURE '999999999.99'
    XTOT6 = ROUND(IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0),2)
    XTOT6 = IIF((XTOT5<0 .AND. XTOT3<0),-XTOT6,XTOT6)
    @ ROW,125 SAY XTOT6  PICTURE '999.99%'
  ENDIF

  IF XGrdTotO='N'
    ROW = ROW + 1
  ENDIF

  XGTOT1 = XGTOT1 + XTOT1
  XGTOT2 = XGTOT2 + XTOT2
  XGTOT3 = XGTOT3 + XTOT3
  XGTOT4 = XGTOT4 + XTOT4
  XGTOT5 = XGTOT5 + (XTOT3-XTOT4)
  XGTOT6 = XGTOT6 + IIF((XTOT5<0 .AND. XTOT3<0),-ROUND(IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0),2),ROUND(IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0),2))

  XGTOTSUM1 = XGTOTSUM1 + XTOT1
  XGTOTSUM2 = XGTOTSUM2 + XTOT2
  XGTOTSUM3 = XGTOTSUM3 + XTOT3
  XGTOTSUM4 = XGTOTSUM4 + XTOT4

  XDTOT1 = XDTOT1 + XTOT1
  XDTOT2 = XDTOT2 + XTOT2
  XDTOT3 = XDTOT3 + XTOT3
  XDTOT4 = XDTOT4 + XTOT4

  IF (LEFT(SumBrk,5) # LEFT(EVAL(lcSumBrk),5) AND SUBSTR(SumBrk,6,5) # SUBSTR(EVAL(lcSumBrk),6,5)) OR;
    (LEFT(SumBrk,5) = LEFT(EVAL(lcSumBrk),5) AND SUBSTR(SumBrk,6,5) # SUBSTR(EVAL(lcSumBrk),6,5)) OR ;
    (LEFT(SumBrk,5) # LEFT(EVAL(lcSumBrk),5) AND SUBSTR(SumBrk,6,5) = SUBSTR(EVAL(lcSumBrk),6,5))
    
    *ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    @ ROW,000 SAY SPACE(9) + 'SubTotal ' + SUBSTR(BREAK,1,5)+ ' ' + SUBSTR(BREAK,6,5) + ' ' + IIF(SEEK(SUBSTR(Break,1,10),'CustDept'),SUBSTR(CustDept.cDeptDesc,1,16),SPACE(16))
    @ ROW,054 SAY XDTOT1 PICTURE '99999999'
    @ ROW,074 SAY XDTOT2 PICTURE '9999999999.99'
    @ ROW,088 SAY XDTOT3 PICTURE '999999999.99'
    @ ROW,101 SAY XDTOT4 PICTURE '9999999.99'
    @ ROW,112 SAY XDTOT3 - XDTOT4 PICTURE '999999999.99'
    @ ROW,125 SAY IIF(((XDTOT3 - XDTOT4)<0 .AND. XDTOT3<0),-ROUND(IIF(XDTOT3<>0,((XDTOT3 - XDTOT4)/XDTOT3)*100,0),2),ROUND(IIF(XDTOT3<>0,((XDTOT3 - XDTOT4)/XDTOT3)*100,0),2)) PICTURE '999.99%'

    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    
    STORE 0 TO XDTOT1 , XDTOT2 , XDTOT3 , XDTOT4
  ENDIF

  *IF LEFT(SumBrk,5) # LEFT(EVAL(lcSumBrk),5) AND SUBSTR(SumBrk,6,5) = SUBSTR(EVAL(lcSumBrk),6,5)
  IF LEFT(SumBrk,5) # LEFT(EVAL(lcSumBrk),5)
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1

    @ ROW,000 SAY SPACE(6) + 'TOTAL ' + SUBSTR(BREAK,1,5)+ ' ' + XAcName
    @ ROW,054 SAY XGTOTSUM1 PICTURE '99999999'
    @ ROW,074 SAY XGTOTSUM2 PICTURE '9999999999.99'
    @ ROW,088 SAY XGTOTSUM3 PICTURE '999999999.99'
    @ ROW,101 SAY XGTOTSUM4 PICTURE '9999999.99'
    @ ROW,112 SAY XGTOTSUM3 - XGTOTSUM4 PICTURE '999999999.99'
    @ ROW,125 SAY IIF(((XGTOTSUM3 - XGTOTSUM4)<0 .AND. XGTOTSUM3<0),-ROUND(IIF(XGTOTSUM3<>0,((XGTOTSUM3 - XGTOTSUM4)/XGTOTSUM3)*100,0),2),ROUND(IIF(XGTOTSUM3<>0,((XGTOTSUM3 - XGTOTSUM4)/XGTOTSUM3)*100,0),2)) PICTURE '999.99%'

    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 2
      
    STORE 0 TO XGTOTSUM1 , XGTOTSUM2 , XGTOTSUM3 , XGTOTSUM4
  ENDIF  

  STORE 0 TO XTOT1 , XTOT2 , XTOT3 , XTOT4 , XTOT5 , XTOT6
  SumBrk = &lcSumBrk
  BREAK = &lcHBreak

  EXIT
ENDDO
*-- End of lpDeptTots.

*!**************************************************************************
*! Name      : lpDeptSumy
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/03/2002
*! Purpose   : printing report in summary for Layout "Summary by Dept."
*!**************************************************************************
*! Example   : DO lpDeptSumy
*!**************************************************************************
*C102514,1
PROCEDURE lpDeptSumy

llRetExst=.F.      &&Flag for Existing of the returns.
DO WHILE .T.
  *--Calculate report fields.
  lnQty    = IIF(Type='INV',TotQty,-(TotQty))
  lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
  lnNet    = lnGross * (1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))
  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost) 
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

  XRTOT1 = XRTOT1 + lnQty

  IF lcSubGros = 'Y'
    XRTOT2 = XRTOT2 + lnGross
  ELSE
    IF TYPE = 'INV'
      XRTOT2 = XRTOT2 + lnGross
    ENDIF
  ENDIF

  XRTOT3 = XRTOT3 + lnNet
  XRTOT4 = XRTOT4 + lnCost

  XAcName = Name
  lcStyGroup = cStyGroup
  BREAK   = &lcHBreak
  SumBrk  = &lcsumBrk
  SKIP

  IF llInclRet
    IF ! (Type='RET' .AND. Invoice<>'ZZZZZZ')
      EXIT
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO
*-- End of lpDeptSumy.

*!***************************************************************************
*! Name      : lfsrDept
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/03/2002
*! Purpose   : Set , Reset for Style Dept Inlist
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfsrDept()
*!***************************************************************************
*! Note      : S symbol is [S,Set]
*!***************************************************************************
*C102514,1
FUNCTION lfsrDept
PARAMETERS lcParm

SELECT ICDeptHd   
DO CASE
  CASE lcParm = 'S'
    lcTmpIndex =gfTempName()
    SELECT DISTINCT DEPT,cDeptDesc FROM ICDeptHd INTO DBF (gcWorkDir + lcTmpIndex)
    USE IN ICDeptHd
    USE IN (lcTmpIndex)
    USE (gcWorkDir + lcTmpIndex) IN 0 ALIAS ICDeptHd
    SELECT ICDeptHd
    INDEX ON Dept TAG Depthd
    GO TOP
  CASE lcParm = 'R'
    USE IN ICDeptHd
    =gfOpenFile(gcDataDir+'ICDeptHd',gcDataDir+'DeptHd','SH')
ENDCASE
*-- End of lfsDpt.

*!**************************************************************************
*! Name      : lfsrCusDep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/03/2002
*! Purpose   : Set function for Customer department
*!**************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfsrCusDep()
*!***************************************************************************
*! Note      : S symbol is [S,Set]
*!***************************************************************************
*C102514,1
FUNCTION lfsrCusDep
PARAMETERS lcParm

SELECT ICDeptHd   
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CustDept
    IF USED(laOGFxFlt[lnAcctPos,6]) AND RECCOUNT(laOGFxFlt[lnAcctPos,6]) = 1
      GO TOP IN (laOGFxFlt[lnAcctPos,6])      
    ENDIF
ENDCASE
*-- End of lfsrCusDep.

*!**************************************************************************
*! Name      : lpDownLPrt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/03/2002
*! Purpose   : printing report for Report Layout Download
*!**************************************************************************
*! Example   : DO lpDownLPrt
*!**************************************************************************
*C102514,1
PROCEDURE lpDownLPrt
PRIVATE llCallGfam , lcAcName , lcStyle , lcStyGroup , lnCost1 , lnCost2 , ;
        lnCost3 , lnCost4 , lnCost5

llCallGfam = EMPTY(laOGFxFlt[2,6]) OR (!EMPTY(laOGFxFlt[2,6]) AND lcRpCurr <> 'F')

SELECT (INVLTEMP)
GO TOP

BREAK = &lcHBreak
STORE ' ' TO lcAcName , lcStyle , lcStyGroup
        
*-- Variables Initialization.
STORE 0 TO XTOT1 , XTOT2 , XTOT3 , XTOT4            &&Subtotals
STORE 0 TO XGTOT1 , XGTOT2 , XGTOT3 , XGTOT4 , XGTOT5 , XGTOT6     &&Grandtotals
STORE 0 TO lnCost1 , lnCost2 , lnCost3 , lnCost4 , lnCost5

CLEAR TYPEAHEAD

=FPUTS(lnHandle,SPACE(95) + 'GMA')
=FPUTS(lnHandle,SPACE(47) + 'CUST DEPT' + SPACE(32) + 'GMA   GROUP' + SPACE(56) + 'STYLE' + SPACE(110) + 'STYLE COST SHEET INFORMATION')
*@ 03,00 SAY 'ACC.  CUSTOMER                       CUST DEPT    DESC                       GMA DEPT GROUP   DESC                         SALES REP STYLE#               DESC               PIECES   GROSS AMT    NET AMT     COST         PROFIT%  P.PRICE     FRT        DUTY          MISC      MATERIAL TOTAL'
=FPUTS(lnHandle,'ACC.  CUSTOMER                       CUST DEPT   DESC                         ' + ;
            'GMA DEPT GROUP   DESC                          SALES REP STYLE#              ' + ;
            'DESC                    PIECES     GROSS AMT      NET AMT       COST       PROFIT  PROFIT% ' + ;
            '      P.PRICE           FRT          DUTY          MISC      MATERIAL         TOTAL')
=FPUTS(lnHandle,' ')

DO WHILE .T.
   WAIT WINDOW 'Printing Transaction # : ' + &INVLTEMP..Invoice NOWAIT
   
   DO lpDownLTot

   STORE 0 TO XRTOT1,XRTOT2,XRTOT3,XRTOT4,XRTOT5    &&RetSubTot

   IF EOF()
     EXIT
   ENDIF

   DO lpDownLSum

   xTot1 = xTot1 + xRTot1
   xTot2 = xTot2 + xRTot2
   xTot3 = xTot3 + xRTot3
   xTot4 = xTot4 + xRTot4

ENDDO

IF xTot1 > 0
  XGTOT1 = XGTOT1 + XTOT1
  XGTOT2 = XGTOT2 + XTOT2
  XGTOT3 = XGTOT3 + XTOT3
  XGTOT4 = XGTOT4 + XTOT4
  STORE 0 TO XTOT1 , XTOT2 , XTOT3 , XTOT4
ENDIF

=FPUTS(lnHandle,' ')
=FPUTS(lnHandle,REPLICATE('=',331))
XGTOT5 = XGTOT3 - XGTOT4
XGTOT6 = ROUND(IIF(XGTOT3<>0,(XGTOT5/XGTOT3)*100,0),2)
XGTOT6 = IIF((XGTOT5<0 .AND. XGTOT3<0),-XGTOT6,XGTOT6)

=FPUT(lnHandle,'* GRAND TOTAL *' + SPACE(162) + ;
  TRANSFORM(XGTOT1,'99999999') + ' ' + TRANSFORM(XGTOT2,'9999999999.99') + ' ' + ;
  TRANSFORM(XGTOT3,'999999999.99') + ' ' + TRANSFORM(XGTOT4,'9999999.99') + ' ' + ;
  TRANSFORM(XGTOT5,'999999999.99') + ' ' + TRANSFORM(XGTOT6,'9999.99%'))
  
=FPUTS(lnHandle,REPLICATE('=',331))
WAIT CLEAR
*--End of lpDownLPrt.

*!**************************************************************************
*! Name      : lpDownLTot
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/03/2002
*! Purpose   : printing Sub totals for Report Layout Download
*!**************************************************************************
*! Example   : DO lpDownLTot
*!**************************************************************************
*C102514,1
PROCEDURE lpDownLTot

*-- Begin Subtotals Loop --
DO WHILE LEN(TRIM(lcHBreak)) <> 0
  IF &lcHBreak = BREAK
    EXIT
  ENDIF

  lcSubTitl = SUBSTR(BREAK,1,5) + ' ' + lcAcName + ' ' + SUBSTR(BREAK,6,5) + SPACE(5) + ;
              IIF(SEEK(SUBSTR(Break,1,10),'CustDept'),CustDept.cDeptDesc,SPACE(30)) + ' ' + ;
              IIF(SEEK(lcStyle,'IcDeptDt'),IcDeptDt.Dept,SPACE(5)) + SPACE(4) + ;
              lcStyGroup + ' ' + gfCodDes(lcStyGroup,'CSTYGROUP') + ' ' + ;
              IIF(SEEK(SUBSTR(Break,1,10),'CustDept'),CustDept.Rep1,SPACE(3)) + SPACE(7) + ;
              + lcStyle + ' ' + IIF(SEEK(lcStyle,'Style'),Style.Desc,SPACE(20)) + '  ' + ;
              TRANSFORM(XTOT1,'99999999') + ' ' + TRANSFORM(XTOT2,'9999999999.99') + ' ' + ;
              TRANSFORM(XTOT3,'999999999.99') + ' ' + TRANSFORM(XTOT4,'9999999.99') + ' ' + ;
              TRANSFORM(XTOT3-XTOT4,'999999999.99') + ' ' + TRANSFORM(IIF(((XTOT3-XTOT4)<0 AND XTOT3<0),-ROUND(IIF(XTOT3<>0,((XTOT3-XTOT4)/XTOT3)*100,0),2),ROUND(IIF(XTOT3<>0,((XTOT3-XTOT4)/XTOT3)*100,0),2)),'9999.99%') + ' ' + ;
              TRANSFORM(lnCost1,'999999999.999') + ' ' + TRANSFORM(lnCost2,'999999999.999') + ' ' + ;
              TRANSFORM(lnCost3,'999999999.999') + ' ' + TRANSFORM(lnCost4,'999999999.999') + ' ' + ;
              TRANSFORM(lnCost5,'999999999.999') + ' ' + TRANSFORM(lnCost1+lnCost2+lnCost3+lnCost4+lnCost5,'999999999.999')

  IF XGrdTotO='N' AND XTOT1 > 0
    =FPUT(lnHandle,lcSubTitl)
  ENDIF

  XGTOT1 = XGTOT1 + XTOT1
  XGTOT2 = XGTOT2 + XTOT2
  XGTOT3 = XGTOT3 + XTOT3
  XGTOT4 = XGTOT4 + XTOT4
  
  STORE 0 TO XTOT1 , XTOT2 , XTOT3 , XTOT4
  STORE 0 TO lnCost1 , lnCost2 , lnCost3 , lnCost4 , lnCost5
  BREAK = &lcHBreak
  EXIT
ENDDO
*-- End of lpDownLTot.

*!**************************************************************************
*! Name      : lpDownLSum
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/03/2002
*! Purpose   : printing report in summary for Layout download
*!**************************************************************************
*! Example   : DO lpDownLSum
*!**************************************************************************
*C102514,1
PROCEDURE lpDownLSum

llRetExst=.F.      &&Flag for Existing of the returns.
DO WHILE .T.
  *--Calculate report fields.
  lnQty    = IIF(Type='INV',TotQty,-(TotQty))
  lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
  lnNet    = lnGross * (1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))
  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost) 
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

  XRTOT1 = XRTOT1 + lnQty

  IF lcSubGros = 'Y'
    XRTOT2 = XRTOT2 + lnGross
  ELSE
    IF TYPE = 'INV'
      XRTOT2 = XRTOT2 + lnGross
    ENDIF
  ENDIF

  XRTOT3 = XRTOT3 + lnNet
  XRTOT4 = XRTOT4 + lnCost

  lcAcName = Name
  lcStyle = Style
  lcStyGroup = cStyGroup
  
  =SEEK(lcStyle,'Style')
  IF Style.Make
    lnCost1 = Style.nMCost1
    lnCost2 = Style.nMCost2
    lnCost3 = Style.nMCost3
    lnCost4 = Style.nMCost4
    lnCost5 = Style.nMCost5
  ELSE
    lnCost1 = Style.nICost1
    lnCost2 = Style.nICost2
    lnCost3 = Style.nICost3
    lnCost4 = Style.nICost4
    lnCost5 = Style.nICost5
  ENDIF

  BREAK   = &lcHBreak
  SKIP

  IF llInclRet
    IF ! (Type='RET' .AND. Invoice<>'ZZZZZZ')
      EXIT
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO
*-- End of lpDownLSum.

*!**************************************************************************
*! Name      : lfTextFile
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/03/2002
*! Purpose   : Valid for selecting a text file to save the download layout.
*!**************************************************************************
*! Example   : =lfTextFile()
*!**************************************************************************
*C102514,1
FUNCTION lfTextFile
lcRpOutPut = PUTFILE('Select Text File','','TXT')
*-- End of lfTextFile.