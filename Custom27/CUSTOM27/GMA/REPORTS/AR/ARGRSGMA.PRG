*:***************************************************************************
*: Program file  : ARGRSGMA
*: Program desc. : Gross profit FOR GMA 
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : Account receivable (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARGRSGMA
*:***************************************************************************
*B803410,1 BWA 07/13/2000 Fix the bug of not printing item description.
*B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*:***************************************************************************

*-- ldRPPLDate Variable that hold From date value       [Invoice Date]
*-- ldRPPHDate Variable that hold to   date value       [Invoice Date]
*-- lcRPFrSty  Variable that hold From style value      [From Style]
*-- lcRPToSty  Variable that hold To   style value      [To Style]
*-- lcRPFrAcc  Variable that hold From Account value    [From Account]
*-- lcRPToAcc  Variable that hold To   Account value    [To Account]
*-- lcRPSeason Variable that hold Season                [Season ] 
*-- lcRPDiv    Variable that hold Division              [Division]
*-- lcValueDep Variable that hold Departement selection.[Depa.]
*-- lnComma    Variable that hold numeric postion of the ','

lnMajorLen = LEN(gfItemMask("PM"))
XLSTYLE    = PADR(lcRPFrSty,lnMajorLen)
XHSTYLE    = PADR(lcRPToSty,lnMajorLen)
lcValueDep = SPACE(0)
lnComma    = 0

*-- Getting class. postion in Fixed filtter array to get all classes that
*-- user selected in option grid
*-- XTCS variable that hold selected classes
lnClassPo = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CLASS'),1)
XTCS = laOGFxFlt[lnClassPo,6]

*-- lcRPSalRep  variable that hold sales rep.
*-- lcRPInvBy   variable that showes how to print invoice (by Ship date OR Inv. date)
*-- lcReport  variable that showes if print report in summary or detail
*-- lcIncRet  variable that showse if returns is included or not
*-- lcSort    variable that showes the report sort 
*-- XGrdTotO  variable that showes printing grand total or not
*-- lcOTitle  variable that hold the optional title

lcReport  = lcRPSumDet
lcSort    = lcRPSortBy
XGrdTotO  = IIF(llRPGrnTot,'Y','N')
llInclRet = llRPIncRet
lcOTitle  = lcRPTitle
lcNamSrep = SPACE(0)

*--initilize filters on file.
XFILTER = "INVHDR->Status<>'V'"
IF llInclRet 
  XRFILTER= ".T." 
ENDIF

*--Set and build the filters due to grid var.
DO lpSetFilter
lcSFilt=' '

*--Creat a temp file. AND make the relations.
INVLTEMP = gfTEMPNAME()
DO lpCreateTmp
DO lpFilesRela

WAIT WINDOW 'Building the invoices records...' NOWAIT
*-- Build the invoice records.
SELE INVLINE
IF ! EMPTY(XLSTYLE)
  lcSetNear=SET('NEAR')
  SET NEAR ON
  SEEK XLSTYLE
  IF EOF()
    WAIT CLEAR
    WAIT WINDOW 'No invoices were found for the selected styles! UNABLE TO PROCEED.' NOWAIT
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

SCAN &lcSFilt FOR &XFILTER
  WAIT WINDOW 'Collecting invoice # : '+Invoice NOWAIT
  
  SCATTER FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,GROS_PRICE,TOTQTY,COST TO laInvRec
  SELE &INVLTEMP 
  APPE BLANK
 
  GATHER FROM laInvRec FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,GROS_PRICE,TOTQTY,COST
  
  REPLACE TYPE  WITH 'INV',;
          CODE  WITH INVLINE.INVOICE        ,;
          DATE  WITH INVLINE.INVDATE        ,;
          NAME  WITH CUSTOMER.BTNAME        ,;
          CLASS WITH CUSTOMER.CLASS         ,;
          DISCPCNT  WITH INVHDR.DISCPCNT    ,;
          TRDE_DISC WITH INVHDR.TRDE_DISC   ,;
          Ccurrcode WITH INVHDR.Ccurrcode   ,;
          Nexrate   WITH INVHDR.Nexrate     ,;
          Ncurrunit WITH INVHDR.Ncurrunit   ,;
          REP1  WITH INVHDR.REP1            ,;
          Group WITH Style.cStyGroup        ,;
          Department WITH InvHdr.Dept
 
ENDSCAN

SELE &INVLTEMP 
IF llRetModu AND llInclRet  
  WAIT WINDOW 'Building the returns records...' NOWAIT
  *-- Build the returns records.
  INDEX ON Invoice+STR(RECNO(),7) TAG &INVLTEMP
  SELE RETLINE
  IF ! EMPTY(XLSTYLE)
    lcOldNear = SET("NEAR")
    SET NEAR ON    
    =SEEK(XLSTYLE)
    SET NEAR &lcOldNear
  ENDIF
  
  SCAN &lcSFilt FOR &XRFILTER
    WAIT WINDOW 'Collecting credit memo # : '+Crmemo NOWAIT
    lcInvoice=Invoice
    DIMENSION laInvRec[6]

    SCATTER FIELDS ACCOUNT,STYLE,LINENO,GROS_PRICE,PRICE,TOTQTY,COST TO laInvRec
    
    SELE &INVLTEMP
    IF ! SEEK(lcInvoice)
      lcInvoice='ZZZZZZ'      && Returns without invoice or for invoices
                              && not in date range are holding the ZZZZZZ.
    ENDIF
    =SEEK(RETLINE.ACCOUNT+RETLINE.CRMEMO,'RETHDR')
    APPE BLANK
    GATHER FROM laInvRec FIELDS ACCOUNT,STYLE,LINENO,GROS_PRICE,PRICE,TOTQTY,COST
            
    REPLACE TYPE WITH 'RET',;
            INVOICE WITH lcInvoice          ,;
            CODE  WITH RETLINE.CRMEMO       ,;
            DATE  WITH RETLINE.CRDATE       ,;
            NAME  WITH CUSTOMER.BTNAME      ,;
            CLASS WITH CUSTOMER.CLASS       ,;
            Ccurrcode WITH RETHDR.Ccurrcode ,;
            Nexrate   WITH RETHDR.Nexrate   ,;
            Ncurrunit WITH RETHDR.Ncurrunit ,;
            REP1  WITH RetHdr.SalesRep1     ,;
            Group WITH Style.cStyGroup      ,;
            Department WITH InvHdr.Dept
 
  ENDSCAN
ENDIF

SELE &INVLTEMP 
IF RECCOUNT() = 0
  WAIT CLEAR
  = gfDialog( 'I', 'No records have been selected for the report.' )
  RETURN
ENDIF

*--Clear relations
SELE INVLINE
SET RELA TO

IF llRetModu
  SELE RETLINE
  SET RELA TO
ENDIF

*-- Indexing file.
Z = RECCOUNT()
WAIT WINDOW 'Sorting '+STR(Z,6)+' records for the report ' NOWAIT

SELE &INVLTEMP 
DO CASE
  CASE lcSort='A'
    INDEX ON Account+Invoice+Style+Type+Code TAG &INVLTEMP
    lcHBreak='ACCOUNT'
  CASE lcSort='I'
    INDEX ON Invoice+Style+Type+Code TAG &INVLTEMP
    lcHBreak='INVOICE'
  CASE lcSort='S'
    INDEX ON Style+Invoice+Type+Code TAG &INVLTEMP
    lcHBreak='LEFT(STYLE,lnMajorLen)'
  CASE lcSort='C'
    INDEX ON Class+Invoice+Account+Style+Type+Code TAG &INVLTEMP
    lcHBreak='CLASS'
  CASE lcSort='R'
    INDEX ON REP1+Invoice+Account+Style+Type+Code TAG &INVLTEMP
    lcHBreak='REP1'
  CASE lcSort='P'
    INDEX ON Rep1+IIF(Invoice='ZZZZZZ',Invoice,'')+Group+Department+IIF(Invoice='ZZZZZZ','',Invoice)+Account+Style+Type+Code TAG &INVLTEMP
    lcHBreak='Rep1'
ENDCASE

WAIT CLEAR
R_WIDTH = 'W'

SET DEVICE TO PRINT
DO lpPrint
DO ENDREPORT
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lpFilesRela
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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

IF !EMPTY(ldRPPHDate)                          && Invoice Date Filter
  XFILTER=XFILTER+ IIF(EMPTY(ldRPPLDate),;
  ".AND.IIF(lcRPInvBy='S',INVHDR.SHIPDATE,INVHDR.INVDATE)<=ldRPPHDate",;
  ".AND.BETWEEN(IIF(lcRPInvBy='S',INVHDR.SHIPDATE,INVHDR.INVDATE),ldRPPLDate,ldRPPHDate)")
  IF llRetModu AND llInclRet
    XRFILTER=XRFILTER+ IIF(EMPTY(ldRPPLDate),;
    ".AND. CRDATE <=ldRPPHDate"             ,;
    ".AND. BETWEEN(CRDATE,ldRPPLDate,ldRPPHDate)")
  ENDIF 
ENDIF

IF !EMPTY(lcRPFrAcc)                           && Account Filter
  XFILTER = XFILTER + ".AND. BETWEEN(ACCOUNT,lcRPFrAcc,lcRPToAcc)"
  IF llRetModu AND llInclRet
    XRFILTER= XRFILTER + ".AND. BETWEEN(ACCOUNT,lcRPFrAcc,lcRPToAcc)"
  ENDIF
ENDIF

IF ! EMPTY(lcRPSeason)                          && Season Filter
  XFILTER = XFILTER + " .AND. INVLINE.SEASON=lcRPSeason"
  IF llRetModu AND llInclRet
    XRFILTER= XRFILTER + ".AND. Style.Season=lcRPSeason"
  ENDIF
ENDIF

IF ! EMPTY(lcRPSalRep)                          && SalesRep Filter
  XFILTER = XFILTER + ".AND.INVHDR.REP1=lcRPSalRep"
  IF llRetModu AND llInclRet  
    XRFILTER= XRFILTER+ ".AND.RETHDR.SALESREP1=lcRPSalRep"  
  ENDIF
ENDIF

IF ! EMPTY(lcRPDiv)                            && Division Filter
  XFILTER = XFILTER + ".AND. INVHDR.cDIVISION=lcRPDiv"
  IF llRetModu AND llInclRet  
    XRFILTER= XRFILTER + ".AND. RETHDR.cDIVISION=lcRPDiv"
  ENDIF
ENDIF

IF ! EMPTY(ALLTRIM(XTCS))                      && Customer Classification Filter
  XFILTER = XFILTER + ".AND.TRIM(CUSTOMER.CLASS)$XTCS"
  IF llRetModu AND llInclRet
    XRFILTER= XRFILTER + ".AND.TRIM(CUSTOMER.CLASS)$XTCS" 
  ENDIF
ENDIF

IF ! EMPTY(laOGFxFlt[2,6])                      && Curruncy Filter
  XFILTER = XFILTER + " AND INVHDR.cCurrcode = ALLTRIM(laOGFxFlt[2,6])"
  IF llRetModu AND llInclRet  
    XRFILTER= XRFILTER + " AND RETHDR.cCurrcode = ALLTRIM(laOGFxFlt[2,6]) "
  ENDIF
ENDIF

IF !EMPTY(lcStyGrp)                              && Style Group Filter
  XFILTER = XFILTER + ".AND. STYLE.cStyGroup=lcStyGrp"
  IF llRetModu AND llInclRet
    XRFILTER= XRFILTER + " .AND. STYLE.cStyGroup=lcStyGrp"
  ENDIF  
ENDIF

IF USED(laOGFxFlt[3,6]) AND Reccount(laOGFxFlt[3,6]) # 0   && Departement Filter   
  SELECT (laOGFxFlt[3,6])
  SCAN
    lcValueDep = lcValueDep + EVAL(laOGFxFlt[3,6]+'.Dept') +','
  ENDSCAN
 
  XFILTER = XFILTER + " AND INVHDR.Dept $ lcValueDep"
  IF llRetModu AND llInclRet  
    XRFILTER= XRFILTER + " AND INVHDR.Dept $ lcValueDep"
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpCreateTmp
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
  (TYPE  C(3)  , DATE D       , CODE  C(6)   , ACCOUNT C(5) ,;
   NAME C(30)  , CLASS C(6)   , INVOICE C(6) , STYLE C(19)  ,;
   LINENO N(6) , PRICE N(9,2) , GROS_PRICE N(9,2) , TOTQTY N(6)  , COST N(10,2) ,;
   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , CCURRCODE  C(3) , Nexrate N(9,4) , Ncurrunit N(4,0),;
   REP1 C(3) , Department C(5) , Group C(6))

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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

= lfvSumDet()
*!*************************************************************
*! Name      : lfchkRet
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
*! Name      : lfvSumDet
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
= lfOGShowGet('llRPGrnTot')    &&Enable the Grand total option

*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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

IF LASTKEY() = 13
  IF !EMPTY(&lcRepCode) AND  !SEEK(&lcRepCode, "SALESREP") 
    XREPCODE = &lcRepCode
    DO REPCHK WITH XREPCODE, .T.
    &lcRepCode = XREPCODE
  ENDIF
ELSE
  &lcRepCode = ''
ENDIF

*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]


ldRPPLDate = ldFrom
ldRPPHDate = ldTo
*-- end of lfvDateRng.

*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*FUNCTION lfvpbDateOk
FUNCTION lfvpbOk
  *B603955,1 ABD - [End]
IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfvObjRng
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\ObjRng.SPR')
DO ObjRng.Spx
*B603955,1 ABD - [End]

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
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
*! Purpose   : Validate Account OR style
*!*************************************************************
*! Called from : the valid of the from or to field in the screen.
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
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
*! Name      : lfvAccount
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
*! Purpose   : Validate Account
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

*!*************************************************************
*! Name      : lfOldVal
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
*! Purpose   : Saving old value
*!*************************************************************
*! Called from : the when of the from or to field in the screen.
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
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
  
  *--Case if the From field of the Account is empty. &bas
  IF EMPTY(lcFrom)
    DO CASE
      CASE UPPER(lcObj) = "ACCOUNT"
        SELECT CUSTOMER
        =SEEK('M')
        lcFrom = CUSTOMER.ACCOUNT
    ENDCASE
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
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
*! Purpose   : Validate range screen's Cancel button
*!*************************************************************
*! Called from : the valid of the from or to field in the screen.
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
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
PRIVATE llCallGfam        && Variable to check if we use single currency or not
llCallGfam=EMPTY(laOGFxFlt[2,6]) OR (!EMPTY(laOGFxFlt[2,6]) AND lcRpCurr <> 'F')

SELECT &INVLTEMP
GO TOP

ROW     = 99
PAGENO  = 0
R_TITLE = 'GROSS PROFIT BY '+lcHBreak
BREAK   = &lcHBreak

*-- Variables Initialization.
STORE 0 TO XTOT1,XTOT2        &&Subtotals
STORE 0 TO XGTOT1,XGTOT2   &&Grandtotlals

XTITLE = SPACE(1)
XTIME  = TIME()
PAGENO = 0
ROW    = 99
XAcName= ' '

CLEAR TYPEAHEAD

*-- Begin main report loop
DO WHILE .T.
  IF ROW >= 53
    PAGENO = PAGENO+1
    DO RPT_HDR WITH 'ARGROSSP'+lcReport,lcOTitle,R_WIDTH
    IF lcReport='S' .OR. ! llInclRet
      @ 05,00 SAY 'ACCOUNT    ...... NAME .........        INVOICE       STYLE        COLOR          PIECES           S. PRICE                GROSS AMT'
    ELSE
      @ 05,00 SAY 'ACCOUNT    ....... NAME .....    TYP    INV/CRD       STYLE        COLOR          PIECES           S. PRICE                GROSS AMT'
    ENDIF
    @ 06,00 SAY REPLICATE('=',132)
    ROW = 07
  ENDIF

  DO lpSubTotals   &&SubTotals

  STORE 0 TO XRTOT1,XRTOT2    &&RetSubTot

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

ENDDO

*--Print returns without invoice case of invoice sort.
IF llInclRet .AND. lcSort='I' .AND. Invoice='ZZZZZZ'
  DO lpRetNOInv
  xGTot1 = xGTot1 + xRTot1
  xGTot2 = xGTot2 + xRTot2
ENDIF
*---- End main report loop ---

*-- GRAND TOTAL part.
ROW=ROW+1
@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW+1
@ ROW,000 SAY '* GRAND TOTAL *'

@ ROW,080 SAY XGTOT1  PICTURE '99999999'
@ ROW,120 SAY XGTOT2  PICTURE '999999999.99'

ROW = ROW+1
@ ROW,00 SAY REPLICATE('=',132)

*!*************************************************************
*! Name      : lpDetail
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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

  IF ! llRetExst
    @ ROW,000 SAY Account
    @ ROW,011 SAY SUBSTR(Name,1,IIF(llInclRet,17,21))
  ENDIF
  IF llInclRet
    @ ROW,033 SAY Type
  ENDIF
  
  @ ROW,040 SAY Code     && inv/crd
  @ ROW,054 SAY Style
  @ ROW,082 SAY lnQty PICTURE '999999'       && Pieces in detail report
  @ ROW,099 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '99999.99'  && the price
  @ ROW,120 SAY lnGross PICTURE '999999999.99'
  *B803410,1 BWA 07/13/2000 Fix the bug of not printing item description.[START]
  IF TYPE='INV' .AND. SEEK(Style,'STYLE') .AND. !EMPTY(Style.Desc1)
    ROW=ROW+1
    @ ROW,038 SAY Style.Desc1
  ENDIF
  *B803410,1 [END]

  XRTOT1 = XRTOT1 + lnQty
  XRTOT2 = XRTOT2 + lnGross       

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

ROW=ROW+1
*!*************************************************************
*! Name      : lpSummary
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
  XRTOT1 = XRTOT1 + lnQty
  XRTOT2 = XRTOT2 + lnGross       

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
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
      lcSubTitl=BREAK+'   '+IIF(lcReport='D',SUBSTR(XAcName,1,26),'   '+ XAcName)
    CASE lcSort='C'
      lcSubTitl=BREAK+'   '+gfCodDes(BREAK,'CLASS')
      SELECT &INVLTEMP
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
     @ ROW,IIF(lcSort = 'S',54,IIF(lcSort = 'I', 40 , 0)) SAY ;
            IIF(lcSort='C',' CLASS : ',IIF(lcSort='R',' Sales Rep : ',''))+lcSubTitl
    ENDIF
  ENDIF

  IF XGrdTotO='N'
    @ ROW,080 SAY XTOT1  PICTURE '99999999'        && Pieces in Summery report
    @ ROW,120 SAY XTOT2  PICTURE '999999999.99'    && Gross Amt. in Summery report
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
  STORE 0 TO XTOT1,XTOT2

  BREAK=&lcHBreak

  EXIT
ENDDO
*-- End Subtotals Loop --
*!*************************************************************
*! Name      : lpRetNOInv
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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

  IF lcReport='D'       && RETURNS WITHOUT INVOICES in detail reports
    ROW=ROW+1
    @ ROW,000 SAY Account
    @ ROW,011 SAY SUBSTR(Name,1,17)    
    @ ROW,033 SAY Type
    @ ROW,040 SAY Code
    @ ROW,054 SAY Style
    @ ROW,082 SAY lnQty PICTURE '999999'
    @ ROW,099 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '99999.99'    
    @ ROW,123 SAY lnGross PICTURE '999999.99'
    
    *B803410,1 BWA 07/13/2000 Fix the bug of not printing item description.[START]
    IF SEEK(Style ,'STYLE') .AND. !EMPTY(Style.Desc1)
      ROW=ROW+1
      @ ROW,038 SAY Style.Desc1
    ENDIF
    *B803410,1 [END]
    
  ENDIF
    
  xRTot1 = xRTot1 + lnQty
  xRTot2 = xRTot2 + lnGross
  BREAK=&lcHBreak

 SKIP
ENDDO  

ROW = ROW + IIF(lcReport='D',1,0)
IF (xRTot1<>0 .AND. lcSort='I'.AND. lcReport='S')
  ROW = ROW + 1
  @ ROW,00 SAY 'RETURNS WITHOUT INVOICES :'       

  @ ROW,082 SAY xRTot1 PICTURE '999999'           && Pieces in Summery report for return
  @ ROW,123 SAY xRTot2 PICTURE '999999.99'        && Gross Amt. in Summery report for return
ENDIF
*!*************************************************************
*! Name      : lfwCurCode
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
  laOGFxFlt[2,6]= gcBaseCurr
ENDIF
SHOW GET laOGFxFlt[2,6]

*!*************************************************************
*! Name      : lfRepShow
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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
FUNCTION lfRepShow

laOGFxFlt[2,6]= gcBaseCurr
=lfOGShowGet("lnRepCurr")

*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
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

*!**************************************************************************
*! Name      : lfStDepart
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
*! Purpose   : Go top in the icdepthd file when browse.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfStDepart()
*!**************************************************************************
*
FUNCTION lfStDepart
PARAMETERS OpGrdParm

SELECT ICDEPTHD
GO TOP
*-- End of lfStDepart.
