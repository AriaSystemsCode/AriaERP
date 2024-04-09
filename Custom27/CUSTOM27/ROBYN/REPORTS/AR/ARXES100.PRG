*:***************************************************************************
*: Program file  : ARXES100.prg  CUST#101365
*: Date          : 11/18/98
*: Program desc. : Gross profit
*:               : Convert report xes100 from 2.6 to 2.7 For Xes CUST#101365
*: System        : Aria Advantage Series.
*: Module        : Account receivable (AR)
*: Developer     : Ahmed Salah Shalaby - (SSH)
*:***************************************************************************
*: Calls : 
*:    Procedures : lpSetFilter,lpInv971,lpDetail,lpSummary,lpSubTotals
*:    Functions  : lfPrnHdr,lfwRepWhen,lfvIncRet,lfvSumDet,lfvSalesRep
*:               : lfvDateRng,lfvpbDateOk,lfvStyle,lfvObjRng,lfvAccount
*:               : lfvpbObjOk,lfvpbObjCa,lfchkRet,lfMajTtGet,lfNonMaj
*:               : lfMajPic,lfOldVal
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARXES100
*:***************************************************************************
*: Modifications
*B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*B603991,1 ASH 10/30/2000 Open OrdHdr&OrdLine files.
*:***************************************************************************

*:C101365,1 SSH ldRPPLDate Variable that hold From date value
*:C101365,1 SSH ldRPPHDate Variable that hold to   date value
*:C101365,1 SSH lcRPFrSty  Variable that hold From style value
*:C101365,1 SSH lcRPToSty  Variable that hold To   style value
*:C101365,1 SSH lcRPFrAcc  Variable that hold From Account value
*:C101365,1 SSH lcRPToAcc  Variable that hold To   Account value
*:C101365,1 SSH lcRPSeason Variable that hold Season
*:C101365,1 SSH lcRPDiv    Variable that hold Division

STORE SPACE(02) TO lcSeason
lcSlsRep = SPACE(03)
lnMajLen = LEN(gfItemMask('PM'))
lnTerm = ' '
DECLARE laTerm[1,2]
laTerm[1,1] = 'NTERDISCR'
laTerm[1,2] = 'lnTerm'
ldLDate     = ldRPPLDate
ldHDate     = ldRPPHDate
XLSTYLE     = lcRPFrSty
XHSTYLE     = lcRPToSty
XLACCOUNT   = lcRPFrAcc
XHACCOUNT   = lcRPToAcc
lcSeason    = lcRPSeason
lcDiv       = lcRPDiv
*B603991,1 ASH 10/30/2000 (Begin) Open OrdHdr&OrdLine files.
=gfOpenFile(gcDataDir+'OrdLine',gcDataDir+'OrdLine','SH')
=gfOpenFile(gcDataDir+'OrdHdr',gcDataDir+'OrdHdr','SH')
*B603991,1 ASH 10/30/2000 (End)
*DIME laCost[1,2]
*laCost[1,1]  = 'M_COST_METH'
*=gfGetMemVar(@laCost,gcAct_Comp)
*llAvrgCost = (laCost[1,2] = 'A')

llAvrgCost = gfGetMemVar('M_COST_METH',gcAct_Comp)='A'

*:C101365,1 SSH Initializing the grids variables.
lcSlsRep  = lcRPSalRep
lcReport  = lcRpSumDet
lcSort    = lcRpSortBy
XGrdTotO  = !llRpGrnTot
lcOTitle  = lcRPTitle 
*:C101365,1 SSH Set and build the filters due to grid var.
XFILTER = " ORDHDR.STATUS <> 'X' .AND. 'M'+ORDHDR.ACCOUNT=Customer.TYPE+Customer.ACCOUNT .AND. STYLE.Status<>'X'"
XFILTER = XFILTER  + ' .AND. ' + lcRpExp
DO lpSetFilter
*:C101365,1 SSH Creat a temp file.
WAIT WINDOW 'Selecting report information. Please wait...' NOWAIT
tmpOrdLn = gfTempName()
lcOrdBy  = 'OrdLine.Style,'+IIF(lcSort='A','OrdLine.Account','OrdHdr.Season')
SET TALK ON
SET TALK WINDOW
SELECT OrdLine.*;
       FROM OrdLine,OrdHdr,Customer,Style;
       WHERE OrdHdr.Order = OrdLine.Order .AND.;
             Style.Style = OrdLine.Style .AND. ;
             &xFilter;
       INTO DBF &gcWorkDir.&tmpOrdLn;
       ORDER BY &lcOrdBy
SET TALK OFF
IF _TALLY <> 0
  SELECT (tmpOrdLn)
  SET RELATION TO 'O'+Order INTO OrdHdr,;
                  'M'+ACCOUNT INTO CUSTOMER,;
                  Order+STR(LineNo,6) INTO INVLINE ADDITIVE
  GO TOP
  R_WIDTH = 'W'
  *:C101365,1 SSH Print layout.
  DO lpInv971
  *:C101365,1 SSH End the report or display on screen.
  DO ENDREPORT
  SET DEVICE TO SCREEN
ELSE
  *:C101365,1 SSH  Text "No Record Selected."
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
ENDIF
*:C101365,1 SSH  Erase temprary file
USE IN &tmpOrdLn
ERASE &gcWorkDir.&tmpOrdLn+'.Dbf'
ERASE &gcWorkDir.&tmpOrdLn+'.Fpt'

*!*************************************************************
*! Name      : lpSetFilter.prg
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
*! Purpose   : Returns with the filters expresions.
*!*************************************************************
*! Called from : Prog
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpSetFilter
*!*************************************************************
PROCEDURE lpSetFilter

IF !EMPTY(xlStyle)
  XFILTER = XFILTER + ".AND. BETWEEN(SUBSTR(OrdLine.Style,1,lnMajLen),xlStyle,xhStyle)"
ENDIF

IF !EMPTY(ldHDate)
  XFILTER=XFILTER+ IIF(EMPTY(ldLDate),;
  ".AND. OrdHdr.Complete<=ldHDate",".AND.BETWEEN(OrdHdr.Complete,ldLDate,ldHDate)")
ENDIF

IF !EMPTY(XLACCOUNT)
  XFILTER = XFILTER + ".AND. BETWEEN(OrdHdr.ACCOUNT,XLACCOUNT,XHACCOUNT)"
ENDIF

IF !EMPTY(lcSeason)
  XFILTER = XFILTER + " .AND.OrdHdr.SEASON=lcSeason"
ENDIF

IF !EMPTY(lcSlsRep)
  XFILTER = XFILTER + ".AND.OrdHdr.REP1=lcSlsRep"
ENDIF

IF !EMPTY(lcDiv)
  XFILTER = XFILTER + ".AND.OrdHdr.cDIVISION=lcDiv"
ENDIF

*!*************************************************************
*! Name      : lpInv971.prg
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
*! Purpose   : Main procedure.
*!*************************************************************
*! Called from : Prog
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpInv971
*!*************************************************************
PROCEDURE lpInv971

SET DEVICE TO PRINT
SELECT (tmpOrdLn)
R_TITLE = 'GROSS PROFIT BY ' + IIF(lcSort= 'S','STYLE','ACCOUNT')
*:C101365,1 SSH Variables Initialization.
XTIME      = TIME()
PAGENO     = 0
lnBokQty   = 0
lnDisAmt   = 0
ROW        = 99
lcOldAct   = OrdHdr.Account
lcOldStyle = SUBSTR(Style,1,lnMajLen)
lcSeekSty  = STYLE
lcOldSeson = OrdHdr.Season

DIMENSION laSub[4], laGrn[4]
STORE 0 TO laSub,laGrn

CLEAR TYPEAHEAD
SET DEVICE TO SCREEN
*WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
SET DEVICE TO PRINT
*:C101365,1 SSH Begin main report loop.
SCAN WHILE INKEY() <> 32
  IF ROW >= 53
    =lfPrnHdr()
  ENDIF
  DO lpSubTotals
  =SEEK(lcSeekSty,'Style')
  =SEEK(InvLine.Invoice,'InvHdr')
  IF lcOldStyle <> SUBSTR(Style,1,lnMajLen)
    IF lcReport = 'D' 
      DO lpDetail
    ELSE
      DO lpSummary
    ENDIF
    lcOldStyle = SUBSTR(Style,1,lnMajLen)
    lcSeekSty  = STYLE    
    lcOldSeson = OrdHdr.Season
    lnBokQty   = 0
    lnDisAmt   = 0
    ROW = ROW + 1
   =SEEK(lcSeekSty,'Style')
    =SEEK(InvLine.Invoice,'InvHdr')
  ENDIF
  *:C101365,1 SSH Calculate report fields.
  lnCurQty = TotQty + InvLine.TotQty
  lnBokQty = lnBokQty + lnCurQty
  *:C101365,1 SSH Get the discount on the price.
  lnPrcDis = Style.PriceA - IIF(OrdHdr.Status='C',InvLine.Price,Price)
  lnPrcDis = IIF(lnPrcDis > 0, lnPrcDis, 0) * lnCurQty
  *:C101365,1 SSH Get the merchandise discoutn.
  lnMerDis = IIF(OrdHdr.Status='C',InvHdr.DiscPcnt,OrdHdr.Disc)
  lnMerDis = (IIF(lnMerDis > 0, lnMerDis, 0) /100 ) * ;
             (IIF(OrdHdr.Status='C',InvLine.Price,Price) * lnCurQty)

  *:C101365,1 SSH Get the trade discount.
  = gfRltFld(IIF(OrdHdr.Status='C',InvHdr.cTermCode,OrdHdr.cTermCode),;
                                   @laTerm , 'CTERMCODE')
  lnTrdDis = IIF(lnTerm>0,;
             ((IIF(OrdHdr.Status='C',InvLine.Price,Price) * lnCurQty) - lnMerDis) ;
             * lnTerm / 100 ,0)
  lnDisAmt = lnDisAmt + lnPrcDis + lnMerDis + lnTrdDis
ENDSCAN
IF lcOldStyle <> SUBSTR(Style,1,lnMajLen)
  IF lcReport = 'D' 
    DO lpDetail
  ELSE
    DO lpSummary
  ENDIF
ENDIF

DO lpSubTotals
*:C101365,1 SSH End main report loop

*:C101365,1 SSH GRAND TOTAL part.
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW+1
@ ROW,000 SAY '* GRAND TOTAL *'

@ ROW,015 SAY laGrn[1]                                     PICTURE '9999999'
@ ROW,032 SAY laGrn[2]                                     PICTURE '99999999.99'
@ ROW,044 SAY laGrn[3]                                     PICTURE '99999999.99'
@ ROW,067 SAY laGrn[4]                                     PICTURE '999999.99'
@ ROW,085 SAY (laGrn[2]-laGrn[4])                          PICTURE '99999999.99'
@ ROW,109 SAY (laGrn[2]-laGrn[3]-laGrn[4])                 PICTURE '99999999.99'
@ ROW,123 SAY (laGrn[2]-laGrn[3]-laGrn[4])/(laGrn[2]-laGrn[4])*100    PICTURE '999.99'
@ ROW,130 SAY '%'
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',132)

*!*************************************************************
*! Name      : lpDetail.prg
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
*! Purpose   : Printing the DETAILS
*!*************************************************************
*! Called from : Prog
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

@ ROW,001 SAY lcOldStyle
@ ROW,011 SAY SUBSTR(lcOldSeson,1,2)

@ ROW,016 SAY lnBokQty     PICTURE '999999'
laSub[1] = laSub[1] + lnBokQty
laGrn[1] = laGrn[1] + lnBokQty

lnSelPrc = Style.PriceA
lnStdCst = IIF(llAvrgCost,Style.Ave_Cost,Style.TotCost)
  
@ ROW,024 SAY lnSelPrc                         PICTURE '9999.99'
@ ROW,033 SAY lnBokQty * lnSelPrc              PICTURE '9999999.99'
laSub[2] = laSub[2] + lnBokQty * lnSelPrc
laGrn[2] = laGrn[2] + lnBokQty * lnSelPrc
  
@ ROW,045 SAY lnBokQty * lnStdCst              PICTURE '9999999.99'
@ ROW,059 SAY lnStdCst                         PICTURE '9999.99'
laSub[3] = laSub[3] + lnBokQty * lnStdCst
laGrn[3] = laGrn[3] + lnBokQty * lnStdCst
   
@ ROW,068 SAY lnDisAmt                         PICTURE '99999.99'
laSub[4] = laSub[4] + lnDisAmt
laGrn[4] = laGrn[4] + lnDisAmt
  
@ ROW,078 SAY lnDisAmt/(lnBokQty*lnSelPrc)*100         PICTURE '999.99'
@ ROW,084 SAY '%'
@ ROW,086 SAY (lnBokQty*lnSelPrc)-lnDisAmt             PICTURE '9999999.99'
@ ROW,099 SAY ((lnBokQty*lnSelPrc)-lnDisAmt)/lnBokQty  PICTURE '9999999.99'
@ ROW,110 SAY (lnBokQty*lnSelPrc)-(lnBokQty*lnStdCst)-;
              lnDisAmt                                 PICTURE '9999999.99'
@ ROW,123 SAY ((lnBokQty*lnSelPrc)-(lnBokQty*lnStdCst)-lnDisAmt);
              /((lnBokQty*lnSelPrc)-lnDisAmt)*100        PICTURE '999.99'
@ ROW,130 SAY '%'
*!*************************************************************
*! Name      : lpSummary.prg
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
*! Purpose   : Print summary form.
*!*************************************************************
*! Called from : Prog
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

laSub[1] = laSub[1] + lnBokQty
laGrn[1] = laGrn[1] + lnBokQty
lnSelPrc = Style.PriceA
lnStdCst = IIF(llAvrgCost,Style.Ave_Cost,Style.TotCost)
laSub[2] = laSub[2] + lnBokQty * lnSelPrc
laGrn[2] = laGrn[2] + lnBokQty * lnSelPrc
laSub[3] = laSub[3] + lnBokQty * lnStdCst
laGrn[3] = laGrn[3] + lnBokQty * lnStdCst
laSub[4] = laSub[4] + lnDisAmt
laGrn[4] = laGrn[4] + lnDisAmt
*!*************************************************************
*! Name      : lpSubTotals.prg
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
*! Purpose   : Print summary form.
*!*************************************************************
*! Called from : Prog
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

IF (lcSort = 'A' .AND. lcOldAct   = Account ) .OR. ;
   (lcSort = 'S' .AND. lcOldSeson = OrdHdr.Season)
  RETURN
ENDIF

IF lcReport = 'D' .AND. lcSort = 'A' .AND. lcOldAct <> ACCOUNT
  ROW = ROW + 1
  @ ROW,001 SAY '*** ACCOUNT  ***  ' + lcOldAct
  lcOldAct = ACCOUNT
  ROW = ROW + 1
ENDIF

lcSubTitl = IIF(lcSort='A',lcOldAct,lcOldSeson)
IF lcReport='D'
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE('-',132)
  ROW = ROW + 1
  @ ROW,000 SAY 'SUBTOTAL   '+IIF(lcSort='A','',SUBSTR(lcSubTitl,1,2))
ELSE
  IF XGrdTotO
    @ ROW,001 SAY lcSubTitl
  ENDIF
ENDIF

IF XGrdTotO
  @ ROW,015 SAY laSub[1]             PICTURE '9999999'
  @ ROW,032 SAY laSub[2]             PICTURE '99999999.99'
  @ ROW,044 SAY laSub[3]             PICTURE '99999999.99'
  @ ROW,067 SAY laSub[4]             PICTURE '999999.99'
  @ ROW,085 SAY (laSub[2]-laSub[4])              PICTURE '99999999.99'
  @ ROW,109 SAY (laSub[2]-laSub[3]-laSub[4])     PICTURE '99999999.99'
  @ ROW,123 SAY (laSub[2]-laSub[3]-laSub[4])/(laSub[2]-laSub[4])*100  PICTURE '999.99'
  @ ROW,130 SAY '%'
ENDIF

IF lcReport='D'
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE('-',132)
ENDIF 

IF XGrdTotO
  ROW = ROW + 1
ENDIF
laSub = 0
*!*************************************************************
*! Name      : lfPrnHdr.Prg
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
*! Purpose   : Print the header of the report.
*!*************************************************************
*! Called from : Prog
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrnHdr()
*!*************************************************************
FUNCTION lfPrnHdr

PAGENO = PAGENO + 1
DO RPT_HDR WITH 'INV970'+lcReport, lcOTitle, R_WIDTH
@ 05,00 SAY ' STYLE     SE  PCS BKD  S.PRICE   GROSS AMT    TOT COST   STD.COST  DISCOUNT   DISC.%    NET AMT  A.NET PRICE     PROFIT   % PROFIT'
@ 06,00 SAY REPLICATE('=',132)
ROW = 07

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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

R_WIDTH = 'W'
SET ORDER TO CODES    IN CODES
SET ORDER TO CUSTOMER IN CUSTOMER
SET ORDER TO SALESREP IN SALESREP
SET ORDER TO STYLE    IN STYLE
SET ORDER TO INVHDR   IN INVHDR
SET ORDER TO INVLINEO IN INVLINE
IF llRetModu
  SET ORDER TO RETHDRA  IN RETHDR
  SET ORDER TO RETLINES IN RETLINE
ENDIF
= lfvIncRet()
= lfvSumDet()

*!*************************************************************
*! Name      : lfvIncRet
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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

lnSubRetPo = ASCAN(laOGObjType,'llRPSubRet',1)
IF lnSubRetPo > 0
  laOGObjCnt[lnSubRetPo] = llRPIncRet
  llRPSubRet = IIF(llRPIncRet,llRPSubRet,.F.)
  = lfOGShowGet('llRPSubRet')
ENDIF  

*!*************************************************************
*! Name      : lfvSumDet
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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
*! Name      : lfvSalesRep
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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

IF LASTKEY() = 13 AND !MDOWN()
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
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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

STORE {} TO ldFrom,ldTo
lcTitle = 'Prod. order comp. date'
*:C101365,1 SSH Run the advance payment screen 

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')
DO DateRng.Spx
*B603955,1 ABD - [End]

ldRPPLDate = ldFrom
ldRPPHDate = ldTo

*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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

*:C101365,1 SSH  this is the validation of from_item

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
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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
*! Name      : lfvpbObjOk
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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

*:C101365,1 SSH  this is the validation of to_item
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

*!*************************************************************
*! Name      : lfvpbObjCa
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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

*:C101365,1 SSH  this is the validation of to_item

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
*! Name      : lfchkRet
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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
*! Name      : lfMajTtlGet
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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

*:C101365,1 SSH Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
llStopConc = .F.
*:C101365,1 SSH  Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  *:C101365,1 SSH If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *:C101365,1 SSH  this means that another type is found rather than color or free
      *:C101365,1 SSH  and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*:C101365,1 SSH  Compute Free/Color Items in Style Structure. [End]
RETURN ''

*!*************************************************************
*! Name      : lfMajPic
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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
*! Name      : lfOldVal
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 11/18/98
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