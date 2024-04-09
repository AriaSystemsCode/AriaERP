*:***************************************************************************
*: Program file  : ARXES500.PRG
*: Program desc. : CUSTOMIZED GROSS PROFIT FOR XES.
*! Date          : 02/04/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C102164
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARXES500
*:***************************************************************************

                   *-- Initializing the grids variables.--*
STORE SPACE(0) TO lcTrue , lcFalse , lcSFilt , XFILTER , XRFILTER

lnMajorLen = LEN(gfItemMask("PM"))
XLSTYLE    = PADR(lcRPFrSty,lnMajorLen)
XHSTYLE    = PADR(lcRPToSty,lnMajorLen)

*-- Getting class. postion in Fixed filtter array to get all classes that
*-- user selected in option grid
*-- XTCS variable that hold selected classes
lnClassPo = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CLASS'),1)
XTCS = laOGFxFlt[lnClassPo,6]

DO lpFilsRela         &&Make the relations between the files.
DO lpSetFiltr         &&Set and build the filters due to grid var.
DO lpCreateTmp        &&Creat a temp file.

WAIT WINDOW 'Building the invoices records...' NOWAIT
*-- Build the invoice records.
SELECT InvLine
IF !EMPTY(XLSTYLE)
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

*--Use the invline tag to make the scan mush faster.
IF EMPTY(XLSTYLE) AND EMPTY(XHSTYLE)
  SET ORDER TO InvLine
ENDIF

SCAN &lcSFilt FOR &XFILTER
  WAIT WINDOW 'Collecting invoice # : '+Invoice NOWAIT
  SCATTER FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,TOTQTY,COST TO laInvRec
  SELECT (lcInvLTmp)
  APPEND BLANK
  GATHER FROM laInvRec FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,TOTQTY,COST

  REPLACE TYPE      WITH 'INV'            ,;
          CODE      WITH InvLine.INVOICE  ,;
          DATE      WITH InvLine.INVDATE  ,;
          NAME      WITH Customer.BTNAME  ,;
          CLASS     WITH Customer.CLASS   ,;
          DISCPCNT  WITH InvHdr.DISCPCNT  ,;
          TRDE_DISC WITH InvHdr.TRDE_DISC ,;
          REP1      WITH InvHdr.REP1
ENDSCAN

SELECT (lcInvLTmp)
IF llRetModu AND llRPIncRet
  WAIT WINDOW 'Building the returns records...' NOWAIT
  *-- Build the returns records.
  INDEX ON Invoice+STR(RECNO(),7) TAG (lcInvLTmp)
  SELECT RetLine
  IF !EMPTY(XLSTYLE)
    lcOldNear = SET("NEAR")
    SET NEAR ON
    =SEEK(XLSTYLE)
    SET NEAR &lcOldNear
  ENDIF

  SCAN &lcSFilt FOR &XRFILTER
    WAIT WINDOW 'Collecting credit memo # : '+ Crmemo NOWAIT
    lcInvoice=Invoice
    DIMENSION laInvRec[6]
    SCATTER FIELDS ACCOUNT,STYLE,LINENO,PRICE,TOTQTY,COST TO laInvRec

    SELECT (lcInvLTmp)
    IF ! SEEK(lcInvoice)
      lcInvoice='ZZZZZZ'      && Returns without invoice or for invoices
                              && not in date range are holding the ZZZZZZ.
    ENDIF
    =SEEK(RetLine.ACCOUNT+RetLine.CRMEMO,'RetHdr')
    APPE BLANK

    GATHER FROM laInvRec FIELDS ACCOUNT,STYLE,LINENO,PRICE,TOTQTY,COST
    REPLACE TYPE WITH 'RET',;
            INVOICE WITH lcInvoice      ,;
            CODE  WITH RetLine.CRMEMO  ,;
            DATE  WITH RetLine.CRDATE  ,;
            NAME  WITH Customer.BTNAME ,;
            CLASS WITH Customer.CLASS  ,;
            REP1  WITH RetHdr.SalesRep1
  ENDSCAN
ENDIF

SELECT (lcInvLTmp)
IF RECCOUNT() = 0
  WAIT CLEAR
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

*--Clear relations
SELECT InvLine
SET RELA TO

IF llRetModu
  SELECT RetLine
  SET RELATION TO
ENDIF

*-- Indexing file.
Z = RECCOUNT()
WAIT WINDOW 'Sorting '+STR(Z,6)+' records for the report ' NOWAIT

SELECT (lcInvLTmp)
INDEX ON Style+Invoice+Type+Code+STR(RECNO(),7) TAG (lcInvLTmp)
lcHBreak = 'LEFT(STYLE,lnMajorLen)'

WAIT CLEAR
R_WIDTH = 'W'

SET DEVICE TO PRINT
DO lpPrint
DO ENDREPORT
SET DEVICE TO SCREEN
                          *--End of the program --*
*!*************************************************************
*! Name      : lpSetFiltr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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
*! Example     : DO lpSetFiltr
*!*************************************************************
PROCEDURE lpSetFiltr

*--initilize filters on file.
XFILTER = "InvHdr.Status<>'V'"
IF llRPIncRet 
  XRFILTER = "RetHdr.Status<>'V'"
ENDIF

*--Filter for date.
lcTrue = IIF(lcRPInvBy ='S' , ".AND. InvHdr.SHIPDATE <=ldRPPHDate" , ".AND. InvHdr.INVDATE <=ldRPPHDate" )
lcFalse = IIF(lcRPInvBy='S', ".AND. BETWEEN(InvHdr.SHIPDATE,ldRPPLDate,ldRPPHDate)", ".AND. BETWEEN(InvHdr.INVDATE,ldRPPLDate,ldRPPHDate)")

IF !EMPTY(ldRPPHDate)
  XFILTER=XFILTER+ IIF(EMPTY(ldRPPLDate),lcTrue,lcFalse)
  IF llRPIncRet
    XRFILTER=XRFILTER+ ".AND. BETWEEN(CRDATE,ldRPPLDate,ldRPPHDate)"
  ENDIF 
ENDIF

*--Filter for Account.
IF !EMPTY(lcRPFrAcc)
  XFILTER = XFILTER + ".AND. BETWEEN(ACCOUNT,lcRPFrAcc,lcRPToAcc)"
  IF llRPIncRet
    XRFILTER= XRFILTER + ".AND. BETWEEN(ACCOUNT,lcRPFrAcc,lcRPToAcc)"
  ENDIF
ENDIF

*--Filter for Season.
IF !EMPTY(lcRPSeason)
  XFILTER = XFILTER + " .AND.InvHdr.SEASON = lcRPSeason"
  IF llRPIncRet
    XRFILTER= XRFILTER + ".AND. Style.Season = lcRPSeason"
  ENDIF
ENDIF

*--Filter for SalesRep.
IF !EMPTY(lcRPSalRep)
  XFILTER = XFILTER + ".AND.InvHdr.REP1 = lcRPSalRep"
  IF llRetModu AND llRPIncRet
    XRFILTER= XRFILTER+ ".AND.RetHdr.SALESREP1 = lcRPSalRep"  
  ENDIF
ENDIF

*--Filter for Division.
IF !EMPTY(lcRPDiv)
  XFILTER = XFILTER + ".AND.InvHdr.cDIVISION = lcRPDiv"
  IF llRetModu AND llRPIncRet  
    XRFILTER= XRFILTER + ".AND. RetHdr.cDIVISION = lcRPDiv"
  ENDIF
ENDIF

*--Filter for classifications.
IF !EMPTY(ALLTRIM(XTCS))
  XFILTER = XFILTER + ".AND.TRIM(Customer.CLASS)$XTCS"
  IF llRPIncRet  
    XRFILTER= XRFILTER + ".AND.TRIM(Customer.CLASS)$XTCS" 
  ENDIF
ENDIF

*--End of lpSetFiltr.
*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

SELECT (lcInvLTmp)
GO TOP
ROW     = 99
PAGENO  = 0
R_TITLE = 'GROSS PROFIT BY '+ 'STYLE'
BREAK   = &lcHBreak

*-- Variables Initialization.
STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5        &&Subtotals
STORE 0 TO XGTOT1,XGTOT2,XGTOT3,XGTOT4,XGTOT5   &&Grandtotlals

XAcName= ' '
CLEAR TYPEAHEAD

DO WHILE .T.
   WAIT WINDOW 'Printing Transaction # : '+ Invoice NOWAIT
   IF ROW >= 53
     PAGENO = PAGENO+1
     DO RPT_HDR WITH 'ARXES500'+'S',lcRPTitle,R_WIDTH
      @ 05,00 SAY 'STYLE         PIECES  GROSS AMT/PIECE  GROSS AMT  NET AMT/PIECE   NET AMT  COST/PIECE  TOTAL COST PROFIT/PIECE  TOTAL PROFIT  PROF. %'
      @ 06,00 SAY REPLICATE('=',132)
     ROW = 07
   ENDIF

   DO lpSubTotals

   STORE 0 TO XRTOT1,XRTOT2,XRTOT3,XRTOT4,XRTOT5    &&RetSubTot

   IF EOF() .OR. (llRPIncRet .AND. Invoice='ZZZZZZ')
     EXIT  
   ENDIF

   IF ROW >=53
     ROW = 99
     LOOP
   ENDIF

   DO lpSummary       

   *--Print returns without invoice.
   IF llRPIncRet .AND. Invoice='ZZZZZZ' 
     DO lpRetNOInv
   ENDIF
   xTot1 = xTot1 + xRTot1 
   xTot2 = xTot2 + xRTot2 
   xTot3 = xTot3 + xRTot3 
   xTot4 = xTot4 + xRTot4 
ENDDO

*-- Print returns without invoice case of invoice sort.
*-- End main report loop.
*-- GRAND TOTAL part.
ROW=ROW+1
@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW+1
@ ROW,000 SAY '*GRAND TOTAL*'

@ ROW,14 SAY XGTOT1  PICTURE '999999'
@ ROW,30 SAY XGTOT2/XGTOT1  PICTURE '9999.99'
@ ROW,38 SAY XGTOT2  PICTURE '9999999.99'
@ ROW,56 SAY XGTOT3/XGTOT1  PICTURE '9999.99'
@ ROW,63 SAY XGTOT3  PICTURE '9999999.99'
@ ROW,78 SAY XGTOT4/XGTOT1 PICTURE '9999.99'
@ ROW,87 SAY XGTOT4 PICTURE '9999999.99'
XGTOT5=XGTOT3-XGTOT4
@ ROW,103 SAY XGTOT5/XGTOT1 PICTURE '9999.99'
@ ROW,114 SAY XGTOT5 PICTURE '9999999.99'
XGTOT6=ROUND(IIF(XGTOT3<>0,(XGTOT5/XGTOT3)*100,0),2)
XGTOT6=IIF((XGTOT5<0 .AND. XGTOT3<0),-XGTOT6,XGTOT6)
@ ROW,125 SAY XGTOT6 PICTURE '999.99 %'
ROW = ROW+1
@ ROW,00 SAY REPLICATE('=',132)

*--End of lpPrint.
*!*************************************************************
*! Name      : lpSummary
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
*! Purpose   : Printing the Summary case.
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
  lnGross  = lnQty*Price
  lnNet    = lnGross * (1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))
  lnCost   = lnQty * Cost 
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

  XRTOT1 = XRTOT1 + lnQty
  
  IF llRPSubRet
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

  IF llRPIncRet
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
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
*! Purpose   : Printing the report SubTotals.
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

  IF !llRPGrnTot
    @ ROW,0 SAY LEFT(BREAK,lnMajorLen)
    @ ROW,14 SAY XTOT1  PICTURE '999999'
    @ ROW,30 SAY XTOT2/XTOT1  PICTURE '9999.99'
    @ ROW,38 SAY XTOT2  PICTURE '9999999.99'
    @ ROW,56 SAY XTOT3/XTOT1  PICTURE '9999.99'
    @ ROW,63 SAY XTOT3  PICTURE '9999999.99'
    @ ROW,78 SAY XTOT4/XTOT1 PICTURE '9999.99'
    @ ROW,87 SAY XTOT4 PICTURE '9999999.99'
    XTOT5=XTOT3-XTOT4
    @ ROW,103 SAY XTOT5/XTOT1 PICTURE '9999.99'
    @ ROW,114 SAY XTOT5 PICTURE '9999999.99'
    XTOT6=ROUND(IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0),2)
    XTOT6=IIF((XTOT5<0 .AND. XTOT3<0),-XTOT6,XTOT6)
    @ ROW,125 SAY XTOT6 PICTURE '999.99 %'
  ENDIF

  IF !llRPGrnTot
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

*--End of lpSubTotals.
*!*************************************************************
*! Name      : lpRetNOInv
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

DO WHILE INVOICE='ZZZZZZ' .AND. &lcHBreak = BREAK

  lnQty    = IIF(TYPE='INV',TOTQTY,-(TOTQTY))
  lnGross  = lnQty*PRICE
  lnNet    = lnGross * (1 - (DISCPCNT/100)) * (1 - (TRDE_DISC/100))
  lnCost   = lnQty * COST 
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

  xRTot1 = xRTot1 + lnQty
  IF llRPSubRet
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
  BREAK  = &lcHBreak
 SKIP
ENDDO

*--End of lpRetNOInv.
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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
*! Date      : 02/04/20001
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

*--End of lfMajTtlGet.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

*--End of lfMajPic.
*!*************************************************************
*! Name      : lfchkRet
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

*--End of lfchkRet.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

*--End of lfvDateRng.
*!*************************************************************
*! Name      : lfvObjRng
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

*--End of lfvObjRng.
*!*************************************************************
*! Name      : lfvSalesRep
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

*--End of lfvSalesRep.
*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

*--End of lfvPbOk.
*!*************************************************************
*! Name      : lfOldVal
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

*--End of lfOldVal.
*!*************************************************************
*! Name      : lfvObj
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

*--End of lfvObj.
*!*************************************************************
*! Name      : lfvAccount
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

*--End of lfvAccount.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

*--End of lfvStyle.
*!*************************************************************
*! Name      : lfvpbObjOk
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

*--End of lfvpbObjOk.
*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
*! Purpose   : Refresh the filters in the option grid.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfClrRead()
*!*************************************************************
FUNCTION lfClrRead
CLEAR READ

*--End of lfClrRead.
*!*************************************************************
*! Name      : lpFilsRela
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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
*! Example     : DO lpFilsRela
*!*************************************************************
PROCEDURE lpFilsRela

SELECT InvLine
SET RELATION TO INVOICE INTO InvHdr,'M'+ACCOUNT INTO Customer
SET RELATION TO STYLE INTO STYLE ADDITIVE

IF llRetModu
  SELECT RetLine
  SET RELATION TO ACCOUNT+CRMEMO INTO RetHdr,'M'+ACCOUNT INTO Customer
  SET RELATION TO STYLE INTO STYLE ADDITIVE
ENDIF

*--End of lpFilsRela.
*!*************************************************************
*! Name      : lpCreateTmp
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/04/20001
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

CREATE TABLE &gcWorkDir.&lcInvLTmp ;
  (TYPE     C(3)   , DATE  D          , CODE    C(6) , ACCOUNT C(5)    ,;
   NAME     C(30)  , CLASS C(6)       , INVOICE C(6) , STYLE   C(19)   ,;
   LINENO   N(6)   , PRICE N(12,2)    , TOTQTY  N(8)  , COST   N(10,2) ,;
   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , REP1    C(3))

*--End of lpCreateTmp.