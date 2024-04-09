*!*************************************************************
*! Name : AROLS14.PRG (Converted from 26 to 27 for OLSEN)
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 08/03/1999
*! Desc.: Print "Wholesale Sales report".
*! Refer to : (C101590)
*! ***********************************************************

*C101386 (Begin) If no records selected.
SELECT InvHdr
GO TOP
LOCATE FOR &lcRpExp
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
*--Open SycComp
=gfOpenFile(gcSysHome+'SycComp',gcSysHome+'cComp_Id','SH')
=SEEK(gcAct_Comp)
lcCompName = cCom_Name
SELECT InvHdr
STORE 0 TO lnGrGros ,lnGrDisc ,lnGrPst ,lnGrGst ,lnGrSpCh ,lnGrCstSal ,lnGrAmInv
STORE 0 TO lnRGros  ,lnRDisc  ,lnRPst  ,lnRGst  ,lnRSpCh  ,lnRAmInv  ,lnRCstSal
*-- Get the date range
lnDatePos = ASCAN('laOGFxFlt','INVHDR.INVDATE')
*-- Get the row.
lnDatePos = ASUBSCRIPT('laOGFxFlt',lnDatePos,1)
lnSndSPos = AT('|',laOGFxFlt[lnDatePos,6])
*-- Date range
ldFrstDate = IIF(lnSndSPos <>1,CTOD(SUBSTR(laOGFxFlt[lnDatePos ,6],1,10)),{})
ldSndDate  = CTOD(SUBSTR(laOGFxFlt[lnDatePos ,6],lnSndSPos+1,10))
*--Prepare the temp file
lcInvHtmp=gfTempName()
COPY REST TO (gcWorkDir+lcInvHtmp)  FOR &lcRpExp
=gfOpenFile(gcWorkDir+lcInvHtmp,'','EX')
INDEX ON REP1+SEASON TAG lcInvHtmp
*--Start printing
GO TOP
R_Width   = 'W'
lcTitle   = SPACE(40)
Row       = 99
PageNo    = 0
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
R_Title   = 'Wholesale Sales report'
*--Prevent the empty rep1 records from printing.
DELETE REST FOR EMPTY(Rep1)
GO TOP
lcRep     = REP1
lcSea     = SEASON
DO WHILE INKEY() <>32
  WAIT WINDOW 'Report printing press <SPACE BAR> to abort' NOWAIT
  IF EOF()
    EXIT
  ENDIF  
  SELECT (lcInvHtmp)
  IF ROW >= 55
    ROW = 0
    PAGENO = PAGENO+1
*    @ 03,000 SAY ALLTRIM(gcAct_Comp)
    @ 03,000 SAY ALLTRIM(lcCompName)
    @ 03,85 SAY 'Date: '
    @ 03,95 SAY DATE()
    @ 04,00 SAY 'Wholesale Sales'
    @ 04,85 SAY 'Time: '
    @ 04,95 SAY TIME()
    @ 05,00 SAY 'For the period'
    @ 05,15 SAY ldFrstDate
    @ 05,25 SAY 'TO'
    @ 05,28 SAY ldSndDate
    @ 05,85 SAY 'Page#'
    @ 05,93 SAY PAGENO
    @ 07,00 SAY '                  Gross                                              Shipping        Amount     Cost of'
    @ 08,00 SAY '                  Sales       Discounts         PST         GST       Charges      Invoiced       Sales'
    @ 09,00 SAY '                ---------------------------------------------------------------------------------------'
    ROW = 10
  ENDIF

  @ ROW,00 SAY 'Agent # '+REP1
  ROW=ROW+1
  DO WHILE lcRep = Rep1
    STORE 0 TO lnGross ,lnDisc ,lnPst ,lnGst ,lnShpChr ,lnCstSal ,lnAmtInv
    lcSea = Season
    SCAN WHILE lcSea = Season  AND lcRep = Rep1
      lnGross  = lnGross  + ShipAmt
      lnDisc   = lnDisc   + Discount
      lnPst    = lnPst    + nPstAmt
      lnGst    = lnGst    + Tax_Amt
      lnShpChr = lnShpChr + (Cod+Freight+Insur)
      lnAmtInv = lnAmtInv + TotalChg
      =SEEK(Invoice,'InvLine')
      SELECT InvLine
      SUM REST (Cost*TotQty) TO lnCost WHILE &lcInvHtmp..INVOICE = Invoice
      SELECT (lcInvHtmp)         
      lnCstSal = lnCstSal + lnCost
      lnCost   = 0  
    ENDSCAN
    @ ROW,00 SAY 'SEASON: ' +lcSea
    @ ROW,12 SAY lnGross       PICTURE '99999999.99'
    @ ROW,27 SAY lnDisc        PICTURE '999999999.99'
    @ ROW,40 SAY lnPst         PICTURE '99999999.99'
    @ ROW,52 SAY lnGst         PICTURE '99999999.99'
    @ ROW,66 SAY lnShpChr      PICTURE '99999999.99'
    @ ROW,78 SAY lnAmtInv      PICTURE '9999999999.99'
    @ ROW,92 SAY lnCstSal      PICTURE '99999999.99'
    ROW=ROW+1
    lnRGros   = lnRGros   + lnGross
    lnRDisc   = lnRDisc   + lnDisc
    lnRPst    = lnRPst    + lnPst
    lnRGst    = lnRGst    + lnGst
    lnRSpCh   = lnRSpCh   + lnShpChr
    lnRAmInv  = lnRAmInv  + lnAmtInv
    lnRCstSal = lnRCstSal + lnCstSal
  ENDDO
  IF lcRep <> Rep1
    DO lpSubRep
    lcRep = Rep1
  ENDIF
ENDDO

ROW=ROW+1
@ ROW,000 SAY REPLICATE('*',103)
ROW=ROW+1

@ ROW,00 SAY 'Grand Total'
@ ROW,12 SAY lnGrGros      PICTURE '99999999.99'
@ ROW,27 SAY lnGrDisc      PICTURE '999999999.99'
@ ROW,40 SAY lnGrPst       PICTURE '99999999.99'
@ ROW,52 SAY lnGrGst       PICTURE '99999999.99'
@ ROW,66 SAY lnGrSpCh      PICTURE '99999999.99'
@ ROW,78 SAY lnGrAmInv      PICTURE '9999999999.99'
@ ROW,92 SAY lnGrCstSal    PICTURE '99999999.99'
ROW=ROW+1
@ ROW,000 SAY REPLICATE('*',103)
ROW=ROW+1
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lpSubRep
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/03/1999
*! Purpose   : PRINT THE SUBTOTAL.
*!*************************************************************
*! Calls     : Procedures : 
*!*************************************************************
*! Passed Parameters  : 
*!         Variables  :
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
***************************************************************
PROCEDURE lpSubRep

ROW=ROW+1
@ ROW,000 SAY REPLICATE('=',103)
ROW=ROW+1
@ ROW,00 SAY 'SubTotal For Agent # '+ lcRep
ROW=ROW+2

@ ROW,12 SAY lnRGros      PICTURE '99999999.99'
@ ROW,27 SAY lnRDisc      PICTURE '999999999.99'
@ ROW,40 SAY lnRPst       PICTURE '99999999.99'
@ ROW,52 SAY lnRGst       PICTURE '99999999.99'
@ ROW,66 SAY lnRSpCh      PICTURE '99999999.99'
@ ROW,78 SAY lnRAmInv     PICTURE '9999999999.99'
@ ROW,92 SAY lnRCstSal    PICTURE '99999999.99'

*-- Added to grand total--*
lnGrGros   = lnGrGros   + lnRGros
lnGrDisc   = lnGrDisc   + lnRDisc
lnGrPst    = lnGrPst    + lnRPst
lnGrGst    = lnGrGst    + lnRGst
lnGrSpCh   = lnGrSpCh   + lnRSpCh
lnGrCstSal = lnGrCstSal + lnRCstSal
lnGrAmInv  = lnGrAmInv  + lnRAmInv

STORE 0 TO lnRGros  ,lnRDisc  ,lnRPst  ,lnRGst  ,lnRSpCh  ,lnRAmInv  ,lnRCstSal
ROW=ROW+1
@ ROW,000 SAY REPLICATE('=',103)
ROW=ROW+1

*!*************************************************************
*! Name      : lfvSalesRep
*! Developer : AAMER (AHM)
*! Date      : 08/03/1999
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
&lcRepCode = IIF(EMPTY(&lcRepCode),lcOldVal,&lcRepCode)

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/03/1999
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))



****************************************************************************
* FUNC: lfwRepWhen
* DESC: To valid the OG WHEN function.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/03/1999
*! Refer to : (C101590)
****************************************************************************
FUNCTION lfwRepWhen

*-- Initilize the date range with system date.
lnDatePos = ASCAN(laOGFxFlt,"INVHDR.INVDATE")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  ENDIF  
ENDIF
