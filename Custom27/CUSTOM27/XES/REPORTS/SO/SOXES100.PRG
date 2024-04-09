*:***************************************************************************
*: Program file  : SOXES100     (C102065)
*: Program desc. : Gross profit Based on bookings (Custom Program)
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER (SO)
*: Developer     : MOHAMED SHOKRY (MHM)
*! Date          : 27/11/2000
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SOXES100
*:***************************************************************************
STORE " " TO lcAvrgCost , tmpOrdLn
STORE .F. TO llAvrgCost
STORE  0  TO lnTrdDIs
lcAvrgCost = gfGetMemVar('M_COST_METH')
llAvrgCost = IIF(lcAvrgCost = 'A',.T.,.F.)
lcRpExp = lcRpExp + " .AND. ORDHDR.STATUS <> 'X' .AND. 'M'+ORDHDR->ACCOUNT=Customer.TYPE+Customer.ACCOUNT .AND. STYLE.Status<>'X'"
IF !EMPTY(lcRpRep1)
  lcRpExp = lcRpExp + ".AND.OrdHdr.REP1=lcRpRep1"
ENDIF
*-- Create a temp file.
tmpOrdLn = gfTempName()
lcOrdBy  = 'OrdLine.Style,'+IIF(lcSortBy='A','OrdLine.Account','OrdHdr.Season')
       
SELECT OrdLine.*;
       FROM OrdLine,OrdHdr,Customer,Style;
       WHERE OrdHdr.Order = OrdLine.Order .AND.;
             Style.Style= OrdLine.Style.AND. ;
             &lcRpExp;
       INTO DBF (gcWorkDir+tmpOrdLn);
       ORDER BY &lcOrdBy

IF _TALLY <> 0
  SELECT (tmpOrdLn)
  SET RELATION TO "O" + Order INTO OrdHdr,'M'+ACCOUNT INTO CUSTOMER,Order+STR(LineNo,6) INTO INVLINE ADDITIVE
  LOCATE
  R_WIDTH = 'W'

  DO lpInv971    &&Print layout.

  DO ENDREPORT      && End the report or display on screen.
  SET DEVICE TO SCREEN
ELSE
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
ENDIF



****************************************************************************
*! Name      : lpInv971.prg
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/27/2000
*! DESC      : Printing the report.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Report LayOut : <By Style>
*!0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*!GROSS PROFIT REPORT BASED ON BOOKINGS               ARIA SYSTEM TEST COMPANY                                           Date()   ~
*!Time()                                                GROSS PROFIT BY STYLE                                             PAGE#    1
*!************************************************************************************************************************************
*! STYLE     SE  PCS BKD  S.PRICE   GROSS AMT    TOT COST   STD.COST  DISCOUNT   DISC.%    NET AMT  A.NET PRICE     PROFIT   % PROFIT
*!====================================================================================================================================
*! °°°°°°°   °°   °°°°°°  °°°°.°°  °°°°°°°.°°  °°°°°°°.°°    °°°°.°°  °°°°°.°°   °°.°°% °°°°°°°.°°   °°°°°°°.°° °°°°°°°.°°   °°°.°° %
*! 1234567   12   123456  1234.12  1234567.12  1234567.12    1234.12  12345.12   12.12% 1234567.12   1234567.12 1234567.12   123.12 %
*! !!!!!!!   !!   999999  9999.99  9999999.99  9999999.99    9999.99  99999.99   99.99% 9999999.99   9999999.99 9999999.99   999.99 %
*! 1°°°°°°   11   16°°°°  24°°.°°  33°°°°°.°°  45°°°°°.°°    59°°.°°  67°°°.°°   79.°°% 86°°°°°.°°   99°°°°°.°° 110°°°°.°°   123°°° %
*!
*!SUBTOTAL   °°  15°°°°°          32°°°°°°.°° 44°°°°°°.°°            66°°°°.°°         85°°°°°°.°°             109°°°°°.°°   123°°° %
*!====================================================================================================================================
*!* GRAND TOTAL *°°°°°°°          °°°°°°°°.°° °°°°°°°°.°°            °°°°°°.°°         °°°°°°°°.°°             °°°°°°°°.°°   °°°.°° %
*!====================================================================================================================================
*!***************************************************************************
PROCEDURE lpInv971

SELECT (tmpOrdLn)
R_TITLE = 'GROSS PROFIT BY ' + IIF(lcSortBy= 'S','STYLE','ACCOUNT')

*-- Variables Initialization.
XTIME      = TIME()
PAGENO     = 0
lnBokQty   = 0
lnDisAmt   = 0
ROW        = 99
lcOldAct   = OrdHdr.Account
lcOldStyle = Style
lcOldSeson = OrdHdr.Season

DIMENSION laSub[4], laGrn[4]
STORE 0 TO laSub,laGrn

*-- Begin main report loop.
SCAN WHILE INKEY() <> 32
  WAIT WINDOW 'PRINTING - <Space Bar> TO ABORT' NOWAIT
  SET DEVICE TO PRINT
  IF ROW >= 53
    =lfPrnHdr()
  ENDIF

  DO lpSubTotals

  =SEEK(lcOldStyle,'Style')
  =SEEK(InvLine.Invoice,'InvHdr')

  IF lcOldStyle <> Style
    IF lcRpSamr  = 'D' 
      DO lpDetail
    ELSE
      DO lpSummary
    ENDIF

    lcOldStyle = Style
    lcOldSeson = OrdHdr.Season
    lnBokQty   = 0
    lnDisAmt   = 0
      
    *ROW = ROW + 1
    =SEEK(lcOldStyle,'Style')
    =SEEK(InvLine.Invoice,'InvHdr')

  ENDIF
  
  *--Calculate report fields.
  lnCurQty = TotQty + InvLine.TotQty
  lnBokQty = lnBokQty + lnCurQty
  
  *-- Get the discount on the price.
  lnPrcDis = Style.PriceA - IIF(OrdHdr.Status='C',InvLine.Price,Price)
  lnPrcDis = IIF(lnPrcDis > 0, lnPrcDis, 0) * lnCurQty
  
  *-- Get the merchandise discoutn.
  lnMerDis = IIF(OrdHdr.Status='C',InvHdr.DiscPcnt,OrdHdr.Disc)
  lnMerDis = (IIF(lnMerDis > 0, lnMerDis, 0) /100 ) * ;
             (IIF(OrdHdr.Status='C',InvLine.Price,Price) * lnCurQty)

  *-- Get the trade discount.
  DECLARE laArray[1,2]
  laArray[1,1] = 'NTERDISCR '     && Array to get the Division long name
  laArray[1,2] = 'lnTrdDIs'
  
  = gfRltFld(IIF(OrdHdr.Status='C',InvHdr.cTermCode,OrdHdr.cTermCode) , @laArray, 'CTERMCODE ').
  lnTradeDis = IIF(lnTrdDIs > 0,;
             ((IIF(OrdHdr.Status='C',InvLine.Price,Price) * lnCurQty) - lnMerDis) * lnTrdDIs / 100 ,0)
  
  lnDisAmt = lnDisAmt + lnPrcDis + lnMerDis + lnTradeDis
ENDSCAN
  
IF lcOldStyle <> Style
  IF lcRpSamr  = 'D' 
    DO lpDetail
  ELSE
    DO lpSummary
  ENDIF
ENDIF

DO lpSubTotals
*---- End main report loop ---

*-- GRAND TOTAL part.
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
@ ROW,123 SAY (laGrn[2]-laGrn[3]-laGrn[4])/(laGrn[2]-laGrn[4])*100    PICTURE '9999.99'
@ ROW,130 SAY '%'
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',132)

****************************************************************************
*! Nmae      : lpDetail
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/27/2000
*! DESC      : Printing the DETAILS
****************************************************************************
PROCEDURE lpDetail

@ ROW,001 SAY left(lcOldStyle,10)     && mhm comment
@ ROW,011 SAY lcOldSeson

@ ROW,016 SAY lnBokQty                         PICTURE '999999'
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
              /((lnBokQty*lnSelPrc)-lnDisAmt)*100        PICTURE '9999.99'
@ ROW,130 SAY '%'

ROW = ROW + 1

****************************************************************************
*! Name       : lpSummary
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/27/2000
*! DESC      : Calculate the summary information.
****************************************************************************
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

****************************************************************************
*! Name      : lpSubTotals
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/27/2000
*! DESC      : Printing the report SubTotals.
****************************************************************************
PROCEDURE lpSubTotals

IF (lcSortBy = 'A' .AND. lcOldAct   = Account ) .OR. ;
   (lcSortBy = 'S' .AND. lcOldSeson = OrdHdr.Season)
  RETURN
ENDIF

IF lcRpSamr = 'D' .AND. lcSortBy = 'A' .AND. lcOldAct <> ACCOUNT
  ROW = ROW + 1
  @ ROW,001 SAY '*** ACCOUNT  ***  ' + lcOldAct
  lcOldAct = ACCOUNT
  ROW = ROW + 1
ENDIF

lcSubTitl = IIF(lcSortBy='A',lcOldAct,lcOldSeson)
IF lcRpSamr='D'
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE('-',132)
  ROW = ROW + 1
  @ ROW,000 SAY 'SUBTOTAL   '+IIF(lcSortBy='A','',SUBSTR(lcSubTitl,1,2))
ELSE
  IF llRpGrand = .F.
    @ ROW,001 SAY lcSubTitl
  ENDIF
ENDIF

IF llRpGrand  = .F.
  @ ROW,015 SAY laSub[1]             PICTURE '9999999'
  @ ROW,032 SAY laSub[2]             PICTURE '99999999.99'
  @ ROW,044 SAY laSub[3]             PICTURE '99999999.99'
  @ ROW,067 SAY laSub[4]             PICTURE '999999.99'
  @ ROW,085 SAY (laSub[2]-laSub[4])              PICTURE '99999999.99'
  @ ROW,109 SAY (laSub[2]-laSub[3]-laSub[4])     PICTURE '99999999.99'
  @ ROW,123 SAY (laSub[2]-laSub[3]-laSub[4])/(laSub[2]-laSub[4])*100  PICTURE '9999.99'
  @ ROW,130 SAY '%'
ENDIF

IF lcRpSamr='D'
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE('-',132)
  ROW = ROW + 1  
ENDIF 

IF llRpGrand = .F.
  ROW = ROW + 1
ENDIF
laSub = 0

*:************************************************************************
*: Name      : lfPrnHdr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/27/2000
*: Purpose   : Print the header of the report.
*:*************************************************************
FUNCTION lfPrnHdr

PAGENO = PAGENO + 1
DO RPT_HDR WITH 'INV970'+lcRpSamr, lcRpTitl, R_WIDTH
@ 05,00 SAY ' STYLE     SE  PCS BKD  S.PRICE   GROSS AMT    TOT COST   STD.COST  DISCOUNT   DISC.%    NET AMT  A.NET PRICE     PROFIT   % PROFIT'
@ 06,00 SAY REPLICATE('=',132)
ROW = 07
*!*************************************************************
*! Name      : lfvRepCode
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/27/2000
*! Purpose   : Validate Primary Sales Rep. in SALESREP file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRepCode()
*!*************************************************************
FUNCTION lfvRepCode
PRIVATE lcVar , lcObj , laTemp
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SALESREP'))
  SELECT SALESREP
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "REPCODE   :R :H= 'Code' , "   +;
               "NAME      :R :H= 'Name' ,"    +;
               "cAddress6 :R :H= 'Country' ," +;
               "PHONE     :R :H= 'Phone' ,"   +;
               "BALANCE   :R :H= 'Balance' "
  
  lcFile_Ttl = "Sales Representative ..."
  = gfBrows('','REPCODE','laTemp')
    
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvRepCode.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/27/2000
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
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvAcc
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/27/2000
*! Purpose   : Validate account in CUSTOMER file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvAcc()
*!*************************************************************
FUNCTION lfvAcc
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK('M' + lcObj , 'CUSTOMER'))
  SELECT CUSTOMER
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "Account   :R :H= 'Account' , "   +;
               "btname    :R :H= 'Name' ,"    +;
               "country   :R :H= 'Country' ," +;
               "buyer     :R :H= 'Buyer' ," +;
               "Salesrep  :R :H= 'Salesrep' ," +;
               "caddress4 :R :H= 'State' ," +;
               "NetBal    :R :H= 'Balance' ," +;
               "PHONE1    :R :H= 'Phone' "   

  
  lcFile_Ttl = "Account ..."
  = gfBrows(['M'],'Account','laTemp')
    
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvRepCode.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/28/2000
*! Purpose   : Validate Style in STYLE file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStyle()
*!*************************************************************
FUNCTION lfvStyle
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'STYLE'))
  SELECT STYLE
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "Style     :R :H= 'Style' , "   +;
               "Season    :R :H= 'Season' ,"    +;
               "CDivision :R :H= 'Division' ," +;
               "Desc      :R :H= 'Desc.' ," +;
               "Desc1     :R :H= 'Long Desc.' ," +;
               "Pattern   :R :H= 'Pattern ' ," +;
               "Fabric    :R :H= 'Fabric ' ," +;
               "PriceA    :R :H= 'PriceA ' ," +;
               "priceB    :R :H= 'PriceB ' ," +;
               "PriceC    :R :H= 'PriceC ' ," +;
               "Totord    :R :H= 'Order ' ," +;
               "totstk    :R :H= 'Stoke ' ," +;
               "Pattern   :R :H= 'Pattern ' ," +;
               "OTS=(TOTWIP+TOTSTK-TOTORD)   :R :H= 'O.T.S. ' ," +;
               "TotWip    :R :H= 'WIP'"   
  
  lcFile_Ttl = "Style ..."
  = gfBrows('','Style','laTemp')
    
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvRepCode.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Shokry (MHM)
*! Date      : 28/11/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :  lfOGShowGet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
PRIVATE lnPos

lnPos = ASCAN(laOgObjType,'llRpGrand')
IF lnPos > 0
  lnPos= ASUBSCRIPT(laOgObjType,lnPos,1)
  laOGObjCnt[lnPos] = lcRpSamr = "S"
  = lfOGShowGet('llRpGrand')
ENDIF
*-- end of lfwRepWhen.
*!*************************************************************
*! Name      : lfvType
*! Developer : Mohamed Shokry (MHM)
*! Date      : 28/11/2000
*! Purpose   : Refresh  option Grid in case of Summary
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvType()
*!*************************************************************
*
FUNCTION lfvType
*clear read report variables in order to refresh the screen 
CLEAR READ 

*-- end of lfvType.


