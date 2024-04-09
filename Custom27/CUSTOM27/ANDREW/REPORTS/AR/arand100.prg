*:************************************************************************
*: Program file  : ARAND100   (Copied form AND100.PRG FOR Andrew Marc )
*: Program desc. : GROSS PROFIT REPORT CUSTOMIZED FOR Andrew Marc 
*:               : Converted from Aria26 to Aria27.
*:         System: ARIA APPAREL SERIES
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           Date: 12/12/98
*:      Refer To : (C101386)
*:************************************************************************
*: Calls         : 
*:    Procedures : 
*:                 lpAnd110
*:                 lpSummary
*:                 lpDetail
*:                 lpSubTotals
*:                 lpRetNOInv
*:                 lpPrnHdr
*:                 lpCrAllo
*:*************************************************************************

*C101386 (Begin) Add the invoice status to the filter
lcRpExp = lcRpExp +" AND InvHdr.Status<>'V'"
*C101386 (End)
*C101386 (Begin) Initilize variables and get grid options separatedly.
STORE '' TO lcFrstSty,lcSndSty,ldFrstDate,ldSndDate,lcFrstAcct
STORE '' TO lcDivStr,lcClassStr,lcGrpStr,lcSndAcct,lcSeaStr,lcRepStr 
*--Get the grid filter options
=lfCutFilr()
*--Prepare the retline filter
IF lcRpIncRet = 'Y'
  lcRFilter= ".T." 
  IF !EMPTY(ldSndDate)
    lcRFilter = lcRFilter+ ".AND. BETWEEN(CrDate,ldFrstDate,ldSndDate)"
  ENDIF
  *C101386 (Begin) If the first account is empty and the second is not the 26's program 
  *C101386         ignored entering the whole account in the filter, so we have fixed it here. 
  IF !EMPTY(lcFrstAcct) OR !EMPTY(lcSndAcct)
    lcRFilter = lcRFilter + ".AND. BETWEEN(Account,lcFrstAcct,lcSndAcct)"
  ENDIF
  IF !EMPTY(lcSeaStr)
    lcRFilter = lcRFilter + ".AND. Style.Season=lcSeaStr"
  ENDIF
  IF ! EMPTY(lcRepStr)
    lcRFilter = lcRFilter + ".AND.RetHdr.SalesRep1=lcRepStr"
  ENDIF
  IF !EMPTY(lcDivStr)
    lcRFilter = lcRFilter + ".AND. RetHdr.cDivision = lcDivStr"
  ENDIF
  IF !EMPTY(ALLTRIM(lcClassStr)) 
    lcRFilter = lcRFilter + ".AND.TRIM(Customer.Class) $ lcClassStr" 
  ENDIF
  IF !EMPTY(ALLTRIM(lcGrpStr)) 
    lcRFilter = lcRFilter + ".AND.TRIM(Style.cStyGroup) $ lcTGrp" 
  ENDIF
ENDIF
*C101386 (End)
lcSFilt=' '
*C101386 (Begin)
*-- The temporary file name variable (lcInvLTemp) has been created from the report generator
*-- to be created each time we enter the option grid not each time we press
*-- PREVIEW OR RUN button.
*-- Build the invoice records.
*--If we press PREVIEW or RUN button, Zap the temp file.
IF USED(lcInvLTmp)
  SELECT (lcInvLTmp)
  IF RECCOUNT() >0
    ZAP
  ENDIF  
ENDIF
*C101386 (End)

SELECT InvLine
IF ! EMPTY(lcFrstSty)
  lcSetNear=SET('NEAR')
  SET NEAR ON
  IF !(SEEK(lcFrstSty))
    *-- Message : No invoices were found for the selected styles.
    =gfModalGen('TRM40149B40011','ALERT')
    SET NEAR &lcSetNear
    RETURN
  ENDIF
  SET NEAR &lcSetNear
  lcSFilt='REST WHILE SUBSTR(Style,1,lnMajLen)<=lcSndSty '
ELSE
  IF !EMPTY(lcSndSty)  
    lcSFilt='REST WHILE SUBSTR(Style,1,lnMajLen)<=lcSndSty  '
  ENDIF
ENDIF
SCAN &lcSFilt FOR &lcRpExp
  WAIT WINDOW 'Collecting invoice # : '+Invoice NOWAIT
  *C101386 (Begin) Omit the color.
  *SCATTER FIELDS Invoice,Account,Style,Color,LineNo,Price,TotQty,Cost TO laInvRec
  SCATTER FIELDS Invoice,Account,Style,LineNo,Price,TotQty,Cost TO laInvRec
  *C101386 (End)
  SELECT (lcInvLTmp)
  APPEND BLANK
  *C101386 (Begin) Omit the color and change group to cGroup and style.group to style.cstyGroup.
  *GATHER FROM laInvRec FIELDS Invoice,Account,Style,Color,LineNo,Price,TotQty,Cost
  GATHER FROM laInvRec FIELDS Invoice,Account,Style,LineNo,Price,TotQty,Cost
  REPLACE Type      WITH 'INV',;
          Code      WITH InvLine.Invoice    ,;
          Date      WITH InvLine.InvDate    ,;             
          Name      WITH Customer.BTName    ,;
          cGroup    WITH Style.cStyGroup    ,;
          Class     WITH Customer.Class     ,;
          DiscPcnt  WITH InvHdr.DiscPcnt    ,;
          Trde_Disc WITH InvHdr.Trde_Disc          
  *C101386 (End)
ENDSCAN
SELECT (lcInvLTmp)
IF lcRpIncRet = 'Y'
  *-- Build the returns records.
  INDEX ON Invoice TAG (lcInvLTmp) 
  SELECT RetLine
  IF ! EMPTY(lcFrstSty)
    lcOldNear = SET("NEAR")
    SET NEAR ON    
    =SEEK(lcFrstSty)
    SET NEAR &lcOldNear
  ENDIF
  
  SCAN &lcSFilt FOR &lcRFilter
    WAIT WINDOW 'Collecting credit memo # : '+Crmemo NOWAIT  
    lcInvoice=Invoice
    *C101386 (Begin) Omit the color.
    *SCATTER FIELDS Account,Style,Color,LineNo,Price,TotQty,Cost TO laInvRec
    SCATTER FIELDS Account,Style,LineNo,Price,TotQty,Cost TO laInvRec
    *C101386 (End)
    SELECT &lcInvLTmp 
    IF ! SEEK(lcInvoice)
      lcInvoice='ZZZZZZ'      && Returns without invoice or for invoices
                              && not in date range are holding the ZZZZZZ.
    ENDIF 
    APPEND BLANK
    *C101386 (Begin) Omit the color.
    *GATHER FROM laInvRec FIELDS Account,Style,Color,LineNo,Price,TotQty,Cost   
    GATHER FROM laInvRec FIELDS Account,Style,LineNo,Price,TotQty,Cost   
    *C101386 (End)
    REPLACE Type WITH 'RET'           ,;
            Invoice WITH lcInvoice    ,;
            Code  WITH RetLine.CrMemo ,;
            Date  WITH RetLine.CrDate ,;             
            Name  WITH Customer.BTName,;
            Class WITH Customer.Class
  ENDSCAN
ENDIF

SELECT (lcInvLTmp)
*C101386 (Begin) If no records selected.
GO TOP
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
*C101386 (End)

*--Clear relations
SELECT InvLine
SET RELATION TO
SELECT RetLine
SET RELATION TO

SELECT (lcInvLTmp)
DO CASE
  CASE lcRpSortBy ='A'
    *C101386 (Begin) Omit the color.
    *INDEX ON Account+Invoice+Style+Color+Type+Code+STR(RECNO(),7) TAG &lcInvLTmp
    INDEX ON Account+Invoice+Style+Type+Code TAG (lcInvLTmp)
    *C101386 (End)
    lcHBreak='ACCOUNT'
  CASE lcRpSortBy = 'I'
    *C101386 (Begin) Omit the color.
    *INDEX ON Invoice+Style+Color+Type+Code+STR(RECNO(),7) TAG &lcInvLTmp
    INDEX ON Invoice+Style+Type+Code TAG (lcInvLTmp)
    *C101386 (End)
    lcHBreak='INVOICE'
  CASE lcRpSortBy = 'S'
    *C101386 (Begin) Omit the color.
    *INDEX ON Style+Invoice+Color+Type+Code+STR(RECNO(),7) TAG &lcInvLTmp    
    *lcHBreak = 'STYLE'
    INDEX ON Style+Invoice+Type+Code TAG (lcInvLTmp)
    lcHBreak = 'SUBSTR(Style,1,lnMajLen)'
    *C101386 (End)
  CASE lcRpSortBy ='C'
    *C101386 (Begin) Omit the color.
    *INDEX ON Class+Invoice+Account+Style+Color+Type+Code+STR(RECNO(),7) TAG &lcInvLTmp
    INDEX ON Class+Invoice+Account+Style+Type+Code TAG (lcInvLTmp)
    *C101386 (End)
    lcHBreak='CLASS'
  CASE lcRpSortBy = 'G'
    *C101386 (Begin) Omit the color.
    *INDEX ON Group+Invoice+Account+Style+Color+Type+Code+STR(RECNO(),7) TAG &lcInvLTmp
    INDEX ON Group+Invoice+Account+Style+Type+Code TAG (lcInvLTmp)
    *C101386 (End)
    lcHBreak='GROUP'
ENDCASE

R_WIDTH = 'W'
*-- Print layout.
DO lpAnd110

DO ENDREPORT

RETURN
*-------------------------
*   End  AND100.PRG
*-------------------------




*!*************************************************************
*! Name      : lpAnd110
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/20/1998
*! Purpose   : Printing the report.
*!*************************************************************
*! Example            : DO lpAnd110
*!*************************************************************

PROCEDURE lpAnd110

***  <SUMMARY>
*ACCOUNT ...... NAME .....     INVOICE STYLE        COLOR    PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %
*12345   XXXXXXXXXXXXXXXXX                                   123456             999999.99  999999.99
*                                      123456789012          123456             999999.99  999999.99
*                               123456                       123456             999999.99  999999.99

***  <DETAILS> without Returns.
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*ACCOUNT ...... NAME ..... TYP INV/CRD STYLE        COLOR    PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %
*12345   XXXXXXXXXXXXXXXXX INV 123456  123456789012 123456   123456  99999.99   999999.99  999999.99
*                          RET 123456  123456789012 123456  -123456  99999.99             -999999.99
*                          RET 123456  123456789012 123456  -123456  99999.99             -999999.99
*                                                              NET AMOUNT AFTER RETURNS  : 999999.99  999999.99  999999.99  999.99 %
*RETURNS WITHOUT INVOICES :
*12345   XXXXXXXXXXXXXXXXX RET 123456  123456789012 123456  -123456  99999.99             -999999.99
*12345   XXXXXXXXXXXXXXXXX RET 123456  123456789012 123456  -123456  99999.99             -999999.99
*

***
SET DEVICE TO PRINT

SELECT (lcInvLTmp)
GO TOP

ROW     = 99
PAGENO  = 0
R_TITLE = 'GROSS PROFIT BY '+lcHBreak
BREAK   = (lcHBreak)

*-- Variables Initialization.
STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5        &&Subtotals
STORE 0 TO XGTOT1,XGTOT2,XGTOT3,XGTOT4,XGTOT5   &&Grandtotlals
STORE 0 TO lnGrAll, lnGrChg, lnGrCrd
XTITLE = SPACE(1)
XTIME  = TIME()
PAGENO = 0
ROW    = 99
XAcName= ' '

CLEAR TYPEAHEAD
SET DEVICE TO PRINT
*-- Begin main report loop
DO WHILE INKEY() <> 32
   WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
   IF ROW >= 53
     PAGENO = PAGENO+1
     DO RPT_HDR WITH 'AND100'+lcRpFormat,lcRpTitle,R_WIDTH    
     *C101386 (Begin) Replace the style color with the style major title.
     *IF lcRpFormat='S' .OR. ! llInclRet  
     *  @ 05,00 SAY 'ACCOUNT ...... NAME ......... INVOICE STYLE        COLOR    PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %'
     *ELSE
     *  @ 05,00 SAY 'ACCOUNT ...... NAME ..... TYP INV/CRD STYLE        COLOR    PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %'
     *ENDIF
     IF lcRpInvD='S' .OR. lcRpIncRet <> 'Y'
       @ 05,00 SAY 'ACCOUNT ...... NAME ......... INVOICE '+lcMajTitle+'   PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %'
     ELSE
       @ 05,00 SAY 'ACCOUNT ...... NAME ..... TYP INV/CRD '+lcMajTitle+'   PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %'
     ENDIF
     *C101386 (End)
     @ 06,00 SAY REPLICATE('=',132)
     ROW = 07
   ENDIF  

   DO lpSubTotals   &&SubTotals

   STORE 0 TO XRTOT1,XRTOT2,XRTOT3,XRTOT4,XRTOT5    &&RetSubTot

   IF EOF() .OR. (lcRpIncRet = 'Y' .AND. lcRpSortBy='I' .AND. Invoice='ZZZZZZ')
     EXIT  
   ENDIF

   IF ROW >=53
     ROW = 99
     LOOP
   ENDIF

   IF lcRpIncRet = 'Y' .AND. lcRpFormat='D' .AND. lcRpSortBy<>'I' .AND. Invoice='ZZZZZZ'
     *@ ROW,00 SAY 'RETURNS WITHOUT INVOICES :'  
   ELSE
     IF lcRpFormat = 'D'
       DO lpDetail    
     ELSE
       DO lpSummary
     ENDIF
   ENDIF

   *--Print returns without invoice.
   IF lcRpIncRet = 'Y' .AND. lcRpSortBy<>'I' .AND. Invoice='ZZZZZZ' 
     DO lpRetNOInv
   ENDIF

   xTot1 = xTot1 + xRTot1 
   xTot2 = xTot2 + xRTot2 
   xTot3 = xTot3 + xRTot3 
   xTot4 = xTot4 + xRTot4 

ENDDO

*--Print returns without invoice case of invoice sort.
IF lcRpIncRet = 'Y' .AND. lcRpSortBy='I' .AND. Invoice='ZZZZZZ'
  DO lpRetNOInv
  xGTot1 = xGTot1 + xRTot1 
  xGTot2 = xGTot2 + xRTot2  
  xGTot3 = xGTot3 + xRTot3 
  xGTot4 = xGTot4 + xRTot4 
ENDIF
*---- End main report loop ---
IF lcRpSortBy = 'A'
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE('=',132)
  ROW = ROW + 1 
  @ ROW,000 SAY '* GRAND TOTAL FOR CREDITS & ALLOWANCES *'
  @ ROW,060 SAY lnGrAll + lnGrChg + lnGrCrd PICTURE '999999.99'
ENDIF  
*-- GRAND TOTAL part.
ROW=ROW+1
@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW+1
@ ROW,000 SAY '* GRAND TOTAL *'
@ ROW,058 SAY XGTOT1  PICTURE '99999999'
@ ROW,077 SAY XGTOT2  PICTURE '99999999.99'
@ ROW,088 SAY XGTOT3  PICTURE '99999999.99'
@ ROW,099 SAY XGTOT4  PICTURE '99999999.99'
XGTOT5=XGTOT3-XGTOT4 
@ ROW,110 SAY XGTOT5  PICTURE '99999999.99'
XGTOT6=ROUND(IIF(XGTOT3<>0,(XGTOT5/XGTOT3)*100,0),2)
XGTOT6=IIF((XGTOT5<0 .AND. XGTOT3<0),-XGTOT6,XGTOT6)
@ ROW,123 SAY XGTOT6 PICTURE '99999.99 %'
ROW = ROW+1
@ ROW,00 SAY REPLICATE('=',132)


*!*************************************************************
*! Name      : lpDetail
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/20/1998
*! Purpose   : Printing the detail report.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : DO lpDetail
*!*************************************************************

PROCEDURE lpDetail

llRetExst=.F.      &&Flag for Existing of the returns.
DO WHILE .T.
  *--Calculate report fields.
  lnQty    = IIF(Type='INV',TotQty,-(TotQty))
  lnGross  = lnQty*Price
  lnNet    = lnGross * (1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))
  lnCost   = lnQty * Cost 
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

  IF ! llRetExst
    @ ROW,000 SAY Account
    @ ROW,008 SAY SUBSTR(Name,1,IIF(lcRpIncRet = 'Y',17,21))
  ENDIF
  IF lcRpIncRet = 'Y'
    @ ROW,026 SAY Type
  ENDIF
  @ ROW,030 SAY Code
  @ ROW,038 SAY Style
  @ ROW,060 SAY lnQty PICTURE '999999'
  @ ROW,068 SAY Price PICTURE '99999.99'
  
  IF lcRpSubtRt = 'Y'
    @ ROW,079 SAY lnGross PICTURE '999999.99'
  ELSE
    IF Type='INV'
      @ ROW,079 SAY lnGross PICTURE '999999.99'
    ENDIF   
  ENDIF
  
  @ ROW,090 SAY lnNet PICTURE '999999.99'

  @ ROW,101 SAY lnCost          PICTURE '999999.99'   
  @ ROW,112 SAY lnNet - lnCost  PICTURE '999999.99'
  lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
  lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)
  @ ROW,123 SAY lnPercnt PICTURE '99999.99 %'

  XRTOT1 = XRTOT1 + lnQty
  
  IF lcRpSubtRt = 'Y'
    XRTOT2 = XRTOT2 + lnGross       
  ELSE
    IF Type='INV'     
      XRTOT2 = XRTOT2 + lnGross       
    ENDIF
  ENDIF

  XRTOT3 = XRTOT3 + lnNet
  XRTOT4 = XRTOT4 + lnCost
 
  XAcName=Name
  BREAK  =&lcHBreak
  SKIP

  IF lcRpIncRet = 'Y'
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
  @ ROW,062 SAY 'NET AMOUNT AFTER RETURNS  :'
  @ ROW,090 SAY XRTOT3   PICTURE '999999.99'

  @ ROW,101 SAY XRTOT4   PICTURE '999999.99'
  XRTOT5=XRTOT3-XRTOT4 
  @ ROW,112 SAY XRTOT5  PICTURE '999999.99'
  XRTOT6=ROUND(IIF(XRTOT3<>0,(XRTOT5/XRTOT3)*100,0),2)
  XRTOT6=IIF((XRTOT5<0 .AND. XRTOT3<0),-XRTOT6,XRTOT6)
 @ ROW,123 SAY XRTOT6 PICTURE '99999.99 %'
ENDIF
ROW=ROW+1

*!*************************************************************
*! Name      : lpSummary
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/20/1998
*! Purpose   : Printing the summary report.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : DO lpSummary
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
  
  IF lcRpSubtRt = 'Y'
    XRTOT2 = XRTOT2 + lnGross       
  ELSE
    IF Type='INV'     
      XRTOT2 = XRTOT2 + lnGross       
    ENDIF
  ENDIF

  XRTOT3 = XRTOT3 + lnNet
  XRTOT4 = XRTOT4 + lnCost

  XAcName=Name
  BREAK  =&lcHBreak
  SKIP

  IF lcRpIncRet = 'Y'
    IF ! (Type='RET' .AND. Invoice<>'ZZZZZZ')
      EXIT
    ENDIF
  ELSE
    EXIT 
  ENDIF
  
ENDDO

*!*************************************************************
*! Name      : lpSubTotals
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/20/1998
*! Purpose   : Printing the report subtotals.
*!*************************************************************
*! Calls     : lpCrAllo
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : DO lpSubTotals
*!*************************************************************

PROCEDURE lpSubTotals

*-- Begin Subtotals Loop --
DO WHILE !EMPTY(lcHBreak)

  IF &lcHBreak = BREAK
     EXIT
  ENDIF
  IF lcRpSortBy = 'A'
    DO lpCrAllo
    ROW = ROW + 1
  ENDIF
  DO CASE
    CASE lcRpSortBy='A'
      lcSubTitl=BREAK+'   '+IIF(lcRpFormat='D',SUBSTR(XAcName,1,26),XAcName)
    CASE lcRpSortBy='C'
      lcSubTitl = Break + '   ' + IIF(SEEK('L'+Break,'Code'),Code.cData,SPACE(15))
      SELECT (lcInvLTmp)
    CASE lcRpSortBy='G'
      lcSubTitl = Break + '   ' + IIF(SEEK('G'+Break,'Code'),Code.cData,SPACE(15))
      SELECT (lcInvLTmp)
    OTHER
      lcSubTitl=BREAK
  ENDCASE  

  IF lcRpFormat='D'
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW+1
    @ ROW,000 SAY '* SUB TOTAL *  '+lcSubTitl
  ELSE
    IF lcRpPrtGnd='N'
      @ ROW,IIF(lcRpSortBy='I',38,IIF(lcRpSortBy='S',46,0)) SAY ;
            IIF(lcRpSortBy='C',' CLASS : ',IIF(lcRpSortBy='G',' GROUP : ',''))+lcSubTitl
    ENDIF
  ENDIF

  IF lcRpPrtGnd='N'
    @ ROW,058 SAY XTOT1  PICTURE '99999999'
    @ ROW,077 SAY XTOT2  PICTURE '99999999.99'
    @ ROW,088 SAY XTOT3  PICTURE '99999999.99'
    @ ROW,099 SAY XTOT4  PICTURE '99999999.99'
    XTOT5=XTOT3-XTOT4
    @ ROW,110 SAY XTOT5  PICTURE '99999999.99'
    XTOT6=ROUND(IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0),2)
    XTOT6=IIF((XTOT5<0 .AND. XTOT3<0),-XTOT6,XTOT6)
    @ ROW,123 SAY XTOT6 PICTURE '99999.99 %'
  ENDIF

  IF lcRpFormat='D'
    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('-',132)
  ENDIF 

  IF lcRpPrtGnd='N'
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/20/1998
*! Purpose   : Print returns without invoice.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : DO lpRetNOInv
*!*************************************************************

PROCEDURE lpRetNOInv

IF lcRpFormat='D' .AND. IIF(lcRpSortBy<>'I',&lcHBreak=BREAK,.T.)
  ROW = ROW + 1
  @ ROW,00 SAY 'RETURNS WITHOUT INVOICES :'       
ENDIF

DO WHILE Invoice='ZZZZZZ' .AND. IIF(lcRpSortBy<>'I',&lcHBreak=BREAK,.T.)

  lnQty    = IIF(Type='INV',TotQty,-(TotQty))
  lnGross  = lnQty*Price
  lnNet    = lnGross * (1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))
  lnCost   = lnQty * Cost 
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

  IF lcRpFormat='D'
    ROW=ROW+1
    @ ROW,000 SAY Account
    @ ROW,008 SAY SUBSTR(Name,1,17)
    @ ROW,026 SAY Type
    @ ROW,030 SAY Code
    @ ROW,038 SAY Style    
    @ ROW,060 SAY lnQty PICTURE '999999'
    @ ROW,068 SAY Price PICTURE '99999.99'
    IF lcRpSubtRt = 'Y'
      @ ROW,079 SAY lnGross PICTURE '999999.99'   &&B800179,1 MFM 09/20/95 Added.
    ELSE
      IF Type='INV'     
        @ ROW,079 SAY lnGross PICTURE '999999.99'   &&B800179,1 MFM 09/20/95 Added.
      ENDIF
    ENDIF
    @ ROW,090 SAY lnNet PICTURE '999999.99'

    @ ROW,101 SAY lnCost          PICTURE '999999.99'   
    @ ROW,112 SAY lnNet - lnCost  PICTURE '999999.99'
    lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
    lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)
    @ ROW,123 SAY lnPercnt PICTURE '99999.99 %'

  ENDIF
    
  xRTot1 = xRTot1 + lnQty
  IF lcRpSubtRt = 'Y'
    xRTot2 = xRTot2 + lnGross    
  ELSE
    IF Type='INV'     
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

ROW = ROW + IIF(lcRpFormat='D',1,0)
IF (xRTot1+xRTot3<>0 .AND. lcRpSortBy='I'.AND. lcRpFormat='S')
  ROW = ROW + 1
  @ ROW,00 SAY 'RETURNS WITHOUT INVOICES :'       
  @ ROW,060 SAY xRTot1 PICTURE '999999'
  @ ROW,079 SAY xRTot2 PICTURE '999999.99' 
  @ ROW,090 SAY xRTot3 PICTURE '999999.99'

  @ ROW,101 SAY xRTot4 PICTURE '999999.99'
  @ ROW,112 SAY xRTot5 PICTURE '999999.99'
  @ ROW,123 SAY xRTot6 PICTURE '99999.99 %'
ENDIF

*End...
*!*************************************************************
*! Name      : lpCrAllo
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/20/1998
*! Purpose   : Print credits,allowances and chargebacks.
*!*************************************************************
*! Calls     : lpPrnHdr
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : DO lpCrAllo
*!*************************************************************

PROCEDURE lpCrAllo
lcAlias = ALIAS()
STORE 0 TO lnSubAll, lnSubCrd, lnSubChg
SELECT Arhist
IF SEEK(Break)

  SCAN WHILE Account = Break FOR TranType = '7' AND BETWEEN(TranDate,ldFrstDate,ldSndDate)
    ROW = ROW + 1   
    IF ROW >= 53
      DO lpPrnHdr
    ENDIF
    @ ROW,00 SAY 'Allowance'    
    @ ROW,30 SAY Tran
    @ ROW,38 SAY Desc
    @ ROW,60 SAY Amount PICTURE '999999.99'
    lnSubAll = lnSubAll + Amount
    lnGrAll  = lnGrAll  + Amount
  ENDSCAN
ENDIF

SELECT Credit
IF SEEK(Break)
  SCAN WHILE Account = Break FOR BETWEEN(TranDate,ldFrstDate,ldSndDate)
    ROW = ROW + 1   
    IF ROW >= 53
      DO lpPrnHdr
    ENDIF
    @ ROW,00 SAY 'Credit'    
    @ ROW,30 SAY Tran
    @ ROW,38 SAY Desc
    @ ROW,60 SAY Amount PICTURE '999999.99'
    lnSubCrd = lnSubCrd + Amount
    lnGrCrd  = lnGrCrd  + Amount
  ENDSCAN
ENDIF


IF lcRpPrint = 'C'
  SELECT Debit
  IF SEEK(Break)
    SCAN WHILE Account = Break FOR TranType = '3' AND BETWEEN(TranDate,ldFrstDate,ldSndDate)
      ROW = ROW + 1   
      IF ROW >= 53
        DO lpPrnHdr
      ENDIF
      @ ROW,00 SAY 'Charge back'
      @ ROW,30 SAY Tran
      @ ROW,38 SAY Desc
      @ ROW,60 SAY Amount PICTURE '999999.99'
      lnSubChg = lnSubChg + Amount
      lnGrChg  = lnGrChg  + Amount
    ENDSCAN
  ENDIF
ENDIF
ROW = ROW + 1   
IF ROW >= 53
  DO lpPrnHdr
ENDIF
@ ROW,00 SAY REPLICATE('-',132)
ROW = ROW+1
@ ROW,000 SAY '* SUB TOTAL FOR CREDITS & ALLOWANCES *'
@ ROW,060 SAY lnSubAll + lnSubChg + lnSubCrd PICTURE '999999.99'
ROW = ROW+1
@ ROW,00 SAY REPLICATE('-',132)

SELECT (lcAlias)

*!*************************************************************
*! Name      : lpPrnHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/20/1998
*! Purpose   : Print report header.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            : DO lpPrnHdr
*!*************************************************************

PROCEDURE lpPrnHdr

PAGENO = PAGENO+1
DO RPT_HDR WITH 'AND100'+lcRpFormat,lcRpTitle,R_WIDTH
*C101386 (Begin) Replace the style color with the style major title.
*IF lcReport='S' .OR. ! llInclRet  
*  @ 05,00 SAY 'ACCOUNT ...... NAME ......... INVOICE STYLE        COLOR    PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %'
*ELSE
*  @ 05,00 SAY 'ACCOUNT ...... NAME ..... TYP INV/CRD STYLE        COLOR    PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %'
*ENDIF
IF lcRpFormat='S' .OR. lcRpIncRet = 'Y'
  @ 05,00 SAY 'ACCOUNT ...... NAME ......... INVOICE '+lcMajTitle+'   PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %'
ELSE
  @ 05,00 SAY 'ACCOUNT ...... NAME ..... TYP INV/CRD '+lcMajTitle+'   PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %'
ENDIF
*C101386 (End)
@ 06,00 SAY REPLICATE('=',132)
ROW = 07

****************************************************************************
* FUNC: lfwRepWhen
* DESC: To valid the OG WHEN function.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/10/98
* Refer To : (C101386)
****************************************************************************
FUNCTION lfwRepWhen

*-- Create the temp file here to be created only once each time we enter the
*-- option grid.
SELECT INVLINE
=AFIELDS(laFileStru)
lnArrLen = ALEN(laFileStru,1)
DIMENSION laFileStru[lnArrLen+8,4]
*--Add the type field.
laFileStru[lnArrLen+1,1] = 'Type'
laFileStru[lnArrLen+1,2] = 'C'
laFileStru[lnArrLen+1,3] = '3'
laFileStru[lnArrLen+1,4] = '0'
*--Add the Date field.
laFileStru[lnArrLen+2,1] = 'Date'
laFileStru[lnArrLen+2,2] = 'D'
laFileStru[lnArrLen+2,3] = '8'
laFileStru[lnArrLen+2,4] = '0'
*--Add the code field.
laFileStru[lnArrLen+3,1] = 'Code'
laFileStru[lnArrLen+3,2] = 'C'
laFileStru[lnArrLen+3,3] = '6'
laFileStru[lnArrLen+3,4] = '0'
*--Add the Name field.
laFileStru[lnArrLen+4,1] = 'Name'
laFileStru[lnArrLen+4,2] = 'C'
laFileStru[lnArrLen+4,3] = '30'
laFileStru[lnArrLen+4,4] = '0'
*--Add the Group field with name cGroup.
laFileStru[lnArrLen+5,1] = 'cGroup'
laFileStru[lnArrLen+5,2] = 'C'
laFileStru[lnArrLen+5,3] = '2'
laFileStru[lnArrLen+5,4] = '0'
*--Add the Class field.
laFileStru[lnArrLen+6,1] = 'Class'
laFileStru[lnArrLen+6,2] = 'C'
laFileStru[lnArrLen+6,3] = '2'
laFileStru[lnArrLen+6,4] = '0'
*--Add the DiscPcnt field.
laFileStru[lnArrLen+7,1] = 'DiscPcnt'
laFileStru[lnArrLen+7,2] = 'N'
laFileStru[lnArrLen+7,3] = '6'
laFileStru[lnArrLen+7,4] = '2'
*--Add the Trde_Disc field.
laFileStru[lnArrLen+8,1] = 'Trde_Disc'
laFileStru[lnArrLen+8,2] = 'N'
laFileStru[lnArrLen+8,3] = '6'
laFileStru[lnArrLen+8,4] = '2'
*--Create the temp file.
CREATE DBF (gcWorkDir+lcInvLTmp) FROM ARRAY laFileStru

*-- Initilize the date range with system date.
lnDatePos = ASCAN(laOGFxFlt,IIF(lcRpInvD = "S","INVHDR.SHIPDATE","INVHDR.INVDATE"))
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  ENDIF  
ENDIF


****************************************************************************
* FUNC: lfwOldVal
* DESC: To get the old value.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/06/98
* Refer To : (C101331)
****************************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))


****************************************************************************
* FUNC: lfvSortBy
* DESC: To clear read in case of sorting by style to get the cost.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/06/98
* Refer To : (C101331)
****************************************************************************
FUNCTION lfvSortBy

IF (lcRpSortBy= 'A') OR (lcOldVal = 2)
  CLEAR READ
ENDIF  


****************************************************************************
* FUNC: lfvFormat
* DESC: To .
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/10/98
* Refer To : (C101386)
****************************************************************************
FUNCTION lfvFormat

IF (lcRpFormat= 'S') OR (lcOldVal = 2)
  CLEAR READ
ENDIF  


****************************************************************************
* FUNC: lfvPrtCrdt
* DESC: To .
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/10/98
* Refer To : (C101386)
****************************************************************************
FUNCTION lfvPrtCrdt

IF (lcRpSortBy= 'A') OR (lcOldVal = 2)
  CLEAR READ
ENDIF  

****************************************************************************
* FUNC: lfvInvBy
* DESC: To clear read in case of sorting by style to get the cost.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/06/98
* Refer To : (C101331)
****************************************************************************
FUNCTION lfvInvBy

CLEAR READ


****************************************************************************
* FUNC: lfvAccount
* DESC: To valid the account.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/14/98
* Refer To : (C101386)
****************************************************************************
FUNCTION lfvAccount

*--Varible to hold  the name of the memory variable used to create the current GET field
lcObjName = SYS(18)
*--- Varible to hold  the value of the current GET field
lcObjVal = EVALUATE(SYS(18))

*--IF The user want to Browse or if the Account he entered is not in the file.
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF 
*--

****************************************************************************
* FUNC: lfvStyle
* DESC: To valid the style.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/14/98
* Refer To : (C101386)
****************************************************************************
FUNCTION lfvStyle

PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 
*-- Varible to hold  the name of the memory variable used to create the current GET field
lcObjName = SYS(18)
*-- Varible to hold  the value of the current GET field
lcObjVal = EVALUATE(SYS(18)) 

*--IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.
  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF 

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)


****************************************************************************
* FUNC: lfvRepCode
* DESC: To valid the sales rep.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/14/98
* Refer To : (C101386)
****************************************************************************
FUNCTION lfvRepCode
PRIVATE lcVar , lcObj , laTemp

lcOldAl = SELECT()
SELECT SALESREP
SET ORDER TO TAG SALESREP
*--Varible to hold  the name of the memory variable used to create the current GET field
lcVar = SYS(18)
*--- Varible to hold  the value of the current GET field
lcObj = EVALUATE(SYS(18))
*--IF Statment to check if we are going to Browse
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
  *--IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = lcOldVal
  ENDIF
ENDIF
&lcVar = lcObj      && Update the field
SELECT (lcOldAl)
****************************************************************************
* FUNC: lfCutFilr
* DESC: To valid the sales rep.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/16/98
* Refer To : (C101386)
****************************************************************************
FUNCTION lfCutFilr

FOR lnFor = 1 to 4
  DO CASE
    CASE lnFor = 1
      *-- Style,Date and account ranges are of BETWEEN statement in the FIXED FILTER
      *-- So we are making them in FOR loop to avoid repeating.
      FOR lnFrstFor = 1 to 3
        lcObjStr = IIF(lnFrstFor = 1,'SUBSTR(INVLINE.STYLE,1,lnMajLen)',IIF(lnFrstFor = 2,IIF(lcRpInvD = "I",'INVHDR.INVDATE','INVHDR.SHIPDATE'),'INVLINE.ACCOUNT'))
        lnObjPos = ASCAN('laOGFxFlt',lcObjStr)
        *--If it does not exist.
        IF lnObjPos =0
          LOOP
        ENDIF  
        *-- Get the row.
        lnObjPos  = ASUBSCRIPT('laOGFxFlt',lnObjPos,1)
        lnSndSPos = AT('|',laOGFxFlt[lnObjPos,6])
        DO CASE 
          CASE lnFrstFor = 1
            *-- Style range
            lcFrstSty = IIF(lnSndSPos <>1,SUBSTR(laOGFxFlt[lnObjPos,6],1,lnMajLen),'')
            lcSndSty  = SUBSTR(laOGFxFlt[lnObjPos,6],lnSndSPos+1,lnMajLen)
            *--Cut the style expression from the lcRpExp filter expression as we will use it
            *--separately in SCAN statement.So we will check if it exists in the lcRpExp Nnot
            *--in the fixed filter as if we just press the style range button and do not choose any
            *-- style the style expression exists in the fixed filter  and does not exist
            *-- in lcRpExp 
             IF lcObjStr $ lcRpExp 
               lnAndPos = AT('AND',lcRpExp)
               lcRpExp   = STRTRAN(lcRpExp,SUBSTR(lcRpExp,1,lnAndPos-1),'.T. ')
             ENDIF  
          CASE lnFrstFor = 2
            *-- Date range
            ldFrstDate = IIF(lnSndSPos <>1,CTOD(SUBSTR(laOGFxFlt[lnObjPos,6],1,10)),{})
            ldSndDate  = CTOD(SUBSTR(laOGFxFlt[lnObjPos,6],lnSndSPos+1,10))
          OTHERWISE
            *-- Account range
            lcFrstAcct = IIF(lnSndSPos <>1,SUBSTR(laOGFxFlt[lnObjPos,6],1,5),'')
            lcSndAcct  = SUBSTR(laOGFxFlt[lnObjPos,6],lnSndSPos+1,10)
        ENDCASE  
      ENDFOR  
    CASE lnFor = 2
      *-- Season and division ranges are of LIKE statement in the FIXED FILTER
      *-- So we are making them in FOR loop to avoid repeating.
      FOR lnSndFor = 1 to 2
        lcObjStr = IIF(lnSndFor = 1,'INVLINE.SEASON','INVHDR.CDIVISION')
        lnObjPos = ASCAN('laOGFxFlt',lcObjStr)
        *--If it does not exist.
        IF lnObjPos =0
          LOOP
        ENDIF  
        *-- Get the row.
        lnObjPos  = ASUBSCRIPT('laOGFxFlt',lnObjPos,1)
        IF lnSndFor = 1
          *-- Season range
          lcSeaStr = laOGFxFlt[lnObjPos,6]
        ELSE
          *-- Division range
          lcDivStr = laOGFxFlt[lnObjPos,6]
        ENDIF
      ENDFOR  
    CASE lnFor = 3
      *-- Class and group ranges are of INLIST statement in the FIXED FILTER
      *-- So we are making them in FOR loop to avoid repeating.
      FOR lnThrdFor = 1 to 2
        lcObjStr = IIF(lnThrdFor = 1,'CUSTOMER.CLASS','STYLE.CSTYGROUP')
        lnObjPos = ASCAN('laOGFxFlt',lcObjStr)
        *--If it does not exist.
        IF lnObjPos =0
          LOOP
        ENDIF  
        *-- Get the row.
        lnObjPos  = ASUBSCRIPT('laOGFxFlt',lnObjPos,1)
        lnSndSPos = AT('|',laOGFxFlt[lnObjPos,6])
        IF lnThrdFor = 1
          *-- Season range
          lcClassStr = STRTRAN(laOGFxFlt[lnObjPos,6],'|',',')
        ELSE
          *-- Division range
          lcGrpStr = STRTRAN(laOGFxFlt[lnObjPos,6],'|',',')
        ENDIF
      ENDFOR  
    OTHERWISE
      *-- Sales rep. range
      lnObjPos = ASCAN('laOGFxFlt','INVHDR.REP1')
      *--If it does not exist.
      IF lnObjPos =0
        LOOP
      ENDIF  
      *-- Get the row.
      lnObjPos  = ASUBSCRIPT('laOGFxFlt',lnObjPos,1)
      *-- Season range
      lcRepStr = laOGFxFlt[lnObjPos,6]
  ENDCASE
ENDFOR

