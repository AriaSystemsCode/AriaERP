***************************************************************************
*: Program file  : RMOLS160.PRG (C# 101591)
*: Program desc. : Print "Wholesale Sales report".
*:                 Convert OLS1600 from 2.6 to 2.7
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY _ (SSH)
*:      Module   : RM
*:*************************************************************************
*: Calls :PROCEDURES :  lpSubRep. 
*:        FUNCTIONS  : 
*:          lfvRepCode,lfwOldVal ,lfOgWhen,lfUpdFltVar
*:          lfClearRep.
*:*************************************************************************
*: Passed Parameters  : None
*:*************************************************************************
*--- Note: We used "Gl_Sales" in "lcRpMTmp file" as salesrep.
*---     : We used "Reason"   in "lcRpMTmp file"as season.
***************************************************************************
* B605396,1 RAE 01/20/2002 Adjust the date range and the page breack
* B605405,1 WAB 02/07/2002 the credit memo amount is not correct 
***************************************************************************
lcFilter = ' '
xFilter  = ' '
STORE SPACE (2) TO lcHsea
STORE SPACE (3) TO lcLrep  , lcHrep
STORE DATE()    TO ldLdate , ldHdate
STORE 0 TO lnGrGros ,lnGrDisc ,lnGrPst ,lnGrGst ,lnGrSpCh ,lnGrCstSal ,lnGrAmInv,lnCost
STORE 0 TO lnRGros  ,lnRDisc  ,lnRPst  ,lnRGst  ,lnRSpCh  ,lnRAmInv  ,lnRCstSal,lnCstpSea
*--- Global variable to indicate if the selection criteria has been changed or not.
*--- To Prevent recollecting Data.
IF llOgFltCh
  **Creat Filter**
  =lfClearRep()
  =lfUpdFltVar()
  IF !EMPTY(lcHrep)
    lcFilter =  'BETWEEN(RetHdr.SalesRep1,lcLrep,lcHrep)'
  ELSE
    lcFilter =  '.T.'
  ENDIF
  
*B605396 RAE [BEGIN]  
*  DO CASE
*    CASE EMPTY(ldLdate) .AND. !EMPTY(ldHdate)
*      lcFilter = lcFilter + ' .AND. RetHdr.crDate <= ldHdate'
*    CASE !EMPTY(ldLdate) .AND. EMPTY(ldHdate)
*      lcFilter = lcFilter + ' .AND. RetHdr.crDate >=ldLdate'
*    CASE !EMPTY(ldLdate) .AND. !EMPTY(ldHdate)
*    IF (ldHdate) >= (ldLdate)
*      lcFilter =lcFilter + ' .AND. BETWEEN(RetHdr.crDate,ldLdate,ldHdate)'
*    ENDIF
*    CASE EMPTY(ldLdate) .AND. EMPTY(ldHdate)
*     lcFilter = '.T.'
*  ENDCASE

*  lcFilter = IIF (EMPTY(lcFilter) OR lcFilter = '.T.'  ,;
             "RetHdr.Status <> 'V' ", lcFilter+ " .AND. ;
              RetHdr.Status <> 'V' ")
              
  DO CASE
    CASE EMPTY(ldLdate) .AND. !EMPTY(ldHdate)
      lcFilter = lcFilter + ' .AND. RetHdr.crDate <= ldHdate .AND.RetHdr.Status <> "V" '
    CASE !EMPTY(ldLdate) .AND. EMPTY(ldHdate)
      lcFilter = lcFilter + ' .AND. RetHdr.crDate >= ldLdate .AND.RetHdr.Status <> "V" '
    CASE !EMPTY(ldLdate) .AND. !EMPTY(ldHdate)
      IF (ldHdate) >= (ldLdate)
        lcFilter = lcFilter + ' .AND. BETWEEN(RetHdr.crDate,ldLdate,ldHdate).AND.RetHdr.Status <> "V" '
      ENDIF
    CASE EMPTY(ldLdate) .AND. EMPTY(ldHdate)
      lcFilter = lcFilter +'.AND. RetHdr.Status <> "V" '
  ENDCASE              
*B605396 RAE [END]              

  SELECT RetHdr
  *B605396 RAE [BEGIN]
  *GO TOP
  LOCATE
  *B605396 RAE [END]
  COPY REST TO &gcWorkDir.&lcRpRtHtmp  FOR &lcFilter
  =gfOpenFile('&gcWorkDir.&lcRpRtHtmp',' ','EX')
  SELECT(lcRpRtHtmp)
  INDEX ON CrMemo TAG lcRpRtHtmp
  *B605396 RAE [BEGIN]
  *GO TOP
  LOCATE
  *B605396 RAE [END]
  
  
  SELECT RETLINE
  COPY STRUCTURE TO &gcWorkDir.&lcRpMTmp
  =gfOpenFile('&gcWorkDir.&lcRpMTmp',' ','EX')
  *B605405,1 WAB (Start) - index the file before adding the records 
  INDEX ON Gl_Sales+REASON+STR(RECNO(),3) TAG lcRpMTmp
  *B605405,1 WAB (End)
  
  SELECT (lcRpRtHtmp)
  *B605396 RAE [BEGIN]
  LOCATE
  *B605396 RAE [END]
  lcCrMemo= CrMemo
  WAIT WINDOW 'Colecting Data Please Wait.. <SPACE BAR> to abort' NOWAIT
  SCAN WHILE INKEY() <>32
    lcCrMemo= CrMemo
    IF SEEK(lcCrMemo,'RetLine')
      SELECT RetLine
      *B605396 RAE [BEGIN]
      *DO WHILE CrMemo = &lcRpRtHtmp->CrMemo
      
      *B605405,1 WAB (Start) - scan only for the type = 2 . because there are onther type when 
      *B605405,1 WAB         - the credit memo created from RA and we return partial of the RA
      *SCAN REST WHILE crmemo+style+cret_linno+cret_trncd = &lcRpRtHtmp->CrMemo
      
      SCAN REST WHILE crmemo+style+cret_linno+cret_trncd = &lcRpRtHtmp->CrMemo FOR cret_trncd = "2" 
      
      *B605405,1 WAB (End)
      
      WAIT WINDOW  'Style  :  ' +   STYLE  NOWAIT
      *B605396 RAE [END]
        SCATTER MEMVAR MEMO
        SELECT (lcRpMTmp)
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE REASON   WITH IIF(SEEK(Style,'Style'),Style.Season,SPACE(2)) 
        REPLACE Gl_Sales WITH &lcRpRtHtmp->SalesRep1
        SELECT RetLine
      *B605396 RAE [BEGIN]
      *  SKIP
      *ENDDO
      ENDSCAN
      *B605396 RAE [END]
    ENDIF
  ENDSCAN
  *B605396 RAE [BEGIN]
  *GO TOP
  LOCATE
  *B605396 RAE [END]
 
  SELECT (lcRpMTmp)
  SET FILTER TO !EMPTY(Gl_Sales)
  SET FILTER TO !EMPTY(REASON)
  IF !EMPTY(lcHsea)
    xFilter  =  '&lcRpMTmp->Reason $ lcHsea'
  ENDIF
  SET FILTER TO &xFilter
  *B605405,1 WAB (Start) - we have create the index after created the file so no need to create the index here
  *INDEX ON Gl_Sales+REASON+STR(RECNO(),3) TAG lcRpMTmp
  *B605405,1 WAB (End)
ELSE
  =lfUpdFltVar()
ENDIF
SELECT (lcRpMTmp)
*B605396 RAE [BEGIN]
*GO TOP
LOCATE
*B605396 RAE [END]
IF RECCOUNT()= 0 OR EOF() 
  =gfModalGen('TRM38155B00000','DIALOG' )
  SET DEVICE TO SCREE
  RETURN
ENDIF
*B605396 RAE [BEGIN]
*GO TOP
LOCATE
*B605396 RAE [END]
*----Print the detail report....
lcReport  = 'RMOLS160'
R_Title   = 'Wholesale Sales Returns report'
lcTitle   = SPACE(40)
Row       = 99
PageNo    = 0
SET DEVICE TO SCREEN
WAIT WINDOW 'Report printing press <SPACE BAR> to abort' NOWAIT
SET DEVICE TO PRINT
lcRep     = Gl_Sales
lcSea     = REASON
SELECT (lcRpMTmp)

DO WHILE INKEY() <>32
  IF EOF()
    EXIT
  ENDIF  
  SELECT (lcRpMTmp)
  IF ROW >= 55
    ROW = 0
    PAGENO = PAGENO+1
    @ 03,000 SAY ALLTRIM(gcCom_Name)
    @ 03,85 SAY 'Date: '
    @ 03,95 SAY gdSysDate
    @ 04,00 SAY 'Wholesale Sales'
    @ 04,85 SAY 'Time: '
    @ 04,95 SAY TIME()
    @ 05,00 SAY 'For the period'
    @ 05,15 SAY ldLdate
    @ 05,25 SAY 'TO'
    @ 05,28 SAY  ldHdate
    @ 05,85 SAY 'Page#'
    @ 05,93 SAY PAGENO
    @ 07,00 SAY '                  Gross                                              Shipping        Amount     Cost of'
    @ 08,00 SAY '                Returns       Discounts         PST         GST       Charges      Credited       Sales'
    @ 09,00 SAY '                ---------------------------------------------------------------------------------------'
    ROW = 10
  ENDIF

  @ ROW,00 SAY 'Agent # '+Gl_Sales
  ROW=ROW+1

  DO WHILE lcRep = Gl_Sales 
    STORE 0 TO lnGross ,lnDisc ,lnPst ,lnGst ,lnShpChr ,lnCstSal ,lnAmtInv
    lcSea = Reason
    SELECT (lcRpMTmp)
    lnCrMemo = SPACE(6)
    SCAN WHILE lcSea = Reason  AND lcRep = Gl_Sales
      *B605405,1 WAB (Start)- recalculate the Amount ( Gross * total qty) because i found in the 
      *B605405,1 WAB        - customer data that there are many transaction without a correct amount.
      *B605405,1 WAB        - i didn't fix the data because i couldn't create the same problem.
      *lnGross  = lnGross  + Amount+Disc_Amt
      *lnDisc   = lnDisc   + Disc_Amt
      *IF SEEK(CrMemo,lcRpRtHtmp)
      *  lnGst    = lnGst    + ((&lcRpRtHtmp->Tax_Amt/&lcRpRtHtmp->Amount)*Amount)
      *  lnPst    = lnPst    + ((&lcRpRtHtmp->nPstAmt/&lcRpRtHtmp->Amount)*Amount)
      *  lnShpChr = lnShpChr + ((&lcRpRtHtmp->Other  /&lcRpRtHtmp->Amount)*Amount)
      *END
      lnGross  = lnGross + (GROS_PRICE*TOTQTY)
      lnDisc   = lnDisc   +((GROS_PRICE*TOTQTY)-(PRICE*TOTQTY))
      lnTotAmnt= (PRICE * TOTQTY)
      IF SEEK(CrMemo,lcRpRtHtmp)
        lnGst    = lnGst    + ((&lcRpRtHtmp->Tax_Amt/&lcRpRtHtmp->Amount)*lnTotAmnt)
        lnPst    = lnPst    + ((&lcRpRtHtmp->nPstAmt/&lcRpRtHtmp->Amount)*lnTotAmnt)
        lnShpChr = lnShpChr + ((&lcRpRtHtmp->Other  /&lcRpRtHtmp->Amount)*lnTotAmnt)
      ENDIF
      *B605405,1 WAB (Start)

      lnCost = lnCost+IIF(SEEK(Style,'Style'),(Style.TotCost*TotQty),0)
      lnCstpSea=lnCstpSea+IIF(SEEK(Style,'Style'),(Style.TotCost*TotQty),0)
      lnCstSal = lnCstpSea
      lnCrMemo = Crmemo
    ENDSCAN
*B605396 RAE [BEGIN]
  IF ROW >= 55
    ROW = 0
    PAGENO = PAGENO+1
    @ 03,000 SAY ALLTRIM(gcCom_Name)
    @ 03,85 SAY 'Date: '
    @ 03,95 SAY gdSysDate
    @ 04,00 SAY 'Wholesale Sales'
    @ 04,85 SAY 'Time: '
    @ 04,95 SAY TIME()
    @ 05,00 SAY 'For the period'
    @ 05,15 SAY ldLdate
    @ 05,25 SAY 'TO'
    @ 05,28 SAY  ldHdate
    @ 05,85 SAY 'Page#'
    @ 05,93 SAY PAGENO
    @ 07,00 SAY '                  Gross                                              Shipping        Amount     Cost of'
    @ 08,00 SAY '                Returns       Discounts         PST         GST       Charges      Credited       Sales'
    @ 09,00 SAY '                ---------------------------------------------------------------------------------------'
    ROW = 10
  ENDIF
*B605396 RAE [END]
    @ ROW,00 SAY 'SEASON: ' +lcSea
    @ ROW,12 SAY lnGross       PICTURE '99999999.99'
    @ ROW,27 SAY lnDisc        PICTURE '999999999.99'
    @ ROW,40 SAY lnPst         PICTURE '99999999.99'
    @ ROW,52 SAY lnGst         PICTURE '99999999.99'
    @ ROW,66 SAY lnShpChr      PICTURE '99999999.99'
    @ ROW,78 SAY (lnGross-lnDisc)+lnPst+lnGst+lnShpChr       PICTURE '9999999999.99'
    lnAmtInv=(lnGross-lnDisc)+lnPst+lnGst+lnShpChr
    @ ROW,92 SAY lnCstpSea     PICTURE '99999999.99'
    ROW=ROW+1
    lnCstpSea=0
    lnRGros   = lnRGros   + lnGross
    lnRDisc   = lnRDisc   + lnDisc
    lnRPst    = lnRPst    + lnPst
    lnRGst    = lnRGst    + lnGst
    lnRSpCh   = lnRSpCh   + lnShpChr
    lnRAmInv  = lnRAmInv  + lnAmtInv
    lnRCstSal = lnRCstSal + lnCstSal
  ENDDO
  IF lcRep <> Gl_Sales
    DO lpSubRep
    STORE 0 TO lnGross,lnCost,lnDisc,lnPst,lnGst,lnShpChr,lnAmtInv
    lcRep = Gl_Sales
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
@ ROW,78 SAY lnGrAmInv     PICTURE '9999999999.99'
@ ROW,92 SAY lnGrCstSal    PICTURE '99999999.99'
ROW=ROW+1
@ ROW,000 SAY REPLICATE('*',103)
ROW=ROW+1
DO ENDREPORT       && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREE
RETURN


*!*************************************************************
*! Name      : lpSubRep   (C# 101591)
*! Developer : Ahmed Salah Shalaby (SSH)
*! Date      : 08/15/99
*! Purpose   : PRINT THE SUBTOTAL..
*!*************************************************************
*! Calls     : 
*!             Procedures : NONE
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lfvRepCode
*!*************************************************************
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
*! Name      : lfvRepCode (C# 101591)
*! Developer : Ahmed Salah Shalaby (SSH)
*! Date      : 08/15/99
*! Purpose   : Validate Primary Sales Rep. in SALESREP file.
*!*************************************************************
*! Calls     : 
*!             Procedures : NONE
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
*! Name      : lfwOldVal  (C# 101591)
*! Developer : Ahmed Salah Shalaby (SSH)
*! Date      : 08/15/99
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
*! Name      : lfOgWhen  (C# 101591)
*! Developer : Ahmed Salah Shalaby (SSH)
*! Date      : 08/15/99
*! Purpose   : When function.
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOgWhen()
*!*************************************************************
FUNCTION lfOgWhen

R_Width   = 'W'

lnDatePos = ASCAN(laOGFxFlt,"RETHDR.CRDATE")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  ENDIF  
ENDIF


*!*************************************************************
*! Name      : lfUpdFltVar  (C# 101591)
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 08/12/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUpdFltVar()
*!*************************************************************
FUNCTION lfUpdFltVar

FOR lnInd  = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.SEASON' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcHsea = laOgFxFlt[lnInd,6]
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'SALESREP.REPCODE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcLrep = SUBSTR(laOgFxFlt[lnInd,6],1,3)
      lcHrep = SUBSTR(laOgFxFlt[lnInd,6],5,3)
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'RETHDR.CRDATE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      ldLdate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      ldHdate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
  ENDCASE
ENDFOR


*!*************************************************************
*! Name      : lfClearRep  (C# 101591)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 08/16/1999
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file
USE IN IIF(USED(lcRpMTmp),lcRpMTmp,0)
ERASE &gcWorkDir.&lcRpMTmp+'.DBF'
ERASE &gcWorkDir.&lcRpMTmp+'.CDX'

USE IN IIF(USED(lcRpRtHtmp),lcRpRtHtmp,0)
ERASE &gcWorkDir.&lcRpRtHtmp+'.DBF'
ERASE &gcWorkDir.&lcRpRtHtmp+'.CDX'