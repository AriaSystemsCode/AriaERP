*:************************************************************************
*: Program file  : ARROB100   (Copied form ROB100.PRG FOR ROBYN MEREIDITH )
*: Program desc. : GROSS PROFIT REPORT CUSTOMIZED FOR ROBYN MEREIDITH
*:               : Converted from Aria26 to Aria27.
*:         System: ARIA APPAREL COMPANY 
*:         Module: REPORT
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:      Refer To : (C101331)
*:************************************************************************
*: Calls : gfItemMask(),gfModalGen(),gfStyBrw(),CusBrowM()
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*B605567,1 SSE 03/20/2001 Fix bug of adding a wait window to indicate collecting of data.

*-- The temporary file name variable (lcInvLTemp) has been created from the report generator
*-- to be created each time we enter the option grid not each time we press
*-- PREVIEW OR RUN button.

*C101331,1 (Begin) Get the style header.
lcStyTit = gfItemMask('HI')
*C101331,1 (End)
SELECT INVLINE
SET RELATION TO INVOICE INTO INVHDR
*C101331,1 (Begin) Get the date for the report header
lnDatePos = ASCAN('laOGFxFlt','INVLINE.INVDATE')
*--IF it exists
IF lnDatePos> 0
  *-- Get the row.
  lnDatePos = ASUBSCRIPT('laOGFxFlt',lnDatePos,1)
  lcDateStr = laOGFxFlt[lnDatePos,6]
  lnSndSPos = AT('|',lcDateStr)
  *--The first date
  lcFrstDate = IIF(lnSndSPos <>1,SUBSTR(lcDateStr,1,10),'')
  lcSndDate  = SUBSTR(lcDateStr,lnSndSPos+1,10)
ENDIF
XTITLE  = 'FROM:'+lcFrstDate+' THRU:'+lcSndDate
*C101331,1 (End)
*C101331,1 (Begin) Get the report lines.
*--If we press PREVIEW or RUN button, Zap the temp file.
IF USED(lcInvLTemp)
  SELECT (lcInvLTemp)
  IF RECCOUNT() >0
    ZAP
  ENDIF  
ENDIF
SELECT INVLINE
*-- The filter expression does not contain the invoice status .
lcRpExp = lcRpExp +"AND INVHDR.STATUS <> 'V'"

SCAN FOR &lcRpExp
  
  *B605567,1 Add a wait window to indicate collecting of data. [Begin]
  WAIT WINDOW 'Collecting invoice # : '+Invoice NOWAIT  
  *B605567,1 Add a wait window to indicate collecting of data. [End]

  SCATTER MEMVAR MEMO  
  INSERT INTO (lcInvLTemp) FROM MEMVAR
ENDSCAN
SET RELATION TO

SELECT (lcInvLTemp)
GOTO TOP
*--If no records selected.
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
*C101331,1 (End)

*B605567,1 Add a wait window to indicate sorting of data. [Begin]
WAIT WINDOW 'Sorting '+STR(RECCOUNT(),6)+' records for the report ' NOWAIT
*B605567,1 Add a wait window to indicate sorting of data. [End]

DO CASE
  CASE lcRpSort='A'
    *C101331,1 (Begin) Color was cleared form the index below.
    INDEX ON ACCOUNT+INVOICE+STYLE TO (lcInvLTemp)
    *C101331,1 (End)
    BREAK = 'ACCOUNT'
  CASE lcRpSort='I'
    *C101331,1 (Begin) Color was cleared form the index below.
    INDEX ON INVOICE+STYLE TO (lcInvLTemp)
    *C101331,1 (End)
    BREAK = 'INVOICE'
  CASE lcRpSort='S'
    *C101331,1 (Begin) Color was cleared form the index below.
    INDEX ON STYLE+INVOICE TO (lcInvLTemp)
    *-- let the break be the style major.
    *BREAK = 'STYLE'
    BREAK = 'SUBSTR(STYLE,1,lnMajOrLen)'
    *C101331,1 (End)
ENDCASE

*--Setting the needed relation.
SET RELATION TO INVOICE INTO INVHDR

R_WIDTH = 'W'
SET DEVICE TO PRINT
*--- START THE REPORT

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*ACCOUNT .......... NAME ......... INVOICE STYLE         PIECES  S. PRICE   GROSS AMT     NET AMT        COST      PROFIT    PROF. %
*12345   XXXXXXXXXXXXXXXXXXXXXXXXX 123456  1234567 123   123456  99999.99   999999.99   999999.99   999999.99   999999.99   999.99 %
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*ACCOUNT .......... NAME ......... TYP INV/CRD STYLE         PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %
*12345   XXXXXXXXXXXXXXXXXXXXXXXXX INV 123456  1234567 123   123456  99999.99   999999.99  999999.99  999999.99  999999.99  999.99 %
*                                  RET 123456  1234567 123   123456  99999.99             -999999.99

DO CASE
  CASE lcRpSort='A'
    R_TITLE = 'GROSS PROFIT BY '+'ACCOUNT'
  CASE lcRpSort='S'
    R_TITLE = 'GROSS PROFIT BY '+'STYLE'
  CASE lcRpSort='I'
    R_TITLE = 'GROSS PROFIT BY '+'INVOICE'
ENDCASE
DO WHILE .T.
  *-- Variables Initialization 
  STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5
  STORE 0 TO XGTOT1,XGTOT2,XGTOT3,XGTOT4,XGTOT5
  STORE 0 TO XTOTPRIC,XTOTINV

  lnCostTot  = 0
  lnCostGTot = 0   
  lnSubNum   = 0
  XTIME  = TIME()
  PAGENO = 0
  ROW    = 99
  SELECT (lcInvLTemp)
  GOTO TOP
  IF !EMPTY(BREAK)
    HBREAK = &BREAK
  ENDIF
  *-- Begin Printing 
  CLEAR TYPEAHEAD
  *---------------------------------------------------------
  * BEGIN [MAIN REPORT] LOOP
  * ---------------------------------------------------------
  DO WHILE INKEY() <> 32

    *B605567,1 Add a wait window to indicate Printing of data. [Begin]
    WAIT WINDOW 'Printing Transaction # : '+ &lcInvLTemp..Invoice NOWAIT
    *B605567,1 Add a wait window to indicate Printing of data. [End]

    IF ROW >= 53
      PAGENO = PAGENO+1
      DO RPT_HDR WITH lcRpFormat,XTITLE,R_WIDTH
      lcCostTTl = IIF(lcRpCost="S","AVE.SCOST","AVE.MCOST")
      IF lcRpFormat='S'
        IF lcRpSort='S'
          @ 05,00 SAY '                                              &lcCostTTl PIECES AVG PRICE   GROSS AMT     NET AMT        COST      PROFIT    PROF. %'
        ELSE
          *C101331,1 (Begin) Replace 'STYLE   CLR' with style major here as no place for the whole style.
          *@ 05,00 SAY 'ACCOUNT .......... NAME ......... INVOICE STYLE   CLR   PIECES AVG PRICE   GROSS AMT     NET AMT        COST      PROFIT    PROF. %'
           @ 05,00 SAY 'ACCOUNT .......... NAME ......... INVOICE '+PADR(lcMajTitle,14)+' PIECES AVG PRICE   GROSS AMT     NET AMT        COST      PROFIT    PROF. %'
          *C101331,1 (End)
        ENDIF
      ELSE
        IF lcRpSort='S'
          *C101331,1 (Begin) Replace 'STYLE   CLR' with style header.
          *@ 05,00 SAY 'ACCOUNT .. NAME . INVOICE STYLE        CLR    &lcCostTTl  PIECES  S. PRICE   GROSS AMT     NET AMT        COST      PROFIT    PROF. %'
          @ 05,00 SAY 'ACCOUNT .. NAME . INVOICE '+lcStyTit+' &lcCostTTl  PIECES  S. PRICE   GROSS AMT     NET AMT        COST      PROFIT    PROF. %'
          *C101331,1 (End)
        ELSE
          *C101331,1 (Begin) Replace 'STYLE   CLR' with style header.
          *@ 05,00 SAY 'ACCOUNT ........ NAME ...... INVOICE STYLE        CLR    PIECES  S. PRICE   GROSS AMT     NET AMT        COST      PROFIT    PROF. %'
          @ 05,00 SAY 'ACCOUNT ........ NAME ...... INVOICE '+lcStyTit+' PIECES  S. PRICE   GROSS AMT     NET AMT        COST      PROFIT    PROF. %'
          *C101331,1 (End)
        ENDIF
      ENDIF
      @ 06,00 SAY REPLICATE('=',132)
      ROW = 07
    ENDIF

    *-- Begin Subtotals Loop 
    DO WHILE !EMPTY(BREAK)
      IF &BREAK = HBREAK
        EXIT
      ENDIF
      IF lcRpFormat='S'
        DO CASE
          CASE lcRpSort='A'
            =SEEK('M'+HBREAK,'CUSTOMER')
            @ ROW,000 SAY HBREAK
            @ ROW,008 SAY SUBSTR(CUSTOMER.BTNAME,1,25)
          CASE lcRpSort='S'
            @ ROW,005 SAY ' SUBSTR(STYLE,1,lnMajOrLen) -->  ' + HBREAK
          CASE lcRpSort='I'
            @ ROW,034 SAY HBREAK
          ENDCASE

          IF lcRpSort='S' 
            lnCostTot =  IIF (XTOT1<>0, XTOT4 / XTOT1, 0)
            @ ROW,045 SAY lnCostTot PICTURE '9999999.99'
            lnCostGTot = lnCostGTot + lnCostTot
            lnSubNum = lnSubNum + 1
          ENDIF  

          @ ROW,056 SAY XTOT1  PICTURE '9999999'
          @ ROW,065 SAY IIF(XTOT1<>0,XTOT2/XTOT1,0) PICTURE '99999.99'      && AVERAGE SELLING PRICE
          @ ROW,075 SAY XTOT2  PICTURE '9999999.99'
          @ ROW,087 SAY XTOT3  PICTURE '9999999.99'
          @ ROW,099 SAY XTOT4  PICTURE '9999999.99'
          @ ROW,111 SAY XTOT5  PICTURE '9999999.99'
          @ ROW,124 SAY IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0) PICTURE '999.99 %'
          ROW=ROW+1
       ELSE           && REPORT IS DETAIL
         ROW = ROW+1
         @ ROW,00 SAY REPLICATE('-',132)
         ROW = ROW+1
         @ ROW,000 SAY '* SUB TOTAL * '

         IF lcRpSort='A'
           =SEEK('M'+HBREAK,'CUSTOMER')
           @ ROW,015 SAY HBREAK+'   '
           @ ROW,23 SAY SUBSTR(CUSTOMER.BTNAME,1,20)
         ELSE
           @ ROW,015 SAY HBREAK
         ENDIF

         IF lcRpSort='S' 
           lnCostTot =  IIF (XTOT1<>0, XTOT4 / XTOT1, 0)
           @ ROW,045 SAY lnCostTot PICTURE '9999999.99'
           lnCostGTot = lnCostGTot + lnCostTot
           lnSubNum = lnSubNum + 1
         ENDIF  

         @ ROW,056 SAY XTOT1  PICTURE '9999999'
         @ ROW,075 SAY XTOT2  PICTURE '9999999.99'
         @ ROW,087 SAY XTOT3  PICTURE '9999999.99'
         @ ROW,099 SAY XTOT4  PICTURE '9999999.99'
         @ ROW,111 SAY XTOT5  PICTURE '9999999.99'
         @ ROW,124 SAY IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0) PICTURE '999.99 %'
         ROW = ROW+1
         @ ROW,000 SAY REPLICATE('-',132)
         ROW = ROW+1
       ENDIF
     
       XGTOT1=XGTOT1 + XTOT1
       XGTOT2=XGTOT2 + XTOT2
       XGTOT3=XGTOT3 + XTOT3
       XGTOT4=XGTOT4 + XTOT4
       XGTOT5=XGTOT5 + XTOT5

       STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5
       STORE 0 TO XTOTPRIC,XTOTINV
       lnCostTot  = 0

       HBREAK = &BREAK
       EXIT
     ENDDO
     *-- End Subtotals Loop

     IF EOF()
       EXIT
     ENDIF

     IF ROW >=53
       ROW = 99
       LOOP
     ENDIF
     SELECT (lcInvLTemp)
     =SEEK('M'+INVHDR.ACCOUNT,'CUSTOMER')
     XGROSS = TOTQTY*PRICE
     XNET   = XGROSS * (1 - (INVHDR.DISCPCNT/100))
     lnManfCost = 0
     xUnt_Cst=&lcInvLTemp..Cost
     IF lcRpSort='S' .AND. lcRpCost="M"
       *C101331,1 (Begin) Use the style major only.
       *lnManfCost = AVG_COST(Style)
       lnManfCost = AVG_COST(SUBSTR(STYLE,1,lnMajOrLen))
       *C101331,1 (End)
       xUnt_Cst   = lnManfCost
     ENDIF
     lnUntOtManf = xUnt_Cst
     XCOST   = TOTQTY * xUnt_Cst
     XPROFIT = XNET - XCOST
     XPROFP  = IIF( XNET<>0 , XPROFIT/XNET , 0)
     IF lcRpFormat='D'
       lnShift = IIF(lcRpSort='S',11,0)
       @ ROW,000 SAY ACCOUNT
       @ ROW,008 SAY SUBSTR(CUSTOMER->BTNAME,1,IIF(lcRpSort='S',9,20))
       @ ROW,029-lnShift SAY INVOICE
       @ ROW,037-lnShift SAY STYLE
       IF lcRpSort='S'
         @ ROW,057-lnShift SAY lnUntOtManf  PICTURE '999999.99'
       ENDIF
       @ ROW,056 SAY TOTQTY  PICTURE '999999'
       @ ROW,064 SAY PRICE   PICTURE '99999.99'
       @ ROW,075 SAY XGROSS  PICTURE '999999.99'
       @ ROW,087 SAY XNET    PICTURE '999999.99'
       @ ROW,099 SAY XCOST   PICTURE '999999.99'
       @ ROW,111 SAY XPROFIT PICTURE '999999.99'
       @ ROW,123 SAY (XPROFP*100)  PICTURE '999.99 %'
       ROW=ROW+1
     ENDIF

     IF lcRpSort='S'
       lnCostTot  = lnCostTot + lnUntOtManf
     ENDIF  

     XTOT1 = XTOT1 + TOTQTY
     XTOT2 = XTOT2 + XGROSS
     XTOT3 = XTOT3 + XNET
     XTOT4 = XTOT4 + XCOST
     XTOT5 = XTOT5 + XPROFIT
     XTOTPRIC=XTOTPRIC + PRICE
     XTOTINV =XTOTINV  + 1
     SELECT (lcInvLTemp)
     SKIP
   ENDDO 
   *-- END MAIN REPORT LOOP 
   ROW=ROW+1
   @ ROW,00 SAY REPLICATE('=',132)
   ROW = ROW+1
   @ ROW,000 SAY '* GRAND TOTAL *'
   IF lcRpSort='S'  &&.AND. lcRpFormat='D'
     lnCostGTot =  IIF (XGTOT1<>0, XGTOT4 / XGTOT1, 0)
     @ ROW,045 SAY lnCostGTot PICTURE '9999999.99'
   ENDIF  
   @ ROW,055 SAY XGTOT1  PICTURE '9999999'
   @ ROW,073 SAY XGTOT2  PICTURE '99999999.99'
   @ ROW,085 SAY XGTOT3  PICTURE '99999999.99'
   @ ROW,097 SAY XGTOT4  PICTURE '99999999.99'
   @ ROW,109 SAY XGTOT5  PICTURE '99999999.99'
   @ ROW,123 SAY IIF(XGTOT3<>0,(XGTOT5/XGTOT3)*100,0) PICTURE '999.99 %'
   ROW = ROW+1
   @ ROW,00 SAY REPLICATE('=',132)
   EXIT
ENDDO

*B605567,1 Remove the wait window. [Begin]
WAIT CLEAR
*B605567,1 Remove the wait window. [End]

DO ENDREPORT
SET DEVICE TO SCREEN

RETURN

*---------------------------
*   END ROB100.PRG
*---------------------------

****************************************************************************
* PROG: AVG_COST.PRG
* DESC: FUNCTION TO COMPUTE THE AVERAGE COST FOR A STYLE FROM CUTTING TICKETS
* DATE: 12/06/98
****************************************************************************
FUNCTION AVG_COST
PARAMETER XSTYLE

lnManfCost = 0
lnTotPsBud = 0
SELECT CUTTKTH
=SEEK (XSTYLE)
SCAN WHILE (STYLE=XSTYLE) FOR (STATUS $ 'ACO')
  *C101331,1 (Begin) Change the following fields as they are no longer used.
  *lnManfCost = lnManfCost + (ac_mat_cst+ ac_lab_cst)
  lnManfCost = lnManfCost + (nAct_Cost1+nAct_Cost2+nAct_Cost3+nAct_Cost4+nAct_Cost5)
  *C101331,1 (End)
  lnTotPsBud = lnTotPsBud + IIF((STATUS='C' .OR. STATUS='A'),PCS_ACT,PCS_OPN)
ENDSCAN

IF lnTotPsBud<>0
  lnManfCost = (lnManfCost/lnTotPsBud)
ELSE
  lnManfCost = 0
ENDIF

SELECT &lcInvLTemp
RETURN(lnManfCost)


****************************************************************************
* FUNC: lfwRepWhen
* DESC: To valid the OG WHEN function.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/06/98
* Refer To : (C101331)
****************************************************************************
FUNCTION lfwRepWhen

*-- Create the temp file here to be created only once each time we enter the
*-- option grid.
SELECT INVLINE
=AFIELDS(laFileStru)
CREATE DBF (gcWorkDir+lcInvLTemp) FROM ARRAY laFileStru

*-- Initilize the invdate range with system date.
lnDatePos = ASCAN(laOGFxFlt,'INVLINE.INVDATE')
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  ENDIF  
ENDIF

****************************************************************************
* FUNC: lfvAccount
* DESC: To valid the account.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/06/98
* Refer To : (C101331)
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
* FUNC: lfwOldVal
* DESC: To get the old value.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/06/98
* Refer To : (C101331)
****************************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))

****************************************************************************
* FUNC: lfvStyle
* DESC: To valid the style.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/06/98
* Refer To : (C101331)
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
* FUNC: lfvSortBy
* DESC: To clear read in case of sorting by style to get the cost.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/06/98
* Refer To : (C101331)
****************************************************************************
FUNCTION lfvSortBy

IF (lcRpSort = 'S') OR (lcOldVal = 3)
  CLEAR READ
ENDIF  

****************************************************************************
* FUNC: lfFillArr
* DESC: To fill the sort by arrays.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 12/07/98
* Refer To : (C101331)
****************************************************************************
FUNCTION lfFillArr

DIMENSION laSortDesc[3,1]
DIMENSION laSortVal [3,1]
laSortDesc[1,1] = 'Invoice'
laSortDesc[2,1] = 'Account'
laSortDesc[3,1] = lcMajTitle
laSortVal[1,1]  = 'I'
laSortVal[2,1]  = 'A'
laSortVal[3,1]  = 'S'
