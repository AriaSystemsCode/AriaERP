*:***************************************************************************
*: Program file  : SRSIMN10
*: Program desc. : Sales Representative commission work sheet Report for simon
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Sales Representative (SR )
*: Developer     : Mohamed Shokry (MHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : RPT_HDR,ENDREPORT
*:    Functions  : lfwRepWhen,lfwOldVal,lfvRepCode,lfvDate
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : - This is the same code as 2.6 (@x,y SAY) but after making some modifications.
*:         : - We have several transaction types we seek repcomm for it, like
*:             TranTypes      Description
*:                     1      INVOICE
*:                     2      PAYMENTS
*:                     3      DEBIT ADJUSTMENT
*:                     4      CREDIT ADJUSTMENT
*:                     5      RETURN  (CREDIT MEMO)
*:                     6      VOID INVOICE
*:                     7      VOID CREDIT MEMO
*:***************************************************************************
*: Example : DO SRREPSH
*:***************************************************************************
*: This Report Program is due to C200244 ...
*:***************************************************************************
*: Modifications:
*:B605353,1 MHM   01/07/2002 Trade Dic. does not appear in case on Credit memo  
*:***************************************************************************
*
*-------- Assiging 2.6 variables with its eq. in 2.7 OG  [begin ] --------
*-- Eval. From and To dates. [begin] 
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.DATE'),1)
LDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,;
            ATC('|',laOGFxFlt[lnDatePos,6])-1))

HDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],;
            ATC('|',laOGFxFlt[lnDatePos,6])+1))
*-- Eval. From and To dates. [end] 

*-- Eval. sales rep. [begin] 
lnRepPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SALESREP.REPCODE'),1)
HSALESREP = laOGFxFlt[lnRepPos,6]
*-- Eval. sales rep. [end] 

XSTATUS   = IIF(lcRpStatus = 'B','OH',lcRpStatus)  && InvHdr Status.
*-------- Assiging 2.6 variables with its eq. in 2.7 OG  [end ] --------

DIMENSION XTOTAL(2,3)
XTOTAL=0

COMFILTER = 'AMOUNT<>0 .AND.BETWEEN(DATE,LDATE,HDATE).AND.;
             STATUS $ ALLTRIM(XSTATUS)'
lcFilter = IIF(!EMPTY(HSALESREP), 'REPCODE = HSALESREP', '.T.')

*--Support multi currency format and add currecy to filter. 
IF llMultCurr 
  lcCurFiltr     = laOGFxFlt[lnCurrPos,6]
  IF lnCurrPos     > 0
    lnCurrPos1   = AT('AND' , SUBSTR(lcRpExp,lnCurrPos))
    IF lnCurrPos1  > 0
      lcFilter   = IIF(EMPTY(lcCurFiltr),lcFilter ,lcFilter + ' AND ' + SUBSTR(lcRpExp ,lnCurrPos, lnCurrPos1-1))
    ELSE
      lnCurrPos1 = LEN(lcRpExp)
      lcFilter   = IIF(EMPTY(lcCurFiltr),lcFilter ,lcFilter+ ' AND ' + SUBSTR(lcRpExp ,lnCurrPos, lnCurrPos1))
    ENDIF
  ENDIF  
ENDIF  
    
FROMDATE = DTOC(LDATE)
THRUDATE = DTOC(HDATE)
PERIOD   = 'PERIOD: &FROMDATE - &THRUDATE'

*------ Collecting data  [Begin ] --------
WAIT WINDOW 'Locating records in file....' NOWAIT
SELECT REPCOMM                       && ADDED BY RAA ON 03/03/1993
LOCATE FOR &COMFILTER AND &lcFilter
IF EOF('REPCOMM')
  =gfModalGen(.f.,.f.,.f.,.f.,'There are no records to display...! ')  
  SET DEVICE TO SCREEN
  lcLastExpr = lcRpExp
  RETURN
ELSE  

  IF llClearFn OR llOGFltCh
    IF USED(RepTemp)
      USE IN (RepTemp)
    ENDIF
    COPY REST TO (gcWorkDir+RepTemp) FOR &ComFilter AND &lcFilter
    USE (gcWorkDir+RepTemp) IN 0  
    SELECT (RepTemp)
    llClearFn  = .F.
  ELSE 
    SELECT (RepTemp)
    GO TOP
  ENDIF  
ENDIF

*------ Collecting data  [End ] --------
*------------------- END REPORT SELECTION ---------------------

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
*SRREPSH                       SALESREP COMMISSION WORKSHEET REPORT
*
*
*SALESREP: 123 12345678901234567890
*
* Data printed from commission file
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
*MM/DD/YY 123456 123456789012345 12345 12345678 12345678 123456 1234567890 99.99  99.99   99.99  1234567.99 1234567.99 _______________
*  DATE   TRAN#  .....DESC...... ACCT# NAME     STORE#  ORDER# CUST PO    PCNT  TRD DSC MER DSC NET-SHIP   COMMISSION AMOUNT-TO-PAY
*
* Data printed from order file for advance payments
* BOOKED   ST    ORDER#   START  COMPLETE    ACCT# .....NAME.....  STORE#   PCNT  AMT-BOOKED COMMISSION    ADVANCE  AMOUNT-TO-PAY
*MM/DD/YY        123456 MM/DD/YY MM/DD/YY    12345 123456789012345 12345678 12.34 1234567.99 1234567.99 1234567.99  _______________
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
A='SRREPSH                                        SALESREP COMMISSION WORKSHEET REPORT'
B='  DATE   TRAN#  .....DESC... ACCT# NAME....  STORE#   ORDER# CUST PO         PCNT TRD DSC MER DSC     NET-SHIP COMMISS. AMNT TO PAY'

*-- Sort Temp. Sales comm file by user selected choice [begin]
CHOICE = lcRpSortBy

*--Support multi currency format and add currecy to Sorting. 
IF llMultCurr .AND.  lcRpCurr = "F" 
  DO CASE
    CASE CHOICE='I'
      SORTFIELD = 'REPCODE+CCURRCODE+TRAN+STR(RECNO(),7)'
    CASE CHOICE='O'
      SORTFIELD = 'REPCODE+CCURRCODE+ORDER+TRAN+STR(RECNO(),7)'
    CASE CHOICE='C'
      SORTFIELD = 'REPCODE+CCURRCODE+CUSTPO+TRAN+STR(RECNO(),7)'
    CASE CHOICE='D'                    && ADDED BY RAA ON 03/03/1993
      SORTFIELD = 'REPCODE+CCURRCODE+DTOS(DATE)+TRAN+STR(RECNO(),7)' 
  ENDCASE
ELSE
  DO CASE
    CASE CHOICE='I'
      SORTFIELD = 'REPCODE+TRAN+STR(RECNO(),7)'
    CASE CHOICE='O' 
      SORTFIELD = 'REPCODE+ORDER+TRAN+STR(RECNO(),7)'
    CASE CHOICE='C'
      SORTFIELD = 'REPCODE+CUSTPO+TRAN+STR(RECNO(),7)'
    CASE CHOICE='D'                    && ADDED BY RAA ON 03/03/1993
      SORTFIELD = 'REPCODE+DTOS(DATE)+TRAN+STR(RECNO(),7)'
  ENDCASE
ENDIF   

DO CASE
  CASE lnRpDeclNo = 0
    lcShpFormt = '@Z 99999999999'
    lcAmtFormt = '@Z 999999999'    
  CASE lnRpDeclNo = 1
    lcShpFormt = '@Z 999999999.9'
    lcAmtFormt = '@Z 9999999.9'    
  OTHERWISE
    lcShpFormt = '@Z 99999999.99'
    lcAmtFormt = '@Z 999999.99'    
ENDCASE

WAIT WINDOW 'Sorting ' + LTRIM(STR(RECCOUNT(),9)) + ' Commission records ...' NOWAIT
SELECT (RepTemp)
INDEX ON &SORTFIELD TAG &REPTEMP
*-- Sort Temp. Sales comm file by user selected choice [begin]

IF HSALESREP = ' '
  SELECT SALESREP
  GOTO TOP
  HSALESREP = CHR(254)
ENDIF

SELECT &RepTemp
IOCOM = SELECT()

lcRepCurr = cCurrCode

*---------------------------------------------------------------
* [REPORT] PRINT COMMISSION WORKSHEET REPORT
*---------------------------------------------------------------
ROW    = 99
NEWREP = .T.
PAGENO = 0
SET DEVICE TO PRINT
STORE 0 To lnNetShip , lnAmount
*- Get tax rate
lnTaxRat = gfGetMemVar('M_TAX_RATE',gcAct_Comp)

DO WHILE .T.

   SELECT SALESREP
   XSALESREP = REPCODE
   
   lcSaleName = Name
   
   IF EOF() .OR. REPCODE>HSALESREP
      EXIT
   ENDIF

   IF NEWREP
      
      IF (HSALESREP == XSALESREP) OR EMPTY(HSALESREP)
        WAIT WINDOW 'Sales commissiom for sales representative ' + XSALESREP NOWAIT
      ENDIF
      
      STORE .F. TO GOTCOM, GOTORD, PTOTAL1, PTOTAL2
      ROW     = 99
      XTOTAL  = 0.00
      CURFILE = '&REPTEMP'

      SELECT &REPTEMP
      SEEK XSALESREP
      GOTCOM = IIF(FOUND(), .T., .F.)
      IF .NOT.GOTCOM
         SELECT SALESREP
         SKIP
         LOOP
      ENDIF
   ENDIF

   NEWREP = .F.

   IF ROW >= 45
     R_TITLE ='SALESREP COMMISSION WORKSHEET'
     XTITLE  = PERIOD
     PAGENO  = PAGENO + 1
     DO RPT_HDR WITH 'SRREPSH',XTITLE,R_WIDTH
     ROW     = 5
     NEWPAGE = .T.
   ENDIF

   *-- [COMMISSION] LOOP ALL RECORDS ON TEMP. COMM FILE FOR THIS REP
   DO WHILE GOTCOM
     SELECT &REPTEMP
     IF llMultCurr .AND. lcRepCurr <> cCurrCode .AND. lcRpCurr = "F"
       =lfSubTot()
       lcRepCurr = cCurrCode
     ENDIF
     IF NEWPAGE
       NEWPAGE = .F.
       @ ROW,01 SAY 'SALESREP: '+XSALESREP+'  '+SALESREP->NAME
       STORE 0.00 TO TSHIPAMT,TCOMMDUE,TBAKORD
       ROW = ROW+2
       @ ROW,00 SAY B
       ROW = ROW+2
     ENDIF

     SELECT &REPTEMP
     IF EOF() .OR. (REPCODE <> XSALESREP)
       GOTCOM  = .F.
       PTOTAL1 = .T.
       NEWPAGE = .T.
       EXIT
     ENDIF

     *-- TEST/PRINT HEADINGS
     IF ROW > 55
       EXIT
     ENDIF

     *----------------------------------------------------------------
     * If the current record on Salescom is a summary, and the user
     * has requested detail history, switch to Saleshst.dbf
     *----------------------------------------------------------------
     SELECT &RepTemp
     CURFILE = '&REPTEMP'
     XTERMSPCT = 00.00

     *:B605353,1 MHM   01/07/2002 Trade Dic. does not appear in case on Credit memo       

     XTERMSPCT =lnRpTrade
    
     *:B605353,1 MHM   01/07/2002 Trade Dic. does not appear in case on Credit memo       
     SELECT INVHDR
     SEEK &REPTEMP..Tran
     lnDisc = 00.00
     IF FOUND() AND &REPTEMP..TranType $ '16'
       lnDisc = DISCPCNT

       IF &REPTEMP..TRANTYPE='1'  AND InvHdr.Consol = "Y" 
         SELECT ConsInvh
         SEEK(InvHdr.Invoice+&RepTemp..Store+&RepTemp..Order)
         XTERMSPCT =lnRpTrade
         SELECT InvHdr
         XNETSHIP = ConsInvh.SHIPAMT + ConsInvh.DISCOUNT
       ENDIF
      
       IF &REPTEMP..TRANTYPE='1'  AND InvHdr.Consol <> "Y" 
         XTERMSPCT =lnRpTrade
         XNETSHIP = IIF(INVHDR.STATUS = 'V',INVHDR.VSHIPAMT+;
                        INVHDR.VDISCOUNT,INVHDR.SHIPAMT + INVHDR.DISCOUNT)
       ENDIF

       IF &REPTEMP..TRANTYPE='6'  AND InvHdr.Consol = "Y" 
         SELECT ConsInvh
         SEEK(InvHdr.Invoice+&RepTemp..Store+&RepTemp..Order)
         XTERMSPCT =lnRpTrade
         SELECT InvHdr
         XNETSHIP = (ConsInvh.SHIPAMT + ConsInvh.DISCOUNT)* -1
       ENDIF
      
       IF &REPTEMP..TRANTYPE='6'  AND InvHdr.Consol <> "Y" 
         XTERMSPCT  = lnRpTrade
         XNETSHIP = (INVHDR.VSHIPAMT + INVHDR.VDISCOUNT)* -1
       ENDIF

       IF XTERMSPCT <>0
         XNETSHIP = XNETSHIP - (XNETSHIP * XTERMSPCT/100)
       ENDIF
     ELSE
      
       *** 1) Print the returns in a -ve sign and the void returns
       ***    in a +ve sign
       *** 2) Print the VAMOUNT field instead of AMOUNT field if
       ***    the status of the returns is 'V' (voided)

       *-- Check for Return Merchandise module. [MAB 10/17/1998 BEGIN]
       IF ('RM' $ gcCmpModules) AND SEEK(&REPTEMP->TRAN,'RETHDR')

         XNETSHIP = IIF(RETHDR.STATUS='V',RETHDR.VAMOUNT,RETHDR.AMOUNT)

         *B605353,3
         *IF SEEK(&REPTEMP..Account+&REPTEMP..TRAN,'Credit')
         *  XNETSHIP = XNETSHIP - Credit.Dsc_Amt
         *ENDIF
         *B605353,3
         XNETSHIP = XNETSHIP * IIF(&REPTEMP..TRANTYPE='5',-1,1)
      
       ELSE
         XNETSHIP = IIF(&REPTEMP->COMMPCNT<>0,;
                       (&REPTEMP->AMOUNT/(&REPTEMP->COMMPCNT/100)),0)

         lnAlias = ALIAS()
         SELECT (REPTEMP)
         FOR lnCount = 1 TO FCOUNT()
           IF FIELD(lnCount) = 'ORGNL_CADJ' AND ORGNL_CADJ<>0
             XNETSHIP = ORGNL_CADJ
             EXIT
           ENDIF  
         ENDFOR
         SELECT (lnAlias)
       ENDIF
       
       *--605353,4
       IF XTERMSPCT <>0
         XNETSHIP = XNETSHIP - (XNETSHIP * XTERMSPCT/100)
       ENDIF
       *--605353,4
    
     ENDIF

     SELECT CUSTOMER
     SEEK 'M'+&REPTEMP->ACCOUNT
     XNAME = IIF(FOUND(), SUBSTR(BTNAME,1,15), '')
    
     SELECT &REPTEMP

     lnNetShip     = IIF(llMultCurr,lfBaseAmt(XNETSHIP),XNETSHIP)

     *B605353,3
     *--200244,4 cmment 
     *lnAmount      = IIF(llMultCurr,lfBaseAmt(nforamnt),AMOUNT)
     *IF trantype = '1'
     *  lnTotalChg     =  IIF(llMultCurr,lfBaseAmt(InvHdr.shipamt),InvHdr.shipamt)
     *  lnDiscont       =  IIF(llMultCurr,lfBaseAmt(InvHdr.discount),InvHdr.discount)
     *  lnNwAmount  =  (lnTotalChg+lnDiscont)  - (lnTotalChg+lnDiscont)   *lnRpTrade/100
     *  lnAmount       = lnNwAmount *commpcnt/100
     *ELSE
     *  IF TranType = '5'
     *    lnTotalChg     =  IIF(llMultCurr,lfBaseAmt(RETHDR.AMOUNT),RETHDR.AMOUNT)
     *    lnDiscont       =  IIF(llMultCurr,lfBaseAmt(RETHDR.disc_amt),RETHDR.disc_amt)
     *    lnNwAmount  =  (lnTotalChg+lnDiscont)  - (lnTotalChg+lnDiscont)   *lnRpTrade/100
     *    lnAmount       = (lnNwAmount *commpcnt/100)*-1
     *  ELSE
     *    lnAmount      = IIF(llMultCurr,lfBaseAmt(nforamnt),AMOUNT)
     *  ENDIF
     *ENDIF  

     DO CASE
      CASE TranType = '1'    && invoice
          IF InvHdr.Status = 'V'
            lnNetChrg     =  InvHdr.vTotalChg - (InvHdr.nvCharges+InvHdr.vTax_Amt)
            lnTotalChg    =  IIF(llMultCurr,lfBaseAmt(lnNetChrg),lnNetChrg)
            *lnDiscont    =  IIF(llMultCurr,lfBaseAmt(InvHdr.discount),InvHdr.discount)
            *lnNwAmount   =  (lnTotalChg+lnDiscont)  - (lnTotalChg+lnDiscont)*lnRpTrade/100
            lnNwAmount    =  lnTotalChg  - lnTotalChg*lnRpTrade/100
            lnAmount      = lnNwAmount *commpcnt/100
          ELSE
            lnNetChrg     =InvHdr.TotalChg - (InvHdr.nCharges+InvHdr.Tax_Amt)
            lnTotalChg    =  IIF(llMultCurr,lfBaseAmt(lnNetChrg),lnNetChrg)
            *lnDiscont     =  IIF(llMultCurr,lfBaseAmt(InvHdr.discount),InvHdr.discount)
            *lnNwAmount    =  (lnTotalChg+lnDiscont)  - (lnTotalChg+lnDiscont)*lnRpTrade/100
            lnNwAmount    =  lnTotalChg  - (lnTotalChg)*lnRpTrade/100
            lnAmount      = lnNwAmount *commpcnt/100
          ENDIF  

      CASE TranType = '5'    && Credit Memo
          IF RETHDR.Status = 'V'
            lnTotalChg     =  IIF(llMultCurr,lfBaseAmt(RETHDR.VAMOUNT),RETHDR.VAMOUNT)
            *lnDiscont      =  IIF(llMultCurr,lfBaseAmt(RETHDR.Vdisc_amt),RETHDR.Vdisc_amt)
            *lnNwAmount     =  (lnTotalChg+lnDiscont)  - (lnTotalChg+lnDiscont)*lnRpTrade/100
            lnNwAmount     =  lnTotalChg - (lnTotalChg)*lnRpTrade/100
            lnAmount       = (lnNwAmount *commpcnt/100)*-1
          ELSE
            lnTotalChg     =  IIF(llMultCurr,lfBaseAmt(RETHDR.AMOUNT),RETHDR.AMOUNT)
            *lnDiscont      =  IIF(llMultCurr,lfBaseAmt(RETHDR.disc_amt),RETHDR.disc_amt)
            *lnNwAmount     =  (lnTotalChg+lnDiscont)  - (lnTotalChg+lnDiscont)*lnRpTrade/100
            lnNwAmount     =  lnTotalChg  - (lnTotalChg)*lnRpTrade/100
            lnAmount       = (lnNwAmount *commpcnt/100)*-1
          ENDIF 
      
      CASE TranType = '6'    && Void Invoice
          lnNetChrg     =InvHdr.vTotalChg - (InvHdr.nvCharges+InvHdr.vTax_Amt)
          lnTotalChg     =  IIF(llMultCurr,lfBaseAmt(lnNetChrg),lnNetChrg)
          *lnDiscont      =  IIF(llMultCurr,lfBaseAmt(InvHdr.discount),InvHdr.discount)
          *lnNwAmount     =  (lnTotalChg+lnDiscont)  - (lnTotalChg+lnDiscont)*lnRpTrade/100
          lnNwAmount     =  lnTotalChg  - (lnTotalChg)*lnRpTrade/100
          IF RepCode = InvHdr.Rep1
            lnAmount       = (lnNwAmount *InvHdr.comm1/100)*-1
          ELSE
           IF RepCode = InvHdr.Rep2 
              lnAmount       = (lnNwAmount *InvHdr.comm2/100)*-1
           ENDIF
          ENDIF

      CASE TranType = '7'    && Void CM 
          lnTotalChg     =  IIF(llMultCurr,lfBaseAmt(RETHDR.VAMOUNT),RETHDR.VAMOUNT)
          *lnDiscont      =  IIF(llMultCurr,lfBaseAmt(RETHDR.Vdisc_amt),RETHDR.Vdisc_amt)
          *lnNwAmount     =  (lnTotalChg+lnDiscont)  - (lnTotalChg+lnDiscont)*lnRpTrade/100
          lnNwAmount     =  lnTotalChg - (lnTotalChg)*lnRpTrade/100
          lnAmount       = (lnNwAmount *commpcnt/100)

     ENDCASE
     *B605353,3
     *--200244,4

     @ ROW,00 SAY DATE
     @ ROW,09 SAY TRAN
     @ ROW,16 SAY SUBSTR(DESC,1,13)
     @ ROW,29 SAY ACCOUNT

     @ ROW,35 SAY SUBSTR(XNAME,1,9)    
     @ ROW,45 SAY STORE
     @ ROW,54 SAY ORDER
     @ ROW,61 SAY LEFT(ALLTRIM(CUSTPO),14)
     @ ROW,76 SAY COMMPCNT             PICTURE '@Z 99.99'
     @ ROW,84 SAY XTERMSPCT            PICTURE '@Z 999.99'
     @ ROW,91 SAY lnDisc               PICTURE '@ 99.99%'
     @ ROW,99 SAY lnNetShip            PICTURE lcShpFormt
     @ ROW,109 SAY lnAmount             PICTURE lcAmtFormt

     @ ROW,120 SAY IIF(llMultCurr .AND. lcRpCurr = "F",ALLTRIM(cCurrCode),ALLTRIM(gcBaseCurr))
     @ ROW,123 SAY  '_________'

     XTOTAL(1,1) = XTOTAL(1,1) + lnNetShip
     XTOTAL(1,3) = XTOTAL(1,3) + lnAmount
    
     ROW=ROW+1
     SELECT &REPTEMP
     SKIP
   ENDDO
   IF llMultCurr .AND. lcRpCurr = "F"  .AND. !GOTCOM .AND. !EOF() 
     =lfSubTot()
     lcRepCurr = cCurrCode
   ENDIF

   IF PTOTAL1 .AND. lcRpCurr <> "F" 
     PTOTAL1 = .F.
     ROW = ROW+1

     @ ROW,020 SAY XSALESREP + ' TOTALS ------------------------------------------------->'
      DO CASE
        CASE lnRpDeclNo = 0
          @ ROW,98 SAY XTOTAL(1,1)  PICTURE '@Z 99999999999'
          @ ROW,110 SAY XTOTAL(1,3) PICTURE '@Z 999999999'
        CASE lnRpDeclNo = 1
          @ ROW,98 SAY XTOTAL(1,1)  PICTURE '@Z 999999999.9'
          @ ROW,110 SAY XTOTAL(1,3) PICTURE '@Z 9999999.9'
        OTHERWISE
          @ ROW,98 SAY XTOTAL(1,1)  PICTURE '@Z 99999999.99'
          @ ROW,110 SAY XTOTAL(1,3) PICTURE '@Z 999999.99'
      ENDCASE
     
     @ ROW,120 SAY IIF(llMultCurr .AND. lcRpCurr = "F",ALLTRIM(cCurrCode),ALLTRIM(gcBaseCurr))
     @ ROW,123 SAY  '_________'
    
     ROW = ROW+1

     *print in case of vatable
     IF SALESREP.LVatable

      DO CASE
        CASE lnRpDeclNo = 0
          @ ROW,094 SAY "VAT @ 17.5%"
          @ ROW,110 SAY  (XTOTAL(1,3)/100)*lnTaxRat PICTURE '@Z 999999999'
          ROW = ROW+1
          @ ROW,094 SAY "Total Commission"
          @ ROW,110 SAY XTOTAL(1,3)+(XTOTAL(1,3)/100)*lnTaxRat PICTURE '@Z 999999999'
          ROW = ROW+1
          
        CASE lnRpDeclNo = 1
          @ ROW,094 SAY "VAT @ 17.5%"
          @ ROW,110 SAY  (XTOTAL(1,3)/100)*lnTaxRat PICTURE '@Z 9999999.9'
          ROW = ROW+1
          @ ROW,094 SAY "Total Commission"
          @ ROW,110 SAY XTOTAL(1,3)+(XTOTAL(1,3)/100)*lnTaxRat PICTURE '@Z 9999999.9'
          ROW = ROW+1
        
        OTHERWISE
          @ ROW,094 SAY "VAT @ 17.5%"
          @ ROW,110 SAY  (XTOTAL(1,3)/100)*lnTaxRat PICTURE '@Z 999999.99'
          ROW = ROW+1
          @ ROW,094 SAY "Total Commission"
          @ ROW,110 SAY XTOTAL(1,3)+(XTOTAL(1,3)/100)*lnTaxRat PICTURE '@Z 999999.99'
          ROW = ROW+1
      ENDCASE
     ENDIF  
     
     @ ROW,00 SAY REPLICATE('-',132)
     ROW = ROW+2
   ENDIF
   *--------------------- END COMMISSION LOOP ---------------------
  
   IF .NOT. GOTCOM
     NEWREP = .T.
     SELECT SALESREP
     SKIP
   ENDIF
ENDDO
*------------------------ END REPORT LOOP -------------------

DO ENDREPORT
SET DEVICE TO SCREEN
lcLastExpr = lcRpExp
RETURN
*------------------------
*   END SRREPSH.PRG
*------------------------

*------- Functions section ---------
*----------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/29/2001
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
R_WIDTH    = 'W'
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.DATE'),1)
IF EMPTY(laOGFxFlt[lnDatePos,6])
  laOGFxFlt[lnDatePos,6] = DTOC(DATE() - DAY(DATE()) - (DAY(DATE() - DAY(DATE()))-1))+;
                           '|'+DTOC(DATE() - DAY(DATE()))
ENDIF

IF llMultCurr
  lnCurrPos  = lfItmPos('REPCOMM.CCURRCODE')
ELSE
  lcRpCurr = "O"
ENDIF  

*-- End of lfwRepWhen.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/29/2001
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
*! Name      : lfvRepCode
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/29/2001
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
*! Name      : lfvDate
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/29/2001
*! Purpose   : Validate Entered date [i.e. Not empty]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid [Period Option]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDate()
*!*************************************************************
FUNCTION lfvDate
IF EMPTY(EVALUATE(SYS(18)))
  WAIT WINDOW 'You must fill period range...' NOWAIT
  lcCurrObj = SYS(18)
  &lcCurrObj = laOldVal
  SHOW GET lcCurrObj
  _CUROBJ = _CUROBJ
ENDIF
*-- end of lfvDate.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/29/2001
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
llClearFn = .T.  &&You erase temporary file.
*-- Close temp. opended files, if it used.

*-- Delete temporary work file.
IF USED(RepTemp)
 USE IN (RepTemp)
 ERASE (gcWorkDir+RepTemp+'.DBF')
 ERASE (gcWorkDir+RepTemp+'.CDX')
ENDIF

IF llOpenComp AND USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign

  IF llOpenInt AND USED("SYCINT")
    USE IN SYCINT 
  ENDIF

  IF llOpenCurr AND USED("SYCCURR")
    USE IN SYCCURR
  ENDIF

  IF llOpenExch AND USED("SYCEXCH")
    USE IN SYCEXCH
  ENDIF  
ENDIF

*-- end of lfClearRep.
*!*************************************************************
*! Name      : lfFillVars
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/29/2001
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
*E301424,1
FUNCTION lfFillVars

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  
IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF

*-- End Of lfFillVars.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/29/2001
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*E301424,1
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End Of lfItmPos.
*!*************************************************************
*! Name      : lfBaseAmt
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/29/2001
*! Purpose   : Compute base amount
*!*************************************************************
*! Example   : = lfBaseAmt()
*!*************************************************************
*E301424,1
FUNCTION lfBaseAmt
PARAMETERS lnAmntCurr
PRIVATE lnBaseAmt

lnBaseAmt = lnAmntCurr
*-- if Multi currency and user want to print in base currency and 
*-- currency not the base currency.
IF llMultCurr .AND. lcRpCurr <> "F" lnBaseAmt <> 0
  lnBaseAmt = gfAmntDisp(lnBaseAmt,lcRpCurr,ldRpExDate,lcRpTmpNam)
ENDIF
RETURN lnBaseAmt

*-- End of lfBaseAmt.
*!*************************************************************
*! Name      : lfSubTot
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/29/2001
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Example     : =lfSubTot()
*!*************************************************************
*!
FUNCTION lfSubTot

@ ROW,00 SAY REPLICATE('-',132)
ROW = ROW+1
@ ROW,00 SAY " *  SUB TOTAL *            CURRENCY    "+lcRepCurr +'    '+lcSaleName

DO CASE
  CASE lnRpDeclNo = 0
    @ ROW,098 SAY XTOTAL(1,1) PICTURE '@Z 999999999999'
    @ ROW,111 SAY XTOTAL(1,3) PICTURE '@Z 9999999999'
  CASE lnRpDeclNo = 1
    @ ROW,098 SAY XTOTAL(1,1) PICTURE '@Z 9999999999.9'
    @ ROW,111 SAY XTOTAL(1,3) PICTURE '@Z 99999999.9'
  OTHERWISE
    @ ROW,098 SAY XTOTAL(1,1) PICTURE '@Z 999999999.99'
    @ ROW,111 SAY XTOTAL(1,3) PICTURE '@Z 9999999.99'
ENDCASE

ROW = ROW+1
@ ROW,00 SAY REPLICATE('-',132)
ROW = ROW+2
XTOTAL  = 0.00

*-- End Of lfSubTot.
*!*************************************************************
